/************************************************************************

*************************************************************************/
#define INITGUID

#include "Driver.h"

#include <Ntddstor.h>
#include <Wdmguid.h>

WCHAR                   deviceNameBuffer[]  = L"\\Device\\diskFilter";
WCHAR                   deviceLinkBuffer[]  = L"\\DosDevices\\diskFilter";

PDEVICE_OBJECT gControlDeviceObject;

PDEVICE_OBJECT gDiskFilterCdo;
PDRIVER_OBJECT gDiskFilterDriver;
PVOID gNotificationEntry;
PDEVICE_OBJECT gLowerDeviceObject[1024];
UINT32 gLowerDevObjCount = 0;
PDEVICE_OBJECT gFilterDeviceObject[1024];
UINT32 gFilterDevObjCount = 0;

NTSTATUS
FgDeviceIrpCompletion(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
    )

/*++

Routine Description:

    Forwarded IRP completion routine. Set an event and return
    STATUS_MORE_PROCESSING_REQUIRED. Irp forwarder will wait on this
    event and then re-complete the irp after cleaning up.

Arguments:

    DeviceObject is the device object of the WMI driver
    Irp is the WMI irp that was just completed
    Context is a PKEVENT that forwarder will wait on

Return Value:

    STATUS_MORE_PORCESSING_REQUIRED

--*/

{
    PKEVENT Event = (PKEVENT) Context;

    UNREFERENCED_PARAMETER(DeviceObject);
    UNREFERENCED_PARAMETER(Irp);

    KeSetEvent(Event, IO_NO_INCREMENT, FALSE);

    return(STATUS_MORE_PROCESSING_REQUIRED);
}


NTSTATUS
FgDeviceForwardIrpSynchronous(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )

/*++

Routine Description:

    This routine sends the Irp to the next driver in line
    when the Irp needs to be processed by the lower drivers
    prior to being processed by this one.

Arguments:

    DeviceObject
    Irp

Return Value:

    NTSTATUS

--*/

{
    PFG_DEVICE_EXTENSION   deviceExtension;
    KEVENT event;
    NTSTATUS status;

    KeInitializeEvent(&event, NotificationEvent, FALSE);
    deviceExtension = (PFG_DEVICE_EXTENSION) DeviceObject->DeviceExtension;

    //
    // copy the irpstack for the next device
    //

    IoCopyCurrentIrpStackLocationToNext(Irp);

    //
    // set a completion routine
    //

    IoSetCompletionRoutine(Irp, FgDeviceIrpCompletion,
                            &event, TRUE, TRUE, TRUE);

    //
    // call the next lower device
    //

    status = IoCallDriver(deviceExtension->TargetDeviceObject, Irp);

    //
    // wait for the actual completion
    //

    if (status == STATUS_PENDING) {
        KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
        status = Irp->IoStatus.Status;
    }

    return status;
}

NTSTATUS
FgDeviceSendToNextDriver(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PFG_DEVICE_EXTENSION   deviceExtension;

    IoSkipCurrentIrpStackLocation(Irp);
    deviceExtension = (PFG_DEVICE_EXTENSION) DeviceObject->DeviceExtension;

    return IoCallDriver(deviceExtension->TargetDeviceObject, Irp);
}

NTSTATUS createDevAndAttach(PDEVICE_OBJECT deviceObject,
							IN PFILE_OBJECT fileObject)
{
	NTSTATUS status;
	PFG_DEVICE_EXTENSION deviceExtension = NULL;
	PDEVICE_OBJECT newDevObj = NULL;

	ASSERT( deviceObject != NULL );

	KdPrint(("  Enter createDevAndAttach(), gFilterDevObjCount(%d)\n", gFilterDevObjCount));
	
	status = IoCreateDevice(gDiskFilterDriver,
                            sizeof(FG_DEVICE_EXTENSION),
                            NULL,
                            FILE_DEVICE_DISK,
                            FILE_DEVICE_SECURE_OPEN,
                            FALSE,
                            &newDevObj);
	if( !NT_SUCCESS(status) ) {
		KdPrint(("  IoCreateDevie() failed.status(0x%x)\n", status));
		return status;
	}

	gFilterDeviceObject[gFilterDevObjCount++] = newDevObj;

    deviceExtension = (PFG_DEVICE_EXTENSION) newDevObj->DeviceExtension;
    RtlZeroMemory(deviceExtension, sizeof(FG_DEVICE_EXTENSION));

	deviceExtension->PhysicalDeviceObject = deviceObject;
	deviceExtension->FileObject = fileObject;
    deviceExtension->TargetDeviceObject = IoAttachDeviceToDeviceStack(newDevObj, deviceObject);

	KdPrint(("  IoCreateDevice() success. newDebObj(0x%x), targetDevObj(0x%x)\n", newDevObj, deviceExtension->TargetDeviceObject));
	deviceExtension->DeviceObject = newDevObj;

	newDevObj->Flags |=  (deviceObject->Flags & (DO_BUFFERED_IO | DO_DIRECT_IO));
	newDevObj->Flags |=  (deviceObject->Flags & DO_POWER_PAGABLE);

    newDevObj->Flags &= ~DO_DEVICE_INITIALIZING;

	KdPrint(("  Leave createDevAndAttach()\n"));

	return STATUS_SUCCESS;
}

NTSTATUS notifyCallback(
	IN PVOID NotificationStructure,
	IN PVOID Context
)
{
	NTSTATUS status;
	PDEVICE_INTERFACE_CHANGE_NOTIFICATION pDevInfChange;
	PUNICODE_STRING ObjectName = NULL;
	PFILE_OBJECT fileObject = NULL;
	PDEVICE_OBJECT deviceObject = NULL;
	PDEVICE_OBJECT topDevObj = NULL;

	pDevInfChange = (PDEVICE_INTERFACE_CHANGE_NOTIFICATION)NotificationStructure;

	ObjectName = pDevInfChange->SymbolicLinkName;
	ASSERT(ObjectName != NULL);
	KdPrint(("  [notifyCallback]: symbolicName:%wZ \n", pDevInfChange->SymbolicLinkName));

	if( IsEqualGUID(&(pDevInfChange->Event), &GUID_DEVICE_INTERFACE_ARRIVAL) ) {
		KdPrint((" enter nofifyCallBask==>  INTERFACE_ARRIVAL \n"));

		status = IoGetDeviceObjectPointer( ObjectName,
			FILE_READ_DATA,
			&fileObject,
			&deviceObject);
		if( !NT_SUCCESS(status) ) {
			KdPrint(("  IoGetDeviceObjectPointer() failed. status(%x)\n", status));
			return status;
		}
		KdPrint(("  IoGetDeviceObjectPointer() success. deviceobject(0x%x), driverName(%wZ)\n", deviceObject, &deviceObject->DriverObject->DriverName));

		//
		// Create filter device object and attach it to the device stack
		//
		status = createDevAndAttach(deviceObject, fileObject);
	} 
	if( IsEqualGUID(&(pDevInfChange->Event), &GUID_DEVICE_INTERFACE_REMOVAL) ) {
		KdPrint((" enter nofifyCallBask==>  GUID_DEVICE_INTERFACE_REMOVAL \n"));
	}

	return STATUS_SUCCESS;
}

NTSTATUS FgDeviceSystemControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PFG_DEVICE_EXTENSION  deviceExtension = DeviceObject->DeviceExtension;

    IoSkipCurrentIrpStackLocation(Irp);
    return IoCallDriver(deviceExtension->TargetDeviceObject, Irp);
}

NTSTATUS
FgDeviceShutdownFlush(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )

/*++

Routine Description:

    This routine is called for a shutdown and flush IRPs.  These are sent by the
    system before it actually shuts down or when the file system does a flush.

Arguments:

    DriverObject - Pointer to device object to being shutdown by system.
    Irp          - IRP involved.

Return Value:

    NT Status

--*/

{
    PFG_DEVICE_EXTENSION  deviceExtension = DeviceObject->DeviceExtension;
	ASSERT( deviceExtension->TargetDeviceObject != NULL);

    KdPrint(("FgDeviceShutdownFlush: DeviceObject %X Irp %X\n", DeviceObject, Irp));

    IoSkipCurrentIrpStackLocation(Irp);
    return IoCallDriver(deviceExtension->TargetDeviceObject, Irp);

}

NTSTATUS
FgDeviceDispatchPower(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PFG_DEVICE_EXTENSION deviceExtension;

//#if (NTDDI_VERSION < NTDDI_VISTA)
    PoStartNextPowerIrp(Irp);
    IoSkipCurrentIrpStackLocation(Irp);

    deviceExtension = (PFG_DEVICE_EXTENSION)DeviceObject->DeviceExtension;
    return PoCallDriver(deviceExtension->TargetDeviceObject, Irp);
//#else
//    IoSkipCurrentIrpStackLocation(Irp);
//
//    deviceExtension = (PFG_DEVICE_EXTENSION)DeviceObject->DeviceExtension;
//    return IoCallDriver(deviceExtension->TargetDeviceObject, Irp);
//#endif
}

NTSTATUS
FgDeviceDispatchPnp(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
	PIO_STACK_LOCATION  irpSp = IoGetCurrentIrpStackLocation(Irp);
	ULONG MajorFunction;
	ULONG MinorFunction;
    NTSTATUS            status = STATUS_SUCCESS;
    //PDEVICE_EXTENSION   deviceExtension;

	KdPrint(("Enter IPR_MJ_PNP, devObj(0x%x)\n", DeviceObject));

	MinorFunction = irpSp->MinorFunction;

	switch(MinorFunction)
	{
		//
		// Plug the FC channel£¬will receive this IRP
		//
	case IRP_MN_SURPRISE_REMOVAL:
		{
			PFG_DEVICE_EXTENSION deviceExtension;
			UINT32 i = 0;

			KdPrint((" => IRP_MN_SURPRISE_REMOVAL \n"));
			ASSERT( DeviceObject != NULL );

			deviceExtension = (PFG_DEVICE_EXTENSION)DeviceObject->DeviceExtension;
			status = FgDeviceSendToNextDriver(DeviceObject, Irp);
			KdPrint((" FgDeviceSendToNextDriver status(0x%x)\n", status));
			if( NT_SUCCESS(status) && deviceExtension->TargetDeviceObject != NULL )
			{
				IoDetachDevice(deviceExtension->TargetDeviceObject);
				for(i=0; i<gFilterDevObjCount; i++) {
					if(gFilterDeviceObject[i] == DeviceObject) {
						gFilterDeviceObject[i] = NULL;
						break;
					}
				}
				gFilterDevObjCount--;
				IoDeleteDevice(DeviceObject);
			}

			Irp->IoStatus.Status = STATUS_SUCCESS;

			return status;
		}
	case IRP_MN_QUERY_REMOVE_DEVICE:
		{
			PFG_DEVICE_EXTENSION deviceExtension;
			PFILE_OBJECT fileObject;
			UINT32 i = 0;

			KdPrint((" => IRP_MN_QUERY_REMOVE_DEVICE \n"));
			ASSERT( DeviceObject != NULL );

			deviceExtension = (PFG_DEVICE_EXTENSION)DeviceObject->DeviceExtension;
			fileObject = deviceExtension->FileObject;
			ObDereferenceObject(fileObject);
			status = FgDeviceSendToNextDriver(DeviceObject, Irp);
			KdPrint((" FgDeviceSendToNextDriver status(0x%x)\n", status));

			Irp->IoStatus.Status = STATUS_SUCCESS;

			status = STATUS_SUCCESS;
			return status;
		}
	case IRP_MN_REMOVE_DEVICE:
		{
			PFG_DEVICE_EXTENSION deviceExtension;
			UINT32 i = 0;

			KdPrint((" => IRP_MN_REMOVE_DEVICE \n"));
			ASSERT( DeviceObject != NULL );

			deviceExtension = (PFG_DEVICE_EXTENSION)DeviceObject->DeviceExtension;
			status = FgDeviceSendToNextDriver(DeviceObject, Irp);
			KdPrint((" FgDeviceSendToNextDriver status(0x%x)\n", status));
			if( NT_SUCCESS(status) && deviceExtension->TargetDeviceObject != NULL )
			{
				IoDetachDevice(deviceExtension->TargetDeviceObject);
				for(i=0; i<gFilterDevObjCount; i++) {
					if(gFilterDeviceObject[i] == DeviceObject) {
						gFilterDeviceObject[i] = NULL;
						break;
					}
				}
				gFilterDevObjCount--;
				IoDeleteDevice(DeviceObject);
			}

			Irp->IoStatus.Status = STATUS_SUCCESS;

			return status;
		}
	case IRP_MN_CANCEL_REMOVE_DEVICE:
		{
			PFG_DEVICE_EXTENSION deviceExtension;

			KdPrint((" => IRP_MN_CANCEL_REMOVE_DEVICE \n"));
			deviceExtension = (PFG_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

			status = FgDeviceForwardIrpSynchronous(DeviceObject, Irp);
			KdPrint((" FgDeviceForwardIrpSynchronous() status(0x%x)\n", status));

			Irp->IoStatus.Status = STATUS_SUCCESS;
			IoCompleteRequest( Irp, IO_NO_INCREMENT );

			status = STATUS_SUCCESS;
			return status;
		}
	//case IRP_MN_DEVICE_USAGE_NOTIFICATION:
	//	{
	//		PIO_STACK_LOCATION irpStack;
	//		BOOLEAN setPagable;
	//		PFG_DEVICE_EXTENSION deviceExtension;

	//		KdPrint(("DiskPerfDispatchPnp: Processing DEVICE_USAGE_NOTIFICATION"));
	//		irpStack = IoGetCurrentIrpStackLocation(Irp);

	//		if (irpStack->Parameters.UsageNotification.Type != DeviceUsageTypePaging) {
	//			status = FgDeviceSendToNextDriver(DeviceObject, Irp);
	//			break; // out of case statement
	//		}

	//		deviceExtension = DeviceObject->DeviceExtension;

	//		//
	//		// wait on the paging path event
	//		//

	//		status = KeWaitForSingleObject(&deviceExtension->PagingPathCountEvent,
	//			Executive, KernelMode,
	//			FALSE, NULL);

	//		//
	//		// if removing last paging device, need to set DO_POWER_PAGABLE
	//		// bit here, and possible re-set it below on failure.
	//		//

	//		setPagable = FALSE;
	//		if (!irpStack->Parameters.UsageNotification.InPath &&
	//			deviceExtension->PagingPathCount == 1 ) {

	//				//
	//				// removing the last paging file
	//				// must have DO_POWER_PAGABLE bits set
	//				//

	//				if (DeviceObject->Flags & DO_POWER_INRUSH) {
	//					KdPrint(("DiskPerfDispatchPnp: last paging file "
	//						"removed but DO_POWER_INRUSH set, so not "
	//						"setting PAGABLE bit "
	//						"for DO %p\n", DeviceObject));
	//				} else {
	//					KdPrint(("DiskPerfDispatchPnp: Setting  PAGABLE "
	//						"bit for DO %p\n", DeviceObject));
	//					DeviceObject->Flags |= DO_POWER_PAGABLE;
	//					setPagable = TRUE;
	//				}

	//		}

	//		//
	//		// send the irp synchronously
	//		//

	//		status = FgDeviceForwardIrpSynchronous(DeviceObject, Irp);

	//		//
	//		// now deal with the failure and success cases.
	//		// note that we are not allowed to fail the irp
	//		// once it is sent to the lower drivers.
	//		//

	//		if (NT_SUCCESS(status)) {

	//			IoAdjustPagingPathCount(
	//				&deviceExtension->PagingPathCount,
	//				irpStack->Parameters.UsageNotification.InPath);

	//			if (irpStack->Parameters.UsageNotification.InPath) {
	//				if (deviceExtension->PagingPathCount == 1) {

	//					//
	//					// first paging file addition
	//					//

	//					KdPrint(("DiskPerfDispatchPnp: Clearing PAGABLE bit "
	//						"for DO %p\n", DeviceObject));
	//					DeviceObject->Flags &= ~DO_POWER_PAGABLE;
	//				}
	//			}

	//		} else {

	//			//
	//			// cleanup the changes done above
	//			//

	//			if (setPagable == TRUE) {
	//				DeviceObject->Flags &= ~DO_POWER_PAGABLE;
	//				setPagable = FALSE;
	//			}

	//		}

	//		//
	//		// set the event so the next one can occur.
	//		//

	//		KeSetEvent(&deviceExtension->PagingPathCountEvent,
	//			IO_NO_INCREMENT, FALSE);

	//		//
	//		// and complete the irp
	//		//

	//		IoCompleteRequest(Irp, IO_NO_INCREMENT);
	//		return status;
	//		break;

	//	}
	}

	switch(MinorFunction)
	{
	case IRP_MN_START_DEVICE:
		KdPrint((" => IRP_MN_START_DEVICE \n"));
		break;
	case IRP_MN_QUERY_STOP_DEVICE :
		KdPrint((" => IRP_MN_QUERY_STOP_DEVICE  \n"));
		break;
	case IRP_MN_STOP_DEVICE:
		KdPrint((" => IRP_MN_STOP_DEVICE   \n"));
		break;
	case IRP_MN_CANCEL_STOP_DEVICE   :
		KdPrint((" => IRP_MN_CANCEL_STOP_DEVICE    \n"));
		break;
	case IRP_MN_QUERY_CAPABILITIES:
		KdPrint((" => IRP_MN_QUERY_CAPABILITIES \n"));
		status = STATUS_NOT_SUPPORTED;
		break;	
	default:
		KdPrint((" => other IPR_MN_XXX, MinorFunction(0x%x) \n", MinorFunction));
		break;
	}

	Irp->IoStatus.Status = status;
	IoCompleteRequest( Irp, IO_NO_INCREMENT );

	return status;
}

#pragma INITCODE
NTSTATUS DriverEntry (
			IN PDRIVER_OBJECT DriverObject,
			IN PUNICODE_STRING RegistryPath	) 
{
	NTSTATUS ntStatus;
	PDEVICE_OBJECT cdoDevice;
	PDEVICE_EXTENSION pDevExt;
    UNICODE_STRING          deviceNameUnicodeString;
    UNICODE_STRING          deviceLinkUnicodeString;
    int i;

	KdPrint(("FgDevice: Enter DriverEntry\n"));

	RtlInitUnicodeString (&deviceNameUnicodeString,
                          deviceNameBuffer );

	ntStatus = IoCreateDevice ( DriverObject,
                                sizeof(DEVICE_EXTENSION),
                                &deviceNameUnicodeString,
                                FILE_DEVICE_UNKNOWN,
                                0,
                                TRUE,
                                &cdoDevice );
	if(NT_SUCCESS(ntStatus)) {
		((PDEVICE_EXTENSION)cdoDevice->DeviceExtension)->pDevice = cdoDevice;
		gControlDeviceObject = cdoDevice;

		RtlInitUnicodeString (&deviceLinkUnicodeString,
                              deviceLinkBuffer );
        ntStatus = IoCreateSymbolicLink (&deviceLinkUnicodeString,
                                         &deviceNameUnicodeString );

        if(!NT_SUCCESS(ntStatus)) {
            KdPrint (("FgDevice: IoCreateSymbolicLink failed, status(0x%x)\n", ntStatus));
            IoDeleteDevice( cdoDevice );
            return ntStatus;      
        }
		KdPrint(("FgDevice: IoCreateSymbolicLink success, devObj(0x%x), linkName(%wZ)", cdoDevice, &deviceLinkUnicodeString));

		gDiskFilterDriver = DriverObject;

		pDevExt = (PDEVICE_EXTENSION)cdoDevice->DeviceExtension;
		pDevExt->magicNum = 0x4c44;
		pDevExt->ustrSymLinkName = deviceLinkUnicodeString;
		KdPrint(("FgDevice: ustrSymLinnkName(%wZ)", &pDevExt->ustrSymLinkName));

		for( i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++ ) {

            DriverObject->MajorFunction[i] = FgDeviceDispatch;
        }

		DriverObject->DriverUnload = FgDeviceUnload;
		DriverObject->MajorFunction[IRP_MJ_SHUTDOWN] = FgDeviceShutdownFlush;
		DriverObject->MajorFunction[IRP_MJ_PNP] = FgDeviceDispatchPnp;
		DriverObject->MajorFunction[IRP_MJ_POWER] = FgDeviceDispatchPower;
		DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = FgDeviceIOControl;
		DriverObject->MajorFunction[IRP_MJ_SYSTEM_CONTROL]  = FgDeviceSystemControl;

		//DriverObject->FastIoDispatch = &FastIOHook;
	} else {

        //
        // If something went wrong, cleanup the device object and don't load
        //
        DbgPrint(("FgDevice: Failed to create our device!\n"));
        return ntStatus;
    }

	//
	// 
	//
	{
		NTSTATUS status;

		status = IoRegisterPlugPlayNotification(EventCategoryDeviceInterfaceChange ,
			PNPNOTIFY_DEVICE_INTERFACE_INCLUDE_EXISTING_INTERFACES,
			(PVOID)(&GUID_DEVINTERFACE_DISK),
			DriverObject,
			(PDRIVER_NOTIFICATION_CALLBACK_ROUTINE)notifyCallback,
			(PVOID)NULL,
			&gNotificationEntry
			);
		if( !NT_SUCCESS(status) ) {
			KdPrint((" IoRegisterPlugPlayNotification() failed, status(0x%x)\n", status));
		} else {
			KdPrint((" IoRegisterPlugPlayNotification() success \n"));
		}
	}

	KdPrint(("FgDevice: DriverEntry end\n"));
	return STATUS_SUCCESS;
}

#pragma PAGEDCODE
VOID FgDeviceUnload (IN PDRIVER_OBJECT pDriverObject) 
{
	NTSTATUS status;
	PDEVICE_EXTENSION pDevExt;
	PFG_DEVICE_EXTENSION pFgDevExt;
	PDEVICE_OBJECT	pNextObj;
	UNICODE_STRING  pLinkName;
	UINT32 i;

	KdPrint(("Enter DriverUnload\n"));

	status = IoUnregisterPlugPlayNotification(gNotificationEntry);
	KdPrint((" IoUnregisterPlugPlayNotification() status(0x%x)\n", status));

	//
	// Detach and delete filter device
	//
	for(i=0; i<gFilterDevObjCount; i++)
	{
		pNextObj = gFilterDeviceObject[i];
		if( pNextObj != NULL )
		{
			pFgDevExt = (PFG_DEVICE_EXTENSION)pNextObj->DeviceExtension;

			KdPrint(("  Detaching from devObj(0x%x)... \n", pFgDevExt->TargetDeviceObject));

			IoDetachDevice( pFgDevExt->TargetDeviceObject );
			IoDeleteDevice( pNextObj );	
		}
	}

	//
	// Delete cdo device
	//
	pDevExt = (PDEVICE_EXTENSION)gControlDeviceObject->DeviceExtension;
	KdPrint(("  devObj(0x%x), magicNum(0x%x), ustrSymLinkName(%wZ)\n", gControlDeviceObject, pDevExt->magicNum, &pDevExt->ustrSymLinkName));
	status = IoDeleteSymbolicLink( &pDevExt->ustrSymLinkName );
	KdPrint(("  IoDeleteSymbolicLink(): status(0x%x)\n", status));
	IoDeleteDevice( gControlDeviceObject );
}

#pragma PAGEDCODE
NTSTATUS FgDeviceIOControl(IN PDEVICE_OBJECT DeviceObject,
								 IN PIRP Irp)
{
	NTSTATUS            ntStatus = STATUS_SUCCESS;
	PIO_STACK_LOCATION  irpStack;
    PVOID               inputBuffer;
    PVOID               outputBuffer;
    ULONG               inputBufferLength;
    ULONG               outputBufferLength;
    ULONG               ioControlCode;
	ULONG               bytesWritten;
	ULONG               information = 0;

	KdPrint(("Enter FgDeviceIOControl, devObj(0x%x)\n", DeviceObject));

	Irp->IoStatus.Status      = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;

	irpStack = IoGetCurrentIrpStackLocation (Irp);

	inputBuffer        = Irp->AssociatedIrp.SystemBuffer;
    inputBufferLength  = irpStack->Parameters.DeviceIoControl.InputBufferLength;
    outputBuffer       = Irp->AssociatedIrp.SystemBuffer;
    outputBufferLength = irpStack->Parameters.DeviceIoControl.OutputBufferLength;
    ioControlCode      = irpStack->Parameters.DeviceIoControl.IoControlCode;

	if( Irp->MdlAddress ) {

        outputBuffer = MmGetSystemAddressForMdl( Irp->MdlAddress );
    }

	switch(ioControlCode) {
	case IOCTL_FGDEVICE_START_LOG:
		KdPrint(("FgDevice: IOCTL_FGDEVICE_START_LOG\n"));

		ntStatus = KrnlWriteFile(&bytesWritten);

		information = bytesWritten;
		Irp->IoStatus.Status      = ntStatus;
		break;

	case IOCTL_FGDEVICE_STOP_LOG:
		KdPrint(("FgDevice: IOCTL_FGDEVICE_STOP_LOG\n"));
		Irp->IoStatus.Status      = STATUS_SUCCESS;
		break;

	case IOCTL_FGDEVICE_GETSTATE:
		KdPrint(("FgDevice: IOCTL_FGDEVICE_GETSTATE\n"));
		Irp->IoStatus.Status      = STATUS_SUCCESS;
		break;

	default:
        //
        // Unknown control
        // 
        KdPrint (("FgDevice: unknown IRP_MJ_DEVICE_CONTROL, controlCode(0x%x)\n", ioControlCode));
		Irp->IoStatus.Status = STATUS_INVALID_DEVICE_REQUEST;
        break;
	}

	Irp->IoStatus.Information = information;
	IoCompleteRequest( Irp, IO_NO_INCREMENT );

	KdPrint(("Leave FgDeviceIOControl\n"));
	return ntStatus;
}

#pragma PAGEDCODE
NTSTATUS FgDeviceDispatch(IN PDEVICE_OBJECT pDevObj,
								 IN PIRP pIrp) 
{
	NTSTATUS status = STATUS_SUCCESS;
	PIO_STACK_LOCATION stack = IoGetCurrentIrpStackLocation(pIrp);
	UCHAR type = stack->MajorFunction;
	static char* irpname[] = 
	{
		"IRP_MJ_CREATE",
		"IRP_MJ_CREATE_NAMED_PIPE",
		"IRP_MJ_CLOSE",
		"IRP_MJ_READ",
		"IRP_MJ_WRITE",
		"IRP_MJ_QUERY_INFORMATION",
		"IRP_MJ_SET_INFORMATION",
		"IRP_MJ_QUERY_EA",
		"IRP_MJ_SET_EA",
		"IRP_MJ_FLUSH_BUFFERS",
		"IRP_MJ_QUERY_VOLUME_INFORMATION",
		"IRP_MJ_SET_VOLUME_INFORMATION",
		"IRP_MJ_DIRECTORY_CONTROL",
		"IRP_MJ_FILE_SYSTEM_CONTROL",
		"IRP_MJ_DEVICE_CONTROL",
		"IRP_MJ_INTERNAL_DEVICE_CONTROL",
		"IRP_MJ_SHUTDOWN",
		"IRP_MJ_LOCK_CONTROL",
		"IRP_MJ_CLEANUP",
		"IRP_MJ_CREATE_MAILSLOT",
		"IRP_MJ_QUERY_SECURITY",
		"IRP_MJ_SET_SECURITY",
		"IRP_MJ_POWER",
		"IRP_MJ_SYSTEM_CONTROL",
		"IRP_MJ_DEVICE_CHANGE",
		"IRP_MJ_QUERY_QUOTA",
		"IRP_MJ_SET_QUOTA",
		"IRP_MJ_PNP",
	};

	KdPrint(("Enter FgDeviceDispatch\n"));

	
	if (type >= arraysize(irpname))
		KdPrint((" - Unknown IRP, major type %X\n", type));
	else
		KdPrint(("\t%s\n", irpname[type]));

	pIrp->IoStatus.Status = status;
	pIrp->IoStatus.Information = 0;	// bytes xfered
	IoCompleteRequest( pIrp, IO_NO_INCREMENT );

	KdPrint(("Leave FgDeviceDispatch\n"));
	return status;
}

unsigned char* PsGetProcessImageFileName( IN PEPROCESS Process );
#include <stdio.h>
#include <stdlib.h>

NTSTATUS
KrnlWriteFile(
	 OUT PULONG pBytesWritten
)
{
	NTSTATUS status;
	PDEVICE_EXTENSION   pCdoDevExt = gControlDeviceObject->DeviceExtension;

	OBJECT_ATTRIBUTES objectAttributes;
	UNICODE_STRING    logFileUnicodeString;
	IO_STATUS_BLOCK   iostatus;
	HANDLE hfile;

    PCHAR                       pStrBuf;
	UINT32                       logStrLen;
	TIME_FIELDS                 TimeFields; 
    LARGE_INTEGER               CurrentTime;
    LARGE_INTEGER               CurrentTimeLocal;
	PEPROCESS                   curProc;
    char*                       procName;
	

	RtlInitUnicodeString( &logFileUnicodeString, L"\\??\\C:\\FgDevice.log");
	InitializeObjectAttributes(&objectAttributes, 
							&logFileUnicodeString,
							OBJ_CASE_INSENSITIVE, 
							NULL, 
							NULL );

	status = ZwCreateFile( &hfile, 
							GENERIC_WRITE,
							&objectAttributes, 
							&iostatus, 
							NULL,
							FILE_ATTRIBUTE_NORMAL, 
							FILE_SHARE_READ | FILE_SHARE_WRITE,
							FILE_OPEN_IF,
							FILE_SYNCHRONOUS_IO_NONALERT, 
							NULL, 
							0 );
	if( !NT_SUCCESS(status) ) {
		DbgPrint("FgDevice: ZwCreateFile faild. status:0x%08x \n", status);
		return status;
	}

    pStrBuf = ExAllocatePoolWithTag(PagedPool, 1024, FGDV_LOG_POOLTAG);

	KeQuerySystemTime(&CurrentTime);  
    ExSystemTimeToLocalTime(&CurrentTime,&CurrentTimeLocal);
    RtlTimeToTimeFields(&CurrentTimeLocal,&TimeFields);
	curProc  = PsGetCurrentProcess();
    procName = (char *)PsGetProcessImageFileName(curProc);

	sprintf(pStrBuf,"[%d-%d-%d,%d:%d:%d:%d][%s]",TimeFields.Year,TimeFields.Month,TimeFields.Day
        ,TimeFields.Hour,TimeFields.Minute,TimeFields.Second,TimeFields.Milliseconds,procName);

	logStrLen = strlen(pStrBuf);

	status = ZwWriteFile(hfile,
				NULL,
				NULL,
				NULL,
				&iostatus,
				pStrBuf,
				logStrLen,
				NULL,
				NULL);
	if( !NT_SUCCESS(status) ) {
		DbgPrint("Disk: ZwWriteFile faild. status:0x%08x \n", status);
		ZwClose(hfile);
		return status;
	}
	*pBytesWritten = iostatus.Information; // ULONG ULONG_PTR ???

	ExFreePoolWithTag(pStrBuf, FGDV_LOG_POOLTAG);
	ZwClose(hfile);

	return status;
}
