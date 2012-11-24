loadableDiskFilter
==================

a loadable windows disk filter driver
一个可动态加载/卸载的windows磁盘过滤驱动示例

大多数过滤驱动都通过INF文件说明为disk的UpFilter,然后将由系统在启动时自动加载，
这带来了一些不方便。

本驱动实现可以动态加载/卸载的驱动。
1.驱动在被加载时通过向PNP管理器注册
EventCategoryDeviceInterfaceChange事件，将发现已经存在的盘并在新盘到来时收到通知，
这时创建过滤设备对象attach到底层的设备对象上。
2.在底层的盘REMOVE时，将收到通知，这时驱动从底层的设备对象上detach。

主要流程：
1.DriverEntry里向PNP管理器注册，并在回调函数里做attach的工作；
2.Unload例程里做detach的工作；
3.PNP例程里做变化相关的处理，比如SURPRISE_REMOVAL,QUERY_REMOVE,CANCLE_REMOVE


包含文件：Driver.c, SOURCES, makefile, README.md
部分代码修改自WDK中的diskPerf

编译：用WDK自带的build环境进入项目路径下build即可
加载：测试时使用了osrLoader(这工具可以在网上找到)，也可以自己通过SCM来加卸载驱动(参考MSDN)。
测试：在32位XP和64位WIN7上测试工作正常。
      注意：本驱动会过滤包括系统盘在内的所有盘(本地盘、移动硬盘、U盘等)，所以最好在虚拟机环境
    下进行测试！！！