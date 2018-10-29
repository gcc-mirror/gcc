/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_ipifcons.d)
 */
module core.sys.windows.ipifcons;
version (Windows):

// FIXME: check types of constants

enum {
    MIB_IF_ADMIN_STATUS_UP = 1,
    MIB_IF_ADMIN_STATUS_DOWN,
    MIB_IF_ADMIN_STATUS_TESTING,
}

enum {
    MIB_IF_OPER_STATUS_NON_OPERATIONAL,
    MIB_IF_OPER_STATUS_UNREACHABLE,
    MIB_IF_OPER_STATUS_DISCONNECTED,
    MIB_IF_OPER_STATUS_CONNECTING,
    MIB_IF_OPER_STATUS_CONNECTED,
    MIB_IF_OPER_STATUS_OPERATIONAL // = 5
}

enum {
    MIB_IF_TYPE_OTHER     =  1,
    MIB_IF_TYPE_ETHERNET  =  6,
    MIB_IF_TYPE_TOKENRING =  9,
    MIB_IF_TYPE_FDDI      = 15,
    MIB_IF_TYPE_PPP       = 23,
    MIB_IF_TYPE_LOOPBACK  = 24,
    MIB_IF_TYPE_SLIP      = 28
}
