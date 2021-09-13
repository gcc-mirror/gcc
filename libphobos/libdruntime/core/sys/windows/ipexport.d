/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_ipexport.d)
 */
module core.sys.windows.ipexport;
version (Windows):
@system:

import core.sys.windows.windef;

enum size_t MAX_ADAPTER_NAME = 128;

// IP STATUS flags
enum : IP_STATUS {
    IP_SUCCESS         =     0,
    IP_STATUS_BASE     = 11000,
    IP_BUF_TOO_SMALL,
    IP_DEST_NET_UNREACHABLE,
    IP_DEST_HOST_UNREACHABLE,
    IP_DEST_PROT_UNREACHABLE,
    IP_DEST_PORT_UNREACHABLE,
    IP_NO_RESOURCES,
    IP_BAD_OPTION,
    IP_HW_ERROR,
    IP_PACKET_TOO_BIG,
    IP_REQ_TIMED_OUT,
    IP_BAD_REQ,
    IP_BAD_ROUTE,
    IP_TTL_EXPIRED_TRANSIT,
    IP_TTL_EXPIRED_REASSEM,
    IP_PARAM_PROBLEM,
    IP_SOURCE_QUENCH,
    IP_OPTION_TOO_BIG,
    IP_BAD_DESTINATION,
    IP_ADDR_DELETED,
    IP_SPEC_MTU_CHANGE,
    IP_MTU_CHANGE,
    IP_UNLOAD,      // = IP_STATUS_BASE + 22
    IP_GENERAL_FAILURE = IP_STATUS_BASE + 50,
    MAX_IP_STATUS      = IP_GENERAL_FAILURE,
    IP_PENDING         = IP_STATUS_BASE + 255
}

// IP header Flags values
enum byte IP_FLAG_DF = 2;

// IP Option types
enum : ubyte {
    IP_OPT_EOL          = 0,
    IP_OPT_NOP          = 0x01,
    IP_OPT_RR           = 0x07,
    IP_OPT_SECURITY     = 0x82,
    IP_OPT_LSRR         = 0x83,
    IP_OPT_SSRR         = 0x89,
    IP_OPT_TS           = 0x44,
    IP_OPT_SID          = 0x88,
    IP_OPT_ROUTER_ALERT = 0x94
}

enum ubyte MAX_OPT_SIZE = 40;

alias uint IPAddr, IPMask, IP_STATUS;

struct IP_OPTION_INFORMATION {
    ubyte  Ttl;
    ubyte  Tos;
    ubyte  Flags;
    ubyte  OptionsSize;
    ubyte* OptionsData;
}
alias IP_OPTION_INFORMATION* PIP_OPTION_INFORMATION;

struct ICMP_ECHO_REPLY {
  IPAddr Address;
  uint   Status;
  uint   RoundTripTime;
  ushort DataSize;
  ushort Reserved;
  void*  Data;
  IP_OPTION_INFORMATION Options;
}
alias ICMP_ECHO_REPLY* PICMP_ECHO_REPLY;

struct IP_ADAPTER_INDEX_MAP {
    ULONG                   Index;
    WCHAR[MAX_ADAPTER_NAME] Name = 0;
}
alias IP_ADAPTER_INDEX_MAP* PIP_ADAPTER_INDEX_MAP;

struct IP_INTERFACE_INFO {
    LONG                    NumAdapters;
    IP_ADAPTER_INDEX_MAP[1] _Adapter;

    IP_ADAPTER_INDEX_MAP* Adapter() return { return _Adapter.ptr; }
}
alias IP_INTERFACE_INFO* PIP_INTERFACE_INFO;

struct IP_UNIDIRECTIONAL_ADAPTER_ADDRESS {
    ULONG     NumAdapters;
    IPAddr[1] _Address;

    IPAddr* Address() return { return _Address.ptr; }
}
alias IP_UNIDIRECTIONAL_ADAPTER_ADDRESS* PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
