/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_iptypes.d)
 */
module core.sys.windows.iptypes;
version (Windows):

import core.sys.windows.windef;
import core.stdc.time;
//#include <sys/types.h>

enum size_t
    DEFAULT_MINIMUM_ENTITIES       =  32,
    MAX_ADAPTER_ADDRESS_LENGTH     =   8,
    MAX_ADAPTER_DESCRIPTION_LENGTH = 128,
    MAX_ADAPTER_NAME_LENGTH        = 256,
    MAX_DOMAIN_NAME_LEN            = 128,
    MAX_HOSTNAME_LEN               = 128,
    MAX_SCOPE_ID_LEN               = 256;

enum UINT
    BROADCAST_NODETYPE    = 1,
    PEER_TO_PEER_NODETYPE = 2,
    MIXED_NODETYPE        = 4,
    HYBRID_NODETYPE       = 8;

enum : UINT {
    IF_OTHER_ADAPTERTYPE,
    IF_ETHERNET_ADAPTERTYPE,
    IF_TOKEN_RING_ADAPTERTYPE,
    IF_FDDI_ADAPTERTYPE,
    IF_PPP_ADAPTERTYPE,
    IF_LOOPBACK_ADAPTERTYPE // = 5
}

struct IP_ADDRESS_STRING {
    char[16] String = 0;
}
alias IP_ADDRESS_STRING IP_MASK_STRING;
alias IP_ADDRESS_STRING* PIP_ADDRESS_STRING, PIP_MASK_STRING;

struct IP_ADDR_STRING {
    IP_ADDR_STRING*   Next;
    IP_ADDRESS_STRING IpAddress;
    IP_MASK_STRING    IpMask;
    DWORD             Context;
}
alias IP_ADDR_STRING* PIP_ADDR_STRING;

struct IP_ADAPTER_INFO {
    IP_ADAPTER_INFO* Next;
    DWORD ComboIndex;
    char[MAX_ADAPTER_NAME_LENGTH+4]        AdapterName = 0;
    char[MAX_ADAPTER_DESCRIPTION_LENGTH+4] Description = 0;
    UINT             AddressLength;
    BYTE[MAX_ADAPTER_ADDRESS_LENGTH]       Address = 0;
    DWORD            Index;
    UINT             Type;
    UINT             DhcpEnabled;
    PIP_ADDR_STRING  CurrentIpAddress;
    IP_ADDR_STRING   IpAddressList;
    IP_ADDR_STRING   GatewayList;
    IP_ADDR_STRING   DhcpServer;
    BOOL             HaveWins;
    IP_ADDR_STRING   PrimaryWinsServer;
    IP_ADDR_STRING   SecondaryWinsServer;
    time_t           LeaseObtained;
    time_t           LeaseExpires;
}
alias IP_ADAPTER_INFO* PIP_ADAPTER_INFO;

struct IP_PER_ADAPTER_INFO {
    UINT AutoconfigEnabled;
    UINT AutoconfigActive;
    PIP_ADDR_STRING CurrentDnsServer;
    IP_ADDR_STRING DnsServerList;
}
alias IP_PER_ADAPTER_INFO* PIP_PER_ADAPTER_INFO;

struct FIXED_INFO {
    char[MAX_HOSTNAME_LEN+4]    HostName = 0;
    char[MAX_DOMAIN_NAME_LEN+4] DomainName = 0;
    PIP_ADDR_STRING             CurrentDnsServer;
    IP_ADDR_STRING              DnsServerList;
    UINT                        NodeType;
    char[MAX_SCOPE_ID_LEN+4]    ScopeId = 0;
    UINT                        EnableRouting;
    UINT                        EnableProxy;
    UINT                        EnableDns;
}
alias FIXED_INFO* PFIXED_INFO;
