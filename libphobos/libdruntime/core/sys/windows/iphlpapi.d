/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_iphlpapi.d)
 */
module core.sys.windows.iphlpapi;
version (Windows):
@system:

import core.sys.windows.ipexport, core.sys.windows.iprtrmib, core.sys.windows.iptypes;
import core.sys.windows.winbase, core.sys.windows.windef;

extern (Windows) {
    DWORD AddIPAddress(IPAddr, IPMask, DWORD, PULONG, PULONG);
    DWORD CreateIpForwardEntry(PMIB_IPFORWARDROW);
    DWORD CreateIpNetEntry(PMIB_IPNETROW);
    DWORD CreateProxyArpEntry(DWORD, DWORD, DWORD);
    DWORD DeleteIPAddress(ULONG);
    DWORD DeleteIpForwardEntry(PMIB_IPFORWARDROW);
    DWORD DeleteIpNetEntry(PMIB_IPNETROW);
    DWORD DeleteProxyArpEntry(DWORD, DWORD, DWORD);
    DWORD EnableRouter(HANDLE*, OVERLAPPED*);
    DWORD FlushIpNetTable(DWORD);
    DWORD GetAdapterIndex(LPWSTR, PULONG);
    DWORD GetAdaptersInfo(PIP_ADAPTER_INFO, PULONG);
    DWORD GetBestInterface(IPAddr, PDWORD);
    DWORD GetBestRoute(DWORD, DWORD, PMIB_IPFORWARDROW);
    DWORD GetFriendlyIfIndex(DWORD);
    DWORD GetIcmpStatistics(PMIB_ICMP);
    DWORD GetIfEntry(PMIB_IFROW);
    DWORD GetIfTable(PMIB_IFTABLE, PULONG, BOOL);
    DWORD GetInterfaceInfo(PIP_INTERFACE_INFO, PULONG);
    DWORD GetIpAddrTable(PMIB_IPADDRTABLE, PULONG, BOOL);
    DWORD GetIpForwardTable(PMIB_IPFORWARDTABLE, PULONG, BOOL);
    DWORD GetIpNetTable(PMIB_IPNETTABLE, PULONG, BOOL);
    DWORD GetIpStatistics(PMIB_IPSTATS);
    DWORD GetNetworkParams(PFIXED_INFO, PULONG);
    DWORD GetNumberOfInterfaces(PDWORD);
    DWORD GetPerAdapterInfo(ULONG, PIP_PER_ADAPTER_INFO, PULONG);
    BOOL GetRTTAndHopCount(IPAddr, PULONG, ULONG, PULONG);
    DWORD GetTcpStatistics(PMIB_TCPSTATS);
    DWORD GetTcpTable(PMIB_TCPTABLE, PDWORD, BOOL);
    DWORD GetUniDirectionalAdapterInfo(PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS,
      PULONG);
    DWORD GetUdpStatistics(PMIB_UDPSTATS);
    DWORD GetUdpTable(PMIB_UDPTABLE, PDWORD, BOOL);
    DWORD IpReleaseAddress(PIP_ADAPTER_INDEX_MAP);
    DWORD IpRenewAddress(PIP_ADAPTER_INDEX_MAP);
    DWORD NotifyAddrChange(PHANDLE, LPOVERLAPPED);
    DWORD NotifyRouteChange(PHANDLE, LPOVERLAPPED);
    DWORD SendARP(IPAddr, IPAddr, PULONG, PULONG);
    DWORD SetIfEntry(PMIB_IFROW);
    DWORD SetIpForwardEntry(PMIB_IPFORWARDROW);
    DWORD SetIpNetEntry(PMIB_IPNETROW);
    DWORD SetIpStatistics(PMIB_IPSTATS);
    DWORD SetIpTTL(UINT);
    DWORD SetTcpEntry(PMIB_TCPROW);
    DWORD UnenableRouter(OVERLAPPED*, LPDWORD);
}
