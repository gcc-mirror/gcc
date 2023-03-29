/*******************************************************************************

    Binding for Mac OSX's <ifaddr.h>, expose network interface addresses

    The following functions are present as of Mac OSX 10.15:
    - getifaddrs(3):   get interface addresses
    - freeifaddrs(3):  deallocates the return value of `getifaddrs`
    - getifmaddrs(3):  get multicast group membership
    - freeifmaddrs(3): deallocates the return value of `getifmaddrs`

    Copyright:  Copyright © 2020, The D Language Foundation
    License:    $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:    Daniel Graczer

*******************************************************************************/

module core.sys.darwin.ifaddrs;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:

import core.sys.posix.sys.socket;

///
struct ifaddrs
{
    /// Next item in the list
    ifaddrs* ifa_next;
    /// Name of the interface
    char* ifa_name;
    /// Flags from SIOCGIFFLAGS
    uint ifa_flags;
    /// Address of interface
    sockaddr* ifa_addr;
    /// Netmask of interface
    sockaddr* ifa_netmask;
    /// Point-to-point destination addresss
    sockaddr* if_dstaddr;
    /// Address specific data
    void* ifa_data;
}

/// Returns: linked list of ifaddrs structures describing interfaces
int getifaddrs(ifaddrs**);
/// Frees the linked list returned by getifaddrs
void freeifaddrs(ifaddrs*);

///
struct ifmaddrs
{
    /// Pointer to next struct
    ifmaddrs* ifma_next;
    /// Interface name (AF_LINK)
    sockaddr* ifma_name;
    /// Multicast address
    sockaddr* ifma_addr;
    /// Link-layer translation, if any
    sockaddr* ifma_lladdr;
}

/// Stores a reference to a linked list of the multicast memberships
/// on the local machine in the memory referenced by ifmaddrs
int getifmaddrs(ifmaddrs**);
/// Frees the list allocated by getifmaddrs
void freeifmaddrs(ifmaddrs*);
