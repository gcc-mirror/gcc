/*******************************************************************************

    D binding for the interface addresses querying

    Defines functions getifaddrs/freeifaddrs and the structure
    they operate on.

    getifaddrs(3)   get interface addresses
    freeifaddrs(3)  deallocates the structure returned from getifaddrs

    Copyright:  Copyright (c) 2016 Sociomantic Labs. All rights reserved.
    License:    $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:    Nemanja Boric

*******************************************************************************/

module core.sys.linux.ifaddrs;

import core.sys.posix.sys.socket;

version (linux):
extern (C):
nothrow:
@nogc:

struct ifaddrs
{
    /// Next item in the list
    ifaddrs*         ifa_next;
    /// Name of the interface
    char*            ifa_name;
    /// Flags from SIOCGIFFLAGS
    uint      ifa_flags;
    /// Address of interface
    sockaddr* ifa_addr;
    /// Netmask of interface
    sockaddr* ifa_netmask;

    union
    {
        /// Broadcast address of the interface
        sockaddr* ifu_broadaddr;
        /// Point-to-point destination addresss
        sockaddr* if_dstaddr;
    }

    /// Address specific data
    void* ifa_data;
};

/// Returns: linked list of ifaddrs structures describing interfaces
int getifaddrs(ifaddrs** );
/// Frees the linked list returned by getifaddrs
void freeifaddrs(ifaddrs* );
