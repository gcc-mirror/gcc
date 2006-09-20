/* Copyright (C) 2003, 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include <sys/param.h>
#include <sys/types.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#define BSD_COMP /* Get FIONREAD on Solaris2. */
#include <sys/ioctl.h>
#endif
#ifdef HAVE_NET_IF_H
#include <net/if.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java/net/InetAddress.h>
#include <java/net/NetworkInterface.h>
#include <java/net/SocketException.h>
#include <java/net/VMNetworkInterface.h>
#include <java/util/Vector.h>

::java::util::Vector*
java::net::VMNetworkInterface::getInterfaces ()
{
  ::java::util::Vector* ht = new ::java::util::Vector ();

#ifdef HAVE_GETIFADDRS

  struct ifaddrs *addrs;
  if (::getifaddrs (&addrs) == -1)
    throw new ::java::net::SocketException(JvNewStringUTF (strerror (errno)));

  for (struct ifaddrs *work = addrs; work != NULL; work = work->ifa_next)
    {
      // Sometimes the address can be NULL; I don't know why but
      // there's nothing we can do with this.
      if (! work->ifa_addr)
	continue;
      // We only return Inet4 or Inet6 addresses.
      jbyteArray laddr;
      if (work->ifa_addr->sa_family == AF_INET)
	{
	  sockaddr_in *real = reinterpret_cast<sockaddr_in *> (work->ifa_addr);
	  laddr = JvNewByteArray(4);
	  memcpy (elements (laddr), &real->sin_addr, 4);
	}
#ifdef HAVE_INET6
      else if (work->ifa_addr->sa_family == AF_INET6)
	{
	  sockaddr_in6 *real
	    = reinterpret_cast<sockaddr_in6 *> (work->ifa_addr);
	  laddr = JvNewByteArray(16);
	  memcpy (elements (laddr), &real->sin6_addr, 16);
	}
#endif
      else
	continue;

      ::java::net::InetAddress *inaddr
	  =  ::java::net::InetAddress::getByAddress(laddr);

      // It is ok to make a new NetworkInterface for each struct; the
      // java code will unify these as necessary; see
      // NetworkInterface.condense().
      jstring name = JvNewStringUTF (work->ifa_name);

      ht->add (new NetworkInterface (name, inaddr));
    }

  freeifaddrs (addrs);

#else /* ! HAVE_GETIFADDRS */

  int fd;
  int num_interfaces = 0;
  struct ifconf if_data;
  struct ifreq* if_record;

  if_data.ifc_len = 0;
  if_data.ifc_buf = NULL;

  // Open a (random) socket to have a file descriptor for the ioctl calls.
  fd = _Jv_socket (PF_INET, SOCK_DGRAM, htons (IPPROTO_IP));

  if (fd < 0)
    throw new ::java::net::SocketException;

  // Get all interfaces. If not enough buffers are available try it
  // with a bigger buffer size.
  do
    {
      num_interfaces += 16;
      
      if_data.ifc_len = sizeof (struct ifreq) * num_interfaces;
      if_data.ifc_buf =
        (char*) _Jv_Realloc (if_data.ifc_buf, if_data.ifc_len);

      // Try to get all local interfaces.
      if (::ioctl (fd, SIOCGIFCONF, &if_data) < 0)
        throw new java::net::SocketException;
    }
  while (if_data.ifc_len >= (int) (sizeof (struct ifreq) * num_interfaces));

  // Get addresses of all interfaces.
  if_record = if_data.ifc_req;

  for (int n = 0; n < if_data.ifc_len; n += sizeof (struct ifreq))
    {
      struct ifreq ifr;
      
      memset (&ifr, 0, sizeof (ifr));
      strcpy (ifr.ifr_name, if_record->ifr_name);

      // Try to get the IPv4-address of the local interface
      if (::ioctl (fd, SIOCGIFADDR, &ifr) < 0)
        throw new java::net::SocketException;

      int len = 4;
      struct sockaddr_in sa = *((sockaddr_in*) &(ifr.ifr_addr));

      jbyteArray baddr = JvNewByteArray (len);
      memcpy (elements (baddr), &(sa.sin_addr), len);
      jstring if_name = JvNewStringLatin1 (if_record->ifr_name);
      InetAddress* address = java::net::InetAddress::getByAddress (baddr);
      ht->add (new NetworkInterface (if_name, address));
      if_record++;
    }

  _Jv_Free (if_data.ifc_buf);
  
  if (fd >= 0)
    _Jv_close (fd);
#endif /* HAVE_GETIFADDRS */ 

  return ht;
}
