// natNetworkInterface.cc

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#ifdef WIN32

#include <windows.h>
#include <winsock.h>
#undef STRICT

#else /* WIN32 */

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

#endif /* WIN32 */

#include <gcj/cni.h>
#include <jvm.h>
#include <java/net/NetworkInterface.h>
#include <java/net/Inet4Address.h>
#include <java/net/SocketException.h>
#include <java/util/Vector.h>

#ifdef DISABLE_JAVA_NET

::java::util::Vector*
java::net::NetworkInterface::getRealNetworkInterfaces ()
{
  ::java::util::Vector* ht = new ::java::util::Vector();
  return ht;
}

#else /* DISABLE_JAVA_NET */

::java::util::Vector*
java::net::NetworkInterface::getRealNetworkInterfaces ()
{
#ifdef WIN32
  throw new ::java::net::SocketException;
#else
  int fd;
  int num_interfaces = 0;
  struct ifconf if_data;
  struct ifreq* if_record;
  ::java::util::Vector* ht = new ::java::util::Vector ();

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
  while (if_data.ifc_len >= (sizeof (struct ifreq) * num_interfaces));

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
      Inet4Address* address =
        new java::net::Inet4Address (baddr, JvNewStringLatin1 (""));
      ht->add (new NetworkInterface (if_name, address));
      if_record++;
    }

#ifdef HAVE_INET6
      // FIXME: read /proc/net/if_inet6 (on Linux 2.4)
#endif

  _Jv_Free (if_data.ifc_buf);
  
  if (fd >= 0)
    _Jv_close (fd);
  
  return ht;
#endif /* WIN32 */
}

#endif // DISABLE_JAVA_NET //
