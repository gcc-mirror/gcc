/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#undef STRICT

#include <java/net/NetworkInterface.h>
#include <java/net/Inet4Address.h>
#include <java/net/SocketException.h>
#include <java/util/Vector.h>

/* As of this writing, NetworkInterface.java has
   getName() == getDisplayName() and only one IP address
   per interface. If this changes, we'll need to use
   iphlpapi (not supported on Win95) to retrieve richer
   adapter information via GetAdaptersInfo(). In this
   module, we provide the necessary hooks to detect the
   presence of iphlpapi and use it if necessary, but
   comment things out for now to avoid compiler warnings. */

enum {MAX_INTERFACES = 50};

typedef int
(*PfnGetRealNetworkInterfaces) (jstring* pjstrName,
  java::net::InetAddress** ppAddress);

static int
winsock2GetRealNetworkInterfaces (jstring* pjstrName,
  java::net::InetAddress** ppAddress)
{
  // FIXME: Add IPv6 support.
  
  INTERFACE_INFO arInterfaceInfo[MAX_INTERFACES];

  // Open a (random) socket to have a file descriptor for the WSAIoctl call.
  SOCKET skt = ::socket (AF_INET, SOCK_DGRAM, 0);
  if (skt == INVALID_SOCKET) 
    _Jv_ThrowSocketException ();
    
  DWORD dwOutBufSize;
  int nRetCode = ::WSAIoctl (skt, SIO_GET_INTERFACE_LIST,
    NULL, 0, &arInterfaceInfo, sizeof(arInterfaceInfo),
    &dwOutBufSize, NULL, NULL);
    
  if (nRetCode == SOCKET_ERROR)
  {
    DWORD dwLastErrorCode = WSAGetLastError ();
    ::closesocket (skt);
    _Jv_ThrowSocketException (dwLastErrorCode);
  }
  
  // Get addresses of all interfaces.
  int nNbInterfaces = dwOutBufSize / sizeof(INTERFACE_INFO);
  int nCurETHInterface = 0;
  for (int i=0; i < nNbInterfaces; ++i) 
    {
      int len = 4;
      jbyteArray baddr = JvNewByteArray (len);
      SOCKADDR_IN* pAddr = (SOCKADDR_IN*) &arInterfaceInfo[i].iiAddress;
      memcpy (elements (baddr), &(pAddr->sin_addr), len);

      // Concoct a name for this interface. Since we don't
      // have access to the real name under Winsock 2, we use
      // "lo" for the loopback interface and ethX for the
      // real ones.
      TCHAR szName[30];
      u_long lFlags = arInterfaceInfo[i].iiFlags;

      if (lFlags & IFF_LOOPBACK)
        _tcscpy (szName, _T("lo"));
      else
        {
          _tcscpy (szName, _T("eth"));
          wsprintf(szName+3, _T("%d"), nCurETHInterface++);
        }

      jstring if_name = _Jv_Win32NewString (szName);
      java::net::Inet4Address* address =
        new java::net::Inet4Address (baddr, JvNewStringLatin1 (""));
      pjstrName[i] = if_name;
      ppAddress[i] = address;
    }

  ::closesocket (skt);
  
  return nNbInterfaces;
}

/*
static int
iphlpapiGetRealNetworkInterfaces (jstring* pjstrName,
  java::net::InetAddress** ppAddress)
{
  return 0;
}
*/

static PfnGetRealNetworkInterfaces
determineGetRealNetworkInterfacesFN ()
{
  /* FIXME: Try to dynamically load iphlpapi.dll and
     detect the presence of GetAdaptersInfo() using
     GetProcAddress(). If successful, return
     iphlpapiGetRealNetworkInterfaces; if not,
     return winsock2GetRealNetworkInterfaces */
  return &winsock2GetRealNetworkInterfaces;
}

::java::util::Vector*
java::net::NetworkInterface::getRealNetworkInterfaces ()
{
  // This next declaration used to be a static local,
  // but this introduced a dependency on libsupc++ due
  // to _cxa_guard_acquire and _cxa_guard_release.
  // When Win95 is gone and we eventually get rid of
  // winsock2GetRealNetworkInterfaces, we can rework
  // all of this. Alternatively, we could move this all
  // to win32.cc and initialize this at startup time,
  // but that seems more trouble than it's worth at
  // the moment.
  PfnGetRealNetworkInterfaces pfn =
    determineGetRealNetworkInterfacesFN ();
    
  jstring arIFName[MAX_INTERFACES];
  InetAddress* arpInetAddress[MAX_INTERFACES];
  ::java::util::Vector* ht = new ::java::util::Vector ();
  
  int nNbInterfaces = (*pfn) (arIFName, arpInetAddress);
  for (int i=0; i < nNbInterfaces; ++i) 
    {
      ht->add (new java::net::NetworkInterface (arIFName[i],
        arpInetAddress[i]));
    }
    
  return ht;
}
