// natInetAddress.cc

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#if GETHOSTBYNAME_R_NEEDS_REENTRANT && !defined(_REENTRANT)
# define _REENTRANT 1
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <errno.h>

#include <sys/param.h>
#include <sys/types.h>
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java/net/InetAddress.h>
#include <java/net/UnknownHostException.h>
#include <java/lang/SecurityException.h>

#if defined(HAVE_UNAME) && ! defined(HAVE_GETHOSTNAME)
#include <sys/utsname.h>
#endif

#ifndef HAVE_GETHOSTNAME_DECL
extern "C" int gethostname (char *name, int namelen);
#endif

#ifdef DISABLE_JAVA_NET

jbyteArray
java::net::InetAddress::aton (jstring)
{
  return NULL;
}

JArray<java::net::InetAddress*> *
java::net::InetAddress::lookup (jstring, java::net::InetAddress *, jboolean)
{
  return NULL;
}

jstring
java::net::InetAddress::getLocalHostname ()
{
  return NULL;
}

#else /* DISABLE_JAVA_NET */

jbyteArray
java::net::InetAddress::aton (jstring host)
{
  char *hostname;
  char buf[100];
  int len = JvGetStringUTFLength(host);
  if (len < 100)
    hostname = buf;
  else
    hostname = (char*) _Jv_AllocBytesChecked (len+1);
  JvGetStringUTFRegion (host, 0, host->length(), hostname);
  buf[len] = '\0';
  char* bytes = NULL;
  int blen = 0;
#ifdef HAVE_INET_ATON
  struct in_addr laddr;
  if (inet_aton (hostname, &laddr))
    {
      bytes = (char*) &laddr;
      blen = 4;
    }
#elif defined(HAVE_INET_ADDR)
#if ! HAVE_IN_ADDR_T
  typedef jint in_addr_t;
#endif
  in_addr_t laddr = inet_addr (hostname);
  if (laddr != (in_addr_t)(-1))
    {
      bytes = (char*) &laddr;
      blen = 4;
    }
#endif
#ifdef HAVE_INET_PTON
  char inet6_addr[16];
  if (len == 0 && inet_pton (AF_INET6, hostname, inet6_addr) > 0)
    {
      bytes = inet6_addr;
      blen = 16;
    }
#endif
  if (blen == 0)
    return NULL;
  jbyteArray result = JvNewByteArray (blen);
  memcpy (elements (result), bytes, blen);
  return result;
}


JArray<java::net::InetAddress*> *
java::net::InetAddress::lookup (jstring host, java::net::InetAddress* iaddr,
				jboolean all)
{
  struct hostent *hptr = NULL;
#if defined (HAVE_GETHOSTBYNAME_R) || defined (HAVE_GETHOSTBYADDR_R)
  struct hostent hent_r;
#if HAVE_STRUCT_HOSTENT_DATA
  struct hostent_data fixed_buffer, *buffer_r = &fixed_buffer;
#else
#if defined (__GLIBC__) 
  // FIXME: in glibc, gethostbyname_r returns NETDB_INTERNAL to herr and
  // ERANGE to errno if the buffer size is too small, rather than what is 
  // expected here. We work around this by setting a bigger buffer size and 
  // hoping that it is big enough.
  char fixed_buffer[1024];
#else
  char fixed_buffer[200];
#endif
  char *buffer_r = fixed_buffer;
  int size_r = sizeof (fixed_buffer);
#endif
#endif

  if (host != NULL)
    {
      char *hostname;
      char buf[100];
      int len = JvGetStringUTFLength(host);
      if (len < 100)
	hostname = buf;
      else
	hostname = (char*) _Jv_AllocBytesChecked (len+1);
      JvGetStringUTFRegion (host, 0, host->length(), hostname);
      buf[len] = '\0';
#ifdef HAVE_GETHOSTBYNAME_R
      while (true)
	{
	  int ok;
#if HAVE_STRUCT_HOSTENT_DATA
	  ok = ! gethostbyname_r (hostname, &hent_r, buffer_r);
#else
	  int herr = 0;
#ifdef GETHOSTBYNAME_R_RETURNS_INT
	  ok = ! gethostbyname_r (hostname, &hent_r, buffer_r, size_r,
				  &hptr, &herr);
#else
	  hptr = gethostbyname_r (hostname, &hent_r, buffer_r, size_r, &herr);
	  ok = hptr != NULL;
#endif /* GETHOSTNAME_R_RETURNS_INT */
	  if (! ok && herr == ERANGE)
	    {
	      size_r *= 2;
	      buffer_r = (char *) _Jv_AllocBytesChecked (size_r);
	    }
	  else
#endif /* HAVE_STRUCT_HOSTENT_DATA */
	    break;
	}
#else
      // FIXME: this is insufficient if some other piece of code calls
      // this gethostbyname.
      JvSynchronize sync (java::net::InetAddress::localhostAddress);
      hptr = gethostbyname (hostname);
#endif /* HAVE_GETHOSTBYNAME_R */
    }
  else
    {
      jbyteArray bytes = iaddr->address;
      char *chars = (char*) elements (bytes);
      int len = bytes->length;
      int type;
      char *val;
      if (len == 4)
	{
	  val = chars;
	  type = AF_INET;
	}
#ifdef HAVE_INET6
      else if (len == 16)
	{
	  val = (char *) &chars;
	  type = AF_INET6;
	}
#endif /* HAVE_INET6 */
      else
	JvFail ("unrecognized size");

#ifdef HAVE_GETHOSTBYADDR_R
      while (true)
	{
	  int ok;
#if HAVE_STRUCT_HOSTENT_DATA
	  ok = ! gethostbyaddr_r (val, len, type, &hent_r, buffer_r);
#else
	  int herr = 0;
#ifdef GETHOSTBYADDR_R_RETURNS_INT
	  ok = ! gethostbyaddr_r (val, len, type, &hent_r,
				  buffer_r, size_r, &hptr, &herr);
#else
	  hptr = gethostbyaddr_r (val, len, type, &hent_r,
				  buffer_r, size_r, &herr);
	  ok = hptr != NULL;
#endif /* GETHOSTBYADDR_R_RETURNS_INT */
	  if (! ok && herr == ERANGE)
	    {
	      size_r *= 2;
	      buffer_r = (char *) _Jv_AllocBytesChecked (size_r);
	    }
	  else 
#endif /* HAVE_STRUCT_HOSTENT_DATA */
	    break;
	}
#else /* HAVE_GETHOSTBYADDR_R */
      // FIXME: this is insufficient if some other piece of code calls
      // this gethostbyaddr.
      JvSynchronize sync (java::net::InetAddress::localhostAddress);
      hptr = gethostbyaddr (val, len, type);
#endif /* HAVE_GETHOSTBYADDR_R */
    }
  if (hptr != NULL)
    {
      if (!all)
        host = JvNewStringUTF (hptr->h_name);
      java::lang::SecurityException *ex = checkConnect (host);
      if (ex != NULL)
	{
	  if (iaddr == NULL || iaddr->address == NULL)
	    JvThrow (ex);
	  hptr = NULL;
	}
    }
  if (hptr == NULL)
    {
      if (iaddr != NULL && iaddr->address != NULL)
	{
	  iaddr->hostname = iaddr->getHostAddress();
	  return NULL;
	}
      else
	JvThrow (new java::net::UnknownHostException(host));
    }
  int count;
  if (all)
    {
      char** ptr = hptr->h_addr_list;
      count = 0;
      while (*ptr++)  count++;
    }
  else
    count = 1;
  JArray<java::net::InetAddress*> *result;
  java::net::InetAddress** iaddrs;
  if (all)
    {
      result = java::net::InetAddress::allocArray (count);
      iaddrs = elements (result);
    }
  else
    {
      result = NULL;
      iaddrs = &iaddr;
    }

  for (int i = 0;  i < count;  i++)
    {
      if (iaddrs[i] == NULL)
	iaddrs[i] = new java::net::InetAddress (NULL, NULL);
      if (iaddrs[i]->hostname == NULL)
        iaddrs[i]->hostname = host;
      if (iaddrs[i]->address == NULL)
	{
	  char *bytes = hptr->h_addr_list[i];
	  iaddrs[i]->address = JvNewByteArray (hptr->h_length);
	  memcpy (elements (iaddrs[i]->address), bytes, hptr->h_length);
	}
    }
  return result;
}

jstring
java::net::InetAddress::getLocalHostname ()
{
  char *chars;
#ifdef HAVE_GETHOSTNAME
  char buffer[MAXHOSTNAMELEN];
  if (gethostname (buffer, MAXHOSTNAMELEN))
    return NULL;
  chars = buffer;
#elif HAVE_UNAME
  struct utsname stuff;
  if (uname (&stuff) != 0)
    return NULL;
  chars = stuff.nodename;
#else
  return NULL;
#endif
  // It is admittedly non-optimal to convert the hostname to Unicode
  // only to convert it back in getByName, but simplicity wins.  Note
  // that unless there is a SecurityManager, we only get called once
  // anyway, thanks to the InetAddress.localhost cache.
  return JvNewStringUTF (chars);
}

#endif /* DISABLE_JAVA_NET */
