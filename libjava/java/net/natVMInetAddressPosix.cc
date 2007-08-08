/* Copyright (C) 2003, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

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
#include <java/net/VMInetAddress.h>
#include <java/net/UnknownHostException.h>

#if defined(HAVE_UNAME) && ! defined(HAVE_GETHOSTNAME)
#include <sys/utsname.h>
#endif

#ifndef HAVE_GETHOSTNAME_DECL
extern "C" int gethostname (char *name, int namelen);
#endif

jstring
java::net::VMInetAddress::getLocalHostname ()
{
  char *chars;
#ifdef HAVE_GETHOSTNAME
#ifdef MAXHOSTNAMELEN
  char buffer[MAXHOSTNAMELEN];
  if (gethostname (buffer, MAXHOSTNAMELEN))
    return NULL;
  chars = buffer;
#else
  size_t size = 256;
  while (1) {
    char buffer[size];
    if (!gethostname (buffer, size-1))
      {
	buffer[size-1] = 0;
	return JvNewStringUTF (buffer);
      }
    else if (errno != ENAMETOOLONG)
      return NULL;
    size *= 2;
  }
#endif
#elif HAVE_UNAME
  struct utsname stuff;
  if (uname (&stuff) != 0)
    return NULL;
  chars = stuff.nodename;
#else
  return NULL;
#endif
  // It is admittedly non-optimal to convert the hostname to Unicode
  // only to convert it back in getByName, but simplicity wins.
  return JvNewStringUTF (chars);
}

jbyteArray
java::net::VMInetAddress::lookupInaddrAny ()
{
#if ! HAVE_IN_ADDR_T
  typedef jint in_addr_t;
#endif
  in_addr_t laddr = INADDR_ANY;
  char *bytes = (char *) &laddr;
  int blen = sizeof (laddr);
  jbyteArray result = JvNewByteArray (blen);
  memcpy (elements (result), bytes, blen);
  return result;
}

jstring
java::net::VMInetAddress::getHostByAddr (jbyteArray addr)
{
  struct hostent *hptr = NULL;
#ifdef HAVE_GETHOSTBYADDR_R
  struct hostent hent_r;
#if HAVE_STRUCT_HOSTENT_DATA
  struct hostent_data fixed_buffer, *buffer_r = &fixed_buffer;
#else
#ifdef __GLIBC__
  // FIXME: in glibc, gethostbyname_r returns NETDB_INTERNAL to herr and
  // ERANGE to errno if the buffer size is too small, rather than what is 
  // expected here. We work around this by setting a bigger buffer size and 
  // hoping that it is big enough.
  char fixed_buffer[1024];
#else
  char fixed_buffer[200];
#endif /* __GLIBC__ */
  char *buffer_r = fixed_buffer;
  int size_r = sizeof (fixed_buffer);
#endif /* HAVE_STRUCT_HOSTENT_DATA */
#endif /* HAVE_GETHOSTBYADDR_R */

  char *bytes = (char*) elements (addr);
  int len = addr->length;
  int type;
  char *val;
  if (len == 4)
    {
      val = bytes;
      type = AF_INET;
    }
#ifdef HAVE_INET6
  else if (len == 16)
    {
      val = (char *) &bytes;
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
	  buffer_r = (char *) _Jv_AllocBytes (size_r);
	}
      else 
#endif /* HAVE_STRUCT_HOSTENT_DATA */
	break;
    }
#else /* HAVE_GETHOSTBYADDR_R */
  // FIXME: this is insufficient if some other piece of code calls
  // this gethostbyaddr.
  JvSynchronize sync (&java::net::VMInetAddress::class$);
  hptr = gethostbyaddr (val, len, type);
#endif /* HAVE_GETHOSTBYADDR_R */

  if (hptr == NULL)
    throw new java::net::UnknownHostException ();

  return JvNewStringUTF (hptr->h_name);
}

JArray<jbyteArray> *
java::net::VMInetAddress::getHostByName (jstring host)
{
  struct hostent *hptr = NULL;
#ifdef HAVE_GETHOSTBYNAME_R
  struct hostent hent_r;
#if HAVE_STRUCT_HOSTENT_DATA
  struct hostent_data fixed_buffer, *buffer_r = &fixed_buffer;
#else
#ifdef __GLIBC__
  // FIXME: in glibc, gethostbyname_r returns NETDB_INTERNAL to herr and
  // ERANGE to errno if the buffer size is too small, rather than what is 
  // expected here. We work around this by setting a bigger buffer size and 
  // hoping that it is big enough.
  char fixed_buffer[1024];
#else
  char fixed_buffer[200];
#endif /* __GLIBC__ */
  char *buffer_r = fixed_buffer;
  int size_r = sizeof (fixed_buffer);
#endif /* HAVE_STRUCT_HOSTENT_DATA */
#endif /* HAVE_GETHOSTBYNAME_R */

  char *hostname;
  char buf[100];
  int len = JvGetStringUTFLength(host);
  if (len < 100)
    hostname = buf;
  else
    hostname = (char *) _Jv_AllocBytes (len + 1);
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
	  buffer_r = (char *) _Jv_AllocBytes (size_r);
	}
      else
#endif /* HAVE_STRUCT_HOSTENT_DATA */
	break;
    }
#else /* HAVE_GETHOSTBYNAME_R */
  // FIXME: this is insufficient if some other piece of code calls
  // this gethostbyname.
  JvSynchronize sync (&java::net::VMInetAddress::class$);
  hptr = gethostbyname (hostname);
#endif /* HAVE_GETHOSTBYNAME_R */

  if (hptr == NULL)
    throw new java::net::UnknownHostException (host);

  int count = 0;
  char ** ptr = hptr->h_addr_list;
  while (*ptr++)  count++;

  JArray<jbyteArray> *result =
    (JArray<jbyteArray> *) _Jv_NewObjectArray (
      count, _Jv_GetArrayClass(JvPrimClass(byte), NULL), NULL);
  jbyteArray* addrs = elements (result);

  for (int i = 0; i < count; i++)
    {
      addrs[i] = JvNewByteArray (hptr->h_length);
      memcpy (elements (addrs[i]), hptr->h_addr_list[i], hptr->h_length);
    }
  return result;
}

jbyteArray
java::net::VMInetAddress::aton (jstring host)
{
  char *hostname;
  char buf[100];
  int len = JvGetStringUTFLength(host);
  if (len < 100)
    hostname = buf;
  else
    hostname = (char *) _Jv_AllocBytes (len+1);
  JvGetStringUTFRegion (host, 0, host->length(), hostname);
  buf[len] = '\0';
  char *bytes = NULL;
  int blen = 0;
#ifdef HAVE_INET_ATON
  struct in_addr laddr;
  if (inet_aton (hostname, &laddr))
    {
      bytes = (char *) &laddr;
      blen = 4;
    }
#elif defined(HAVE_INET_ADDR)
#if ! HAVE_IN_ADDR_T
  typedef jint in_addr_t;
#endif
  in_addr_t laddr = inet_addr (hostname);
  if (laddr != (in_addr_t)(-1))
    {
      bytes = (char *) &laddr;
      blen = 4;
    }
#endif
#if defined (HAVE_INET_PTON) && defined (HAVE_INET6)
  char inet6_addr[16];
  if (len != 0 && inet_pton (AF_INET6, hostname, inet6_addr) > 0)
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
