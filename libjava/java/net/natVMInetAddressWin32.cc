/* Copyright (C) 2003, 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#undef STRICT

#include <java/net/VMInetAddress.h>
#include <java/net/UnknownHostException.h>

jstring
java::net::VMInetAddress::getLocalHostname ()
{
  char buffer[400];
  if (gethostname (buffer, sizeof(buffer)))
    return NULL;
  // It is admittedly non-optimal to convert the hostname to Unicode
  // only to convert it back in getByName, but simplicity wins.
  return JvNewStringUTF (buffer);
}

jbyteArray
java::net::VMInetAddress::lookupInaddrAny ()
{
  unsigned long laddr = INADDR_ANY;
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

  // FIXME: this is insufficient if some other piece of code calls
  // this gethostbyaddr.
  JvSynchronize sync (&java::net::VMInetAddress::class$);
  hptr = gethostbyaddr (val, len, type);

  if (hptr == NULL)
    throw new java::net::UnknownHostException ();

  return JvNewStringUTF (hptr->h_name);
}

JArray<jbyteArray> *
java::net::VMInetAddress::getHostByName (jstring host)
{
  struct hostent *hptr = NULL;
  JV_TEMP_UTF_STRING (hostname, host);

  // FIXME: this is insufficient if some other piece of code calls
  // this gethostbyname.
  JvSynchronize sync (&java::net::VMInetAddress::class$);
  hptr = gethostbyname (hostname);

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
  JV_TEMP_UTF_STRING (hostname, host);
  char* bytes = NULL;
  int blen = 0;
  unsigned long laddr = inet_addr (hostname);
  if (laddr != INADDR_NONE)
    {
      bytes = (char *) &laddr;
      blen = 4;
    }
  if (blen == 0)
    return NULL;
  jbyteArray result = JvNewByteArray (blen);
  memcpy (elements (result), bytes, blen);
  return result;
}
