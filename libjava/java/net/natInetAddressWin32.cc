/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#undef STRICT

#include <java/net/InetAddress.h>
#include <java/net/UnknownHostException.h>
#include <java/lang/SecurityException.h>

jbyteArray
java::net::InetAddress::aton (jstring host)
{
  JV_TEMP_UTF_STRING (hostname, host);
  char* bytes = NULL;
  int blen = 0;
  unsigned long laddr = inet_addr (hostname);
  if (laddr != INADDR_NONE)
    {
      bytes = (char*) &laddr;
      blen = 4;
    }
  if (blen == 0)
    return NULL;
  jbyteArray result = JvNewByteArray (blen);
  memcpy (elements (result), bytes, blen);
  return result;
}

jint
java::net::InetAddress::getFamily (jbyteArray bytes)
{
  int len = bytes->length;
  if (len == 4)
    return AF_INET;
#ifdef HAVE_INET6
  else if (len == 16)
    return AF_INET6;
#endif /* HAVE_INET6 */
  else
    JvFail ("unrecognized size");
}


JArray<java::net::InetAddress*> *
java::net::InetAddress::lookup (jstring host, java::net::InetAddress* iaddr,
        jboolean all)
{
  struct hostent *hptr = NULL;
  if (host != NULL)
    {
      JV_TEMP_UTF_STRING (hostname, host);

      // FIXME: this is insufficient if some other piece of code calls
      // this gethostbyname.
      JvSynchronize sync (java::net::InetAddress::localhostAddress);
      hptr = gethostbyname (hostname);
    }
  else
    {
      jbyteArray bytes = iaddr->addr;
      char *chars = (char*) elements (bytes);
      int len = bytes->length;
      int type;
      char *val;
      if (len == 4)
        {
          val = chars;
          type = iaddr->family = AF_INET;
        }
#ifdef HAVE_INET6
      else if (len == 16)
      {
        val = (char *) &chars;
        type = iaddr->family = AF_INET6;
      }
#endif /* HAVE_INET6 */
      else
        JvFail ("unrecognized size");

      // FIXME: this is insufficient if some other piece of code calls
      // this gethostbyaddr.
      JvSynchronize sync (java::net::InetAddress::localhostAddress);
      hptr = gethostbyaddr (val, len, type);
    }
  if (hptr != NULL)
    {
      if (!all)
        host = JvNewStringUTF (hptr->h_name);
      java::lang::SecurityException *ex = checkConnect (host);
      if (ex != NULL)
        {
          if (iaddr == NULL || iaddr->addr == NULL)
            throw ex;
          hptr = NULL;
        }
    }
  if (hptr == NULL)
    {
      if (iaddr != NULL && iaddr->addr != NULL)
        {
          iaddr->hostName = iaddr->getHostAddress();
          return NULL;
        }
      else
        throw new java::net::UnknownHostException(host);
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
      if (iaddrs[i]->hostName == NULL)
        iaddrs[i]->hostName = host;
      if (iaddrs[i]->addr == NULL)
        {
          char *bytes = hptr->h_addr_list[i];
          iaddrs[i]->addr = JvNewByteArray (hptr->h_length);
          iaddrs[i]->family = getFamily (iaddrs[i]->addr);
          memcpy (elements (iaddrs[i]->addr), bytes, hptr->h_length);
        }
    }
    
  return result;
}

jstring
java::net::InetAddress::getLocalHostname ()
{
  char buffer[400];
  if (gethostname (buffer, sizeof(buffer)))
    return NULL;
  // It is admittedly non-optimal to convert the hostname to Unicode
  // only to convert it back in getByName, but simplicity wins.  Note
  // that unless there is a SecurityManager, we only get called once
  // anyway, thanks to the InetAddress.localhost cache.
  return JvNewStringUTF (buffer);
}
