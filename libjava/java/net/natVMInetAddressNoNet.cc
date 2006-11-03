/* Copyright (C) 2003, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <stddef.h>

jstring
java::net::VMInetAddress::getLocalHostname ()
{
  return NULL;
}

jbyteArray
java::net::VMInetAddress::lookupInaddrAny ()
{
  return NULL;
}

jstring
java::net::VMInetAddress::getHostByAddr (jbyteArray addr)
{
  return NULL;
}

JArray<jbyteArray> *
java::net::VMInetAddress::getHostByName (jstring host)
{
  return NULL;
}

jbyteArray
java::net::VMInetAddress::aton (jstring host)
{
  return NULL;
}
