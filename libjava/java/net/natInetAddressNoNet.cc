/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <stddef.h>

#include <java/net/InetAddress.h>

jbyteArray
java::net::InetAddress::aton (jstring)
{
  return NULL;
}

jint
java::net::InetAddress::getFamily (jbyteArray bytes)
{
  return 0;
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
