// natProxy.cc - Native code for Proxy class.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/reflect/Proxy.h>
#include <java/lang/reflect/Proxy$ProxyData.h>

::java::lang::Class *
java::lang::reflect::Proxy::getProxyClass0 (::java::lang::ClassLoader *, JArray< ::java::lang::Class *> *)
{
  return 0;
}

::java::lang::reflect::Proxy$ProxyData *
java::lang::reflect::Proxy::getProxyData0 (::java::lang::ClassLoader *, JArray< ::java::lang::Class *> *)
{
  return 0;
}

::java::lang::Class *
java::lang::reflect::Proxy::generateProxyClass0 (::java::lang::ClassLoader *, 
						 ::java::lang::reflect::Proxy$ProxyData *)
{
  return 0;
}
