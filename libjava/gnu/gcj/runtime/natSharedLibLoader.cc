// natSharedLibLoader.cc - Implementation of FirstThread native methods.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <gnu/gcj/runtime/SharedLibLoader.h>
#include <java/io/IOException.h>
#include <java/lang/UnsupportedOperationException.h>

#ifdef HAVE_DLOPEN
#include <dlfcn.h>

/* Only used during dlopen, while having a lock on Class.class. */
static gnu::gcj::runtime::SharedLibLoader* curLoader;

typedef void (*ClassHookFunc) (jclass);

static void
::register_hook(jclass cls)
{
  curLoader->registerClass(cls->getName(), cls);
}

struct SharedLibDummy
{
  ClassHookFunc saved;
  SharedLibDummy()
  {
    saved = _Jv_RegisterClassHook;
  }
  ~SharedLibDummy()
  {
    _Jv_RegisterClassHook = saved;
    curLoader = NULL;
  }
};
#endif

void
gnu::gcj::runtime::SharedLibLoader::init(jbyteArray libname, jint flags)
{
#ifdef HAVE_DLOPEN
  char *lname = (char*) elements(libname);
  if (flags==0)
    flags = RTLD_LAZY;
  JvSynchronize dummy1(&java::lang::Class::class$);
  SharedLibDummy dummy2;
  curLoader = this;
  _Jv_RegisterClassHook = ::register_hook;
  void *h = dlopen(lname, flags);
  if (h == NULL)
    {
      const char *msg = dlerror();
    }
  handler = (gnu::gcj::RawData*) h;
#else
  const char *msg = "ShareedLibLoader is not supported on this platform";
  throw new java::lang::UnsupportedOperationException(JvNewStringLatin1(msg));
#endif
}

void
gnu::gcj::runtime::SharedLibLoader::finalize()
{
#ifdef HAVE_DLOPEN
  dlclose (handler);
#endif
}
