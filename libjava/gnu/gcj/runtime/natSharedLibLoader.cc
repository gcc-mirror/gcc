// natSharedLibLoader.cc - Implementation of SharedLibHelper native methods.

/* Copyright (C) 2001, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <execution.h>

#include <gnu/gcj/runtime/SharedLibHelper.h>
#include <java/io/IOException.h>
#include <java/lang/UnsupportedOperationException.h>
#include <java/lang/UnknownError.h>

#ifdef HAVE_DLOPEN
#include <dlfcn.h>

/* Only used during dlopen, while having a lock on Class.class. */
static java::lang::ClassLoader *curLoader;
static gnu::gcj::runtime::SharedLibHelper *curHelper;

typedef void (*ClassHookFunc) (jclass);
typedef void (*CoreHookFunc) (_Jv_core_chain *);

void
_Jv_sharedlib_register_hook (jclass cls)
{
  cls->protectionDomain = curHelper->domain;
  cls->loader = curLoader;
  cls->engine = &_Jv_soleCompiledEngine;
  curHelper->registerClass(cls->getName(), cls);
}

static void
core_hook (_Jv_core_chain *chain)
{
  chain->next = (_Jv_core_chain *) curHelper->core_chain;
  curHelper->core_chain = (gnu::gcj::RawData *) chain;
}

struct SharedLibDummy
{
  ClassHookFunc saved;
  CoreHookFunc saved_core;
  SharedLibDummy()
  {
    saved = _Jv_RegisterClassHook;
    saved_core = _Jv_RegisterCoreHook;
  }
  ~SharedLibDummy()
  {
    _Jv_RegisterClassHook = saved;
    _Jv_RegisterCoreHook = saved_core;
    curLoader = NULL;
  }
};
#endif

void
gnu::gcj::runtime::SharedLibHelper::init(void)
{
#ifdef HAVE_DLOPEN
  char *lname = (char *) __builtin_alloca (JvGetStringUTFLength (baseName)
					   + 1);
  jsize total = JvGetStringUTFRegion (baseName, 0, baseName->length(), lname);
  lname[total] = '\0';

  if (flags==0)
    flags = RTLD_GLOBAL | RTLD_LAZY;
  JvSynchronize dummy1(&java::lang::Class::class$);
  SharedLibDummy dummy2;
  curLoader = loader;
  curHelper = this;
  _Jv_RegisterClassHook = _Jv_sharedlib_register_hook;
  _Jv_RegisterCoreHook = core_hook;
  void *h = dlopen(lname, flags);
  if (h == NULL)
    {
      const char *msg = dlerror();
      throw new java::lang::UnknownError(JvNewStringLatin1(msg));
    }
  handler = (gnu::gcj::RawData*) h;
#else
  const char *msg
    = "shared library class loading is not supported on this platform";
  throw new java::lang::UnsupportedOperationException(JvNewStringLatin1(msg));
#endif
}

jboolean
gnu::gcj::runtime::SharedLibHelper::hasResource (jstring name)
{
#ifdef HAVE_DLOPEN
  _Jv_core_chain *node = _Jv_FindCore ((_Jv_core_chain *) core_chain, name);
  return node != NULL;
#else
  return false;
#endif
}

gnu::gcj::Core *
gnu::gcj::runtime::SharedLibHelper::findCore (jstring name)
{
#ifdef HAVE_DLOPEN
  extern gnu::gcj::Core *_Jv_create_core (_Jv_core_chain *node, jstring name);
  ensureInit();
  return _Jv_create_core ((_Jv_core_chain *) core_chain, name);
#else
  return NULL;
#endif
}

void
gnu::gcj::runtime::SharedLibHelper::finalize()
{
  _Jv_FreeCoreChain ((_Jv_core_chain *) core_chain);
#ifdef HAVE_DLOPEN
  if (handler)
    dlclose (handler);
#endif
}

void
gnu::gcj::runtime::SharedLibHelper::ensureSupersLinked(jclass k)
{
  _Jv_Linker::wait_for_state (k, JV_STATE_LOADING);
}
