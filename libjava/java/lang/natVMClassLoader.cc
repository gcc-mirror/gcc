// natVMClassLoader.cc - VMClassLoader native methods

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

#include <stdlib.h>
#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java-threads.h>
#include <java-interp.h>

#include <java/lang/VMClassLoader.h>
#include <gnu/gcj/runtime/VMClassLoader.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>
#include <java/lang/Throwable.h>
#include <java/security/ProtectionDomain.h>
#include <java/lang/ClassFormatError.h>

java::lang::Class *
java::lang::VMClassLoader::defineClass (java::lang::ClassLoader *loader,
					jstring name,
					jbyteArray data, 
					jint offset,
					jint length,
					java::security::ProtectionDomain *pd)
{
#ifdef INTERPRETER
  jclass klass;
  klass = new java::lang::Class ();
  klass->aux_info = (void *) _Jv_AllocBytes (sizeof (_Jv_InterpClass));

  // Synchronize on the class, so that it is not attempted initialized
  // until we're done loading.
  JvSynchronize sync (klass);

  // Record the defining loader.  For the system class loader, we
  // record NULL.
  if (loader != java::lang::ClassLoader::getSystemClassLoader())
    klass->loader = loader;

  if (name != 0)
    {
      _Jv_Utf8Const *name2 = _Jv_makeUtf8Const (name);

      if (! _Jv_VerifyClassName (name2))
	throw new java::lang::ClassFormatError
	  (JvNewStringLatin1 ("erroneous class name"));

      klass->name = name2;
    }

  try
    {
      _Jv_DefineClass (klass, data, offset, length);
    }
  catch (java::lang::Throwable *ex)
    {
      klass->state = JV_STATE_ERROR;
      klass->notifyAll ();

      _Jv_UnregisterClass (klass);

      // If EX is not a ClassNotFoundException, that's ok, because we
      // account for the possibility in defineClass().
      throw ex;
    }
    
  klass->protectionDomain = pd;

  // if everything proceeded sucessfully, we're loaded.
  JvAssert (klass->state == JV_STATE_LOADED);

  return klass;

#else // INTERPRETER

  return 0;
#endif
}

// Finish linking a class.  Only called from ClassLoader::resolveClass.
void
java::lang::VMClassLoader::linkClass0 (java::lang::Class *klass)
{
  _Jv_WaitForState (klass, JV_STATE_LINKED);
}

void
java::lang::VMClassLoader::markClassErrorState0 (java::lang::Class *klass)
{
  klass->state = JV_STATE_ERROR;
  klass->notifyAll ();
}

java::lang::ClassLoader *
java::lang::VMClassLoader::getSystemClassLoaderInternal()
{
  _Jv_InitClass (&gnu::gcj::runtime::VMClassLoader::class$);
  return gnu::gcj::runtime::VMClassLoader::instance;
}

jclass
java::lang::VMClassLoader::getPrimitiveClass (jchar type)
{
  char sig[2];
  sig[0] = (char) type;
  sig[1] = '\0';
  return _Jv_FindClassFromSignature (sig, NULL);
}

jclass
java::lang::VMClassLoader::loadClass(jstring name, jboolean resolve)
{
  _Jv_Utf8Const *utf = _Jv_makeUtf8Const (name);
  // FIXME: we culd make _Jv_FindClassFromSignature a template.
  jclass klass = _Jv_FindClassInCache (utf, NULL);
  if (klass && resolve)
    _Jv_InitClass (klass);
  return klass;
}
