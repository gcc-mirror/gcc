// natVMClassLoader.cc - VMClassLoader native methods

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation

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
#include <java/lang/VMCompiler.h>
#include <gnu/gcj/runtime/ExtensionClassLoader.h>
#include <gnu/gcj/runtime/SystemClassLoader.h>
#include <gnu/gcj/runtime/BootClassLoader.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>
#include <java/lang/Throwable.h>
#include <java/security/ProtectionDomain.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Runtime.h>
#include <java/util/HashSet.h>

void
java::lang::VMClassLoader::resolveClass (jclass klass)
{
  JvSynchronize sync (klass);
  try
    {
      _Jv_Linker::wait_for_state (klass, JV_STATE_LINKED);
    }
  catch (java::lang::Throwable *x)
    {
      klass->set_state(JV_STATE_ERROR);
      transformException(klass, x);
    }
}

java::lang::Class *
java::lang::VMClassLoader::defineClass (java::lang::ClassLoader *loader,
					jstring name,
					jbyteArray data, 
					jint offset,
					jint length,
					java::security::ProtectionDomain *pd)
{
  jclass klass = VMCompiler::compileClass(loader, name, data,
					  offset, length, pd);

#ifdef INTERPRETER
  if (klass == NULL)
    {
      klass = new java::lang::Class ();

      // Synchronize on the class, so that it is not attempted initialized
      // until we're done loading.
      JvSynchronize sync (klass);

      // Record the defining loader.  For the bootstrap class loader,
      // we record NULL.
      if (loader != bootLoader)
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
	  _Jv_DefineClass (klass, data, offset, length, pd);
	}
      catch (java::lang::Throwable *ex)
	{
	  klass->state = JV_STATE_ERROR;
	  klass->notifyAll ();

	  _Jv_UnregisterInitiatingLoader (klass, klass->loader);

	  // If EX is not a ClassNotFoundException, that's ok, because we
	  // account for the possibility in defineClass().
	  throw ex;
	}

      // if everything proceeded sucessfully, we're loaded.
      JvAssert (klass->state == JV_STATE_LOADED);
    }
#endif // INTERPRETER

  return klass;
}

java::lang::ClassLoader *
java::lang::VMClassLoader::getSystemClassLoaderInternal()
{
  _Jv_InitClass (&gnu::gcj::runtime::ExtensionClassLoader::class$);
  return gnu::gcj::runtime::ExtensionClassLoader::system_instance;
}

jclass
java::lang::VMClassLoader::getPrimitiveClass (jchar type)
{
  char sig[2];
  sig[0] = (char) type;
  sig[1] = '\0';
  // Note: this cannot return NULL, since the input is always correct.
  return _Jv_FindClassFromSignature (sig, NULL);
}

void
java::lang::VMClassLoader::initBootLoader(jstring libdir)
{
  bootLoader = new gnu::gcj::runtime::BootClassLoader(libdir);
}

jclass
java::lang::VMClassLoader::nativeFindClass (jstring name)
{
  jclass klass = NULL;

  if (lib_control != LIB_NEVER)
    {
      // Turn `gnu.pkg.quux' into `lib-gnu-pkg-quux'.  Then search for
      // a module named (eg, on Linux) `lib-gnu-pkg-quux.so', followed
      // by `lib-gnu-pkg.so' and `lib-gnu.so'.  If loading one of
      // these causes the class to appear in the cache, then use it.
      java::lang::StringBuffer *sb
	= new java::lang::StringBuffer (JvNewStringLatin1("lib-"));
      // Skip inner classes
      jstring cn;
      jint ci = name->indexOf('$');
      if (ci == -1)
	cn = name;
      else
	cn = name->substring (0, ci);
      jstring so_base_name
	= (sb->append (cn)->toString ())->replace ('.', '-');

      using namespace ::java::lang;
      Runtime *rt = Runtime::getRuntime();

      _Jv_Utf8Const *name_u = NULL;

      // Compare against `3' because that is the length of "lib".
      while (! klass && so_base_name && so_base_name->length() > 3)
	{
	  if (lib_control == LIB_CACHE)
	    {
	      // If we've already tried this name, we're done.
	      if (tried_libraries->contains(so_base_name))
		break;
	      tried_libraries->add(so_base_name);
	    }

	  jboolean loaded = rt->loadLibraryInternal (so_base_name);

	  jint nd = so_base_name->lastIndexOf ('-');
	  if (nd == -1)
	    so_base_name = NULL;
	  else
	    so_base_name = so_base_name->substring (0, nd);

	  if (loaded)
	    {
	      if (name_u == NULL)
		name_u = _Jv_makeUtf8Const (name);
	      klass = _Jv_FindClassInCache (name_u);
	    }
	}
    }

  if (klass)
    definePackageForNative(name);

  return klass;
}

jclass
java::lang::VMClassLoader::loadClass(jstring name, jboolean resolve)
{
  // We try the boot loader first, so that the endorsed directory
  // overrides compiled-in classes.
  jclass klass = NULL;
  if (bootLoader)
    klass = bootLoader->bootLoadClass(name);
  if (! klass)
    {
      _Jv_Utf8Const *utf = _Jv_makeUtf8Const (name);
      klass = _Jv_FindClassInCache (utf);
    }
  if (! klass)
    klass = nativeFindClass(name);
  if (klass)
    {
      // We never want to return a class without its supers linked.
      // It isn't clear from the spec, but this is what other
      // implementations do in practice.
      if (resolve)
	_Jv_InitClass (klass);
      else
	_Jv_Linker::wait_for_state (klass, JV_STATE_LOADING);

      definePackageForNative(name);
    }

  return klass;
}
