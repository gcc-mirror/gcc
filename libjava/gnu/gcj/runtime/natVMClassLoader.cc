// Native code for VMClassLoader

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/gcj/runtime/VMClassLoader.h>
#include <java/lang/Class.h>
#include <java/lang/StringBuffer.h>
#include <java/net/URLClassLoader.h>
#include <java/lang/Runtime.h>
#include <java/util/HashSet.h>

jclass
gnu::gcj::runtime::VMClassLoader::findClass (jstring name)
{
  _Jv_Utf8Const *name_u = _Jv_makeUtf8Const (name);
  jclass klass = _Jv_FindClassInCache (name_u, 0);

  if (! klass && lib_control != LIB_NEVER)
    {
      // Turn `gnu.pkg.quux' into `lib-gnu-pkg-quux'.  Then search for
      // a module named (eg, on Linux) `lib-gnu-pkg-quux.so', followed
      // by `lib-gnu-pkg.so' and `lib-gnu.so'.  If loading one of
      // these causes the class to appear in the cache, then use it.
      java::lang::StringBuffer *sb = new java::lang::StringBuffer (JvNewStringLatin1("lib-"));
      // Skip inner classes
      jstring cn;
      jint ci = name->indexOf('$');
      if (ci == -1)
	cn = name;
      else
	cn = name->substring (0, ci);
      jstring so_base_name = (sb->append (cn)->toString ())->replace ('.', '-');

      using namespace ::java::lang;
      Runtime *rt = Runtime::getRuntime();

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
	    klass = _Jv_FindClassInCache (name_u, 0);
	}
    }

  // Now try loading using the interpreter.
  if (! klass)
    klass = java::net::URLClassLoader::findClass (name);

  return klass;
}
