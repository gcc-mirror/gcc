// natVMObjectStreamClass.cc - Native part of VMObjectStreamClass class.

/* Copyright (C) 2003  Free Software Foundation

   This VMObjectStreamClass is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the ObjectInputStream "LIBGCJ_LICENSE" for
details.  */

#include <gcj/cni.h>
#include <jvm.h>

#include <java/io/VMObjectStreamClass.h>
#include <java/lang/Class.h>

jboolean
java::io::VMObjectStreamClass::hasClassInitializer (jclass klass)
{
  _Jv_Method *meth = _Jv_GetMethodLocal(klass, gcj::clinit_name,
					       gcj::void_signature);
  return (meth != NULL);
}
