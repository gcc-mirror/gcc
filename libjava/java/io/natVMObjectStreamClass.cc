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
#include <java/lang/reflect/Field.h>

using namespace java::lang::reflect;

jboolean
java::io::VMObjectStreamClass::hasClassInitializer (jclass klass)
{
  if (klass->isPrimitive())
    return false;
  _Jv_Method *meth = _Jv_GetMethodLocal(klass, gcj::clinit_name,
					       gcj::void_signature);
  return (meth != NULL);
}

void
java::io::VMObjectStreamClass::setDoubleNative (Field *f, jobject obj, 
						jdouble val)
{
  f->setDouble (NULL, obj, val, false);
}

void 
java::io::VMObjectStreamClass::setFloatNative (Field *f, jobject obj, 
					       jfloat val)
{
  f->setFloat (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setLongNative (Field *f, jobject obj, jlong val)
{
  f->setLong (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setIntNative (Field *f, jobject obj, jint val)
{
  f->setInt (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setShortNative (Field *f, jobject obj, 
					       jshort val)
{
  f->setShort (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setCharNative (Field *f, jobject obj, jchar val)
{
  f->setChar (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setByteNative (Field *f, jobject obj, jbyte val)
{
  f->setByte (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setBooleanNative (Field *f, jobject obj,
						 jboolean val)
{
  f->setBoolean (NULL, obj, val, false);
}

void
java::io::VMObjectStreamClass::setObjectNative (Field *f, jobject obj, 
						jobject val)
{
  f->set (NULL, obj, val, f->getType(), false);
}
