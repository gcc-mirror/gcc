// natObjectInputStream.cc - Native part of ObjectInputStream class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This ObjectInputStream is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the ObjectInputStream "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/io/ObjectInputStream$GetField.h>
#include <java/io/ObjectInputStream.h>
#include <java/io/IOException.h>
#include <java/lang/Class.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/reflect/Method.h>

#ifdef DEBUG
#include <java/lang/System.h>
#include <java/io/PrintStream.h>
#endif

jobject
java::io::ObjectInputStream::allocateObject (jclass klass)
{
  jobject obj = NULL;
  using namespace java::lang::reflect;

  try
    {
      JvAssert (klass && ! klass->isArray ());
      if (klass->isInterface() || Modifier::isAbstract(klass->getModifiers()))
	obj = NULL;	
      else
	{
	  // FIXME: will this work for String?
	  obj = JvAllocObject (klass);
	}
    }
  catch (jthrowable t)
    {
      return NULL;
    }

  return obj;
}


#define ObjectClass java::lang::Object::class$
#define ClassClass java::lang::Class::class$

void 
java::io::ObjectInputStream::callConstructor (jclass klass, jobject obj)
{ 
  jstring init_name = JvNewStringLatin1 ("<init>");
  JArray<jclass> *arg_types
    = (JArray<jclass> *) JvNewObjectArray (0, &ClassClass, NULL);
  JArray<jobject> *args
    = (JArray<jobject> *) JvNewObjectArray (0, &ObjectClass, NULL);
  java::lang::reflect::Method *m = klass->getPrivateMethod (init_name, arg_types);
  m->invoke (obj, args);
}
  
java::lang::reflect::Field *
java::io::ObjectInputStream::getField (jclass klass, jstring name)
{
  return klass->getPrivateField (name);
}

java::lang::reflect::Method *
java::io::ObjectInputStream::getMethod (jclass klass, jstring name, 
					JArray<jclass> *arg_types)
{
  return klass->getPrivateMethod (name, arg_types);
}

#ifdef DEBUG
void
java::io::ObjectInputStream::setDump (jboolean dump)
{
  java::io::ObjectInputStream::dump = dump;
}

void
java::io::ObjectInputStream::dumpElement (jstring msg)
{
  if (dump)
    java::lang::System::out->print (msg);
}

void
java::io::ObjectInputStream::dumpElementln (jstring msg)
{
  if (dump)
    java::lang::System::out->println (msg);
}
#else
void
java::io::ObjectInputStream::setDump (jboolean dump)
{
}

void
java::io::ObjectInputStream::dumpElement (jstring msg)
{
}

void
java::io::ObjectInputStream::dumpElementln (jstring msg)
{
}
#endif
