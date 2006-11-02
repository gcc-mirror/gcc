// natVMMethod.cc -- native support for VMMethod

/* Copyright (C) 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <java-interp.h>

#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/exception/JdwpInternalErrorException.h>
#include <gnu/classpath/jdwp/util/LineTable.h>
#include <gnu/classpath/jdwp/util/VariableTable.h>

java::lang::String*
gnu::classpath::jdwp::VMMethod::getName ()
{
  return NULL;
}

java::lang::String*
gnu::classpath::jdwp::VMMethod::getSignature ()
{
  return NULL;
}

jint
gnu::classpath::jdwp::VMMethod::getModifiers ()
{
  return 0;
}

gnu::classpath::jdwp::util::LineTable *
gnu::classpath::jdwp::VMMethod::getLineTable ()
{
  if (!_Jv_IsInterpretedClass (getDeclaringClass ()))
    {
      // this should not happen
      ::java::lang::String *msg = JvNewStringLatin1 ("native class");
      throw new exception::JdwpInternalErrorException (msg);
    }

  jmethodID desired_method = reinterpret_cast<jmethodID> (_methodId);

  _Jv_MethodBase *theMethod
    = _Jv_FindInterpreterMethod (getDeclaringClass (), desired_method);

  if (theMethod == NULL)
    {
      // this should not happen
      ::java::lang::String *msg
	= JvNewStringLatin1 ("could not find method in class");
      throw new exception::JdwpInternalErrorException (msg);
    }

  if (::java::lang::reflect::Modifier::isNative (desired_method->accflags))
    {
      jintArray lines = JvNewIntArray (0);
      jlongArray indices = JvNewLongArray (0);
      return new util::LineTable (-1, -1, lines, indices);
    }

  // get the linetable
  _Jv_InterpMethod *imeth = reinterpret_cast<_Jv_InterpMethod *> (theMethod);
  jlong start;
  jlong end;
  jintArray lines;
  jlongArray indices;
  imeth->get_line_table (start, end, lines, indices);
  return new util::LineTable (start, end, lines, indices);
}


gnu::classpath::jdwp::util::VariableTable*
gnu::classpath::jdwp::VMMethod::getVariableTable ()
{
  return NULL;
}
