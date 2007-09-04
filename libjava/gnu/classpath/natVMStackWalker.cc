// natVMStackWalker.cc

/* Copyright (C) 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-stack.h>
#include <gnu/classpath/VMStackWalker.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>

#ifndef __ARM_EABI_UNWINDER__
// Return the class of the method that contains PC.
// This is a macro not a function, since defining it as one would
// introduce an extra frame on the stack.  */
#define GET_CALLING_CLASS(PC)						\
({									\
  void *f = _Unwind_FindEnclosingFunction (PC);				\
									\
  /* FIXME: it might well be a good idea to cache pc values here in	\
     order to avoid repeated invocations of				\
     _Unwind_FindEnclosingFunction, which is quite expensive.  On the	\
     other hand, which not simply write a caching version of		\
     _Unwind_FindEnclosingFunction itself?  That would probably be	\
     worthwhile.  */							\
									\
  _Jv_StackTrace::UpdateNCodeMap ();					\
  jclass klass = (jclass) _Jv_StackTrace::ncodeMap->get ((jobject) f);	\
									\
  /* If the caller is a compiled frame and the caller of the caller is	\
     an interpreted frame then klass will be null and we need to	\
     unwind the stack.  */						\
  if (!klass)								\
    klass = _Jv_StackTrace::GetStackWalkerCallingClass ();		\
									\
  klass;								\
 })
#else // __ARM_EABI_UNWINDER__
// ARM EABI doesn't support _Unwind_FindEnclosingFunction.
#define GET_CALLING_CLASS(PC)				\
  (_Jv_StackTrace::GetStackWalkerCallingClass ())
#endif

JArray<jclass> *
gnu::classpath::VMStackWalker::getClassContext(void)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  JArray<jclass> *result = _Jv_StackTrace::GetStackWalkerStack ();
  // Prevent GetStackWalkerStack() from being sibcalled.
  __asm__ __volatile__ ("" : : "g" (result));
  return result;
}

jclass
gnu::classpath::VMStackWalker::getCallingClass(void)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  jclass result = _Jv_StackTrace::GetStackWalkerCallingClass ();
  __asm__ __volatile__ ("" : : "g" (result));
  return result;
}

jclass
gnu::classpath::VMStackWalker::getCallingClass(::gnu::gcj::RawData *pc)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  jclass result = GET_CALLING_CLASS(pc);
  __asm__ __volatile__ ("" : : "g" (result));
  return result;
}

::java::lang::ClassLoader *
gnu::classpath::VMStackWalker::getClassLoader(::java::lang::Class *c)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  return c->getClassLoaderInternal ();
}

::java::lang::ClassLoader *
gnu::classpath::VMStackWalker::getCallingClassLoader(void)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  jclass klass = _Jv_StackTrace::GetStackWalkerCallingClass ();
  if (klass)
    return klass->getClassLoaderInternal ();
  else
    return NULL;
}

::java::lang::ClassLoader *
gnu::classpath::VMStackWalker::getCallingClassLoader(::gnu::gcj::RawData *pc)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  jclass klass = GET_CALLING_CLASS(pc);
  if (klass)
    return klass->getClassLoaderInternal ();
  else
    return NULL;
}

::java::lang::ClassLoader *
gnu::classpath::VMStackWalker::firstNonNullClassLoader(void)
{
  _Jv_InitClass (&::gnu::classpath::VMStackWalker::class$);
  return _Jv_StackTrace::GetStackWalkerFirstNonNullLoader ();
}
