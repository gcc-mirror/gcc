// natStackTrace.cc - native helper methods for Throwable

/* Copyright (C) 2000, 2002, 2003  Free Software Foundation, Inc

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Andrew Haley <aph@cygnus.com>
 * @author Mark Wielaard <mark@klomp.org>
 *
 * Native helper methods for VM specific Throwable support.
 */

#include <config.h>
#include <platform.h>

#include <string.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/Object.h>
#include <java-threads.h>
#include <gnu/gcj/runtime/MethodRef.h>
#include <gnu/gcj/runtime/StackTrace.h>
#include <java/lang/Thread.h>
#include <java-interp.h>
#include <java/util/IdentityHashMap.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>

#include <sys/types.h>

#include <stdlib.h>

#include <unistd.h>

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#include <unwind.h>


// Fill in this stack trace with MAXLEN elements starting at offset.
void
gnu::gcj::runtime::StackTrace::fillInStackTrace (jint maxlen, jint offset)
{
#ifdef HAVE_BACKTRACE
  offset += 1;
  void *_p[maxlen + offset];
  len = backtrace (_p, maxlen + offset) - offset;
  void **p = _p + offset;
  _Jv_frame_info *frame;
  if (len > 0)
    {
#ifdef INTERPRETER
      extern void _Jv_StartOfInterpreter (void);
      extern void _Jv_EndOfInterpreter (void);

      java::lang::Thread *thread = java::lang::Thread::currentThread();
      _Jv_MethodChain *interp_frame
	= (thread ? reinterpret_cast<_Jv_MethodChain *> (thread->interp_frame)
	   : NULL);
#endif // INTERPRETER

      frame = (_Jv_frame_info *) _Jv_Malloc (len * sizeof (_Jv_frame_info));
      for (int n = 0; n < len; n++)
	{
	  frame[n].addr = p[n];
#ifdef INTERPRETER
	  if (p[n] >= &_Jv_StartOfInterpreter && p[n] <= &_Jv_EndOfInterpreter)
	    {
	      frame[n].interp = (void *) interp_frame->self;
	      interp_frame = interp_frame->next;
	    }
	  else
	    frame[n].interp = 0;
#endif // INTERPRETER
	}
    }
  else
    frame = NULL;

  addrs = reinterpret_cast<gnu::gcj::RawData *> (frame);
#else // HAVE_BACKTRACE
  (void)maxlen;
  (void)offset;
#endif // HAVE_BACKTRACE
}

/* Obtain the next power-of-2 of some integer.  */
static inline jint
nextpowerof2 (jint n)
{
  n |= (n >> 1);
  n |= (n >> 2);
  n |= (n >> 4);
  n |= (n >> 8);
  n |= (n >> 16);
  return n+1;
}

#define GET_FRAME(N)						\
({								\
  if ((N) >= len)						\
    fillInStackTrace (nextpowerof2 (N), 1);			\
  if ((N) < 0 || (N) >= len)					\
    throw new ::java::lang::ArrayIndexOutOfBoundsException ();	\
								\
  _Jv_frame_info *frame = (_Jv_frame_info *)addrs;		\
  &frame[N];							\
})

gnu::gcj::runtime::MethodRef *
gnu::gcj::runtime::StackTrace::getCompiledMethodRef (gnu::gcj::RawData *addr)
{
  void *p = _Unwind_FindEnclosingFunction (addr);
  return gnu::gcj::runtime::StackTrace
    ::methodAtAddress ((gnu::gcj::RawData *)p);
}

java::lang::Class *
gnu::gcj::runtime::StackTrace::classAt (jint n)
{
  _Jv_frame_info *frame = GET_FRAME (n);

#ifdef INTERPRETER
  if (frame->interp)
    {
      _Jv_InterpMethod *meth
	= reinterpret_cast<_Jv_InterpMethod *> (frame->interp);
      return meth->defining_class;
    }
#endif // INTERPRETER
  
  gnu::gcj::runtime::MethodRef *ref 
    = getCompiledMethodRef ((gnu::gcj::RawData *)frame->addr);
  if (ref)
    return ref->klass;
  else
    return NULL;
}

java::lang::String*
gnu::gcj::runtime::StackTrace::methodAt (jint n)
{
  _Jv_frame_info *frame = GET_FRAME (n);
  _Jv_Method *meth = NULL;

#ifdef INTERPRETER
  if (frame->interp)
    {
      meth
	= reinterpret_cast<_Jv_InterpMethod *> (frame->interp)
	->get_method();
    }
#endif // INTERPRETER
  
  if (! meth)
    {
      gnu::gcj::runtime::MethodRef *ref
	= getCompiledMethodRef ((gnu::gcj::RawData *)frame->addr);
      if (ref)
	meth = (_Jv_Method *)ref->method;
    }

  return meth 
    ? _Jv_NewStringUtf8Const (meth->name)
    : NULL ;
}

void
gnu::gcj::runtime::StackTrace::update(void)
{
  jclass klass;

  while ((klass = _Jv_PopClass ()))
    {
      for (int i=0; i<klass->method_count; i++)
	{
	  JvSynchronize sync (map);
	  _Jv_Method *meth = &(klass->methods[i]);
	  if (meth->ncode) // i.e. if p is not abstract
	    {
	      gnu::gcj::runtime::MethodRef *ref
		= new gnu::gcj::runtime::MethodRef 
		((gnu::gcj::RawData *)meth, klass);
	      map->put ((java::lang::Object*)(meth->ncode), ref);
	    }
	}
    }
}

void
gnu::gcj::runtime::StackTrace::finalize(void)
{
  if (addrs != NULL)
    _Jv_Free (addrs);
}
