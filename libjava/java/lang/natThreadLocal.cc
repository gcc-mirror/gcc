// natThreadLocal.cc - Native part of ThreadLocal class.

// Fast thread local storage for systems that support the __thread
// variable attribute.
   
/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-threads.h>

#include <gnu/gcj/RawDataManaged.h>
#include <java/lang/ThreadLocal.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/IllegalThreadStateException.h>
#include <java/lang/InterruptedException.h>
#include <java/util/Map.h>

#include <jni.h>

/* We would like to have fast thread local variables that behave in
   the same way as C and C++ thread local variables.  This would mean
   having an field attribute "thread" (like static, final, etc.).
   However, this is not compatible with java semantics, which we wish
   to support transparently.  The problems we must overcome are:

   * In Java, ThreadLocal variables are not statically allocated: they
     are objects, created at runtime.

   * Class ThreadLocal is not final and neither are its methods, so it
     is possible to create a subclass of ThreadLocal that overrides
     any method.

   * __thread variables in DSOs are not visible to the garbage
     collector, so we must ensure that we keep a copy of every thread
     local variable somewhere on the heap.

   * Once a ThreadLocal instance has been created and assigned to a
     static field, that field may be reassigned to a different
     ThreadLocal instance or null.  

   So, we can't simply replace get() and set() with accesses of a
   __thread variable.

   So, we create a pthread_key in each ThreadLocal object and use that
   as a kind of "look-aside cache".  When a ThreadLocal is set, we
   also set the corresponding thread-specific value.  When the
   ThreadLocal is collected, we delete the key.

   This scheme is biased towards efficiency when get() is called much
   more frequently than set().  It is slightly internaler than the
   all-Java solution using the underlying map in the set() case.
   However, get() is very much more frequently invoked than set().

*/


#ifdef _POSIX_PTHREAD_SEMANTICS

class tls_t
{
public:
  pthread_key_t key;
};

void
java::lang::ThreadLocal::constructNative (void)
{
  tls_t *tls = (tls_t *)_Jv_Malloc (sizeof (tls_t));
  if (pthread_key_create (&tls->key, NULL) == 0)
    TLSPointer = (::gnu::gcj::RawData *)tls;
  else
    _Jv_Free (tls);
}

void 
java::lang::ThreadLocal::set (::java::lang::Object *value)
{
  if (TLSPointer != NULL)
    {
      tls_t* tls = (tls_t*)TLSPointer;
      pthread_setspecific (tls->key, value);
    } 

  internalSet (value);
}

::java::lang::Object *
java::lang::ThreadLocal::get (void)
{
  if (TLSPointer == NULL)
    return internalGet ();

  tls_t* tls = (tls_t*)TLSPointer;
  void *obj = pthread_getspecific(tls->key);

  if (obj)
    return (::java::lang::Object *)obj;

  ::java::lang::Object *value = internalGet ();
  pthread_setspecific (tls->key, value);

  return value;
}

void 
java::lang::ThreadLocal::remove (void)
{
  if (TLSPointer != NULL)
    {
      tls_t* tls = (tls_t*)TLSPointer;
      pthread_setspecific (tls->key, NULL);
    }

  internalRemove ();
}

void 
java::lang::ThreadLocal::finalize (void)
{
  if (TLSPointer != NULL)
    {
      tls_t* tls = (tls_t*)TLSPointer;
      pthread_key_delete (tls->key);
      _Jv_Free (tls);
    }
}

#else

void
java::lang::ThreadLocal::constructNative (void)
{
}

void 
java::lang::ThreadLocal::set (::java::lang::Object *value)
{
  internalSet (value);
}

::java::lang::Object *
java::lang::ThreadLocal::get (void)
{
  return internalGet ();
}

void 
java::lang::ThreadLocal::remove (void)
{
  internalRemove ();
}

void 
java::lang::ThreadLocal::finalize (void)
{
}

#endif
