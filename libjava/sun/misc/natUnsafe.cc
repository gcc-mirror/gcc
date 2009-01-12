// natUnsafe.cc - Implementation of sun.misc.Unsafe native methods.

/* Copyright (C) 2006, 2007
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <gcj/cni.h>
#include <gcj/field.h>
#include <gcj/javaprims.h>
#include <jvm.h>
#include <sun/misc/Unsafe.h>
#include <java/lang/System.h>
#include <java/lang/InterruptedException.h>

#include <java/lang/Thread.h>
#include <java/lang/Long.h>

#include "sysdep/locks.h"

// Use a spinlock for multi-word accesses
class spinlock
{
  static volatile obj_addr_t lock;

public:

spinlock ()
  {
    while (! compare_and_swap (&lock, 0, 1))
      _Jv_ThreadYield ();
  }
  ~spinlock ()
  {
    release_set (&lock, 0);
  }
};
  
// This is a single lock that is used for all synchronized accesses if
// the compiler can't generate inline compare-and-swap operations.  In
// most cases it'll never be used, but the i386 needs it for 64-bit
// locked accesses and so does PPC32.  It's worth building libgcj with
// target=i486 (or above) to get the inlines.
volatile obj_addr_t spinlock::lock;


static inline bool
compareAndSwap (volatile jint *addr, jint old, jint new_val)
{
  jboolean result = false;
  spinlock lock;
  if ((result = (*addr == old)))
    *addr = new_val;
  return result;
}
  
static inline bool
compareAndSwap (volatile jlong *addr, jlong old, jlong new_val)
{
  jboolean result = false;
  spinlock lock;
  if ((result = (*addr == old)))
    *addr = new_val;
  return result;
}
  
static inline bool
compareAndSwap (volatile jobject *addr, jobject old, jobject new_val)
{
  jboolean result = false;
  spinlock lock;
  if ((result = (*addr == old)))
    *addr = new_val;
  return result;
}
  

jlong
sun::misc::Unsafe::objectFieldOffset (::java::lang::reflect::Field *field)
{
  _Jv_Field *fld = _Jv_FromReflectedField (field);
  // FIXME: what if it is not an instance field?
  return fld->getOffset();
}

jint
sun::misc::Unsafe::arrayBaseOffset (jclass arrayClass)
{
  // FIXME: assert that arrayClass is array.
  jclass eltClass = arrayClass->getComponentType();
  return (jint)(jlong) _Jv_GetArrayElementFromElementType (NULL, eltClass);
}

jint
sun::misc::Unsafe::arrayIndexScale (jclass arrayClass)
{
  // FIXME: assert that arrayClass is array.
  jclass eltClass = arrayClass->getComponentType();
  if (eltClass->isPrimitive())
    return eltClass->size();
  return sizeof (void *);
}

// These methods are used when the compiler fails to generate inline
// versions of the compare-and-swap primitives.

jboolean
sun::misc::Unsafe::compareAndSwapInt (jobject obj, jlong offset,
				      jint expect, jint update)
{
  jint *addr = (jint *)((char *)obj + offset);
  return compareAndSwap (addr, expect, update);
}

jboolean
sun::misc::Unsafe::compareAndSwapLong (jobject obj, jlong offset,
				       jlong expect, jlong update)
{
  volatile jlong *addr = (jlong*)((char *) obj + offset);
  return compareAndSwap (addr, expect, update);
}

jboolean
sun::misc::Unsafe::compareAndSwapObject (jobject obj, jlong offset,
					 jobject expect, jobject update)
{
  jobject *addr = (jobject*)((char *) obj + offset);
  return compareAndSwap (addr, expect, update);
}

void
sun::misc::Unsafe::putOrderedInt (jobject obj, jlong offset, jint value)
{
  volatile jint *addr = (jint *) ((char *) obj + offset);
  *addr = value;
}

void
sun::misc::Unsafe::putOrderedLong (jobject obj, jlong offset, jlong value)
{
  volatile jlong *addr = (jlong *) ((char *) obj + offset);
  spinlock lock;
  *addr = value;
}

void
sun::misc::Unsafe::putOrderedObject (jobject obj, jlong offset, jobject value)
{
  volatile jobject *addr = (jobject *) ((char *) obj + offset);
  *addr = value;
}

void
sun::misc::Unsafe::putIntVolatile (jobject obj, jlong offset, jint value)
{
  write_barrier ();
  volatile jint *addr = (jint *) ((char *) obj + offset);
  *addr = value;
}

void
sun::misc::Unsafe::putLongVolatile (jobject obj, jlong offset, jlong value)
{
  volatile jlong *addr = (jlong *) ((char *) obj + offset);
  spinlock lock;
  *addr = value;
}

void
sun::misc::Unsafe::putObjectVolatile (jobject obj, jlong offset, jobject value)
{
  write_barrier ();
  volatile jobject *addr = (jobject *) ((char *) obj + offset);
  *addr = value;
}

#if 0  // FIXME
void
sun::misc::Unsafe::putInt (jobject obj, jlong offset, jint value)
{
  jint *addr = (jint *) ((char *) obj + offset);
  *addr = value;
}
#endif

void
sun::misc::Unsafe::putLong (jobject obj, jlong offset, jlong value)
{
  jlong *addr = (jlong *) ((char *) obj + offset);
  spinlock lock;
  *addr = value;
}

void
sun::misc::Unsafe::putObject (jobject obj, jlong offset, jobject value)
{
  jobject *addr = (jobject *) ((char *) obj + offset);
  *addr = value;
}

jint
sun::misc::Unsafe::getIntVolatile (jobject obj, jlong offset)
{
  volatile jint *addr = (jint *) ((char *) obj + offset);
  jint result = *addr;
  read_barrier ();
  return result;
}

jobject
sun::misc::Unsafe::getObjectVolatile (jobject obj, jlong offset)
{
  volatile jobject *addr = (jobject *) ((char *) obj + offset);
  jobject result = *addr;
  read_barrier ();
  return result;
}

jlong
sun::misc::Unsafe::getLong (jobject obj, jlong offset)
{
  jlong *addr = (jlong *) ((char *) obj + offset);
  spinlock lock;
  return *addr;
}

jlong
sun::misc::Unsafe::getLongVolatile (jobject obj, jlong offset)
{
  volatile jlong *addr = (jlong *) ((char *) obj + offset);
  spinlock lock;
  return *addr;
}

void
sun::misc::Unsafe::unpark (::java::lang::Thread *thread)
{
  natThread *nt = (natThread *) thread->data;
  nt->park_helper.unpark ();
}

void
sun::misc::Unsafe::park (jboolean isAbsolute, jlong time)
{
  using namespace ::java::lang;
  Thread *thread = Thread::currentThread();
  natThread *nt = (natThread *) thread->data;
  nt->park_helper.park (isAbsolute, time);
}
