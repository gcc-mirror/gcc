// nogc.cc - Implement null garbage collector.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2006, 2007
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>

// Total amount of memory allocated.
static long total = 0;

void *
_Jv_BuildGCDescr(jclass)
{
  return 0;
}

void *
_Jv_AllocObj (jsize size, jclass klass)
{
  total += size;
  void *obj = calloc (size, 1);
  if (!obj) _Jv_ThrowNoMemory();
  *((_Jv_VTable **) obj) = klass->vtable;
  return obj;
}

void *
_Jv_AllocPtrFreeObj (jsize size, jclass klass)
{
  total += size;
  void *obj = calloc (size, 1);
  if (!obj) _Jv_ThrowNoMemory();
  *((_Jv_VTable **) obj) = klass->vtable;
  return obj;
}

void *
_Jv_AllocArray (jsize size, jclass klass)
{
  total += size;
  void *obj = calloc (size, 1);
  if (!obj) _Jv_ThrowNoMemory();
  *((_Jv_VTable **) obj) = klass->vtable;
  return obj;
}

void *
_Jv_AllocBytes (jsize size)
{
  total += size;
  void *obj = calloc (size, 1);
  if (!obj) _Jv_ThrowNoMemory();
  return obj;
}

void *
_Jv_AllocRawObj (jsize size)
{
  total += size;
  return calloc (size, 1);
}

_Jv_ClosureList **
_Jv_ClosureListFinalizer ()
{
  _Jv_ClosureList **clpp;
  clpp = (_Jv_ClosureList **)_Jv_AllocBytes (sizeof (*clpp));
  return clpp;
}

void
_Jv_RegisterFinalizer (void *, _Jv_FinalizerFunc *)
{
  // FIXME: should actually register so that finalizers can be run on
  // exit.
}

void
_Jv_RunFinalizers (void)
{
}

void
_Jv_RunAllFinalizers (void)
{
  // FIXME: should still run all finalizers.
}

void
_Jv_GCInitializeFinalizers (void (*) (void))
{
}

void
_Jv_RunGC (void)
{
}

long
_Jv_GCTotalMemory (void)
{
  return total;
}

long
_Jv_GCFreeMemory (void)
{
  return 0;
}

void
_Jv_GCSetInitialHeapSize (size_t)
{
}

void
_Jv_GCSetMaximumHeapSize (size_t)
{
}

void
_Jv_DisableGC (void)
{
}

void
_Jv_EnableGC (void)
{
}

void
_Jv_InitGC (void)
{
}

void
_Jv_GCRegisterDisappearingLink (jobject *)
{
}

jboolean
_Jv_GCCanReclaimSoftReference (jobject)
{
  // For now, always reclaim soft references.  FIXME.
  return true;
}

#ifdef JV_HASH_SYNCHRONIZATION
void *
_Jv_AllocTraceOne (jsize size /* includes vtable slot */) 
{
  void *obj = calloc(size, 1);
  if (!obj) _Jv_ThrowNoMemory();
  return result;
}

void *
_Jv_AllocTraceTwo (jsize size /* includes vtable slot */) 
{
  void *obj = calloc(size, 1);
  if (!obj) _Jv_ThrowNoMemory();
  return result;
}
#endif /* JV_HASH_SYNCHRONIZATION */

void
_Jv_SuspendThread (_Jv_Thread_t *thread)
{
}

void
_Jv_ResumeThread (_Jv_Thread_t *thread)
{
}

int
_Jv_IsThreadSuspended (_Jv_Thread_t *thread)
{
  return 0;
}
