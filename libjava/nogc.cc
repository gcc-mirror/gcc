// nogc.cc - Code to implement no GC.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

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
_Jv_AllocObj (jsize size)
{
  total += size;
  return calloc (size, 1);
}

void *
_Jv_AllocArray (jsize size)
{
  total += size;
  return calloc (size, 1);
}

void *
_Jv_AllocBytes (jsize size)
{
  total += size;
  return calloc (size, 1);
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
_Jv_GCSetInitialHeapSize (size_t size)
{
}

void
_Jv_GCSetMaximumHeapSize (size_t size)
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
