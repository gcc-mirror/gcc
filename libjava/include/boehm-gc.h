// -*- c++ -*-
// boehm-gc.h - Defines for Boehm collector.

/* Copyright (C) 1998, 1999, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_BOEHM_GC__
#define __JV_BOEHM_GC__

#define JV_MARKOBJ_DECL void *_Jv_MarkObj (void *, void *, void *, void *)
#define JV_MARKARRAY_DECL void *_Jv_MarkArray (void *, void *, void *, void *)

extern "C"
{
  JV_MARKOBJ_DECL;
  JV_MARKARRAY_DECL;
}

// Enough stuff to inline _Jv_AllocObj.  Ugly.
#include <gcj/javaprims.h>
#include <java/lang/Class.h>
#include <string.h>

extern "C" void * GC_gcj_malloc(size_t, void *);
extern "C" void * GC_malloc_atomic(size_t);
#ifdef THREAD_LOCAL_ALLOC
extern "C" void * GC_local_gcj_malloc(size_t, void *);
extern "C" void * GC_local_malloc_atomic(size_t);
#endif

inline void *
_Jv_AllocObj (jsize size, jclass klass)
{
  // This should call GC_GCJ_MALLOC, but that would involve
  // including gc.h.
#ifdef THREAD_LOCAL_ALLOC
  return GC_local_gcj_malloc (size, klass->vtable);
#else 
  return GC_gcj_malloc (size, klass->vtable);
#endif
}

inline void *
_Jv_AllocPtrFreeObj (jsize size, jclass klass)
{
#ifdef JV_HASH_SYNCHRONIZATION
# ifdef THREAD_LOCAL_ALLOC
    void * obj = GC_local_malloc_atomic(size);
# else
    void * obj = GC_malloc_atomic(size);
# endif
  *((_Jv_VTable **) obj) = klass->vtable;
#else
# ifdef THREAD_LOCAL_ALLOC
    void * obj = GC_local_gcj_malloc(size, klass->vtable);
# else
    void * obj = GC_gcj_malloc(size, klass->vtable);
# endif
#endif
  return obj;
}

// _Jv_AllocBytes (jsize size) should go here, too.  But clients don't
// usually include this header.

#endif /* __JV_BOEHM_GC__ */
