// -*- c++ -*-
// boehm-gc.h - Defines for Boehm collector.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

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
};

#endif /* __JV_BOEHM_GC__ */
