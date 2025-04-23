/* Global constructor/destructor support

   Copyright (C) 2024-2025 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "auto-target.h"

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include "gbl-ctors.h"

extern int atexit (void (*function) (void));


/* Host/device compatibility: '__cxa_finalize'.  Dummy; if necessary,
   overridden via libgomp 'target-cxa-dso-dtor.c'.  */

extern void __GCC_offload___cxa_finalize (void *);

void __attribute__((weak))
__GCC_offload___cxa_finalize (void *dso_handle __attribute__((unused)))
{
}

/* There are no DSOs; this is the main program.  */
static void * const __dso_handle = 0;


/* Handler functions ('static', in contrast to the 'gbl-ctors.h'
   prototypes).  */

static void __static_do_global_ctors (void);

static void
__static_do_global_ctors (void)
{
  __SIZE_TYPE__ nptrs = (__SIZE_TYPE__) __CTOR_LIST__[0];
  for (__SIZE_TYPE__ i = nptrs; i >= 1; --i)
    __CTOR_LIST__[i] ();
}

static void __static_do_global_dtors (void);

static void
__static_do_global_dtors (void)
{
  __GCC_offload___cxa_finalize (__dso_handle);

  func_ptr *p = __DTOR_LIST__;
  ++p;
  for (; *p; ++p)
    (*p) ();
}


/* For nvptx target configurations, override the 'crt0.c' dummy.  */

extern void __gbl_ctors (void);

void
__gbl_ctors (void)
{
  __static_do_global_ctors ();
  atexit (__static_do_global_dtors);
}


/* For nvptx offloading configurations, need '.entry' wrappers.  */

# if defined(__nvptx_softstack__) && defined(__nvptx_unisimt__)

/* OpenMP */

/* See 'crt0.c', 'mgomp.c'.  */
extern void *__nvptx_stacks[32] __attribute__((shared,nocommon));
extern unsigned __nvptx_uni[32] __attribute__((shared,nocommon));

__attribute__((kernel)) void __do_global_ctors__entry__mgomp (void *);

void
__do_global_ctors__entry__mgomp (void *nvptx_stacks_0)
{
  __nvptx_stacks[0] = nvptx_stacks_0;
  __nvptx_uni[0] = 0;

  __static_do_global_ctors ();
}

__attribute__((kernel)) void __do_global_dtors__entry__mgomp (void *);

void
__do_global_dtors__entry__mgomp (void *nvptx_stacks_0)
{
  __nvptx_stacks[0] = nvptx_stacks_0;
  __nvptx_uni[0] = 0;

  __static_do_global_dtors ();
}

# else

/* OpenACC */

__attribute__((kernel)) void __do_global_ctors__entry (void);

void
__do_global_ctors__entry (void)
{
  __static_do_global_ctors ();
}

__attribute__((kernel)) void __do_global_dtors__entry (void);

void
__do_global_dtors__entry (void)
{
  __static_do_global_dtors ();
}

# endif


/* The following symbol just provides a means for the nvptx-tools 'ld' to
   trigger linking in this file.  */

int __trigger_gbl_ctors;
