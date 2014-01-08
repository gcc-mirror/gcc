/* Header file for libgcov-*.c.
   Copyright (C) 1996-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_LIBGCOV_H
#define GCC_LIBGCOV_H

/* work around the poisoned malloc/calloc in system.h.  */
#ifndef xmalloc
#define xmalloc malloc
#endif
#ifndef xcalloc
#define xcalloc calloc
#endif

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

#if BITS_PER_UNIT == 8
typedef unsigned gcov_unsigned_t __attribute__ ((mode (SI)));
typedef unsigned gcov_position_t __attribute__ ((mode (SI)));
#if LONG_LONG_TYPE_SIZE > 32
typedef signed gcov_type __attribute__ ((mode (DI)));
typedef unsigned gcov_type_unsigned __attribute__ ((mode (DI)));
#else
typedef signed gcov_type __attribute__ ((mode (SI)));
typedef unsigned gcov_type_unsigned __attribute__ ((mode (SI)));
#endif
#else
#if BITS_PER_UNIT == 16
typedef unsigned gcov_unsigned_t __attribute__ ((mode (HI)));
typedef unsigned gcov_position_t __attribute__ ((mode (HI)));
#if LONG_LONG_TYPE_SIZE > 32
typedef signed gcov_type __attribute__ ((mode (SI)));
typedef unsigned gcov_type_unsigned __attribute__ ((mode (SI)));
#else
typedef signed gcov_type __attribute__ ((mode (HI)));
typedef unsigned gcov_type_unsigned __attribute__ ((mode (HI)));
#endif
#else
typedef unsigned gcov_unsigned_t __attribute__ ((mode (QI)));
typedef unsigned gcov_position_t __attribute__ ((mode (QI)));
#if LONG_LONG_TYPE_SIZE > 32
typedef signed gcov_type __attribute__ ((mode (HI)));
typedef unsigned gcov_type_unsigned __attribute__ ((mode (HI)));
#else
typedef signed gcov_type __attribute__ ((mode (QI)));
typedef unsigned gcov_type_unsigned __attribute__ ((mode (QI)));
#endif
#endif
#endif

#if defined (TARGET_POSIX_IO)
#define GCOV_LOCKED 1
#else
#define GCOV_LOCKED 0
#endif

#if defined(inhibit_libc)
#define IN_LIBGCOV (-1)
#else
#define IN_LIBGCOV 1
#if defined(L_gcov)
#define GCOV_LINKAGE /* nothing */
#endif
#endif

/* In libgcov we need these functions to be extern, so prefix them with
   __gcov.  In libgcov they must also be hidden so that the instance in
   the executable is not also used in a DSO.  */
#define gcov_var __gcov_var
#define gcov_open __gcov_open
#define gcov_close __gcov_close
#define gcov_write_tag_length __gcov_write_tag_length
#define gcov_position __gcov_position
#define gcov_seek __gcov_seek
#define gcov_rewrite __gcov_rewrite
#define gcov_is_error __gcov_is_error
#define gcov_write_unsigned __gcov_write_unsigned
#define gcov_write_counter __gcov_write_counter
#define gcov_write_summary __gcov_write_summary
#define gcov_read_unsigned __gcov_read_unsigned
#define gcov_read_counter __gcov_read_counter
#define gcov_read_summary __gcov_read_summary

/* Poison these, so they don't accidentally slip in.  */
#pragma GCC poison gcov_write_string gcov_write_tag gcov_write_length
#pragma GCC poison gcov_time gcov_magic

#ifdef HAVE_GAS_HIDDEN
#define ATTRIBUTE_HIDDEN  __attribute__ ((__visibility__ ("hidden")))
#else
#define ATTRIBUTE_HIDDEN
#endif

#include "gcov-io.h"

/* Structures embedded in coveraged program.  The structures generated
   by write_profile must match these.  */

/* Information about counters for a single function.  */
struct gcov_ctr_info
{
  gcov_unsigned_t num;		/* number of counters.  */
  gcov_type *values;		/* their values.  */
};

/* Information about a single function.  This uses the trailing array
   idiom. The number of counters is determined from the merge pointer
   array in gcov_info.  The key is used to detect which of a set of
   comdat functions was selected -- it points to the gcov_info object
   of the object file containing the selected comdat function.  */

struct gcov_fn_info
{
  const struct gcov_info *key;		/* comdat key */
  gcov_unsigned_t ident;		/* unique ident of function */
  gcov_unsigned_t lineno_checksum;	/* function lineo_checksum */
  gcov_unsigned_t cfg_checksum;		/* function cfg checksum */
  struct gcov_ctr_info ctrs[0];		/* instrumented counters */
};

/* Type of function used to merge counters.  */
typedef void (*gcov_merge_fn) (gcov_type *, gcov_unsigned_t);

/* Information about a single object file.  */
struct gcov_info
{
  gcov_unsigned_t version;	/* expected version number */
  struct gcov_info *next;	/* link to next, used by libgcov */

  gcov_unsigned_t stamp;	/* uniquifying time stamp */
  const char *filename;		/* output file name */

  gcov_merge_fn merge[GCOV_COUNTERS];  /* merge functions (null for
					  unused) */
  
  unsigned n_functions;		/* number of functions */
  const struct gcov_fn_info *const *functions; /* pointer to pointers
					          to function information  */
};

/* Register a new object file module.  */
extern void __gcov_init (struct gcov_info *) ATTRIBUTE_HIDDEN;

/* Called before fork, to avoid double counting.  */
extern void __gcov_flush (void) ATTRIBUTE_HIDDEN;

/* Function to reset all counters to 0.  */
extern void __gcov_reset (void);

/* Function to enable early write of profile information so far.  */
extern void __gcov_dump (void);

/* The merge function that just sums the counters.  */
extern void __gcov_merge_add (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function to select the minimum valid counter value.  */
extern void __gcov_merge_time_profile (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function to choose the most common value.  */
extern void __gcov_merge_single (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function to choose the most common difference between
   consecutive values.  */
extern void __gcov_merge_delta (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function that just ors the counters together.  */
extern void __gcov_merge_ior (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The profiler functions.  */
extern void __gcov_interval_profiler (gcov_type *, gcov_type, int, unsigned);
extern void __gcov_pow2_profiler (gcov_type *, gcov_type);
extern void __gcov_one_value_profiler (gcov_type *, gcov_type);
extern void __gcov_indirect_call_profiler (gcov_type*, gcov_type,
                                           void*, void*);
extern void __gcov_indirect_call_profiler_v2 (gcov_type, void *);
extern void __gcov_time_profiler (gcov_type *);
extern void __gcov_average_profiler (gcov_type *, gcov_type);
extern void __gcov_ior_profiler (gcov_type *, gcov_type);

#ifndef inhibit_libc
/* The wrappers around some library functions..  */
extern pid_t __gcov_fork (void) ATTRIBUTE_HIDDEN;
extern int __gcov_execl (const char *, char *, ...) ATTRIBUTE_HIDDEN;
extern int __gcov_execlp (const char *, char *, ...) ATTRIBUTE_HIDDEN;
extern int __gcov_execle (const char *, char *, ...) ATTRIBUTE_HIDDEN;
extern int __gcov_execv (const char *, char *const []) ATTRIBUTE_HIDDEN;
extern int __gcov_execvp (const char *, char *const []) ATTRIBUTE_HIDDEN;
extern int __gcov_execve (const char *, char  *const [], char *const [])
  ATTRIBUTE_HIDDEN;

/* Functions that only available in libgcov.  */
GCOV_LINKAGE int gcov_open (const char */*name*/) ATTRIBUTE_HIDDEN;
GCOV_LINKAGE void gcov_write_counter (gcov_type) ATTRIBUTE_HIDDEN;
GCOV_LINKAGE void gcov_write_tag_length (gcov_unsigned_t, gcov_unsigned_t)
    ATTRIBUTE_HIDDEN;
GCOV_LINKAGE void gcov_write_summary (gcov_unsigned_t /*tag*/,
                                      const struct gcov_summary *)
    ATTRIBUTE_HIDDEN;
GCOV_LINKAGE void gcov_seek (gcov_position_t /*position*/) ATTRIBUTE_HIDDEN;
GCOV_LINKAGE inline void gcov_rewrite (void);

#endif /* !inhibit_libc */

#endif /* GCC_LIBGCOV_H */
