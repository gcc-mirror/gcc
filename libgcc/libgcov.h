/* Header file for libgcov-*.c.
   Copyright (C) 1996-2024 Free Software Foundation, Inc.

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

#ifndef IN_GCOV_TOOL
/* About the target.  */
/* This path will be used by libgcov runtime.  */

#include "tconfig.h"
#include "auto-target.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "gcov.h"

#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#if __CHAR_BIT__ == 8
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
#if __CHAR_BIT__ == 16
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

#if defined (__MSVCRT__)
#define GCOV_LOCKED_WITH_LOCKING 1
#else
#define GCOV_LOCKED_WITH_LOCKING 0
#endif

/* Detect whether target can support atomic update of profilers.  */
#if (__SIZEOF_LONG_LONG__ == 4 && __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4) \
    || (__SIZEOF_LONG_LONG__ == 8 && __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8) \
    || __LIBGCC_HAVE_LIBATOMIC
#define GCOV_SUPPORTS_ATOMIC 1
#else
#define GCOV_SUPPORTS_ATOMIC 0
#endif

/* In libgcov we need these functions to be extern, so prefix them with
   __gcov.  In libgcov they must also be hidden so that the instance in
   the executable is not also used in a DSO.  */
#define gcov_var __gcov_var
#define gcov_open __gcov_open
#define gcov_close __gcov_close
#define gcov_position __gcov_position
#define gcov_rewrite __gcov_rewrite
#define gcov_is_error __gcov_is_error
#define gcov_write_unsigned __gcov_write_unsigned
#define gcov_write_object_summary __gcov_write_object_summary
#define gcov_read_unsigned __gcov_read_unsigned
#define gcov_read_counter __gcov_read_counter
#define gcov_read_summary __gcov_read_summary

#else /* IN_GCOV_TOOL */
/* About the host.  */
/* This path will be compiled for the host and linked into
   gcov-tool binary.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

typedef unsigned gcov_unsigned_t;
typedef unsigned gcov_position_t;
/* gcov_type is typedef'd elsewhere for the compiler */

#if defined (HOST_HAS_F_SETLKW)
#define GCOV_LOCKED 1
#else
#define GCOV_LOCKED 0
#endif

#if defined (HOST_HAS_LK_LOCK)
#define GCOV_LOCKED_WITH_LOCKING 1
#else
#define GCOV_LOCKED_WITH_LOCKING 0
#endif

/* Some Macros specific to gcov-tool.  */

#define L_gcov 1
#define L_gcov_merge_add 1
#define L_gcov_merge_topn 1
#define L_gcov_merge_ior 1
#define L_gcov_merge_time_profile 1

extern gcov_type gcov_read_counter_mem ();
extern unsigned gcov_get_merge_weight ();
extern struct gcov_info *gcov_list;

#endif /* !IN_GCOV_TOOL */

#if defined(inhibit_libc)
#define IN_LIBGCOV (-1)
#else
#define IN_LIBGCOV 1
#if defined(L_gcov)
#define GCOV_LINKAGE /* nothing */
#endif
#endif

/* Poison these, so they don't accidentally slip in.  */
#pragma GCC poison gcov_write_string gcov_write_tag gcov_write_length
#pragma GCC poison gcov_time

#ifdef HAVE_GAS_HIDDEN
#define ATTRIBUTE_HIDDEN  __attribute__ ((__visibility__ ("hidden")))
#else
#define ATTRIBUTE_HIDDEN
#endif

#if HAVE_SYS_MMAN_H
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

#if !defined (MAP_ANONYMOUS) && defined (MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif
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
  struct gcov_ctr_info ctrs[1];		/* instrumented counters */
};

/* Type of function used to merge counters.  */
typedef void (*gcov_merge_fn) (gcov_type *, gcov_unsigned_t);

/* Information about a single object file.  */
struct gcov_info
{
  gcov_unsigned_t version;	/* expected version number */
  struct gcov_info *next;	/* link to next, used by libgcov */

  gcov_unsigned_t stamp;	/* uniquifying time stamp */
  gcov_unsigned_t checksum;	/* unique object checksum */
  const char *filename;		/* output file name */

  gcov_merge_fn merge[GCOV_COUNTERS];  /* merge functions (null for
					  unused) */

  gcov_unsigned_t n_functions;		/* number of functions */

#ifndef IN_GCOV_TOOL
  const struct gcov_fn_info *const *functions; /* pointer to pointers
                                                  to function information  */
#else
  struct gcov_fn_info **functions;
  struct gcov_summary summary;
#endif /* !IN_GCOV_TOOL */
};

/* Root of a program/shared-object state */
struct gcov_root
{
  struct gcov_info *list;
  unsigned dumped : 1;	/* counts have been dumped.  */
  unsigned run_counted : 1;  /* run has been accounted for.  */
  struct gcov_root *next;
  struct gcov_root *prev;
};

extern struct gcov_root __gcov_root ATTRIBUTE_HIDDEN;

struct gcov_master
{
  gcov_unsigned_t version;
  struct gcov_root *root;
};

struct indirect_call_tuple
{
  /* Callee function.  */
  void *callee;

  /* Pointer to counters.  */
  gcov_type *counters;
};

/* Exactly one of these will be active in the process.  */
extern struct gcov_master __gcov_master;
extern struct gcov_kvp *__gcov_kvp_dynamic_pool;
extern unsigned __gcov_kvp_dynamic_pool_index;
extern unsigned __gcov_kvp_dynamic_pool_size;

/* Dump a set of gcov objects.  */
extern void __gcov_dump_one (struct gcov_root *) ATTRIBUTE_HIDDEN;

/* Register a new object file module.  */
extern void __gcov_init (struct gcov_info *) ATTRIBUTE_HIDDEN;

/* GCOV exit function registered via a static destructor.  */
extern void __gcov_exit (void) ATTRIBUTE_HIDDEN;

/* Function to reset all counters to 0.  Both externally visible (and
   overridable) and internal version.  */
extern void __gcov_reset_int (void) ATTRIBUTE_HIDDEN;

/* User function to enable early write of profile information so far.  */
extern void __gcov_dump_int (void) ATTRIBUTE_HIDDEN;

/* Lock critical section for __gcov_dump and __gcov_reset functions.  */
extern void __gcov_lock (void) ATTRIBUTE_HIDDEN;

/* Unlock critical section for __gcov_dump and __gcov_reset functions.  */
extern void __gcov_unlock (void) ATTRIBUTE_HIDDEN;

/* The merge function that just sums the counters.  */
extern void __gcov_merge_add (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function to select the minimum valid counter value.  */
extern void __gcov_merge_time_profile (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function to choose the most common N values.  */
extern void __gcov_merge_topn (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The merge function that just ors the counters together.  */
extern void __gcov_merge_ior (gcov_type *, unsigned) ATTRIBUTE_HIDDEN;

/* The profiler functions.  */
extern void __gcov_interval_profiler (gcov_type *, gcov_type, int, unsigned);
extern void __gcov_interval_profiler_atomic (gcov_type *, gcov_type, int,
					     unsigned);
extern void __gcov_pow2_profiler (gcov_type *, gcov_type);
extern void __gcov_pow2_profiler_atomic (gcov_type *, gcov_type);
extern void __gcov_topn_values_profiler (gcov_type *, gcov_type);
extern void __gcov_topn_values_profiler_atomic (gcov_type *, gcov_type);
extern void __gcov_indirect_call_profiler_v4 (gcov_type, void *);
extern void __gcov_indirect_call_profiler_v4_atomic (gcov_type, void *);
extern void __gcov_time_profiler (gcov_type *);
extern void __gcov_time_profiler_atomic (gcov_type *);
extern void __gcov_average_profiler (gcov_type *, gcov_type);
extern void __gcov_average_profiler_atomic (gcov_type *, gcov_type);
extern void __gcov_ior_profiler (gcov_type *, gcov_type);
extern void __gcov_ior_profiler_atomic (gcov_type *, gcov_type);

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
GCOV_LINKAGE void gcov_write_object_summary (const struct gcov_summary *)
    ATTRIBUTE_HIDDEN;
GCOV_LINKAGE void gcov_rewrite (void) ATTRIBUTE_HIDDEN;

/* "Counts" stored in gcda files can be a real counter value, or
   an target address. When differentiate these two types because
   when manipulating counts, we should only change real counter values,
   rather target addresses.  */

static inline gcov_type
gcov_get_counter (void)
{
#ifndef IN_GCOV_TOOL
  /* This version is for reading count values in libgcov runtime:
     we read from gcda files.  */

  return gcov_read_counter ();
#else
  /* This version is for gcov-tool. We read the value from memory and
     multiply it by the merge weight.  */

  return gcov_read_counter_mem () * gcov_get_merge_weight ();
#endif
}

/* Similar function as gcov_get_counter(), but do not scale
   when read value is equal to IGNORE_SCALING.  */

static inline gcov_type
gcov_get_counter_ignore_scaling (gcov_type ignore_scaling ATTRIBUTE_UNUSED)
{
#ifndef IN_GCOV_TOOL
  /* This version is for reading count values in libgcov runtime:
     we read from gcda files.  */

  return gcov_read_counter ();
#else
  /* This version is for gcov-tool. We read the value from memory and
     multiply it by the merge weight.  */

  gcov_type v = gcov_read_counter_mem ();
  if (v != ignore_scaling)
    v *= gcov_get_merge_weight ();

  return v;
#endif
}

/* Similar function as gcov_get_counter(), but handles target address
   counters.  */

static inline gcov_type
gcov_get_counter_target (void)
{
#ifndef IN_GCOV_TOOL
  /* This version is for reading count target values in libgcov runtime:
     we read from gcda files.  */

  return gcov_read_counter ();
#else
  /* This version is for gcov-tool.  We read the value from memory and we do NOT
     multiply it by the merge weight.  */

  return gcov_read_counter_mem ();
#endif
}

/* Add VALUE to *COUNTER and make it with atomic operation
   if USE_ATOMIC is true.  */

static inline void
gcov_counter_add (gcov_type *counter, gcov_type value,
		  int use_atomic ATTRIBUTE_UNUSED)
{
#if GCOV_SUPPORTS_ATOMIC
  if (use_atomic)
    __atomic_fetch_add (counter, value, __ATOMIC_RELAXED);
  else
#endif
    *counter += value;
}

#if HAVE_SYS_MMAN_H

/* Allocate LENGTH with mmap function.  */

static inline void *
malloc_mmap (size_t length)
{
  return mmap (NULL, length, PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

#endif

/* Allocate gcov_kvp from statically pre-allocated pool,
   or use heap otherwise.  */

static inline struct gcov_kvp *
allocate_gcov_kvp (void)
{
#define MMAP_CHUNK_SIZE	(128 * 1024)
  struct gcov_kvp *new_node = NULL;
  unsigned kvp_sizeof = sizeof(struct gcov_kvp);

  /* Try mmaped pool if available.  */
#if !defined(IN_GCOV_TOOL) && !defined(L_gcov_merge_topn) && HAVE_SYS_MMAN_H
  if (__gcov_kvp_dynamic_pool == NULL
      || __gcov_kvp_dynamic_pool_index >= __gcov_kvp_dynamic_pool_size)
    {
      void *ptr = malloc_mmap (MMAP_CHUNK_SIZE);
      if (ptr != MAP_FAILED)
	{
	  __gcov_kvp_dynamic_pool = ptr;
	  __gcov_kvp_dynamic_pool_size = MMAP_CHUNK_SIZE / kvp_sizeof;
	  __gcov_kvp_dynamic_pool_index = 0;
	}
    }

  if (__gcov_kvp_dynamic_pool != NULL)
    {
      unsigned index;
#if GCOV_SUPPORTS_ATOMIC
      index
	= __atomic_fetch_add (&__gcov_kvp_dynamic_pool_index, 1,
			      __ATOMIC_RELAXED);
#else
      index = __gcov_kvp_dynamic_pool_index++;
#endif
      if (index < __gcov_kvp_dynamic_pool_size)
	new_node = __gcov_kvp_dynamic_pool + index;
    }
#endif

  /* Fallback to malloc.  */
  if (new_node == NULL)
    new_node = (struct gcov_kvp *)xcalloc (1, kvp_sizeof);

  return new_node;
}

/* Add key value pair VALUE:COUNT to a top N COUNTERS.  When INCREMENT_TOTAL
   is true, add COUNT to total of the TOP counter.  If USE_ATOMIC is true,
   do it in atomic way.  Return true when the counter is full, otherwise
   return false.  */

static inline unsigned
gcov_topn_add_value (gcov_type *counters, gcov_type value, gcov_type count,
		     int use_atomic, int increment_total)
{
  if (increment_total)
    {
      /* In the multi-threaded mode, we can have an already merged profile
	 with a negative total value.  In that case, we should bail out.  */
      if (counters[0] < 0)
	return 0;
      gcov_counter_add (&counters[0], 1, use_atomic);
    }

  struct gcov_kvp *prev_node = NULL;
  struct gcov_kvp *minimal_node = NULL;
  struct gcov_kvp *current_node  = (struct gcov_kvp *)(intptr_t)counters[2];

  while (current_node)
    {
      if (current_node->value == value)
	{
	  gcov_counter_add (&current_node->count, count, use_atomic);
	  return 0;
	}

      if (minimal_node == NULL
	  || current_node->count < minimal_node->count)
	minimal_node = current_node;

      prev_node = current_node;
      current_node = current_node->next;
    }

  if (counters[1] == GCOV_TOPN_MAXIMUM_TRACKED_VALUES)
    {
      if (--minimal_node->count < count)
	{
	  minimal_node->value = value;
	  minimal_node->count = count;
	}

      return 1;
    }
  else
    {
      struct gcov_kvp *new_node = allocate_gcov_kvp ();
      if (new_node == NULL)
	return 0;

      new_node->value = value;
      new_node->count = count;

      int success = 0;
      if (!counters[2])
	{
#if GCOV_SUPPORTS_ATOMIC
	  if (use_atomic)
	    {
	      struct gcov_kvp **ptr = (struct gcov_kvp **)(intptr_t)&counters[2];
	      success = !__sync_val_compare_and_swap (ptr, 0, new_node);
	    }
	  else
#endif
	    {
	      counters[2] = (intptr_t)new_node;
	      success = 1;
	    }
	}
      else if (prev_node && !prev_node->next)
	{
#if GCOV_SUPPORTS_ATOMIC
	  if (use_atomic)
	    success = !__sync_val_compare_and_swap (&prev_node->next, 0,
						    new_node);
	  else
#endif
	    {
	      prev_node->next = new_node;
	      success = 1;
	    }
	}

      /* Increment number of nodes.  */
      if (success)
	gcov_counter_add (&counters[1], 1, use_atomic);
    }

  return 0;
}

#endif /* !inhibit_libc */

#endif /* GCC_LIBGCOV_H */
