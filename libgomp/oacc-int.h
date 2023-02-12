/* OpenACC Runtime - internal declarations

   Copyright (C) 2013-2023 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains data types and function declarations that are not
   part of the official OpenACC user interface.  There are declarations
   in here that are part of the GNU OpenACC ABI, in that the compiler is
   required to know about them and use them.

   The convention is that the all caps prefix "GOACC" is used group items
   that are part of the external ABI, and the lower case prefix "goacc"
   is used group items that are completely private to the library.  */

#ifndef OACC_INT_H
#define OACC_INT_H 1

#include "openacc.h"
#include "config.h"
#include "acc_prof.h"
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility push(hidden)
#endif

static inline enum acc_device_t
acc_device_type (enum offload_target_type type)
{
  return (enum acc_device_t) type;
}

struct goacc_thread
{
  /* The base device for the current thread.  */
  struct gomp_device_descr *base_dev;

  /* The device for the current thread.  */
  struct gomp_device_descr *dev;

  struct gomp_device_descr *saved_bound_dev;

  /* This is a linked list of data mapped by the "acc data" pragma, following
     strictly push/pop semantics according to lexical scope.  */
  struct target_mem_desc *mapped_data;

  /* Data of the OpenACC Profiling Interface.  */
  acc_prof_info *prof_info;
  acc_api_info *api_info;
  /* Per-thread toggle of OpenACC Profiling Interface callbacks.  */
  bool prof_callbacks_enabled;

  /* These structures form a list: this is the next thread in that list.  */
  struct goacc_thread *next;

  /* Target-specific data (used by plugin).  */
  void *target_tls;
};

#ifdef __AMDGCN__
static inline struct goacc_thread *
goacc_thread (void)
{
  /* Unused in the offload libgomp for OpenACC: return a dummy value.  */
  return 0;
}
#elif defined HAVE_TLS || defined USE_EMUTLS
extern __thread struct goacc_thread *goacc_tls_data;
static inline struct goacc_thread *
goacc_thread (void)
{
  return goacc_tls_data;
}
#else
extern pthread_key_t goacc_tls_key;
static inline struct goacc_thread *
goacc_thread (void)
{
  return pthread_getspecific (goacc_tls_key);
}
#endif

void goacc_register (struct gomp_device_descr *) __GOACC_NOTHROW;
void goacc_attach_host_thread_to_device (int);
void goacc_runtime_initialize (void);
void goacc_save_and_set_bind (acc_device_t);
void goacc_restore_bind (void);
void goacc_lazy_initialize (void);
void goacc_host_init (void);

void goacc_wait (int, int, va_list *);
void goacc_init_asyncqueues (struct gomp_device_descr *);
bool goacc_fini_asyncqueues (struct gomp_device_descr *);
void goacc_async_free (struct gomp_device_descr *, struct goacc_asyncqueue *,
		       void *);
struct goacc_asyncqueue *get_goacc_asyncqueue (int);
struct goacc_asyncqueue *lookup_goacc_asyncqueue (struct goacc_thread *, bool,
						  int);
static inline bool
async_valid_stream_id_p (int async)
{
  return async >= 0;
}

static inline bool
async_valid_p (int async)
{
  return (async == acc_async_noval || async == acc_async_sync
	  || async_valid_stream_id_p (async));
}

static inline bool
async_synchronous_p (int async)
{
  if (!async_valid_p (async))
    return true;

  return async == acc_async_sync;
}


extern bool goacc_prof_enabled;
/* Tune for the (very common) case that profiling is not enabled.  */
#define GOACC_PROF_ENABLED \
  (__builtin_expect (__atomic_load_n (&goacc_prof_enabled, \
				      MEMMODEL_ACQUIRE) == true, false))

void goacc_profiling_initialize (void);
bool _goacc_profiling_dispatch_p (bool);
/* Tune for the (very common) case that profiling is not enabled.  */
#define GOACC_PROFILING_DISPATCH_P(...) \
  (GOACC_PROF_ENABLED \
   && _goacc_profiling_dispatch_p (__VA_ARGS__))
bool _goacc_profiling_setup_p (struct goacc_thread *,
			       acc_prof_info *, acc_api_info *);
/* Tune for the (very common) case that profiling is not enabled.  */
#define GOACC_PROFILING_SETUP_P(...) \
  (GOACC_PROFILING_DISPATCH_P (false) \
   && _goacc_profiling_setup_p (__VA_ARGS__))
void goacc_profiling_dispatch (acc_prof_info *, acc_event_info *,
			       acc_api_info *);

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility pop
#endif

#endif
