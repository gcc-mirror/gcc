/* The libgomp plugin API.

   Copyright (C) 2014-2025 Free Software Foundation, Inc.

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

#ifndef LIBGOMP_PLUGIN_H
#define LIBGOMP_PLUGIN_H 1

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef _LIBGOMP_PLUGIN_INCLUDE
  /* Include 'omp.h' for the interop definitions.  */
  #define _LIBGOMP_OMP_LOCK_DEFINED 1
  typedef struct omp_lock_t omp_lock_t;
  typedef struct omp_nest_lock_t omp_nest_lock_t;
  #include "omp.h.in"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Capabilities of offloading devices.  */
#define GOMP_OFFLOAD_CAP_SHARED_MEM	(1 << 0)
#define GOMP_OFFLOAD_CAP_NATIVE_EXEC	(1 << 1)
#define GOMP_OFFLOAD_CAP_OPENMP_400	(1 << 2)
#define GOMP_OFFLOAD_CAP_OPENACC_200	(1 << 3)

/* Type of offload target device.  Keep in sync with include/gomp-constants.h.  */
enum offload_target_type
{
  OFFLOAD_TARGET_TYPE_HOST = 2,
  /* OFFLOAD_TARGET_TYPE_HOST_NONSHM = 3 removed.  */
  OFFLOAD_TARGET_TYPE_NVIDIA_PTX = 5,
  OFFLOAD_TARGET_TYPE_HSA = 7,
  OFFLOAD_TARGET_TYPE_GCN = 8
};

/* Opaque type to represent plugin-dependent implementation of an
   OpenACC asynchronous queue.  */
struct goacc_asyncqueue;

/* Used to keep a list of active asynchronous queues.  */
struct goacc_asyncqueue_list
{
  struct goacc_asyncqueue *aq;
  struct goacc_asyncqueue_list *next;
};

typedef struct goacc_asyncqueue *goacc_aq;
typedef struct goacc_asyncqueue_list *goacc_aq_list;


/* OpenACC 'acc_get_property' support.  */

/* Device property values.  Keep in sync with
   'libgomp/{openacc.h,openacc.f90}:acc_device_property_t'.  */
enum goacc_property
  {
   /* Mask to tell numeric and string values apart.  */
#define GOACC_PROPERTY_STRING_MASK 0x10000

   /* Start from 1 to catch uninitialized use.  */
   GOACC_PROPERTY_MEMORY =		1,
   GOACC_PROPERTY_FREE_MEMORY =		2,
   GOACC_PROPERTY_NAME =		GOACC_PROPERTY_STRING_MASK | 1,
   GOACC_PROPERTY_VENDOR =		GOACC_PROPERTY_STRING_MASK | 2,
   GOACC_PROPERTY_DRIVER =		GOACC_PROPERTY_STRING_MASK | 3
  };

/* Container type for passing device properties.  */
union goacc_property_value
{
  const char *ptr;
  size_t val;
};


/* Auxiliary struct, used for transferring pairs of addresses from plugin
   to libgomp.  */
struct addr_pair
{
  uintptr_t start;
  uintptr_t end;
};


#ifdef _LIBGOMP_OMP_LOCK_DEFINED
/* Only define when omp.h.in was included, as in plugin/ and in libgomp.h.   */
struct interop_obj_t
{
  void *stream;
  void *device_data;
  omp_interop_fr_t fr;
  int device_num;
};

enum gomp_interop_flag
{
  gomp_interop_flag_init,
  gomp_interop_flag_use,
  gomp_interop_flag_destroy
};
#endif

/* This following symbol is used to name the target side variable struct that
   holds the designated ICVs of the target device. The symbol needs to be
   available to libgomp code and the offload plugin (which in the latter case
   must be stringified).  */
#define GOMP_ADDITIONAL_ICVS __gomp_additional_icvs

#define GOMP_INDIRECT_ADDR_MAP __gomp_indirect_addr_map

/* Miscellaneous functions.  */
extern void *GOMP_PLUGIN_malloc (size_t) __attribute__ ((malloc));
extern void *GOMP_PLUGIN_malloc_cleared (size_t) __attribute__ ((malloc));
extern void *GOMP_PLUGIN_realloc (void *, size_t);
void GOMP_PLUGIN_target_task_completion (void *);

extern void GOMP_PLUGIN_debug (int, const char *, ...)
	__attribute__ ((format (printf, 2, 3)));
extern void GOMP_PLUGIN_error (const char *, ...)
	__attribute__ ((format (printf, 1, 2)));
extern void GOMP_PLUGIN_fatal (const char *, ...)
	__attribute__ ((noreturn, format (printf, 1, 2)));

extern void GOMP_PLUGIN_target_rev (uint64_t, uint64_t, uint64_t, uint64_t,
				    uint64_t, int, struct goacc_asyncqueue *);

/* Prototypes for functions implemented by libgomp plugins.  */
extern const char *GOMP_OFFLOAD_get_name (void);
extern const char *GOMP_OFFLOAD_get_uid (int);
extern unsigned int GOMP_OFFLOAD_get_caps (void);
extern int GOMP_OFFLOAD_get_type (void);
extern int GOMP_OFFLOAD_get_num_devices (unsigned int);
extern bool GOMP_OFFLOAD_init_device (int);
extern bool GOMP_OFFLOAD_fini_device (int);
extern unsigned GOMP_OFFLOAD_version (void);
extern int GOMP_OFFLOAD_load_image (int, unsigned, const void *,
				    struct addr_pair **, uint64_t **,
				    uint64_t *);
extern bool GOMP_OFFLOAD_unload_image (int, unsigned, const void *);
extern void *GOMP_OFFLOAD_alloc (int, size_t);
extern bool GOMP_OFFLOAD_free (int, void *);
extern bool GOMP_OFFLOAD_dev2host (int, void *, const void *, size_t);
extern bool GOMP_OFFLOAD_host2dev (int, void *, const void *, size_t);
extern bool GOMP_OFFLOAD_dev2dev (int, void *, const void *, size_t);
extern int GOMP_OFFLOAD_memcpy2d (int, int, size_t, size_t,
				  void*, size_t, size_t, size_t,
				  const void*, size_t, size_t, size_t);
extern int GOMP_OFFLOAD_memcpy3d (int, int, size_t, size_t, size_t, void *,
				  size_t, size_t, size_t, size_t, size_t,
				  const void *, size_t, size_t, size_t, size_t,
				  size_t);
extern bool GOMP_OFFLOAD_can_run (void *);
extern void GOMP_OFFLOAD_run (int, void *, void *, void **);
extern void GOMP_OFFLOAD_async_run (int, void *, void *, void **, void *);

extern void GOMP_OFFLOAD_openacc_exec (void (*) (void *), size_t, void **,
				       void **, unsigned *, void *);
extern void *GOMP_OFFLOAD_openacc_create_thread_data (int);
extern void GOMP_OFFLOAD_openacc_destroy_thread_data (void *);
extern struct goacc_asyncqueue *GOMP_OFFLOAD_openacc_async_construct (int);
extern bool GOMP_OFFLOAD_openacc_async_destruct (struct goacc_asyncqueue *);
extern int GOMP_OFFLOAD_openacc_async_test (struct goacc_asyncqueue *);
extern bool GOMP_OFFLOAD_openacc_async_synchronize (struct goacc_asyncqueue *);
extern bool GOMP_OFFLOAD_openacc_async_serialize (struct goacc_asyncqueue *,
						  struct goacc_asyncqueue *);
extern void GOMP_OFFLOAD_openacc_async_queue_callback (struct goacc_asyncqueue *,
						       void (*)(void *), void *);
extern void GOMP_OFFLOAD_openacc_async_exec (void (*) (void *), size_t, void **,
					     void **, unsigned *, void *,
					     struct goacc_asyncqueue *);
extern bool GOMP_OFFLOAD_openacc_async_dev2host (int, void *, const void *, size_t,
						 struct goacc_asyncqueue *);
extern bool GOMP_OFFLOAD_openacc_async_host2dev (int, void *, const void *, size_t,
						 struct goacc_asyncqueue *);
extern void *GOMP_OFFLOAD_openacc_cuda_get_current_device (void);
extern void *GOMP_OFFLOAD_openacc_cuda_get_current_context (void);
extern void *GOMP_OFFLOAD_openacc_cuda_get_stream (struct goacc_asyncqueue *);
extern int GOMP_OFFLOAD_openacc_cuda_set_stream (struct goacc_asyncqueue *,
						 void *);
extern union goacc_property_value
  GOMP_OFFLOAD_openacc_get_property (int, enum goacc_property);

#ifdef _LIBGOMP_OMP_LOCK_DEFINED
/* Only define when omp.h.in was included, as in plugin/ and in libgomp.h.   */
extern void GOMP_OFFLOAD_interop (struct interop_obj_t *, int,
				  enum gomp_interop_flag, bool, const char *);
extern intptr_t GOMP_OFFLOAD_get_interop_int (struct interop_obj_t *,
					      omp_interop_property_t,
					      omp_interop_rc_t *);
extern void *GOMP_OFFLOAD_get_interop_ptr (struct interop_obj_t *,
					   omp_interop_property_t,
					   omp_interop_rc_t *);
extern const char *GOMP_OFFLOAD_get_interop_str (struct interop_obj_t *obj,
						 omp_interop_property_t,
						 omp_interop_rc_t *);
extern const char *GOMP_OFFLOAD_get_interop_type_desc (struct interop_obj_t *,
						       omp_interop_property_t);
#endif

#ifdef __cplusplus
}
#endif

#endif
