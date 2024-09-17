/* Plugin for AMD GCN execution.

   Copyright (C) 2013-2024 Free Software Foundation, Inc.

   Contributed by Mentor Embedded

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

/* {{{ Includes and defines  */

#include "config.h"
#include "symcat.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <inttypes.h>
#include <stdbool.h>
#include <limits.h>
#include <hsa.h>
#include <hsa_ext_amd.h>
#include <dlfcn.h>
#include <signal.h>
#include "libgomp-plugin.h"
#include "config/gcn/libgomp-gcn.h"  /* For struct output.  */
#include "gomp-constants.h"
#include <elf.h>
#include "oacc-plugin.h"
#include "oacc-int.h"
#include <assert.h>

/* These probably won't be in elf.h for a while.  */
#ifndef R_AMDGPU_NONE
#define R_AMDGPU_NONE		0
#define R_AMDGPU_ABS32_LO	1	/* (S + A) & 0xFFFFFFFF  */
#define R_AMDGPU_ABS32_HI	2	/* (S + A) >> 32  */
#define R_AMDGPU_ABS64		3	/* S + A  */
#define R_AMDGPU_REL32		4	/* S + A - P  */
#define R_AMDGPU_REL64		5	/* S + A - P  */
#define R_AMDGPU_ABS32		6	/* S + A  */
#define R_AMDGPU_GOTPCREL	7	/* G + GOT + A - P  */
#define R_AMDGPU_GOTPCREL32_LO	8	/* (G + GOT + A - P) & 0xFFFFFFFF  */
#define R_AMDGPU_GOTPCREL32_HI	9	/* (G + GOT + A - P) >> 32  */
#define R_AMDGPU_REL32_LO	10	/* (S + A - P) & 0xFFFFFFFF  */
#define R_AMDGPU_REL32_HI	11	/* (S + A - P) >> 32  */
#define R_AMDGPU_RELATIVE64	13	/* B + A  */
#endif

/* GCN specific definitions for asynchronous queues.  */

#define ASYNC_QUEUE_SIZE 64
#define DRAIN_QUEUE_SYNCHRONOUS_P false
#define DEBUG_QUEUES 0
#define DEBUG_THREAD_SLEEP 0
#define DEBUG_THREAD_SIGNAL 0

/* Defaults.  */
#define DEFAULT_GCN_HEAP_SIZE (100*1024*1024)  /* 100MB.  */

/* Secure getenv() which returns NULL if running as SUID/SGID.  */
#ifndef HAVE_SECURE_GETENV
#ifdef HAVE___SECURE_GETENV
#define secure_getenv __secure_getenv
#elif defined (HAVE_UNISTD_H) && defined(HAVE_GETUID) && defined(HAVE_GETEUID) \
  && defined(HAVE_GETGID) && defined(HAVE_GETEGID)

#include <unistd.h>

/* Implementation of secure_getenv() for targets where it is not provided but
   we have at least means to test real and effective IDs. */

static char *
secure_getenv (const char *name)
{
  if ((getuid () == geteuid ()) && (getgid () == getegid ()))
    return getenv (name);
  else
    return NULL;
}

#else
#define secure_getenv getenv
#endif
#endif

/* }}}  */
/* {{{ Types  */

/* GCN-specific implementation of the GOMP_PLUGIN_acc_thread data.  */

struct gcn_thread
{
  /* The thread number from the async clause, or GOMP_ASYNC_SYNC.  */
  int async;
};

/* As an HSA runtime is dlopened, following structure defines function
   pointers utilized by the HSA plug-in.  */

struct hsa_runtime_fn_info
{
  /* HSA runtime.  */
  hsa_status_t (*hsa_status_string_fn) (hsa_status_t status,
					const char **status_string);
  hsa_status_t (*hsa_system_get_info_fn) (hsa_system_info_t attribute,
					  void *value);
  hsa_status_t (*hsa_agent_get_info_fn) (hsa_agent_t agent,
					 hsa_agent_info_t attribute,
					 void *value);
  hsa_status_t (*hsa_isa_get_info_fn)(hsa_isa_t isa,
				      hsa_isa_info_t attribute,
				      uint32_t index,
				      void *value);
  hsa_status_t (*hsa_init_fn) (void);
  hsa_status_t (*hsa_iterate_agents_fn)
    (hsa_status_t (*callback)(hsa_agent_t agent, void *data), void *data);
  hsa_status_t (*hsa_region_get_info_fn) (hsa_region_t region,
					  hsa_region_info_t attribute,
					  void *value);
  hsa_status_t (*hsa_queue_create_fn)
    (hsa_agent_t agent, uint32_t size, hsa_queue_type_t type,
     void (*callback)(hsa_status_t status, hsa_queue_t *source, void *data),
     void *data, uint32_t private_segment_size,
     uint32_t group_segment_size, hsa_queue_t **queue);
  hsa_status_t (*hsa_agent_iterate_regions_fn)
    (hsa_agent_t agent,
     hsa_status_t (*callback)(hsa_region_t region, void *data), void *data);
  hsa_status_t (*hsa_executable_destroy_fn) (hsa_executable_t executable);
  hsa_status_t (*hsa_executable_create_fn)
    (hsa_profile_t profile, hsa_executable_state_t executable_state,
     const char *options, hsa_executable_t *executable);
  hsa_status_t (*hsa_executable_global_variable_define_fn)
    (hsa_executable_t executable, const char *variable_name, void *address);
  hsa_status_t (*hsa_executable_load_code_object_fn)
    (hsa_executable_t executable, hsa_agent_t agent,
     hsa_code_object_t code_object, const char *options);
  hsa_status_t (*hsa_executable_freeze_fn)(hsa_executable_t executable,
					   const char *options);
  hsa_status_t (*hsa_signal_create_fn) (hsa_signal_value_t initial_value,
					uint32_t num_consumers,
					const hsa_agent_t *consumers,
					hsa_signal_t *signal);
  hsa_status_t (*hsa_memory_allocate_fn) (hsa_region_t region, size_t size,
					  void **ptr);
  hsa_status_t (*hsa_memory_assign_agent_fn) (void *ptr, hsa_agent_t agent,
					      hsa_access_permission_t access);
  hsa_status_t (*hsa_memory_copy_fn)(void *dst, const void *src, size_t size);
  hsa_status_t (*hsa_memory_free_fn) (void *ptr);
  hsa_status_t (*hsa_signal_destroy_fn) (hsa_signal_t signal);
  hsa_status_t (*hsa_executable_get_symbol_fn)
    (hsa_executable_t executable, const char *module_name,
     const char *symbol_name, hsa_agent_t agent, int32_t call_convention,
     hsa_executable_symbol_t *symbol);
  hsa_status_t (*hsa_executable_symbol_get_info_fn)
    (hsa_executable_symbol_t executable_symbol,
     hsa_executable_symbol_info_t attribute, void *value);
  hsa_status_t (*hsa_executable_iterate_symbols_fn)
    (hsa_executable_t executable,
     hsa_status_t (*callback)(hsa_executable_t executable,
			      hsa_executable_symbol_t symbol, void *data),
     void *data);
  uint64_t (*hsa_queue_add_write_index_release_fn) (const hsa_queue_t *queue,
						    uint64_t value);
  uint64_t (*hsa_queue_load_read_index_acquire_fn) (const hsa_queue_t *queue);
  void (*hsa_signal_store_relaxed_fn) (hsa_signal_t signal,
				       hsa_signal_value_t value);
  void (*hsa_signal_store_release_fn) (hsa_signal_t signal,
				       hsa_signal_value_t value);
  hsa_signal_value_t (*hsa_signal_wait_acquire_fn)
    (hsa_signal_t signal, hsa_signal_condition_t condition,
     hsa_signal_value_t compare_value, uint64_t timeout_hint,
     hsa_wait_state_t wait_state_hint);
  hsa_signal_value_t (*hsa_signal_load_acquire_fn) (hsa_signal_t signal);
  hsa_status_t (*hsa_queue_destroy_fn) (hsa_queue_t *queue);

  hsa_status_t (*hsa_code_object_deserialize_fn)
    (void *serialized_code_object, size_t serialized_code_object_size,
     const char *options, hsa_code_object_t *code_object);
  hsa_status_t (*hsa_amd_memory_lock_fn)
    (void *host_ptr, size_t size, hsa_agent_t *agents, int num_agent,
     void **agent_ptr);
  hsa_status_t (*hsa_amd_memory_unlock_fn) (void *host_ptr);
  hsa_status_t (*hsa_amd_memory_async_copy_rect_fn)
    (const hsa_pitched_ptr_t *dst, const hsa_dim3_t *dst_offset,
     const hsa_pitched_ptr_t *src, const hsa_dim3_t *src_offset,
     const hsa_dim3_t *range, hsa_agent_t copy_agent,
     hsa_amd_copy_direction_t dir, uint32_t num_dep_signals,
     const hsa_signal_t *dep_signals, hsa_signal_t completion_signal);
};

/* Structure describing the run-time and grid properties of an HSA kernel
   lauch.  This needs to match the format passed to GOMP_OFFLOAD_run.  */

struct GOMP_kernel_launch_attributes
{
  /* Number of dimensions the workload has.  Maximum number is 3.  */
  uint32_t ndim;
  /* Size of the grid in the three respective dimensions.  */
  uint32_t gdims[3];
  /* Size of work-groups in the respective dimensions.  */
  uint32_t wdims[3];
};

/* Collection of information needed for a dispatch of a kernel from a
   kernel.  */

struct kernel_dispatch
{
  struct agent_info *agent;
  /* Pointer to a command queue associated with a kernel dispatch agent.  */
  void *queue;
  /* Pointer to a memory space used for kernel arguments passing.  */
  void *kernarg_address;
  /* Kernel object.  */
  uint64_t object;
  /* Synchronization signal used for dispatch synchronization.  */
  uint64_t signal;
  /* Private segment size.  */
  uint32_t private_segment_size;
  /* Group segment size.  */
  uint32_t group_segment_size;
};

/* Structure of the kernargs segment, supporting console output.
 
   This needs to match the definitions in Newlib, and the expectations
   in libgomp target code.  */

struct kernargs {
  struct kernargs_abi abi;

  /* Output data.  */
  struct output output_data;
};

/* A queue entry for a future asynchronous launch.  */

struct kernel_launch
{
  struct kernel_info *kernel;
  void *vars;
  struct GOMP_kernel_launch_attributes kla;
};

/* A queue entry for a future callback.  */

struct callback
{
  void (*fn)(void *);
  void *data;
};

/* A data struct for the copy_data callback.  */

struct copy_data
{
  void *dst;
  const void *src;
  size_t len;
  struct goacc_asyncqueue *aq;
};

/* A queue entry for a placeholder.  These correspond to a wait event.  */

struct placeholder
{
  int executed;
  pthread_cond_t cond;
  pthread_mutex_t mutex;
};

/* A queue entry for a wait directive.  */

struct asyncwait_info
{
  struct placeholder *placeholderp;
};

/* Encode the type of an entry in an async queue.  */

enum entry_type
{
  KERNEL_LAUNCH,
  CALLBACK,
  ASYNC_WAIT,
  ASYNC_PLACEHOLDER
};

/* An entry in an async queue.  */

struct queue_entry
{
  enum entry_type type;
  union {
    struct kernel_launch launch;
    struct callback callback;
    struct asyncwait_info asyncwait;
    struct placeholder placeholder;
  } u;
};

/* An async queue header.

   OpenMP may create one of these.
   OpenACC may create many.  */

struct goacc_asyncqueue
{
  struct agent_info *agent;
  hsa_queue_t *hsa_queue;

  pthread_t thread_drain_queue;
  pthread_mutex_t mutex;
  pthread_cond_t queue_cond_in;
  pthread_cond_t queue_cond_out;
  struct queue_entry queue[ASYNC_QUEUE_SIZE];
  int queue_first;
  int queue_n;
  int drain_queue_stop;

  int id;
  struct goacc_asyncqueue *prev;
  struct goacc_asyncqueue *next;
};

/* Mkoffload uses this structure to describe a kernel.

   OpenMP kernel dimensions are passed at runtime.
   OpenACC kernel dimensions are passed at compile time, here.  */

struct hsa_kernel_description
{
  const char *name;
  int oacc_dims[3];  /* Only present for GCN kernels.  */
  int sgpr_count;
  int vpgr_count;
};

/* Mkoffload uses this structure to describe an offload variable.  */

struct global_var_info
{
  const char *name;
  void *address;
};

/* Mkoffload uses this structure to describe all the kernels in a
   loadable module.  These are passed the libgomp via static constructors.  */

struct gcn_image_desc
{
  struct gcn_image {
    size_t size;
    void *image;
  } *gcn_image;
  const unsigned kernel_count;
  struct hsa_kernel_description *kernel_infos;
  const unsigned ind_func_count;
  const unsigned global_variable_count;
};

/* Enum values corresponding to the the ELF architecture codes.
   Only 'special' values are actually referenced in this file, but having them
   all may aid debugging.  */

typedef enum {
  EF_AMDGPU_MACH_UNSUPPORTED = -1,
#define GCN_DEVICE(name, NAME, ELF, ...) \
  EF_AMDGPU_MACH_AMDGCN_ ## NAME = ELF,
#include "../../gcc/config/gcn/gcn-devices.def"
} EF_AMDGPU_MACH;

const static int EF_AMDGPU_MACH_MASK = 0x000000ff;
typedef EF_AMDGPU_MACH gcn_isa;

/* Description of an HSA GPU agent (device) and the program associated with
   it.  */

struct agent_info
{
  /* The HSA ID of the agent.  Assigned when hsa_context is initialized.  */
  hsa_agent_t id;
  /* The user-visible device number.  */
  int device_id;
  /* Whether the agent has been initialized.  The fields below are usable only
     if it has been.  */
  bool initialized;

  /* The instruction set architecture of the device. */
  gcn_isa device_isa;
  /* Name of the agent. */
  char name[64];
  /* Name of the vendor of the agent. */
  char vendor_name[64];
  /* Command queues of the agent.  */
  hsa_queue_t *sync_queue;
  struct goacc_asyncqueue *async_queues, *omp_async_queue;
  pthread_mutex_t async_queues_mutex;

  /* The HSA memory region from which to allocate kernel arguments.  */
  hsa_region_t kernarg_region;

  /* The HSA memory region from which to allocate device data.  */
  hsa_region_t data_region;

  /* Allocated ephemeral memories (team arena and stack space).  */
  struct ephemeral_memories_list *ephemeral_memories_list;
  pthread_mutex_t ephemeral_memories_write_lock;

  /* Read-write lock that protects kernels which are running or about to be run
     from interference with loading and unloading of images.  Needs to be
     locked for reading while a kernel is being run, and for writing if the
     list of modules is manipulated (and thus the HSA program invalidated).  */
  pthread_rwlock_t module_rwlock;

  /* The module associated with this kernel.  */
  struct module_info *module;

  /* Mutex enforcing that only one thread will finalize the HSA program.  A
     thread should have locked agent->module_rwlock for reading before
     acquiring it.  */
  pthread_mutex_t prog_mutex;
  /* Flag whether the HSA program that consists of all the modules has been
     finalized.  */
  bool prog_finalized;
  /* HSA executable - the finalized program that is used to locate kernels.  */
  hsa_executable_t executable;
};

/* Information required to identify, finalize and run any given kernel.  */

enum offload_kind {KIND_UNKNOWN, KIND_OPENMP, KIND_OPENACC};

struct kernel_info
{
  /* Name of the kernel, required to locate it within the GCN object-code
     module.  */
  const char *name;
  /* The specific agent the kernel has been or will be finalized for and run
     on.  */
  struct agent_info *agent;
  /* The specific module where the kernel takes place.  */
  struct module_info *module;
  /* Information provided by mkoffload associated with the kernel.  */
  struct hsa_kernel_description *description;
  /* Mutex enforcing that at most once thread ever initializes a kernel for
     use.  A thread should have locked agent->module_rwlock for reading before
     acquiring it.  */
  pthread_mutex_t init_mutex;
  /* Flag indicating whether the kernel has been initialized and all fields
     below it contain valid data.  */
  bool initialized;
  /* Flag indicating that the kernel has a problem that blocks an execution.  */
  bool initialization_failed;
  /* The object to be put into the dispatch queue.  */
  uint64_t object;
  /* Required size of kernel arguments.  */
  uint32_t kernarg_segment_size;
  /* Required size of group segment.  */
  uint32_t group_segment_size;
  /* Required size of private segment.  */
  uint32_t private_segment_size;
  /* Set up for OpenMP or OpenACC?  */
  enum offload_kind kind;
};

/* Information about a particular GCN module, its image and kernels.  */

struct module_info
{
  /* The description with which the program has registered the image.  */
  struct gcn_image_desc *image_desc;
  /* GCN heap allocation.  */
  struct heap *heap;
  /* Physical boundaries of the loaded module.  */
  Elf64_Addr phys_address_start;
  Elf64_Addr phys_address_end;

  bool constructors_run_p;
  struct kernel_info *init_array_func, *fini_array_func;

  /* Number of kernels in this module.  */
  int kernel_count;
  /* An array of kernel_info structures describing each kernel in this
     module.  */
  struct kernel_info kernels[];
};

/* A linked list of memory arenas allocated on the device.
   These are used by OpenMP, as a means to optimize per-team malloc,
   and for host-accessible stack space.  */

struct ephemeral_memories_list
{
  struct ephemeral_memories_list *next;

  /* The size is determined by the number of teams and threads.  */
  size_t size;
  /* The device address allocated memory.  */
  void *address;
  /* A flag to prevent two asynchronous kernels trying to use the same memory.
     The mutex is locked until the kernel exits.  */
  pthread_mutex_t in_use;
};

/* Information about the whole HSA environment and all of its agents.  */

struct hsa_context_info
{
  /* Whether the structure has been initialized.  */
  bool initialized;
  /* Number of usable GPU HSA agents in the system.  */
  int agent_count;
  /* Array of agent_info structures describing the individual HSA agents.  */
  struct agent_info *agents;
  /* Driver version string. */
  char driver_version_s[30];
};

/* }}}  */
/* {{{ Global variables  */

/* Information about the whole HSA environment and all of its agents.  */

static struct hsa_context_info hsa_context;

/* HSA runtime functions that are initialized in init_hsa_context.  */

static struct hsa_runtime_fn_info hsa_fns;

/* Heap space, allocated target-side, provided for use of newlib malloc.
   Each module should have it's own heap allocated.
   Beware that heap usage increases with OpenMP teams.  See also arenas.  */

static size_t gcn_kernel_heap_size = DEFAULT_GCN_HEAP_SIZE;

/* Ephemeral memory sizes for each kernel launch.  */

static int team_arena_size = DEFAULT_TEAM_ARENA_SIZE;
static int stack_size = DEFAULT_GCN_STACK_SIZE;
static int lowlat_size = -1;

/* Flag to decide whether print to stderr information about what is going on.
   Set in init_debug depending on environment variables.  */

static bool debug;

/* Flag to decide if the runtime should suppress a possible fallback to host
   execution.  */

static bool suppress_host_fallback;

/* Flag to locate HSA runtime shared library that is dlopened
   by this plug-in.  */

static const char *hsa_runtime_lib;

/* Flag to decide if the runtime should support also CPU devices (can be
   a simulator).  */

static bool support_cpu_devices;

/* Runtime dimension overrides.  Zero indicates default.  */

static int override_x_dim = 0;
static int override_z_dim = 0;

/* }}}  */
/* {{{ Debug & Diagnostic  */

/* Print a message to stderr if GCN_DEBUG value is set to true.  */

#define DEBUG_PRINT(...) \
  do \
  { \
    if (debug) \
      { \
	fprintf (stderr, __VA_ARGS__); \
      } \
  } \
  while (false);

/* Flush stderr if GCN_DEBUG value is set to true.  */

#define DEBUG_FLUSH()				\
  do {						\
    if (debug)					\
      fflush (stderr);				\
  } while (false)

/* Print a logging message with PREFIX to stderr if GCN_DEBUG value
   is set to true.  */

#define DEBUG_LOG(prefix, ...)			\
  do						\
    {						\
      DEBUG_PRINT (prefix);			\
      DEBUG_PRINT (__VA_ARGS__);			\
      DEBUG_FLUSH ();				\
    } while (false)

/* Print a debugging message to stderr.  */

#define GCN_DEBUG(...) DEBUG_LOG ("GCN debug: ", __VA_ARGS__)

/* Print a warning message to stderr.  */

#define GCN_WARNING(...) DEBUG_LOG ("GCN warning: ", __VA_ARGS__)

/* Print HSA warning STR with an HSA STATUS code.  */

static void
hsa_warn (const char *str, hsa_status_t status)
{
  if (!debug)
    return;

  const char *hsa_error_msg = "[unknown]";
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);

  fprintf (stderr, "GCN warning: %s\nRuntime message: %s\n", str,
	   hsa_error_msg);
}

/* Report a fatal error STR together with the HSA error corresponding to STATUS
   and terminate execution of the current process.  */

static void
hsa_fatal (const char *str, hsa_status_t status)
{
  const char *hsa_error_msg = "[unknown]";
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);
  GOMP_PLUGIN_fatal ("GCN fatal error: %s\nRuntime message: %s\n", str,
		     hsa_error_msg);
}

/* Like hsa_fatal, except only report error message, and return FALSE
   for propagating error processing to outside of plugin.  */

static bool
hsa_error (const char *str, hsa_status_t status)
{
  const char *hsa_error_msg = "[unknown]";
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);
  GOMP_PLUGIN_error ("GCN fatal error: %s\nRuntime message: %s\n", str,
		     hsa_error_msg);
  return false;
}

/* Dump information about the available hardware.  */

static void
dump_hsa_system_info (void)
{
  hsa_status_t status;

  hsa_endianness_t endianness;
  status = hsa_fns.hsa_system_get_info_fn (HSA_SYSTEM_INFO_ENDIANNESS,
					   &endianness);
  if (status == HSA_STATUS_SUCCESS)
    switch (endianness)
      {
      case HSA_ENDIANNESS_LITTLE:
	GCN_DEBUG ("HSA_SYSTEM_INFO_ENDIANNESS: LITTLE\n");
	break;
      case HSA_ENDIANNESS_BIG:
	GCN_DEBUG ("HSA_SYSTEM_INFO_ENDIANNESS: BIG\n");
	break;
      default:
	GCN_WARNING ("HSA_SYSTEM_INFO_ENDIANNESS: UNKNOWN\n");
      }
  else
    GCN_WARNING ("HSA_SYSTEM_INFO_ENDIANNESS: FAILED\n");

  uint8_t extensions[128];
  status = hsa_fns.hsa_system_get_info_fn (HSA_SYSTEM_INFO_EXTENSIONS,
					   &extensions);
  if (status == HSA_STATUS_SUCCESS)
    {
      if (extensions[0] & (1 << HSA_EXTENSION_IMAGES))
	GCN_DEBUG ("HSA_SYSTEM_INFO_EXTENSIONS: IMAGES\n");
    }
  else
    GCN_WARNING ("HSA_SYSTEM_INFO_EXTENSIONS: FAILED\n");
}

/* Dump information about the available hardware.  */

static void
dump_machine_model (hsa_machine_model_t machine_model, const char *s)
{
  switch (machine_model)
    {
    case HSA_MACHINE_MODEL_SMALL:
      GCN_DEBUG ("%s: SMALL\n", s);
      break;
    case HSA_MACHINE_MODEL_LARGE:
      GCN_DEBUG ("%s: LARGE\n", s);
      break;
    default:
      GCN_WARNING ("%s: UNKNOWN\n", s);
      break;
    }
}

/* Dump information about the available hardware.  */

static void
dump_profile (hsa_profile_t profile, const char *s)
{
  switch (profile)
    {
    case HSA_PROFILE_FULL:
      GCN_DEBUG ("%s: FULL\n", s);
      break;
    case HSA_PROFILE_BASE:
      GCN_DEBUG ("%s: BASE\n", s);
      break;
    default:
      GCN_WARNING ("%s: UNKNOWN\n", s);
      break;
    }
}

/* Dump information about a device memory region.  */

static hsa_status_t
dump_hsa_region (hsa_region_t region, void *data __attribute__((unused)))
{
  hsa_status_t status;

  hsa_region_segment_t segment;
  status = hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_SEGMENT,
					   &segment);
  if (status == HSA_STATUS_SUCCESS)
    {
      if (segment == HSA_REGION_SEGMENT_GLOBAL)
	GCN_DEBUG ("HSA_REGION_INFO_SEGMENT: GLOBAL\n");
      else if (segment == HSA_REGION_SEGMENT_READONLY)
	GCN_DEBUG ("HSA_REGION_INFO_SEGMENT: READONLY\n");
      else if (segment == HSA_REGION_SEGMENT_PRIVATE)
	GCN_DEBUG ("HSA_REGION_INFO_SEGMENT: PRIVATE\n");
      else if (segment == HSA_REGION_SEGMENT_GROUP)
	GCN_DEBUG ("HSA_REGION_INFO_SEGMENT: GROUP\n");
      else
	GCN_WARNING ("HSA_REGION_INFO_SEGMENT: UNKNOWN\n");
    }
  else
    GCN_WARNING ("HSA_REGION_INFO_SEGMENT: FAILED\n");

  if (segment == HSA_REGION_SEGMENT_GLOBAL)
    {
      uint32_t flags;
      status
	= hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_GLOBAL_FLAGS,
					  &flags);
      if (status == HSA_STATUS_SUCCESS)
	{
	  if (flags & HSA_REGION_GLOBAL_FLAG_KERNARG)
	    GCN_DEBUG ("HSA_REGION_INFO_GLOBAL_FLAGS: KERNARG\n");
	  if (flags & HSA_REGION_GLOBAL_FLAG_FINE_GRAINED)
	    GCN_DEBUG ("HSA_REGION_INFO_GLOBAL_FLAGS: FINE_GRAINED\n");
	  if (flags & HSA_REGION_GLOBAL_FLAG_COARSE_GRAINED)
	    GCN_DEBUG ("HSA_REGION_INFO_GLOBAL_FLAGS: COARSE_GRAINED\n");
	}
      else
	GCN_WARNING ("HSA_REGION_INFO_GLOBAL_FLAGS: FAILED\n");
    }

  size_t size;
  status = hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_SIZE, &size);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_REGION_INFO_SIZE: %zu\n", size);
  else
    GCN_WARNING ("HSA_REGION_INFO_SIZE: FAILED\n");

  status
    = hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_ALLOC_MAX_SIZE,
				      &size);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_REGION_INFO_ALLOC_MAX_SIZE: %zu\n", size);
  else
    GCN_WARNING ("HSA_REGION_INFO_ALLOC_MAX_SIZE: FAILED\n");

  bool alloc_allowed;
  status
    = hsa_fns.hsa_region_get_info_fn (region,
				      HSA_REGION_INFO_RUNTIME_ALLOC_ALLOWED,
				      &alloc_allowed);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_REGION_INFO_RUNTIME_ALLOC_ALLOWED: %u\n", alloc_allowed);
  else
    GCN_WARNING ("HSA_REGION_INFO_RUNTIME_ALLOC_ALLOWED: FAILED\n");

  if (status != HSA_STATUS_SUCCESS || !alloc_allowed)
    return HSA_STATUS_SUCCESS;

  status
    = hsa_fns.hsa_region_get_info_fn (region,
				      HSA_REGION_INFO_RUNTIME_ALLOC_GRANULE,
				      &size);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_REGION_INFO_RUNTIME_ALLOC_GRANULE: %zu\n", size);
  else
    GCN_WARNING ("HSA_REGION_INFO_RUNTIME_ALLOC_GRANULE: FAILED\n");

  size_t align;
  status
    = hsa_fns.hsa_region_get_info_fn (region,
				      HSA_REGION_INFO_RUNTIME_ALLOC_ALIGNMENT,
				      &align);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_REGION_INFO_RUNTIME_ALLOC_ALIGNMENT: %zu\n", align);
  else
    GCN_WARNING ("HSA_REGION_INFO_RUNTIME_ALLOC_ALIGNMENT: FAILED\n");

  return HSA_STATUS_SUCCESS;
}

/* Dump information about all the device memory regions.  */

static void
dump_hsa_regions (hsa_agent_t agent)
{
  hsa_status_t status;
  status = hsa_fns.hsa_agent_iterate_regions_fn (agent,
						 dump_hsa_region,
						 NULL);
  if (status != HSA_STATUS_SUCCESS)
    hsa_error ("Dumping hsa regions failed", status);
}

/* Dump information about the available devices.  */

static hsa_status_t
dump_hsa_agent_info (hsa_agent_t agent, void *data __attribute__((unused)))
{
  hsa_status_t status;

  char buf[64];
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_NAME,
					  &buf);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_NAME: %s\n", buf);
  else
    GCN_WARNING ("HSA_AGENT_INFO_NAME: FAILED\n");

  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_VENDOR_NAME,
					  &buf);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_VENDOR_NAME: %s\n", buf);
  else
    GCN_WARNING ("HSA_AGENT_INFO_VENDOR_NAME: FAILED\n");

  hsa_machine_model_t machine_model;
  status
    = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_MACHINE_MODEL,
				     &machine_model);
  if (status == HSA_STATUS_SUCCESS)
    dump_machine_model (machine_model, "HSA_AGENT_INFO_MACHINE_MODEL");
  else
    GCN_WARNING ("HSA_AGENT_INFO_MACHINE_MODEL: FAILED\n");

  hsa_profile_t profile;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_PROFILE,
					  &profile);
  if (status == HSA_STATUS_SUCCESS)
    dump_profile (profile, "HSA_AGENT_INFO_PROFILE");
  else
    GCN_WARNING ("HSA_AGENT_INFO_PROFILE: FAILED\n");

  hsa_device_type_t device_type;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_DEVICE,
					  &device_type);
  if (status == HSA_STATUS_SUCCESS)
    {
      switch (device_type)
	{
	case HSA_DEVICE_TYPE_CPU:
	  GCN_DEBUG ("HSA_AGENT_INFO_DEVICE: CPU\n");
	  break;
	case HSA_DEVICE_TYPE_GPU:
	  GCN_DEBUG ("HSA_AGENT_INFO_DEVICE: GPU\n");
	  break;
	case HSA_DEVICE_TYPE_DSP:
	  GCN_DEBUG ("HSA_AGENT_INFO_DEVICE: DSP\n");
	  break;
	default:
	  GCN_WARNING ("HSA_AGENT_INFO_DEVICE: UNKNOWN\n");
	  break;
	}
    }
  else
    GCN_WARNING ("HSA_AGENT_INFO_DEVICE: FAILED\n");

  uint32_t cu_count;
  status = hsa_fns.hsa_agent_get_info_fn
    (agent, HSA_AMD_AGENT_INFO_COMPUTE_UNIT_COUNT, &cu_count);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AMD_AGENT_INFO_COMPUTE_UNIT_COUNT: %u\n", cu_count);
  else
    GCN_WARNING ("HSA_AMD_AGENT_INFO_COMPUTE_UNIT_COUNT: FAILED\n");

  uint32_t size;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_WAVEFRONT_SIZE,
					  &size);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_WAVEFRONT_SIZE: %u\n", size);
  else
    GCN_WARNING ("HSA_AGENT_INFO_WAVEFRONT_SIZE: FAILED\n");

  uint32_t max_dim;
  status = hsa_fns.hsa_agent_get_info_fn (agent,
					  HSA_AGENT_INFO_WORKGROUP_MAX_DIM,
					  &max_dim);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_WORKGROUP_MAX_DIM: %u\n", max_dim);
  else
    GCN_WARNING ("HSA_AGENT_INFO_WORKGROUP_MAX_DIM: FAILED\n");

  uint32_t max_size;
  status = hsa_fns.hsa_agent_get_info_fn (agent,
					  HSA_AGENT_INFO_WORKGROUP_MAX_SIZE,
					  &max_size);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_WORKGROUP_MAX_SIZE: %u\n", max_size);
  else
    GCN_WARNING ("HSA_AGENT_INFO_WORKGROUP_MAX_SIZE: FAILED\n");

  uint32_t grid_max_dim;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_GRID_MAX_DIM,
					  &grid_max_dim);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_GRID_MAX_DIM: %u\n", grid_max_dim);
  else
    GCN_WARNING ("HSA_AGENT_INFO_GRID_MAX_DIM: FAILED\n");

  uint32_t grid_max_size;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_GRID_MAX_SIZE,
					  &grid_max_size);
  if (status == HSA_STATUS_SUCCESS)
    GCN_DEBUG ("HSA_AGENT_INFO_GRID_MAX_SIZE: %u\n", grid_max_size);
  else
    GCN_WARNING ("HSA_AGENT_INFO_GRID_MAX_SIZE: FAILED\n");

  dump_hsa_regions (agent);

  return HSA_STATUS_SUCCESS;
}

/* Forward reference.  */

static char *get_executable_symbol_name (hsa_executable_symbol_t symbol);

/* Helper function for dump_executable_symbols.  */

static hsa_status_t
dump_executable_symbol (hsa_executable_t executable,
			hsa_executable_symbol_t symbol,
			void *data __attribute__((unused)))
{
  char *name = get_executable_symbol_name (symbol);

  if (name)
    {
      GCN_DEBUG ("executable symbol: %s\n", name);
      free (name);
    }

  return HSA_STATUS_SUCCESS;
}

/* Dump all global symbol in an executable.  */

static void
dump_executable_symbols (hsa_executable_t executable)
{
  hsa_status_t status;
  status
    = hsa_fns.hsa_executable_iterate_symbols_fn (executable,
						 dump_executable_symbol,
						 NULL);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not dump HSA executable symbols", status);
}

/* Dump kernel DISPATCH data structure and indent it by INDENT spaces.  */

static void
print_kernel_dispatch (struct kernel_dispatch *dispatch, unsigned indent)
{
  struct kernargs *kernargs = (struct kernargs *)dispatch->kernarg_address;

  fprintf (stderr, "%*sthis: %p\n", indent, "", dispatch);
  fprintf (stderr, "%*squeue: %p\n", indent, "", dispatch->queue);
  fprintf (stderr, "%*skernarg_address: %p\n", indent, "", kernargs);
  fprintf (stderr, "%*sheap address: %p\n", indent, "",
	   (void*)kernargs->abi.heap_ptr);
  fprintf (stderr, "%*sarena address: %p (%d bytes per workgroup)\n", indent,
	   "", (void*)kernargs->abi.arena_ptr,
	   kernargs->abi.arena_size_per_team);
  fprintf (stderr, "%*sstack address: %p (%d bytes per wavefront)\n", indent,
	   "", (void*)kernargs->abi.stack_ptr,
	   kernargs->abi.stack_size_per_thread);
  fprintf (stderr, "%*sobject: %lu\n", indent, "", dispatch->object);
  fprintf (stderr, "%*sprivate_segment_size: %u\n", indent, "",
	   dispatch->private_segment_size);
  fprintf (stderr, "%*sgroup_segment_size: %u (low-latency pool)\n", indent,
	   "", dispatch->group_segment_size);
  fprintf (stderr, "\n");
}

/* }}}  */
/* {{{ Utility functions  */

/* Cast the thread local storage to gcn_thread.  */

static inline struct gcn_thread *
gcn_thread (void)
{
  return (struct gcn_thread *) GOMP_PLUGIN_acc_thread ();
}

/* Initialize debug and suppress_host_fallback according to the environment.  */

static void
init_environment_variables (void)
{
  if (secure_getenv ("GCN_DEBUG"))
    debug = true;
  else
    debug = false;

  if (secure_getenv ("GCN_SUPPRESS_HOST_FALLBACK"))
    suppress_host_fallback = true;
  else
    suppress_host_fallback = false;

  hsa_runtime_lib = secure_getenv ("HSA_RUNTIME_LIB");
  if (hsa_runtime_lib == NULL)
    hsa_runtime_lib = "libhsa-runtime64.so.1";

  support_cpu_devices = secure_getenv ("GCN_SUPPORT_CPU_DEVICES");

  const char *x = secure_getenv ("GCN_NUM_TEAMS");
  if (!x)
    x = secure_getenv ("GCN_NUM_GANGS");
  if (x)
    override_x_dim = atoi (x);

  const char *z = secure_getenv ("GCN_NUM_THREADS");
  if (!z)
    z = secure_getenv ("GCN_NUM_WORKERS");
  if (z)
    override_z_dim = atoi (z);

  const char *heap = secure_getenv ("GCN_HEAP_SIZE");
  if (heap)
    {
      size_t tmp = atol (heap);
      if (tmp)
	gcn_kernel_heap_size = tmp;
    }

  const char *arena = secure_getenv ("GCN_TEAM_ARENA_SIZE");
  if (arena)
    {
      int tmp = atoi (arena);
      if (tmp)
	team_arena_size = tmp;;
    }

  const char *stack = secure_getenv ("GCN_STACK_SIZE");
  if (stack)
    {
      int tmp = atoi (stack);
      if (tmp)
	stack_size = tmp;;
    }

  const char *lowlat = secure_getenv ("GOMP_GCN_LOWLAT_POOL");
  if (lowlat)
    lowlat_size = atoi (lowlat);
}

/* Return malloc'd string with name of SYMBOL.  */

static char *
get_executable_symbol_name (hsa_executable_symbol_t symbol)
{
  hsa_status_t status;
  char *res;
  uint32_t len;
  const hsa_executable_symbol_info_t info_name_length
    = HSA_EXECUTABLE_SYMBOL_INFO_NAME_LENGTH;

  status = hsa_fns.hsa_executable_symbol_get_info_fn (symbol, info_name_length,
						      &len);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not get length of symbol name", status);
      return NULL;
    }

  res = GOMP_PLUGIN_malloc (len + 1);

  const hsa_executable_symbol_info_t info_name
    = HSA_EXECUTABLE_SYMBOL_INFO_NAME;

  status = hsa_fns.hsa_executable_symbol_get_info_fn (symbol, info_name, res);

  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not get symbol name", status);
      free (res);
      return NULL;
    }

  res[len] = '\0';

  return res;
}

/* Get the number of GPU Compute Units.  */

static int
get_cu_count (struct agent_info *agent)
{
  uint32_t cu_count;
  hsa_status_t status = hsa_fns.hsa_agent_get_info_fn
    (agent->id, HSA_AMD_AGENT_INFO_COMPUTE_UNIT_COUNT, &cu_count);
  if (status == HSA_STATUS_SUCCESS)
    return cu_count;
  else
    return 64;  /* The usual number for older devices.  */
}

/* Calculate the maximum grid size for OMP threads / OACC workers.
   This depends on the kernel's resource usage levels.  */

static int
limit_worker_threads (int threads)
{
  /* FIXME Do something more inteligent here.
     GCN can always run 4 threads within a Compute Unit, but
     more than that depends on register usage.  */
  if (threads > 16)
    threads = 16;
  return threads;
}

/* This sets the maximum number of teams to twice the number of GPU Compute
   Units to avoid memory waste and corresponding memory access faults.  */

static int
limit_teams (int teams, struct agent_info *agent)
{
  int max_teams = 2 * get_cu_count (agent);
  if (teams > max_teams)
    teams = max_teams;
  return teams;
}

/* Parse the target attributes INPUT provided by the compiler and return true
   if we should run anything all.  If INPUT is NULL, fill DEF with default
   values, then store INPUT or DEF into *RESULT.
 
   This is used for OpenMP only.  */

static bool
parse_target_attributes (void **input,
			 struct GOMP_kernel_launch_attributes *def,
			 struct GOMP_kernel_launch_attributes **result,
			 struct agent_info *agent)
{
  if (!input)
    GOMP_PLUGIN_fatal ("No target arguments provided");

  bool grid_attrs_found = false;
  bool gcn_dims_found = false;
  int gcn_teams = 0;
  int gcn_threads = 0;
  while (*input)
    {
      intptr_t id = (intptr_t) *input++, val;

      if (id & GOMP_TARGET_ARG_SUBSEQUENT_PARAM)
	val = (intptr_t) *input++;
      else
	val = id >> GOMP_TARGET_ARG_VALUE_SHIFT;

      val = (val > INT_MAX) ? INT_MAX : val;

      if ((id & GOMP_TARGET_ARG_DEVICE_MASK) == GOMP_DEVICE_GCN
	  && ((id & GOMP_TARGET_ARG_ID_MASK)
	      == GOMP_TARGET_ARG_HSA_KERNEL_ATTRIBUTES))
	{
	  grid_attrs_found = true;
	  break;
	}
      else if ((id & GOMP_TARGET_ARG_DEVICE_MASK)
	       == GOMP_TARGET_ARG_DEVICE_ALL)
	{
	  gcn_dims_found = true;
	  switch (id & GOMP_TARGET_ARG_ID_MASK)
	    {
	    case GOMP_TARGET_ARG_NUM_TEAMS:
	      gcn_teams = limit_teams (val, agent);
	      break;
	    case GOMP_TARGET_ARG_THREAD_LIMIT:
	      gcn_threads = limit_worker_threads (val);
	      break;
	    default:
	      ;
	    }
	}
    }

  if (gcn_dims_found)
    {
      bool gfx900_workaround_p = false;

      if (agent->device_isa == EF_AMDGPU_MACH_AMDGCN_GFX900
	  && gcn_threads == 0 && override_z_dim == 0)
	{
	  gfx900_workaround_p = true;
	  GCN_WARNING ("VEGA BUG WORKAROUND: reducing default number of "
		       "threads to at most 4 per team.\n");
	  GCN_WARNING (" - If this is not a Vega 10 device, please use "
		       "GCN_NUM_THREADS=16\n");
	}

      /* Ideally, when a dimension isn't explicitly specified, we should
	 tune it to run 40 (or 32?) threads per CU with no threads getting queued.
	 In practice, we tune for peak performance on BabelStream, which
	 for OpenACC is currently 32 threads per CU.  */
      def->ndim = 3;
      if (gcn_teams <= 0 && gcn_threads <= 0)
	{
	  /* Set up a reasonable number of teams and threads.  */
	  gcn_threads = gfx900_workaround_p ? 4 : 16; // 8;
	  def->gdims[0] = get_cu_count (agent); // * (40 / gcn_threads);
	  def->gdims[2] = gcn_threads;
	}
      else if (gcn_teams <= 0 && gcn_threads > 0)
	{
	  /* Auto-scale the number of teams with the number of threads.  */
	  def->gdims[0] = get_cu_count (agent); // * (40 / gcn_threads);
	  def->gdims[2] = gcn_threads;
	}
      else if (gcn_teams > 0 && gcn_threads <= 0)
	{
	  int max_threads = gfx900_workaround_p ? 4 : 16;

	  /* Auto-scale the number of threads with the number of teams.  */
	  def->gdims[0] = gcn_teams;
	  def->gdims[2] = 16; // get_cu_count (agent) * 40 / gcn_teams;
	  if (def->gdims[2] == 0)
	    def->gdims[2] = 1;
	  else if (def->gdims[2] > max_threads)
	    def->gdims[2] = max_threads;
	}
      else
	{
	  def->gdims[0] = gcn_teams;
	  def->gdims[2] = gcn_threads;
	}
      def->gdims[1] = 64; /* Each thread is 64 work items wide.  */
      def->wdims[0] = 1;  /* Single team per work-group.  */
      def->wdims[1] = 64;
      def->wdims[2] = 16;
      *result = def;
      return true;
    }
  else if (!grid_attrs_found)
    {
      def->ndim = 1;
      def->gdims[0] = 1;
      def->gdims[1] = 1;
      def->gdims[2] = 1;
      def->wdims[0] = 1;
      def->wdims[1] = 1;
      def->wdims[2] = 1;
      *result = def;
      GCN_WARNING ("GOMP_OFFLOAD_run called with no launch attributes\n");
      return true;
    }

  struct GOMP_kernel_launch_attributes *kla;
  kla = (struct GOMP_kernel_launch_attributes *) *input;
  *result = kla;
  if (kla->ndim == 0 || kla->ndim > 3)
    GOMP_PLUGIN_fatal ("Invalid number of dimensions (%u)", kla->ndim);

  GCN_DEBUG ("GOMP_OFFLOAD_run called with %u dimensions:\n", kla->ndim);
  unsigned i;
  for (i = 0; i < kla->ndim; i++)
    {
      GCN_DEBUG ("  Dimension %u: grid size %u and group size %u\n", i,
		 kla->gdims[i], kla->wdims[i]);
      if (kla->gdims[i] == 0)
	return false;
    }
  return true;
}

/* Return the group size given the requested GROUP size, GRID size and number
   of grid dimensions NDIM.  */

static uint32_t
get_group_size (uint32_t ndim, uint32_t grid, uint32_t group)
{
  if (group == 0)
    {
      /* TODO: Provide a default via environment or device characteristics.  */
      if (ndim == 1)
	group = 64;
      else if (ndim == 2)
	group = 8;
      else
	group = 4;
    }

  if (group > grid)
    group = grid;
  return group;
}

/* Atomically store pair of uint16_t values (HEADER and REST) to a PACKET.  */

static void
packet_store_release (uint32_t* packet, uint16_t header, uint16_t rest)
{
  __atomic_store_n (packet, header | (rest << 16), __ATOMIC_RELEASE);
}

/* A never-called callback for the HSA command queues.  These signal events
   that we don't use, so we trigger an error.
 
   This "queue" is not to be confused with the async queues, below.  */

static void
hsa_queue_callback (hsa_status_t status,
		hsa_queue_t *queue __attribute__ ((unused)),
		void *data __attribute__ ((unused)))
{
  hsa_fatal ("Asynchronous queue error", status);
}

/* }}}  */
/* {{{ HSA initialization  */

/* Populate hsa_fns with the function addresses from libhsa-runtime64.so.  */

static bool
init_hsa_runtime_functions (void)
{
#define DLSYM_FN(function) \
  hsa_fns.function##_fn = dlsym (handle, #function); \
  if (hsa_fns.function##_fn == NULL) \
    GOMP_PLUGIN_fatal ("'%s' is missing '%s'", hsa_runtime_lib, #function);
#define DLSYM_OPT_FN(function) \
  hsa_fns.function##_fn = dlsym (handle, #function);

  void *handle = dlopen (hsa_runtime_lib, RTLD_LAZY);
  if (handle == NULL)
    return false;

  DLSYM_FN (hsa_status_string)
  DLSYM_FN (hsa_system_get_info)
  DLSYM_FN (hsa_agent_get_info)
  DLSYM_FN (hsa_init)
  DLSYM_FN (hsa_iterate_agents)
  DLSYM_FN (hsa_region_get_info)
  DLSYM_FN (hsa_queue_create)
  DLSYM_FN (hsa_agent_iterate_regions)
  DLSYM_FN (hsa_executable_destroy)
  DLSYM_FN (hsa_executable_create)
  DLSYM_FN (hsa_executable_global_variable_define)
  DLSYM_FN (hsa_executable_load_code_object)
  DLSYM_FN (hsa_executable_freeze)
  DLSYM_FN (hsa_signal_create)
  DLSYM_FN (hsa_memory_allocate)
  DLSYM_FN (hsa_memory_assign_agent)
  DLSYM_FN (hsa_memory_copy)
  DLSYM_FN (hsa_memory_free)
  DLSYM_FN (hsa_signal_destroy)
  DLSYM_FN (hsa_executable_get_symbol)
  DLSYM_FN (hsa_executable_symbol_get_info)
  DLSYM_FN (hsa_executable_iterate_symbols)
  DLSYM_FN (hsa_queue_add_write_index_release)
  DLSYM_FN (hsa_queue_load_read_index_acquire)
  DLSYM_FN (hsa_signal_wait_acquire)
  DLSYM_FN (hsa_signal_store_relaxed)
  DLSYM_FN (hsa_signal_store_release)
  DLSYM_FN (hsa_signal_load_acquire)
  DLSYM_FN (hsa_queue_destroy)
  DLSYM_FN (hsa_code_object_deserialize)
  DLSYM_OPT_FN (hsa_amd_memory_lock)
  DLSYM_OPT_FN (hsa_amd_memory_unlock)
  DLSYM_OPT_FN (hsa_amd_memory_async_copy_rect)
  return true;
#undef DLSYM_OPT_FN
#undef DLSYM_FN
}

static gcn_isa isa_code (const char *isa);

/* Return true if the agent is a GPU and can accept of concurrent submissions
   from different threads.  */

static bool
suitable_hsa_agent_p (hsa_agent_t agent)
{
  hsa_device_type_t device_type;
  hsa_status_t status
    = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_DEVICE,
				     &device_type);
  if (status != HSA_STATUS_SUCCESS)
    return false;

  switch (device_type)
    {
    case HSA_DEVICE_TYPE_GPU:
      {
	char name[64];
	hsa_status_t status
	  = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_NAME, name);
	if (status != HSA_STATUS_SUCCESS
	    || isa_code (name) == EF_AMDGPU_MACH_UNSUPPORTED)
	  {
	    GCN_DEBUG ("Ignoring unsupported agent '%s'\n",
		       status == HSA_STATUS_SUCCESS ? name : "invalid");
	    return false;
	  }
      }
      break;
    case HSA_DEVICE_TYPE_CPU:
      if (!support_cpu_devices)
	return false;
      break;
    default:
      return false;
    }

  uint32_t features = 0;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_FEATURE,
					  &features);
  if (status != HSA_STATUS_SUCCESS
      || !(features & HSA_AGENT_FEATURE_KERNEL_DISPATCH))
    return false;
  hsa_queue_type_t queue_type;
  status = hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_QUEUE_TYPE,
					  &queue_type);
  if (status != HSA_STATUS_SUCCESS
      || (queue_type != HSA_QUEUE_TYPE_MULTI))
    return false;

  return true;
}

/* Callback of hsa_iterate_agents; if AGENT is a GPU device, increment
   agent_count in hsa_context.  */

static hsa_status_t
count_gpu_agents (hsa_agent_t agent, void *data __attribute__ ((unused)))
{
  if (suitable_hsa_agent_p (agent))
    hsa_context.agent_count++;
  return HSA_STATUS_SUCCESS;
}

/* Callback of hsa_iterate_agents; if AGENT is a GPU device, assign the agent
   id to the describing structure in the hsa context.  The index of the
   structure is pointed to by DATA, increment it afterwards.  */

static hsa_status_t
assign_agent_ids (hsa_agent_t agent, void *data)
{
  if (suitable_hsa_agent_p (agent))
    {
      int *agent_index = (int *) data;
      hsa_context.agents[*agent_index].id = agent;
      ++*agent_index;
    }
  return HSA_STATUS_SUCCESS;
}

/* Initialize hsa_context if it has not already been done.
   If !PROBE: returns TRUE on success.
   If PROBE: returns TRUE on success or if the plugin/device shall be silently
   ignored, and otherwise emits an error and returns FALSE.  */

static bool
init_hsa_context (bool probe)
{
  hsa_status_t status;
  int agent_index = 0;

  if (hsa_context.initialized)
    return true;
  init_environment_variables ();
  if (!init_hsa_runtime_functions ())
    {
      const char *msg = "Run-time could not be dynamically opened";
      if (suppress_host_fallback)
	GOMP_PLUGIN_fatal ("%s\n", msg);
      else
	GCN_WARNING ("%s\n", msg);
      return probe ? true : false;
    }
  status = hsa_fns.hsa_init_fn ();
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Run-time could not be initialized", status);
  GCN_DEBUG ("HSA run-time initialized for GCN\n");

  if (debug)
    dump_hsa_system_info ();

  status = hsa_fns.hsa_iterate_agents_fn (count_gpu_agents, NULL);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("GCN GPU devices could not be enumerated", status);
  GCN_DEBUG ("There are %i GCN GPU devices.\n", hsa_context.agent_count);

  hsa_context.agents
    = GOMP_PLUGIN_malloc_cleared (hsa_context.agent_count
				  * sizeof (struct agent_info));
  status = hsa_fns.hsa_iterate_agents_fn (assign_agent_ids, &agent_index);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Scanning compute agents failed", status);
  if (agent_index != hsa_context.agent_count)
    {
      GOMP_PLUGIN_error ("Failed to assign IDs to all GCN agents");
      return false;
    }

  if (debug)
    {
      status = hsa_fns.hsa_iterate_agents_fn (dump_hsa_agent_info, NULL);
      if (status != HSA_STATUS_SUCCESS)
	GOMP_PLUGIN_error ("Failed to list all HSA runtime agents");
    }

  uint16_t minor, major;
  status = hsa_fns.hsa_system_get_info_fn (HSA_SYSTEM_INFO_VERSION_MINOR,
					   &minor);
  if (status != HSA_STATUS_SUCCESS)
    GOMP_PLUGIN_error ("Failed to obtain HSA runtime minor version");
  status = hsa_fns.hsa_system_get_info_fn (HSA_SYSTEM_INFO_VERSION_MAJOR,
					   &major);
  if (status != HSA_STATUS_SUCCESS)
    GOMP_PLUGIN_error ("Failed to obtain HSA runtime major version");

  size_t len = sizeof hsa_context.driver_version_s;
  int printed = snprintf (hsa_context.driver_version_s, len,
			  "HSA Runtime %hu.%hu", (unsigned short int)major,
			  (unsigned short int)minor);
  if (printed >= len)
    GCN_WARNING ("HSA runtime version string was truncated."
		 "Version %hu.%hu is too long.", (unsigned short int)major,
		 (unsigned short int)minor);

  hsa_context.initialized = true;
  return true;
}

/* Verify that hsa_context has already been initialized and return the
   agent_info structure describing device number N.  Return NULL on error.  */

static struct agent_info *
get_agent_info (int n)
{
  if (!hsa_context.initialized)
    {
      GOMP_PLUGIN_error ("Attempt to use uninitialized GCN context.");
      return NULL;
    }
  if (n >= hsa_context.agent_count)
    {
      GOMP_PLUGIN_error ("Request to operate on non-existent GCN device %i", n);
      return NULL;
    }
  if (!hsa_context.agents[n].initialized)
    {
      GOMP_PLUGIN_error ("Attempt to use an uninitialized GCN agent.");
      return NULL;
    }
  return &hsa_context.agents[n];
}

/* Callback of hsa_agent_iterate_regions, via get_*_memory_region functions.

   Selects (breaks at) a suitable region of type KIND.  */

static hsa_status_t
get_memory_region (hsa_region_t region, hsa_region_t *retval,
		   hsa_region_global_flag_t kind)
{
  hsa_status_t status;
  hsa_region_segment_t segment;

  status = hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_SEGMENT,
					   &segment);
  if (status != HSA_STATUS_SUCCESS)
    return status;
  if (segment != HSA_REGION_SEGMENT_GLOBAL)
    return HSA_STATUS_SUCCESS;

  uint32_t flags;
  status = hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_GLOBAL_FLAGS,
					   &flags);
  if (status != HSA_STATUS_SUCCESS)
    return status;
  if (flags & kind)
    {
      *retval = region;
      return HSA_STATUS_INFO_BREAK;
    }
  return HSA_STATUS_SUCCESS;
}

/* Callback of hsa_agent_iterate_regions.
 
   Selects a kernargs memory region.  */

static hsa_status_t
get_kernarg_memory_region (hsa_region_t region, void *data)
{
  return get_memory_region (region, (hsa_region_t *)data,
			    HSA_REGION_GLOBAL_FLAG_KERNARG);
}

/* Callback of hsa_agent_iterate_regions.

   Selects a coarse-grained memory region suitable for the heap and
   offload data.  */

static hsa_status_t
get_data_memory_region (hsa_region_t region, void *data)
{
  return get_memory_region (region, (hsa_region_t *)data,
			    HSA_REGION_GLOBAL_FLAG_COARSE_GRAINED);
}

static int
elf_gcn_isa_field (Elf64_Ehdr *image)
{
  return image->e_flags & EF_AMDGPU_MACH_MASK;
}

/* Returns the name that the HSA runtime uses for the ISA or NULL if we do not
   support the ISA. */

static const char*
isa_name (int isa) {
  switch(isa)
    {
#define GCN_DEVICE(name, NAME, ELF, ...) \
    case ELF: return #name;
#include "../../gcc/config/gcn/gcn-devices.def"
    }
  return NULL;
}

/* Returns the code which is used in the GCN object code to identify the ISA with
   the given name (as used by the HSA runtime).  */

static gcn_isa
isa_code(const char *isa) {
#define GCN_DEVICE(name, NAME, ELF, ...) \
  if (!strcmp (isa, #name)) return ELF;
#include "../../gcc/config/gcn/gcn-devices.def"

  return EF_AMDGPU_MACH_UNSUPPORTED;
}

/* CDNA2 devices have twice as many VGPRs compared to older devices.  */

static int
max_isa_vgprs (int isa)
{
  switch (isa)
    {
#define GCN_DEVICE(name, NAME, ELF, ISA, XNACK, SRAM, WAVE64, CU, \
		   MAX_ISA_VGPRS, ...) \
    case ELF: return MAX_ISA_VGPRS;
#include "../../gcc/config/gcn/gcn-devices.def"
    default:
      GOMP_PLUGIN_fatal ("unhandled ISA in max_isa_vgprs");
    }
}

/* }}}  */
/* {{{ Run  */

/* Create or reuse a team arena and stack space.
 
   Team arenas are used by OpenMP to avoid calling malloc multiple times
   while setting up each team.  This is purely a performance optimization.

   The stack space is used by all kernels.  We must allocate it in such a
   way that the reverse offload implmentation can access the data.

   Allocating this memory costs performance, so this function will reuse an
   existing allocation if a large enough one is idle.
   The memory lock is released, but not deallocated, when the kernel exits.  */

static void
configure_ephemeral_memories (struct kernel_info *kernel,
			      struct kernargs_abi *kernargs, int num_teams,
			      int num_threads)
{
  struct agent_info *agent = kernel->agent;
  struct ephemeral_memories_list **next_ptr = &agent->ephemeral_memories_list;
  struct ephemeral_memories_list *item;

  int actual_arena_size = (kernel->kind == KIND_OPENMP
			   ? team_arena_size : 0);
  int actual_arena_total_size = actual_arena_size * num_teams;
  size_t size = (actual_arena_total_size
		 + num_teams * num_threads * stack_size);

  for (item = *next_ptr; item; next_ptr = &item->next, item = item->next)
    {
      if (item->size < size)
	continue;

      if (pthread_mutex_trylock (&item->in_use) == 0)
	break;
    }

  if (!item)
    {
      GCN_DEBUG ("Creating a new %sstack for %d teams with %d threads"
		 " (%zd bytes)\n", (actual_arena_size ? "arena and " : ""),
		 num_teams, num_threads, size);

      if (pthread_mutex_lock (&agent->ephemeral_memories_write_lock))
	{
	  GOMP_PLUGIN_error ("Could not lock a GCN agent program mutex");
	  return;
	}
      item = malloc (sizeof (*item));
      item->size = size;
      item->next = NULL;
      *next_ptr = item;

      if (pthread_mutex_init (&item->in_use, NULL))
	{
	  GOMP_PLUGIN_error ("Failed to initialize a GCN memory write mutex");
	  return;
	}
      if (pthread_mutex_lock (&item->in_use))
	{
	  GOMP_PLUGIN_error ("Could not lock a GCN agent program mutex");
	  return;
	}
      if (pthread_mutex_unlock (&agent->ephemeral_memories_write_lock))
	{
	  GOMP_PLUGIN_error ("Could not unlock a GCN agent program mutex");
	  return;
	}

      hsa_status_t status;
      status = hsa_fns.hsa_memory_allocate_fn (agent->data_region, size,
					       &item->address);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not allocate memory for GCN kernel arena", status);
      status = hsa_fns.hsa_memory_assign_agent_fn (item->address, agent->id,
						   HSA_ACCESS_PERMISSION_RW);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not assign arena & stack memory to device", status);
    }

  kernargs->arena_ptr = (actual_arena_total_size
			 ? (uint64_t)item->address
			 : 0);
  kernargs->stack_ptr = (uint64_t)item->address + actual_arena_total_size;
  kernargs->arena_size_per_team = actual_arena_size;
  kernargs->stack_size_per_thread = stack_size;
}

/* Mark an ephemeral memory space available for reuse.  */

static void
release_ephemeral_memories (struct agent_info* agent, void *address)
{
  struct ephemeral_memories_list *item;

  for (item = agent->ephemeral_memories_list; item; item = item->next)
    {
      if (item->address == address)
	{
	  if (pthread_mutex_unlock (&item->in_use))
	    GOMP_PLUGIN_error ("Could not unlock a GCN agent program mutex");
	  return;
	}
    }
  GOMP_PLUGIN_error ("Could not find a GCN arena to release.");
}

/* Clean up all the allocated team arenas.  */

static bool
destroy_ephemeral_memories (struct agent_info *agent)
{
  struct ephemeral_memories_list *item, *next;

  for (item = agent->ephemeral_memories_list; item; item = next)
    {
      next = item->next;
      hsa_fns.hsa_memory_free_fn (item->address);
      if (pthread_mutex_destroy (&item->in_use))
	{
	  GOMP_PLUGIN_error ("Failed to destroy a GCN memory mutex");
	  return false;
	}
      free (item);
    }
  agent->ephemeral_memories_list = NULL;

  return true;
}

/* Allocate memory on a specified device.  */

static void *
alloc_by_agent (struct agent_info *agent, size_t size)
{
  GCN_DEBUG ("Allocating %zu bytes on device %d\n", size, agent->device_id);

  void *ptr;
  hsa_status_t status = hsa_fns.hsa_memory_allocate_fn (agent->data_region,
							size, &ptr);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not allocate device memory", status);
      return NULL;
    }

  status = hsa_fns.hsa_memory_assign_agent_fn (ptr, agent->id,
					       HSA_ACCESS_PERMISSION_RW);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not assign data memory to device", status);
      return NULL;
    }

  struct goacc_thread *thr = GOMP_PLUGIN_goacc_thread ();
  bool profiling_dispatch_p
    = __builtin_expect (thr != NULL && thr->prof_info != NULL, false);
  if (profiling_dispatch_p)
    {
      acc_prof_info *prof_info = thr->prof_info;
      acc_event_info data_event_info;
      acc_api_info *api_info = thr->api_info;

      prof_info->event_type = acc_ev_alloc;

      data_event_info.data_event.event_type = prof_info->event_type;
      data_event_info.data_event.valid_bytes
	= _ACC_DATA_EVENT_INFO_VALID_BYTES;
      data_event_info.data_event.parent_construct
	= acc_construct_parallel;
      data_event_info.data_event.implicit = 1;
      data_event_info.data_event.tool_info = NULL;
      data_event_info.data_event.var_name = NULL;
      data_event_info.data_event.bytes = size;
      data_event_info.data_event.host_ptr = NULL;
      data_event_info.data_event.device_ptr = (void *) ptr;

      api_info->device_api = acc_device_api_other;

      GOMP_PLUGIN_goacc_profiling_dispatch (prof_info, &data_event_info,
					    api_info);
    }

  return ptr;
}

/* Create kernel dispatch data structure for given KERNEL, along with
   the necessary device signals and memory allocations.  */

static struct kernel_dispatch *
create_kernel_dispatch (struct kernel_info *kernel, int num_teams,
			int num_threads)
{
  struct agent_info *agent = kernel->agent;
  struct kernel_dispatch *shadow
    = GOMP_PLUGIN_malloc_cleared (sizeof (struct kernel_dispatch));

  shadow->agent = kernel->agent;
  shadow->object = kernel->object;

  hsa_signal_t sync_signal;
  hsa_status_t status = hsa_fns.hsa_signal_create_fn (1, 0, NULL, &sync_signal);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Error creating the GCN sync signal", status);

  shadow->signal = sync_signal.handle;
  shadow->private_segment_size = kernel->private_segment_size;

  if (lowlat_size < 0)
    {
      /* Divide the LDS between the number of running teams.
	 Allocate not less than is defined in the kernel metadata.  */
      int teams_per_cu = num_teams / get_cu_count (agent);
      int LDS_per_team = (teams_per_cu ? 65536 / teams_per_cu : 65536);
      shadow->group_segment_size
	= (kernel->group_segment_size > LDS_per_team
	   ? kernel->group_segment_size
	   : LDS_per_team);;
    }
  else if (lowlat_size < GCN_LOWLAT_HEAP+8)
    /* Ensure that there's space for the OpenMP libgomp data.  */
    shadow->group_segment_size = GCN_LOWLAT_HEAP+8;
  else
    shadow->group_segment_size = (lowlat_size > 65536
				  ? 65536
				  : lowlat_size);

  /* We expect kernels to request a single pointer, explicitly, and the
     rest of struct kernargs, implicitly.  If they request anything else
     then something is wrong.  */
  if (kernel->kernarg_segment_size > 8)
    {
      GOMP_PLUGIN_fatal ("Unexpectedly large kernargs segment requested");
      return NULL;
    }

  status = hsa_fns.hsa_memory_allocate_fn (agent->kernarg_region,
					   sizeof (struct kernargs),
					   &shadow->kernarg_address);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not allocate memory for GCN kernel arguments", status);
  struct kernargs *kernargs = shadow->kernarg_address;

  /* Zero-initialize the output_data (minimum needed).  */
  kernargs->abi.out_ptr = (int64_t)&kernargs->output_data;
  kernargs->output_data.next_output = 0;
  for (unsigned i = 0;
       i < (sizeof (kernargs->output_data.queue)
	    / sizeof (kernargs->output_data.queue[0]));
       i++)
    kernargs->output_data.queue[i].written = 0;
  kernargs->output_data.consumed = 0;

  /* Pass in the heap location.  */
  kernargs->abi.heap_ptr = (int64_t)kernel->module->heap;

  /* Create the ephemeral memory spaces.  */
  configure_ephemeral_memories (kernel, &kernargs->abi, num_teams, num_threads);

  /* Ensure we can recognize unset return values.  */
  kernargs->output_data.return_value = 0xcafe0000;

  return shadow;
}

static void
process_reverse_offload (uint64_t fn, uint64_t mapnum, uint64_t hostaddrs,
			 uint64_t sizes, uint64_t kinds, uint64_t dev_num64)
{
  int dev_num = dev_num64;
  GOMP_PLUGIN_target_rev (fn, mapnum, hostaddrs, sizes, kinds, dev_num,
			  NULL);
}

/* Output any data written to console output from the kernel.  It is expected
   that this function is polled during kernel execution.

   We print all entries from the last item printed to the next entry without
   a "written" flag.  If the "final" flag is set then it'll continue right to
   the end.
 
   The print buffer is circular, but the from and to locations don't wrap when
   the buffer does, so the output limit is UINT_MAX.  The target blocks on
   output when the buffer is full.  */

static void
console_output (struct kernel_info *kernel, struct kernargs *kernargs,
		bool final)
{
  unsigned int limit = (sizeof (kernargs->output_data.queue)
			/ sizeof (kernargs->output_data.queue[0]));

  unsigned int from = __atomic_load_n (&kernargs->output_data.consumed,
				       __ATOMIC_ACQUIRE);
  unsigned int to = kernargs->output_data.next_output;

  if (from > to)
    {
      /* Overflow.  */
      if (final)
	printf ("GCN print buffer overflowed.\n");
      return;
    }

  unsigned int i;
  for (i = from; i < to; i++)
    {
      struct printf_data *data = &kernargs->output_data.queue[i%limit];

      if (!data->written && !final)
	break;

      switch (data->type)
	{
	case 0: printf ("%.128s%ld\n", data->msg, data->ivalue); break;
	case 1: printf ("%.128s%f\n", data->msg, data->dvalue); break;
	case 2: printf ("%.128s%.128s\n", data->msg, data->text); break;
	case 3: printf ("%.128s%.128s", data->msg, data->text); break;
	case 4:
	  process_reverse_offload (data->value_u64[0], data->value_u64[1],
				   data->value_u64[2], data->value_u64[3],
				   data->value_u64[4], data->value_u64[5]);
	  break;
	default: printf ("GCN print buffer error!\n"); break;
	}
      data->written = 0;
      __atomic_store_n (&kernargs->output_data.consumed, i+1,
			__ATOMIC_RELEASE);
    }
  fflush (stdout);
}

/* Release data structure created for a kernel dispatch in SHADOW argument,
   and clean up the signal and memory allocations.  */

static void
release_kernel_dispatch (struct kernel_dispatch *shadow)
{
  GCN_DEBUG ("Released kernel dispatch: %p\n", shadow);

  struct kernargs *kernargs = shadow->kernarg_address;
  void *addr = (void *)kernargs->abi.arena_ptr;
  if (!addr)
    addr = (void *)kernargs->abi.stack_ptr;
  release_ephemeral_memories (shadow->agent, addr);

  hsa_fns.hsa_memory_free_fn (shadow->kernarg_address);

  hsa_signal_t s;
  s.handle = shadow->signal;
  hsa_fns.hsa_signal_destroy_fn (s);

  free (shadow);
}

/* Extract the properties from a kernel binary.  */

static void
init_kernel_properties (struct kernel_info *kernel)
{
  hsa_status_t status;
  struct agent_info *agent = kernel->agent;
  hsa_executable_symbol_t kernel_symbol;
  char *buf = alloca (strlen (kernel->name) + 4);
  sprintf (buf, "%s.kd", kernel->name);
  status = hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						 buf, agent->id,
						 0, &kernel_symbol);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_warn ("Could not find symbol for kernel in the code object", status);
      fprintf (stderr, "not found name: '%s'\n", buf);
      dump_executable_symbols (agent->executable);
      goto failure;
    }
  GCN_DEBUG ("Located kernel %s\n", kernel->name);
  status = hsa_fns.hsa_executable_symbol_get_info_fn
    (kernel_symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_OBJECT, &kernel->object);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not extract a kernel object from its symbol", status);
  status = hsa_fns.hsa_executable_symbol_get_info_fn
    (kernel_symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_KERNARG_SEGMENT_SIZE,
     &kernel->kernarg_segment_size);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not get info about kernel argument size", status);
  status = hsa_fns.hsa_executable_symbol_get_info_fn
    (kernel_symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_GROUP_SEGMENT_SIZE,
     &kernel->group_segment_size);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not get info about kernel group segment size", status);
  status = hsa_fns.hsa_executable_symbol_get_info_fn
    (kernel_symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_PRIVATE_SEGMENT_SIZE,
     &kernel->private_segment_size);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not get info about kernel private segment size",
	       status);

  /* The kernel type is not known until something tries to launch it.  */
  kernel->kind = KIND_UNKNOWN;

  GCN_DEBUG ("Kernel structure for %s fully initialized with "
	     "following segment sizes: \n", kernel->name);
  GCN_DEBUG ("  group_segment_size: %u\n",
	     (unsigned) kernel->group_segment_size);
  GCN_DEBUG ("  private_segment_size: %u\n",
	     (unsigned) kernel->private_segment_size);
  GCN_DEBUG ("  kernarg_segment_size: %u\n",
	     (unsigned) kernel->kernarg_segment_size);
  return;

failure:
  kernel->initialization_failed = true;
}

/* Do all the work that is necessary before running KERNEL for the first time.
   The function assumes the program has been created, finalized and frozen by
   create_and_finalize_hsa_program.  */

static void
init_kernel (struct kernel_info *kernel)
{
  if (pthread_mutex_lock (&kernel->init_mutex))
    GOMP_PLUGIN_fatal ("Could not lock a GCN kernel initialization mutex");
  if (kernel->initialized)
    {
      if (pthread_mutex_unlock (&kernel->init_mutex))
	GOMP_PLUGIN_fatal ("Could not unlock a GCN kernel initialization "
			   "mutex");

      return;
    }

  init_kernel_properties (kernel);

  if (!kernel->initialization_failed)
    {
      GCN_DEBUG ("\n");

      kernel->initialized = true;
    }
  if (pthread_mutex_unlock (&kernel->init_mutex))
    GOMP_PLUGIN_fatal ("Could not unlock a GCN kernel initialization "
		       "mutex");
}

/* Run KERNEL on its agent, pass VARS to it as arguments and take
   launch attributes from KLA.
   
   MODULE_LOCKED indicates that the caller already holds the lock and
   run_kernel need not lock it again.
   If AQ is NULL then agent->sync_queue will be used.  */

static void
run_kernel (struct kernel_info *kernel, void *vars,
	    struct GOMP_kernel_launch_attributes *kla,
	    struct goacc_asyncqueue *aq, bool module_locked)
{
  struct agent_info *agent = kernel->agent;
  GCN_DEBUG ("SGPRs: %d, VGPRs: %d\n", kernel->description->sgpr_count,
	     kernel->description->vpgr_count);

  /* Reduce the number of threads/workers if there are insufficient
     VGPRs available to run the kernels together.  */
  if (kla->ndim == 3 && kernel->description->vpgr_count > 0)
    {
      int max_vgprs = max_isa_vgprs (agent->device_isa);
      int granulated_vgprs = (kernel->description->vpgr_count + 3) & ~3;
      int max_threads = (max_vgprs / granulated_vgprs) * 4;
      if (kla->gdims[2] > max_threads)
	{
	  GCN_WARNING ("Too many VGPRs required to support %d threads/workers"
		       " per team/gang - reducing to %d threads/workers.\n",
		       kla->gdims[2], max_threads);
	  kla->gdims[2] = max_threads;
	}
    }

  GCN_DEBUG ("GCN launch on queue: %d:%d\n", kernel->agent->device_id,
	     (aq ? aq->id : 0));
  GCN_DEBUG ("GCN launch attribs: gdims:[");
  int i;
  for (i = 0; i < kla->ndim; ++i)
    {
      if (i)
	DEBUG_PRINT (", ");
      DEBUG_PRINT ("%u", kla->gdims[i]);
    }
  DEBUG_PRINT ("], normalized gdims:[");
  for (i = 0; i < kla->ndim; ++i)
    {
      if (i)
	DEBUG_PRINT (", ");
      DEBUG_PRINT ("%u", kla->gdims[i] / kla->wdims[i]);
    }
  DEBUG_PRINT ("], wdims:[");
  for (i = 0; i < kla->ndim; ++i)
    {
      if (i)
	DEBUG_PRINT (", ");
      DEBUG_PRINT ("%u", kla->wdims[i]);
    }
  DEBUG_PRINT ("]\n");
  DEBUG_FLUSH ();

  if (!module_locked && pthread_rwlock_rdlock (&agent->module_rwlock))
    GOMP_PLUGIN_fatal ("Unable to read-lock a GCN agent rwlock");

  if (!agent->initialized)
    GOMP_PLUGIN_fatal ("Agent must be initialized");

  if (!kernel->initialized)
    GOMP_PLUGIN_fatal ("Called kernel must be initialized");

  hsa_queue_t *command_q = (aq ? aq->hsa_queue : kernel->agent->sync_queue);

  uint64_t index
    = hsa_fns.hsa_queue_add_write_index_release_fn (command_q, 1);
  GCN_DEBUG ("Got AQL index %llu\n", (long long int) index);

  /* Wait until the queue is not full before writing the packet.   */
  while (index - hsa_fns.hsa_queue_load_read_index_acquire_fn (command_q)
	 >= command_q->size)
    ;

  /* Do not allow the dimensions to be overridden when running
     constructors or destructors.  */
  int override_x = kernel->kind == KIND_UNKNOWN ? 0 : override_x_dim;
  int override_z = kernel->kind == KIND_UNKNOWN ? 0 : override_z_dim;

  hsa_kernel_dispatch_packet_t *packet;
  packet = ((hsa_kernel_dispatch_packet_t *) command_q->base_address)
	   + index % command_q->size;

  memset (((uint8_t *) packet) + 4, 0, sizeof (*packet) - 4);
  packet->grid_size_x = override_x ? : kla->gdims[0];
  packet->workgroup_size_x = get_group_size (kla->ndim,
					     packet->grid_size_x,
					     kla->wdims[0]);

  if (kla->ndim >= 2)
    {
      packet->grid_size_y = kla->gdims[1];
      packet->workgroup_size_y = get_group_size (kla->ndim, kla->gdims[1],
						 kla->wdims[1]);
    }
  else
    {
      packet->grid_size_y = 1;
      packet->workgroup_size_y = 1;
    }

  if (kla->ndim == 3)
    {
      packet->grid_size_z = limit_worker_threads (override_z
						  ? : kla->gdims[2]);
      packet->workgroup_size_z = get_group_size (kla->ndim,
						 packet->grid_size_z,
						 kla->wdims[2]);
    }
  else
    {
      packet->grid_size_z = 1;
      packet->workgroup_size_z = 1;
    }

  GCN_DEBUG ("GCN launch actuals: grid:[%u, %u, %u],"
	     " normalized grid:[%u, %u, %u], workgroup:[%u, %u, %u]\n",
	     packet->grid_size_x, packet->grid_size_y, packet->grid_size_z,
	     packet->grid_size_x / packet->workgroup_size_x,
	     packet->grid_size_y / packet->workgroup_size_y,
	     packet->grid_size_z / packet->workgroup_size_z,
	     packet->workgroup_size_x, packet->workgroup_size_y,
	     packet->workgroup_size_z);

  struct kernel_dispatch *shadow
    = create_kernel_dispatch (kernel, packet->grid_size_x,
			      packet->grid_size_z);
  shadow->queue = command_q;

  if (debug)
    {
      fprintf (stderr, "\nKernel has following dependencies:\n");
      print_kernel_dispatch (shadow, 2);
    }

  packet->private_segment_size = shadow->private_segment_size;
  packet->group_segment_size = shadow->group_segment_size;
  packet->kernel_object = shadow->object;
  packet->kernarg_address = shadow->kernarg_address;
  hsa_signal_t s;
  s.handle = shadow->signal;
  packet->completion_signal = s;
  hsa_fns.hsa_signal_store_relaxed_fn (s, 1);
  memcpy (shadow->kernarg_address, &vars, sizeof (vars));

  GCN_DEBUG ("Copying kernel runtime pointer to kernarg_address\n");

  uint16_t header;
  header = HSA_PACKET_TYPE_KERNEL_DISPATCH << HSA_PACKET_HEADER_TYPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_ACQUIRE_FENCE_SCOPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_RELEASE_FENCE_SCOPE;

  GCN_DEBUG ("Going to dispatch kernel %s on device %d\n", kernel->name,
	     agent->device_id);

  packet_store_release ((uint32_t *) packet, header,
			(uint16_t) kla->ndim
			<< HSA_KERNEL_DISPATCH_PACKET_SETUP_DIMENSIONS);

  hsa_fns.hsa_signal_store_release_fn (command_q->doorbell_signal,
				       index);

  GCN_DEBUG ("Kernel dispatched, waiting for completion\n");

  /* Root signal waits with 1ms timeout.  */
  while (hsa_fns.hsa_signal_wait_acquire_fn (s, HSA_SIGNAL_CONDITION_LT, 1,
					     1000 * 1000,
					     HSA_WAIT_STATE_BLOCKED) != 0)
    {
      console_output (kernel, shadow->kernarg_address, false);
    }
  console_output (kernel, shadow->kernarg_address, true);

  struct kernargs *kernargs = shadow->kernarg_address;
  unsigned int return_value = (unsigned int)kernargs->output_data.return_value;

  release_kernel_dispatch (shadow);

  if (!module_locked && pthread_rwlock_unlock (&agent->module_rwlock))
    GOMP_PLUGIN_fatal ("Unable to unlock a GCN agent rwlock");

  unsigned int upper = (return_value & ~0xffff) >> 16;
  if (upper == 0xcafe)
    ; // exit not called, normal termination.
  else if (upper == 0xffff)
    ; // exit called.
  else
    {
      GOMP_PLUGIN_error ("Possible kernel exit value corruption, 2 most"
			 " significant bytes aren't 0xffff or 0xcafe: 0x%x\n",
			 return_value);
      abort ();
    }

  if (upper == 0xffff)
    {
      unsigned int signal = (return_value >> 8) & 0xff;

      if (signal == SIGABRT)
	{
	  GCN_WARNING ("GCN Kernel aborted\n");
	  abort ();
	}
      else if (signal != 0)
	{
	  GCN_WARNING ("GCN Kernel received unknown signal\n");
	  abort ();
	}

      GCN_DEBUG ("GCN Kernel exited with value: %d\n", return_value & 0xff);
      exit (return_value & 0xff);
    }
}

/* }}}  */
/* {{{ Load/Unload  */

/* Initialize KERNEL from D and other parameters.  Return true on success. */

static bool
init_basic_kernel_info (struct kernel_info *kernel,
			struct hsa_kernel_description *d,
			struct agent_info *agent,
			struct module_info *module)
{
  kernel->agent = agent;
  kernel->module = module;
  kernel->name = d->name;
  kernel->description = d;
  if (pthread_mutex_init (&kernel->init_mutex, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN kernel mutex");
      return false;
    }
  return true;
}

/* Check that the GCN ISA of the given image matches the ISA of the agent. */

static bool
isa_matches_agent (struct agent_info *agent, Elf64_Ehdr *image)
{
  int isa_field = elf_gcn_isa_field (image);
  const char* isa_s = isa_name (isa_field);
  if (!isa_s)
    {
      hsa_error ("Unsupported ISA in GCN code object.", HSA_STATUS_ERROR);
      return false;
    }

  if (isa_field != agent->device_isa)
    {
      char msg[120];
      const char *agent_isa_s = isa_name (agent->device_isa);
      assert (agent_isa_s);

      snprintf (msg, sizeof msg,
		"GCN code object ISA '%s' does not match GPU ISA '%s'.\n"
		"Try to recompile with '-foffload-options=-march=%s'.\n",
		isa_s, agent_isa_s, agent_isa_s);

      hsa_error (msg, HSA_STATUS_ERROR);
      return false;
    }

  return true;
}

/* Create and finalize the program consisting of all loaded modules.  */

static bool
create_and_finalize_hsa_program (struct agent_info *agent)
{
  hsa_status_t status;
  bool res = true;
  if (pthread_mutex_lock (&agent->prog_mutex))
    {
      GOMP_PLUGIN_error ("Could not lock a GCN agent program mutex");
      return false;
    }
  if (agent->prog_finalized)
    goto final;

  status
    = hsa_fns.hsa_executable_create_fn (HSA_PROFILE_FULL,
					HSA_EXECUTABLE_STATE_UNFROZEN,
					"", &agent->executable);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not create GCN executable", status);
      goto fail;
    }

  /* Load any GCN modules.  */
  struct module_info *module = agent->module;
  if (module)
    {
      Elf64_Ehdr *image = (Elf64_Ehdr *)module->image_desc->gcn_image->image;

      if (!isa_matches_agent (agent, image))
	goto fail;

      hsa_code_object_t co = { 0 };
      status = hsa_fns.hsa_code_object_deserialize_fn
	(module->image_desc->gcn_image->image,
	 module->image_desc->gcn_image->size,
	 NULL, &co);
      if (status != HSA_STATUS_SUCCESS)
	{
	  hsa_error ("Could not deserialize GCN code object", status);
	  goto fail;
	}

      status = hsa_fns.hsa_executable_load_code_object_fn
	(agent->executable, agent->id, co, "");
      if (status != HSA_STATUS_SUCCESS)
	{
	  hsa_error ("Could not load GCN code object", status);
	  goto fail;
	}

      if (!module->heap)
	{
	  status = hsa_fns.hsa_memory_allocate_fn (agent->data_region,
						   gcn_kernel_heap_size,
						   (void**)&module->heap);
	  if (status != HSA_STATUS_SUCCESS)
	    {
	      hsa_error ("Could not allocate memory for GCN heap", status);
	      goto fail;
	    }

	  status = hsa_fns.hsa_memory_assign_agent_fn
			(module->heap, agent->id, HSA_ACCESS_PERMISSION_RW);
	  if (status != HSA_STATUS_SUCCESS)
	    {
	      hsa_error ("Could not assign GCN heap memory to device", status);
	      goto fail;
	    }

	  hsa_fns.hsa_memory_copy_fn (&module->heap->size,
				      &gcn_kernel_heap_size,
				      sizeof (gcn_kernel_heap_size));
	}

    }

  if (debug)
    dump_executable_symbols (agent->executable);

  status = hsa_fns.hsa_executable_freeze_fn (agent->executable, "");
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not freeze the GCN executable", status);
      goto fail;
    }

final:
  agent->prog_finalized = true;

  if (pthread_mutex_unlock (&agent->prog_mutex))
    {
      GOMP_PLUGIN_error ("Could not unlock a GCN agent program mutex");
      res = false;
    }

  return res;

fail:
  res = false;
  goto final;
}

/* Free the HSA program in agent and everything associated with it and set
   agent->prog_finalized and the initialized flags of all kernels to false.
   Return TRUE on success.  */

static bool
destroy_hsa_program (struct agent_info *agent)
{
  if (!agent->prog_finalized)
    return true;

  hsa_status_t status;

  GCN_DEBUG ("Destroying the current GCN program.\n");

  status = hsa_fns.hsa_executable_destroy_fn (agent->executable);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Could not destroy GCN executable", status);

  if (agent->module)
    {
      int i;
      for (i = 0; i < agent->module->kernel_count; i++)
	agent->module->kernels[i].initialized = false;

      if (agent->module->heap)
	{
	  hsa_fns.hsa_memory_free_fn (agent->module->heap);
	  agent->module->heap = NULL;
	}
    }
  agent->prog_finalized = false;
  return true;
}

/* Deinitialize all information associated with MODULE and kernels within
   it.  Return TRUE on success.  */

static bool
destroy_module (struct module_info *module, bool locked)
{
  /* Run destructors before destroying module.  */
  struct GOMP_kernel_launch_attributes kla =
    { 3,
      /* Grid size.  */
      { 1, 64, 1 },
      /* Work-group size.  */
      { 1, 64, 1 }
    };

  if (module->fini_array_func)
    {
      init_kernel (module->fini_array_func);
      run_kernel (module->fini_array_func, NULL, &kla, NULL, locked);
    }
  module->constructors_run_p = false;

  int i;
  for (i = 0; i < module->kernel_count; i++)
    if (pthread_mutex_destroy (&module->kernels[i].init_mutex))
      {
	GOMP_PLUGIN_error ("Failed to destroy a GCN kernel initialization "
			   "mutex");
	return false;
      }

  return true;
}

/* }}}  */
/* {{{ Async  */

/* Callback of dispatch queues to report errors.  */

static void
execute_queue_entry (struct goacc_asyncqueue *aq, int index)
{
  struct queue_entry *entry = &aq->queue[index];

  switch (entry->type)
    {
    case KERNEL_LAUNCH:
      if (DEBUG_QUEUES)
	GCN_DEBUG ("Async thread %d:%d: Executing launch entry (%d)\n",
		   aq->agent->device_id, aq->id, index);
      run_kernel (entry->u.launch.kernel,
		  entry->u.launch.vars,
		  &entry->u.launch.kla, aq, false);
      if (DEBUG_QUEUES)
	GCN_DEBUG ("Async thread %d:%d: Executing launch entry (%d) done\n",
		   aq->agent->device_id, aq->id, index);
      break;

    case CALLBACK:
      if (DEBUG_QUEUES)
	GCN_DEBUG ("Async thread %d:%d: Executing callback entry (%d)\n",
		   aq->agent->device_id, aq->id, index);
      entry->u.callback.fn (entry->u.callback.data);
      if (DEBUG_QUEUES)
	GCN_DEBUG ("Async thread %d:%d: Executing callback entry (%d) done\n",
		   aq->agent->device_id, aq->id, index);
      break;

    case ASYNC_WAIT:
      {
	/* FIXME: is it safe to access a placeholder that may already have
	   been executed?  */
        struct placeholder *placeholderp = entry->u.asyncwait.placeholderp;

	if (DEBUG_QUEUES)
          GCN_DEBUG ("Async thread %d:%d: Executing async wait entry (%d)\n",
		     aq->agent->device_id, aq->id, index);

	pthread_mutex_lock (&placeholderp->mutex);

	while (!placeholderp->executed)
          pthread_cond_wait (&placeholderp->cond, &placeholderp->mutex);

	pthread_mutex_unlock (&placeholderp->mutex);

	if (pthread_cond_destroy (&placeholderp->cond))
	  GOMP_PLUGIN_error ("Failed to destroy serialization cond");

	if (pthread_mutex_destroy (&placeholderp->mutex))
	  GOMP_PLUGIN_error ("Failed to destroy serialization mutex");

	if (DEBUG_QUEUES)
          GCN_DEBUG ("Async thread %d:%d: Executing async wait "
		     "entry (%d) done\n", aq->agent->device_id, aq->id, index);
      }
      break;

    case ASYNC_PLACEHOLDER:
      pthread_mutex_lock (&entry->u.placeholder.mutex);
      entry->u.placeholder.executed = 1;
      pthread_cond_signal (&entry->u.placeholder.cond);
      pthread_mutex_unlock (&entry->u.placeholder.mutex);
      break;

    default:
      GOMP_PLUGIN_fatal ("Unknown queue element");
    }
}

/* This function is run as a thread to service an async queue in the
   background.  It runs continuously until the stop flag is set.  */

static void *
drain_queue (void *thread_arg)
{
  struct goacc_asyncqueue *aq = thread_arg;

  if (DRAIN_QUEUE_SYNCHRONOUS_P)
    {
      aq->drain_queue_stop = 2;
      return NULL;
    }

  pthread_mutex_lock (&aq->mutex);

  while (true)
    {
      if (aq->drain_queue_stop)
	break;

      if (aq->queue_n > 0)
	{
	  pthread_mutex_unlock (&aq->mutex);
	  execute_queue_entry (aq, aq->queue_first);

	  pthread_mutex_lock (&aq->mutex);
	  aq->queue_first = ((aq->queue_first + 1)
			     % ASYNC_QUEUE_SIZE);
	  aq->queue_n--;

	  if (DEBUG_THREAD_SIGNAL)
	    GCN_DEBUG ("Async thread %d:%d: broadcasting queue out update\n",
		       aq->agent->device_id, aq->id);
	  pthread_cond_broadcast (&aq->queue_cond_out);
	  pthread_mutex_unlock (&aq->mutex);

	  if (DEBUG_QUEUES)
	    GCN_DEBUG ("Async thread %d:%d: continue\n", aq->agent->device_id,
		       aq->id);
	  pthread_mutex_lock (&aq->mutex);
	}
      else
	{
	  if (DEBUG_THREAD_SLEEP)
	    GCN_DEBUG ("Async thread %d:%d: going to sleep\n",
		       aq->agent->device_id, aq->id);
	  pthread_cond_wait (&aq->queue_cond_in, &aq->mutex);
	  if (DEBUG_THREAD_SLEEP)
	    GCN_DEBUG ("Async thread %d:%d: woke up, rechecking\n",
		       aq->agent->device_id, aq->id);
	}
    }

  aq->drain_queue_stop = 2;
  if (DEBUG_THREAD_SIGNAL)
    GCN_DEBUG ("Async thread %d:%d: broadcasting last queue out update\n",
	       aq->agent->device_id, aq->id);
  pthread_cond_broadcast (&aq->queue_cond_out);
  pthread_mutex_unlock (&aq->mutex);

  GCN_DEBUG ("Async thread %d:%d: returning\n", aq->agent->device_id, aq->id);
  return NULL;
}

/* This function is used only when DRAIN_QUEUE_SYNCHRONOUS_P is set, which
   is not usually the case.  This is just a debug tool.  */

static void
drain_queue_synchronous (struct goacc_asyncqueue *aq)
{
  pthread_mutex_lock (&aq->mutex);

  while (aq->queue_n > 0)
    {
      execute_queue_entry (aq, aq->queue_first);

      aq->queue_first = ((aq->queue_first + 1)
			 % ASYNC_QUEUE_SIZE);
      aq->queue_n--;
    }

  pthread_mutex_unlock (&aq->mutex);
}

/* Block the current thread until an async queue is writable.  The aq->mutex
   lock should be held on entry, and remains locked on exit.  */

static void
wait_for_queue_nonfull (struct goacc_asyncqueue *aq)
{
  if (aq->queue_n == ASYNC_QUEUE_SIZE)
    {
      /* Queue is full.  Wait for it to not be full.  */
      while (aq->queue_n == ASYNC_QUEUE_SIZE)
	pthread_cond_wait (&aq->queue_cond_out, &aq->mutex);
    }
}

/* Request an asynchronous kernel launch on the specified queue.  This
   may block if the queue is full, but returns without waiting for the
   kernel to run.  */

static void
queue_push_launch (struct goacc_asyncqueue *aq, struct kernel_info *kernel,
		   void *vars, struct GOMP_kernel_launch_attributes *kla)
{
  assert (aq->agent == kernel->agent);

  pthread_mutex_lock (&aq->mutex);

  wait_for_queue_nonfull (aq);

  int queue_last = ((aq->queue_first + aq->queue_n)
		    % ASYNC_QUEUE_SIZE);
  if (DEBUG_QUEUES)
    GCN_DEBUG ("queue_push_launch %d:%d: at %i\n", aq->agent->device_id,
	       aq->id, queue_last);

  aq->queue[queue_last].type = KERNEL_LAUNCH;
  aq->queue[queue_last].u.launch.kernel = kernel;
  aq->queue[queue_last].u.launch.vars = vars;
  aq->queue[queue_last].u.launch.kla = *kla;

  aq->queue_n++;

  if (DEBUG_THREAD_SIGNAL)
    GCN_DEBUG ("signalling async thread %d:%d: cond_in\n",
	       aq->agent->device_id, aq->id);
  pthread_cond_signal (&aq->queue_cond_in);

  pthread_mutex_unlock (&aq->mutex);
}

/* Request an asynchronous callback on the specified queue.  The callback
   function will be called, with the given opaque data, from the appropriate
   async thread, when all previous items on that queue are complete.  */

static void
queue_push_callback (struct goacc_asyncqueue *aq, void (*fn)(void *),
		     void *data)
{
  pthread_mutex_lock (&aq->mutex);

  wait_for_queue_nonfull (aq);

  int queue_last = ((aq->queue_first + aq->queue_n)
		    % ASYNC_QUEUE_SIZE);
  if (DEBUG_QUEUES)
    GCN_DEBUG ("queue_push_callback %d:%d: at %i\n", aq->agent->device_id,
	       aq->id, queue_last);

  aq->queue[queue_last].type = CALLBACK;
  aq->queue[queue_last].u.callback.fn = fn;
  aq->queue[queue_last].u.callback.data = data;

  aq->queue_n++;

  if (DEBUG_THREAD_SIGNAL)
    GCN_DEBUG ("signalling async thread %d:%d: cond_in\n",
	       aq->agent->device_id, aq->id);
  pthread_cond_signal (&aq->queue_cond_in);

  pthread_mutex_unlock (&aq->mutex);
}

/* Request that a given async thread wait for another thread (unspecified) to
   reach the given placeholder.  The wait will occur when all previous entries
   on the queue are complete.  A placeholder is effectively a kind of signal
   which simply sets a flag when encountered in a queue.  */

static void
queue_push_asyncwait (struct goacc_asyncqueue *aq,
		      struct placeholder *placeholderp)
{
  pthread_mutex_lock (&aq->mutex);

  wait_for_queue_nonfull (aq);

  int queue_last = ((aq->queue_first + aq->queue_n) % ASYNC_QUEUE_SIZE);
  if (DEBUG_QUEUES)
    GCN_DEBUG ("queue_push_asyncwait %d:%d: at %i\n", aq->agent->device_id,
	       aq->id, queue_last);

  aq->queue[queue_last].type = ASYNC_WAIT;
  aq->queue[queue_last].u.asyncwait.placeholderp = placeholderp;

  aq->queue_n++;

  if (DEBUG_THREAD_SIGNAL)
    GCN_DEBUG ("signalling async thread %d:%d: cond_in\n",
	       aq->agent->device_id, aq->id);
  pthread_cond_signal (&aq->queue_cond_in);

  pthread_mutex_unlock (&aq->mutex);
}

/* Add a placeholder into an async queue.  When the async thread reaches the
   placeholder it will set the "executed" flag to true and continue.
   Another thread may be waiting on this thread reaching the placeholder.  */

static struct placeholder *
queue_push_placeholder (struct goacc_asyncqueue *aq)
{
  struct placeholder *placeholderp;

  pthread_mutex_lock (&aq->mutex);

  wait_for_queue_nonfull (aq);

  int queue_last = ((aq->queue_first + aq->queue_n) % ASYNC_QUEUE_SIZE);
  if (DEBUG_QUEUES)
    GCN_DEBUG ("queue_push_placeholder %d:%d: at %i\n", aq->agent->device_id,
	       aq->id, queue_last);

  aq->queue[queue_last].type = ASYNC_PLACEHOLDER;
  placeholderp = &aq->queue[queue_last].u.placeholder;

  if (pthread_mutex_init (&placeholderp->mutex, NULL))
    {
      pthread_mutex_unlock (&aq->mutex);
      GOMP_PLUGIN_error ("Failed to initialize serialization mutex");
    }

  if (pthread_cond_init (&placeholderp->cond, NULL))
    {
      pthread_mutex_unlock (&aq->mutex);
      GOMP_PLUGIN_error ("Failed to initialize serialization cond");
    }

  placeholderp->executed = 0;

  aq->queue_n++;

  if (DEBUG_THREAD_SIGNAL)
    GCN_DEBUG ("signalling async thread %d:%d: cond_in\n",
	       aq->agent->device_id, aq->id);
  pthread_cond_signal (&aq->queue_cond_in);

  pthread_mutex_unlock (&aq->mutex);

  return placeholderp;
}

/* Signal an asynchronous thread to terminate, and wait for it to do so.  */

static void
finalize_async_thread (struct goacc_asyncqueue *aq)
{
  pthread_mutex_lock (&aq->mutex);
  if (aq->drain_queue_stop == 2)
    {
      pthread_mutex_unlock (&aq->mutex);
      return;
    }

  aq->drain_queue_stop = 1;

  if (DEBUG_THREAD_SIGNAL)
    GCN_DEBUG ("Signalling async thread %d:%d: cond_in\n",
	       aq->agent->device_id, aq->id);
  pthread_cond_signal (&aq->queue_cond_in);

  while (aq->drain_queue_stop != 2)
    {
      if (DEBUG_THREAD_SLEEP)
	GCN_DEBUG ("Waiting for async thread %d:%d to finish, putting thread"
		   " to sleep\n", aq->agent->device_id, aq->id);
      pthread_cond_wait (&aq->queue_cond_out, &aq->mutex);
      if (DEBUG_THREAD_SLEEP)
	GCN_DEBUG ("Waiting, woke up thread %d:%d.  Rechecking\n",
		   aq->agent->device_id, aq->id);
    }

  GCN_DEBUG ("Done waiting for async thread %d:%d\n", aq->agent->device_id,
	     aq->id);
  pthread_mutex_unlock (&aq->mutex);

  int err = pthread_join (aq->thread_drain_queue, NULL);
  if (err != 0)
    GOMP_PLUGIN_fatal ("Join async thread %d:%d: failed: %s",
		       aq->agent->device_id, aq->id, strerror (err));
  GCN_DEBUG ("Joined with async thread %d:%d\n", aq->agent->device_id, aq->id);
}

/* Set up an async queue for OpenMP.  There will be only one.  The
   implementation simply uses an OpenACC async queue.
   FIXME: is this thread-safe if two threads call this function?  */

static void
maybe_init_omp_async (struct agent_info *agent)
{
  if (!agent->omp_async_queue)
    agent->omp_async_queue
      = GOMP_OFFLOAD_openacc_async_construct (agent->device_id);
}

/* A wrapper that works around an issue in the HSA runtime with host-to-device
   copies from read-only pages.  */

static void
hsa_memory_copy_wrapper (void *dst, const void *src, size_t len)
{
  hsa_status_t status = hsa_fns.hsa_memory_copy_fn (dst, src, len);

  if (status == HSA_STATUS_SUCCESS)
    return;

  /* It appears that the copy fails if the source data is in a read-only page.
     We can't detect that easily, so try copying the data to a temporary buffer
     and doing the copy again if we got an error above.  */

  GCN_WARNING ("Read-only data transfer bug workaround triggered for "
	       "[%p:+%d]\n", (void *) src, (int) len);

  void *src_copy = malloc (len);
  memcpy (src_copy, src, len);
  status = hsa_fns.hsa_memory_copy_fn (dst, (const void *) src_copy, len);
  free (src_copy);
  if (status != HSA_STATUS_SUCCESS)
    GOMP_PLUGIN_error ("memory copy failed");
}

/* Copy data to or from a device.  This is intended for use as an async
   callback event.  */

static void
copy_data (void *data_)
{
  struct copy_data *data = (struct copy_data *)data_;
  GCN_DEBUG ("Async thread %d:%d: Copying %zu bytes from (%p) to (%p)\n",
	     data->aq->agent->device_id, data->aq->id, data->len, data->src,
	     data->dst);
  hsa_memory_copy_wrapper (data->dst, data->src, data->len);
  free (data);
}

/* Request an asynchronous data copy, to or from a device, on a given queue.
   The event will be registered as a callback.  */

static void
queue_push_copy (struct goacc_asyncqueue *aq, void *dst, const void *src,
		 size_t len)
{
  if (DEBUG_QUEUES)
    GCN_DEBUG ("queue_push_copy %d:%d: %zu bytes from (%p) to (%p)\n",
	       aq->agent->device_id, aq->id, len, src, dst);
  struct copy_data *data
    = (struct copy_data *)GOMP_PLUGIN_malloc (sizeof (struct copy_data));
  data->dst = dst;
  data->src = src;
  data->len = len;
  data->aq = aq;
  queue_push_callback (aq, copy_data, data);
}

/* Return true if the given queue is currently empty.  */

static int
queue_empty (struct goacc_asyncqueue *aq)
{
  pthread_mutex_lock (&aq->mutex);
  int res = aq->queue_n == 0 ? 1 : 0;
  pthread_mutex_unlock (&aq->mutex);

  return res;
}

/* Wait for a given queue to become empty.  This implements an OpenACC wait
   directive.  */

static void
wait_queue (struct goacc_asyncqueue *aq)
{
  if (DRAIN_QUEUE_SYNCHRONOUS_P)
    {
      drain_queue_synchronous (aq);
      return;
    }

  pthread_mutex_lock (&aq->mutex);

  while (aq->queue_n > 0)
    {
      if (DEBUG_THREAD_SLEEP)
	GCN_DEBUG ("waiting for thread %d:%d, putting thread to sleep\n",
		   aq->agent->device_id, aq->id);
      pthread_cond_wait (&aq->queue_cond_out, &aq->mutex);
      if (DEBUG_THREAD_SLEEP)
	GCN_DEBUG ("thread %d:%d woke up.  Rechecking\n", aq->agent->device_id,
		   aq->id);
    }

  pthread_mutex_unlock (&aq->mutex);
  GCN_DEBUG ("waiting for thread %d:%d, done\n", aq->agent->device_id, aq->id);
}

/* }}}  */
/* {{{ OpenACC support  */

/* Execute an OpenACC kernel, synchronously or asynchronously.  */

static void
gcn_exec (struct kernel_info *kernel,
	  void **devaddrs, unsigned *dims, void *targ_mem_desc, bool async,
	  struct goacc_asyncqueue *aq)
{
  if (!GOMP_OFFLOAD_can_run (kernel))
    GOMP_PLUGIN_fatal ("OpenACC host fallback unimplemented.");

  /* If we get here then this must be an OpenACC kernel.  */
  kernel->kind = KIND_OPENACC;

  struct hsa_kernel_description *hsa_kernel_desc = NULL;
  for (unsigned i = 0; i < kernel->module->image_desc->kernel_count; i++)
    {
      struct hsa_kernel_description *d
	= &kernel->module->image_desc->kernel_infos[i];
      if (d->name == kernel->name)
	{
	  hsa_kernel_desc = d;
	  break;
	}
    }

  /* We may have statically-determined dimensions in
     hsa_kernel_desc->oacc_dims[] or dimensions passed to this offload kernel
     invocation at runtime in dims[].  We allow static dimensions to take
     priority over dynamic dimensions when present (non-zero).  */
  if (hsa_kernel_desc->oacc_dims[0] > 0)
    dims[0] = hsa_kernel_desc->oacc_dims[0];
  if (hsa_kernel_desc->oacc_dims[1] > 0)
    dims[1] = hsa_kernel_desc->oacc_dims[1];
  if (hsa_kernel_desc->oacc_dims[2] > 0)
    dims[2] = hsa_kernel_desc->oacc_dims[2];

  /* Ideally, when a dimension isn't explicitly specified, we should
     tune it to run 40 (or 32?) threads per CU with no threads getting queued.
     In practice, we tune for peak performance on BabelStream, which
     for OpenACC is currently 32 threads per CU.  */
  if (dims[0] == 0 && dims[1] == 0)
    {
      /* If any of the OpenACC dimensions remain 0 then we get to pick a
	 number.  There isn't really a correct answer for this without a clue
	 about the problem size, so let's do a reasonable number of workers
	 and gangs.  */

      dims[0] = get_cu_count (kernel->agent) * 4; /* Gangs.  */
      dims[1] = 8; /* Workers.  */
    }
  else if (dims[0] == 0 && dims[1] > 0)
    {
      /* Auto-scale the number of gangs with the requested number of workers.  */
      dims[0] = get_cu_count (kernel->agent) * (32 / dims[1]);
    }
  else if (dims[0] > 0 && dims[1] == 0)
    {
      /* Auto-scale the number of workers with the requested number of gangs.  */
      dims[1] = get_cu_count (kernel->agent) * 32 / dims[0];
      if (dims[1] == 0)
	dims[1] = 1;
      if (dims[1] > 16)
	dims[1] = 16;
    }

  /* The incoming dimensions are expressed in terms of gangs, workers, and
     vectors.  The HSA dimensions are expressed in terms of "work-items",
     which means multiples of vector lanes.

     The "grid size" specifies the size of the problem space, and the
     "work-group size" specifies how much of that we want a single compute
     unit to chew on at once.

     The three dimensions do not really correspond to hardware, but the
     important thing is that the HSA runtime will launch as many
     work-groups as it takes to process the entire grid, and each
     work-group will contain as many wave-fronts as it takes to process
     the work-items in that group.

     Essentially, as long as we set the Y dimension to 64 (the number of
     vector lanes in hardware), and the Z group size to the maximum (16),
     then we will get the gangs (X) and workers (Z) launched as we expect.

     The reason for the apparent reversal of vector and worker dimension
     order is to do with the way the run-time distributes work-items across
     v1 and v2.  */
  struct GOMP_kernel_launch_attributes kla =
    {3,
     /* Grid size.  */
     {dims[0], 64, dims[1]},
     /* Work-group size.  */
     {1,       64, 16}
    };

  struct goacc_thread *thr = GOMP_PLUGIN_goacc_thread ();
  acc_prof_info *prof_info = thr->prof_info;
  acc_event_info enqueue_launch_event_info;
  acc_api_info *api_info = thr->api_info;
  bool profiling_dispatch_p = __builtin_expect (prof_info != NULL, false);
  if (profiling_dispatch_p)
    {
      prof_info->event_type = acc_ev_enqueue_launch_start;

      enqueue_launch_event_info.launch_event.event_type
	= prof_info->event_type;
      enqueue_launch_event_info.launch_event.valid_bytes
	= _ACC_LAUNCH_EVENT_INFO_VALID_BYTES;
      enqueue_launch_event_info.launch_event.parent_construct
	= acc_construct_parallel;
      enqueue_launch_event_info.launch_event.implicit = 1;
      enqueue_launch_event_info.launch_event.tool_info = NULL;
      enqueue_launch_event_info.launch_event.kernel_name
	= (char *) kernel->name;
      enqueue_launch_event_info.launch_event.num_gangs = kla.gdims[0];
      enqueue_launch_event_info.launch_event.num_workers = kla.gdims[2];
      enqueue_launch_event_info.launch_event.vector_length = kla.gdims[1];

      api_info->device_api = acc_device_api_other;

      GOMP_PLUGIN_goacc_profiling_dispatch (prof_info,
	&enqueue_launch_event_info, api_info);
    }

  if (!async)
    run_kernel (kernel, devaddrs, &kla, NULL, false);
  else
    queue_push_launch (aq, kernel, devaddrs, &kla);

  if (profiling_dispatch_p)
    {
      prof_info->event_type = acc_ev_enqueue_launch_end;
      enqueue_launch_event_info.launch_event.event_type = prof_info->event_type;
      GOMP_PLUGIN_goacc_profiling_dispatch (prof_info,
					    &enqueue_launch_event_info,
					    api_info);
    }
}

/* }}}  */
/* {{{ Generic Plugin API  */

/* Return the name of the accelerator, which is "gcn".  */

const char *
GOMP_OFFLOAD_get_name (void)
{
  return "gcn";
}

/* Return the UID; if not available return NULL.
   Returns freshly allocated memoy.  */

const char *
GOMP_OFFLOAD_get_uid (int ord)
{
  char *str;
  hsa_status_t status;
  struct agent_info *agent = get_agent_info (ord);

  /* HSA documentation states: maximally 21 characters including NUL.  */
  str = GOMP_PLUGIN_malloc (21 * sizeof (char));
  status = hsa_fns.hsa_agent_get_info_fn (agent->id, HSA_AMD_AGENT_INFO_UUID,
					  str);
  if (status != HSA_STATUS_SUCCESS)
    {
      free (str);
      return NULL;
    }
  return str;
}

/* Return the specific capabilities the HSA accelerator have.  */

unsigned int
GOMP_OFFLOAD_get_caps (void)
{
  /* FIXME: Enable shared memory for APU, but not discrete GPU.  */
  return /*GOMP_OFFLOAD_CAP_SHARED_MEM |*/ GOMP_OFFLOAD_CAP_OPENMP_400
	    | GOMP_OFFLOAD_CAP_OPENACC_200;
}

/* Identify as GCN accelerator.  */

int
GOMP_OFFLOAD_get_type (void)
{
  return OFFLOAD_TARGET_TYPE_GCN;
}

/* Return the libgomp version number we're compatible with.  There is
   no requirement for cross-version compatibility.  */

unsigned
GOMP_OFFLOAD_version (void)
{
  return GOMP_VERSION;
}

/* Return the number of GCN devices on the system.  */

int
GOMP_OFFLOAD_get_num_devices (unsigned int omp_requires_mask)
{
  if (!init_hsa_context (true))
    exit (EXIT_FAILURE);
  /* Return -1 if no omp_requires_mask cannot be fulfilled but
     devices were present.  */
  if (hsa_context.agent_count > 0
      && ((omp_requires_mask
	   & ~(GOMP_REQUIRES_UNIFIED_ADDRESS
	       | GOMP_REQUIRES_UNIFIED_SHARED_MEMORY
	       | GOMP_REQUIRES_SELF_MAPS
	       | GOMP_REQUIRES_REVERSE_OFFLOAD)) != 0))
    return -1;
  /* Check whether host page access is supported; this is per system level
     (all GPUs supported by HSA).  While intrinsically true for APUs, it
     requires XNACK support for discrete GPUs.  */
  if (hsa_context.agent_count > 0
      && (omp_requires_mask
	  & (GOMP_REQUIRES_UNIFIED_SHARED_MEMORY | GOMP_REQUIRES_SELF_MAPS)))
    {
      bool b;
      hsa_system_info_t type = HSA_AMD_SYSTEM_INFO_SVM_ACCESSIBLE_BY_DEFAULT;
      hsa_status_t status = hsa_fns.hsa_system_get_info_fn (type, &b);
      if (status != HSA_STATUS_SUCCESS)
	GOMP_PLUGIN_error ("HSA_AMD_SYSTEM_INFO_SVM_ACCESSIBLE_BY_DEFAULT "
			   "failed");
      if (!b)
	return -1;
    }

  return hsa_context.agent_count;
}

/* Initialize device (agent) number N so that it can be used for computation.
   Return TRUE on success.  */

bool
GOMP_OFFLOAD_init_device (int n)
{
  if (!init_hsa_context (false))
    return false;
  if (n >= hsa_context.agent_count)
    {
      GOMP_PLUGIN_error ("Request to initialize non-existent GCN device %i", n);
      return false;
    }
  struct agent_info *agent = &hsa_context.agents[n];

  if (agent->initialized)
    return true;

  agent->device_id = n;

  if (pthread_rwlock_init (&agent->module_rwlock, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN agent rwlock");
      return false;
    }
  if (pthread_mutex_init (&agent->prog_mutex, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN agent program mutex");
      return false;
    }
  if (pthread_mutex_init (&agent->async_queues_mutex, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN agent queue mutex");
      return false;
    }
  if (pthread_mutex_init (&agent->ephemeral_memories_write_lock, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN team arena write mutex");
      return false;
    }
  agent->async_queues = NULL;
  agent->omp_async_queue = NULL;
  agent->ephemeral_memories_list = NULL;

  uint32_t queue_size;
  hsa_status_t status;
  status = hsa_fns.hsa_agent_get_info_fn (agent->id,
					  HSA_AGENT_INFO_QUEUE_MAX_SIZE,
					  &queue_size);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error requesting maximum queue size of the GCN agent",
		      status);

  status = hsa_fns.hsa_agent_get_info_fn (agent->id, HSA_AGENT_INFO_NAME,
					  &agent->name);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error querying the name of the agent", status);

  agent->device_isa = isa_code (agent->name);
  if (agent->device_isa == EF_AMDGPU_MACH_UNSUPPORTED)
    {
      char msg[33 + 64 + 1];
      snprintf (msg, sizeof msg,
		"Unknown GCN agent architecture '%s'", agent->name);
      return hsa_error (msg, HSA_STATUS_ERROR);
    }

  status = hsa_fns.hsa_agent_get_info_fn (agent->id, HSA_AGENT_INFO_VENDOR_NAME,
					  &agent->vendor_name);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error querying the vendor name of the agent", status);

  status = hsa_fns.hsa_queue_create_fn (agent->id, queue_size,
					HSA_QUEUE_TYPE_MULTI,
					hsa_queue_callback, NULL, UINT32_MAX,
					UINT32_MAX, &agent->sync_queue);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error creating command queue", status);

  agent->kernarg_region.handle = (uint64_t) -1;
  status = hsa_fns.hsa_agent_iterate_regions_fn (agent->id,
						 get_kernarg_memory_region,
						 &agent->kernarg_region);
  if (status != HSA_STATUS_SUCCESS
      && status != HSA_STATUS_INFO_BREAK)
    hsa_error ("Scanning memory regions failed", status);
  if (agent->kernarg_region.handle == (uint64_t) -1)
    {
      GOMP_PLUGIN_error ("Could not find suitable memory region for kernel "
			 "arguments");
      return false;
    }
  GCN_DEBUG ("Selected kernel arguments memory region:\n");
  dump_hsa_region (agent->kernarg_region, NULL);

  agent->data_region.handle = (uint64_t) -1;
  status = hsa_fns.hsa_agent_iterate_regions_fn (agent->id,
						 get_data_memory_region,
						 &agent->data_region);
  if (status != HSA_STATUS_SUCCESS
      && status != HSA_STATUS_INFO_BREAK)
    hsa_error ("Scanning memory regions failed", status);
  if (agent->data_region.handle == (uint64_t) -1)
    {
      GOMP_PLUGIN_error ("Could not find suitable memory region for device "
			 "data");
      return false;
    }
  GCN_DEBUG ("Selected device data memory region:\n");
  dump_hsa_region (agent->data_region, NULL);

  GCN_DEBUG ("GCN agent %d initialized\n", n);

  agent->initialized = true;
  return true;
}

/* Load GCN object-code module described by struct gcn_image_desc in
   TARGET_DATA and return references to kernel descriptors in TARGET_TABLE.
   If there are any constructors then run them.  If not NULL, REV_FN_TABLE will
   contain the on-device addresses of the functions for reverse offload.  To be
   freed by the caller.  */

int
GOMP_OFFLOAD_load_image (int ord, unsigned version, const void *target_data,
			 struct addr_pair **target_table,
			 uint64_t **rev_fn_table,
			 uint64_t *host_ind_fn_table)
{
  if (GOMP_VERSION_DEV (version) != GOMP_VERSION_GCN)
    {
      GOMP_PLUGIN_error ("Offload data incompatible with GCN plugin"
			 " (expected %u, received %u)",
			 GOMP_VERSION_GCN, GOMP_VERSION_DEV (version));
      return -1;
    }

  struct gcn_image_desc *image_desc = (struct gcn_image_desc *) target_data;
  struct agent_info *agent;
  struct addr_pair *pair;
  struct module_info *module;
  struct kernel_info *kernel;
  int kernel_count = image_desc->kernel_count;
  unsigned ind_func_count = GOMP_VERSION_SUPPORTS_INDIRECT_FUNCS (version)
			      ? image_desc->ind_func_count : 0;
  unsigned var_count = image_desc->global_variable_count;
  /* Currently, "others" is a struct of ICVS.  */
  int other_count = 1;

  agent = get_agent_info (ord);
  if (!agent)
    return -1;

  if (pthread_rwlock_wrlock (&agent->module_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to write-lock a GCN agent rwlock");
      return -1;
    }
  if (agent->prog_finalized
      && !destroy_hsa_program (agent))
    return -1;

  GCN_DEBUG ("Encountered %d kernels in an image\n", kernel_count);
  GCN_DEBUG ("Encountered %d indirect functions in an image\n", ind_func_count);
  GCN_DEBUG ("Encountered %u global variables in an image\n", var_count);
  GCN_DEBUG ("Expect %d other variables in an image\n", other_count);
  pair = GOMP_PLUGIN_malloc ((kernel_count + var_count + other_count - 2)
			     * sizeof (struct addr_pair));
  *target_table = pair;
  module = (struct module_info *)
    GOMP_PLUGIN_malloc_cleared (sizeof (struct module_info)
				+ kernel_count * sizeof (struct kernel_info));
  module->image_desc = image_desc;
  module->kernel_count = kernel_count;
  module->heap = NULL;
  module->constructors_run_p = false;

  kernel = &module->kernels[0];

  /* Allocate memory for kernel dependencies.  */
  for (unsigned i = 0; i < kernel_count; i++)
    {
      struct hsa_kernel_description *d = &image_desc->kernel_infos[i];
      if (!init_basic_kernel_info (kernel, d, agent, module))
	return -1;
      if (strcmp (d->name, "_init_array") == 0)
	module->init_array_func = kernel;
      else if (strcmp (d->name, "_fini_array") == 0)
        module->fini_array_func = kernel;
      else
	{
	  pair->start = (uintptr_t) kernel;
	  pair->end = (uintptr_t) (kernel + 1);
	  pair++;
	}
      kernel++;
    }

  agent->module = module;
  if (pthread_rwlock_unlock (&agent->module_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to unlock a GCN agent rwlock");
      return -1;
    }

  if (!create_and_finalize_hsa_program (agent))
    return -1;

  if (var_count > 0)
    {
      hsa_status_t status;
      hsa_executable_symbol_t var_symbol;
      status = hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						     ".offload_var_table",
						     agent->id,
						     0, &var_symbol);

      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not find symbol for variable in the code object",
		   status);

      uint64_t var_table_addr;
      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(var_symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS,
	 &var_table_addr);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable from its symbol", status);

      struct {
	uint64_t addr;
	uint64_t size;
      } var_table[var_count];
      GOMP_OFFLOAD_dev2host (agent->device_id, var_table,
			     (void*)var_table_addr, sizeof (var_table));

      for (unsigned i = 0; i < var_count; i++)
	{
	  pair->start = var_table[i].addr;
	  pair->end = var_table[i].addr + var_table[i].size;
	  GCN_DEBUG ("Found variable at %p with size %lu\n",
		     (void *)var_table[i].addr, var_table[i].size);
	  pair++;
	}
    }

  if (ind_func_count > 0)
    {
      hsa_status_t status;

      /* Read indirect function table from image.  */
      hsa_executable_symbol_t ind_funcs_symbol;
      status = hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						     ".offload_ind_func_table",
						     agent->id,
						     0, &ind_funcs_symbol);

      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not find .offload_ind_func_table symbol in the "
		   "code object", status);

      uint64_t ind_funcs_table_addr;
      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(ind_funcs_symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS,
	 &ind_funcs_table_addr);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable from its symbol", status);

      uint64_t ind_funcs_table[ind_func_count];
      GOMP_OFFLOAD_dev2host (agent->device_id, ind_funcs_table,
			     (void*) ind_funcs_table_addr,
			     sizeof (ind_funcs_table));

      /* Build host->target address map for indirect functions.  */
      uint64_t ind_fn_map[ind_func_count * 2 + 1];
      for (unsigned i = 0; i < ind_func_count; i++)
	{
	  ind_fn_map[i * 2] = host_ind_fn_table[i];
	  ind_fn_map[i * 2 + 1] = ind_funcs_table[i];
	  GCN_DEBUG ("Indirect function %d: %lx->%lx\n",
		     i, host_ind_fn_table[i], ind_funcs_table[i]);
	}
      ind_fn_map[ind_func_count * 2] = 0;

      /* Write the map onto the target.  */
      void *map_target_addr
	= GOMP_OFFLOAD_alloc (agent->device_id, sizeof (ind_fn_map));
      GCN_DEBUG ("Allocated indirect map at %p\n", map_target_addr);

      GOMP_OFFLOAD_host2dev (agent->device_id, map_target_addr,
			     (void*) ind_fn_map,
			     sizeof (ind_fn_map));

      /* Write address of the map onto the target.  */
      hsa_executable_symbol_t symbol;

      status
	= hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						XSTRING (GOMP_INDIRECT_ADDR_MAP),
						agent->id, 0, &symbol);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not find GOMP_INDIRECT_ADDR_MAP in code object",
		   status);

      uint64_t varptr;
      uint32_t varsize;

      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS,
	 &varptr);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable from its symbol", status);
      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_SIZE,
	&varsize);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable size from its symbol",
		   status);

      GCN_DEBUG ("Found GOMP_INDIRECT_ADDR_MAP at %lx with size %d\n",
		 varptr, varsize);

      GOMP_OFFLOAD_host2dev (agent->device_id, (void *) varptr,
			     &map_target_addr,
			     sizeof (map_target_addr));
    }

  GCN_DEBUG ("Looking for variable %s\n", XSTRING (GOMP_ADDITIONAL_ICVS));

  hsa_status_t status;
  hsa_executable_symbol_t var_symbol;
  status = hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						 XSTRING (GOMP_ADDITIONAL_ICVS),
						 agent->id, 0, &var_symbol);
  if (status == HSA_STATUS_SUCCESS)
    {
      uint64_t varptr;
      uint32_t varsize;

      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(var_symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS,
	 &varptr);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable from its symbol", status);
      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(var_symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_SIZE,
	 &varsize);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable size from its symbol",
		   status);

      pair->start = varptr;
      pair->end = varptr + varsize;
    }
  else
    {
      /* The variable was not in this image.  */
      GCN_DEBUG ("Variable not found in image: %s\n",
		 XSTRING (GOMP_ADDITIONAL_ICVS));
      pair->start = pair->end = 0;
    }

  /* Ensure that constructors are run first.  */
  struct GOMP_kernel_launch_attributes kla =
    { 3,
      /* Grid size.  */
      { 1, 64, 1 },
      /* Work-group size.  */
      { 1, 64, 1 }
    };

  if (module->init_array_func)
    {
      init_kernel (module->init_array_func);
      run_kernel (module->init_array_func, NULL, &kla, NULL, false);
    }
  module->constructors_run_p = true;

  /* Don't report kernels that libgomp need not know about.  */
  if (module->init_array_func)
    kernel_count--;
  if (module->fini_array_func)
    kernel_count--;

  if (rev_fn_table != NULL && kernel_count == 0)
    *rev_fn_table = NULL;
  else if (rev_fn_table != NULL)
    {
      hsa_status_t status;
      hsa_executable_symbol_t var_symbol;
      status = hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						     ".offload_func_table",
						     agent->id, 0, &var_symbol);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not find symbol for variable in the code object",
		   status);
      uint64_t fn_table_addr;
      status = hsa_fns.hsa_executable_symbol_get_info_fn
	(var_symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS,
	 &fn_table_addr);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not extract a variable from its symbol", status);
      *rev_fn_table = GOMP_PLUGIN_malloc (kernel_count * sizeof (uint64_t));
      GOMP_OFFLOAD_dev2host (agent->device_id, *rev_fn_table,
			     (void*) fn_table_addr,
			     kernel_count * sizeof (uint64_t));
    }

  return kernel_count + var_count + other_count;
}

/* Unload GCN object-code module described by struct gcn_image_desc in
   TARGET_DATA from agent number N.  Return TRUE on success.  */

bool
GOMP_OFFLOAD_unload_image (int n, unsigned version, const void *target_data)
{
  if (GOMP_VERSION_DEV (version) != GOMP_VERSION_GCN)
    {
      GOMP_PLUGIN_error ("Offload data incompatible with GCN plugin"
			 " (expected %u, received %u)",
			 GOMP_VERSION_GCN, GOMP_VERSION_DEV (version));
      return false;
    }

  struct agent_info *agent;
  agent = get_agent_info (n);
  if (!agent)
    return false;

  if (pthread_rwlock_wrlock (&agent->module_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to write-lock a GCN agent rwlock");
      return false;
    }

  if (!agent->module || agent->module->image_desc != target_data)
    {
      GOMP_PLUGIN_error ("Attempt to unload an image that has never been "
			 "loaded before");
      return false;
    }

  if (!destroy_module (agent->module, true))
    return false;
  free (agent->module);
  agent->module = NULL;
  if (!destroy_hsa_program (agent))
    return false;
  if (pthread_rwlock_unlock (&agent->module_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to unlock a GCN agent rwlock");
      return false;
    }
  return true;
}

/* Deinitialize all information and status associated with agent number N.  We
   do not attempt any synchronization, assuming the user and libgomp will not
   attempt deinitialization of a device that is in any way being used at the
   same time.  Return TRUE on success.  */

bool
GOMP_OFFLOAD_fini_device (int n)
{
  struct agent_info *agent = get_agent_info (n);
  if (!agent)
    return false;

  if (!agent->initialized)
    return true;

  if (agent->omp_async_queue)
    {
      GOMP_OFFLOAD_openacc_async_destruct (agent->omp_async_queue);
      agent->omp_async_queue = NULL;
    }

  if (agent->module)
    {
      if (!destroy_module (agent->module, false))
	return false;
      free (agent->module);
      agent->module = NULL;
    }

  if (!destroy_ephemeral_memories (agent))
    return false;

  if (!destroy_hsa_program (agent))
    return false;

  hsa_status_t status = hsa_fns.hsa_queue_destroy_fn (agent->sync_queue);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error destroying command queue", status);

  if (pthread_mutex_destroy (&agent->prog_mutex))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN agent program mutex");
      return false;
    }
  if (pthread_rwlock_destroy (&agent->module_rwlock))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN agent rwlock");
      return false;
    }

  if (pthread_mutex_destroy (&agent->async_queues_mutex))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN agent queue mutex");
      return false;
    }
  if (pthread_mutex_destroy (&agent->ephemeral_memories_write_lock))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN memory mutex");
      return false;
    }
  agent->initialized = false;
  return true;
}

/* Return true if the HSA runtime can run function FN_PTR.  */

bool
GOMP_OFFLOAD_can_run (void *fn_ptr)
{
  struct kernel_info *kernel = (struct kernel_info *) fn_ptr;

  init_kernel (kernel);
  if (kernel->initialization_failed)
    GOMP_PLUGIN_fatal ("kernel initialization failed");

  return true;
}

/* Allocate memory on device N.  */

void *
GOMP_OFFLOAD_alloc (int n, size_t size)
{
  struct agent_info *agent = get_agent_info (n);
  return alloc_by_agent (agent, size);
}

/* Free memory from device N.  */

bool
GOMP_OFFLOAD_free (int device, void *ptr)
{
  GCN_DEBUG ("Freeing memory on device %d\n", device);

  hsa_status_t status = hsa_fns.hsa_memory_free_fn (ptr);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Could not free device memory", status);
      return false;
    }

  struct goacc_thread *thr = GOMP_PLUGIN_goacc_thread ();
  bool profiling_dispatch_p
    = __builtin_expect (thr != NULL && thr->prof_info != NULL, false);
  if (profiling_dispatch_p)
    {
      acc_prof_info *prof_info = thr->prof_info;
      acc_event_info data_event_info;
      acc_api_info *api_info = thr->api_info;

      prof_info->event_type = acc_ev_free;

      data_event_info.data_event.event_type = prof_info->event_type;
      data_event_info.data_event.valid_bytes
	= _ACC_DATA_EVENT_INFO_VALID_BYTES;
      data_event_info.data_event.parent_construct
	= acc_construct_parallel;
      data_event_info.data_event.implicit = 1;
      data_event_info.data_event.tool_info = NULL;
      data_event_info.data_event.var_name = NULL;
      data_event_info.data_event.bytes = 0;
      data_event_info.data_event.host_ptr = NULL;
      data_event_info.data_event.device_ptr = (void *) ptr;

      api_info->device_api = acc_device_api_other;

      GOMP_PLUGIN_goacc_profiling_dispatch (prof_info, &data_event_info,
					    api_info);
    }

  return true;
}

/* Copy data from DEVICE to host.  */

bool
GOMP_OFFLOAD_dev2host (int device, void *dst, const void *src, size_t n)
{
  GCN_DEBUG ("Copying %zu bytes from device %d (%p) to host (%p)\n", n, device,
	     src, dst);
  hsa_status_t status = hsa_fns.hsa_memory_copy_fn (dst, src, n);
  if (status != HSA_STATUS_SUCCESS)
    GOMP_PLUGIN_error ("memory copy failed");
  return true;
}

/* Copy data from host to DEVICE.  */

bool
GOMP_OFFLOAD_host2dev (int device, void *dst, const void *src, size_t n)
{
  GCN_DEBUG ("Copying %zu bytes from host (%p) to device %d (%p)\n", n, src,
	     device, dst);
  hsa_memory_copy_wrapper (dst, src, n);
  return true;
}

/* Copy data within DEVICE.  Do the copy asynchronously, if appropriate.  */

bool
GOMP_OFFLOAD_dev2dev (int device, void *dst, const void *src, size_t n)
{
  struct gcn_thread *thread_data = gcn_thread ();

  if (thread_data && !async_synchronous_p (thread_data->async))
    {
      struct agent_info *agent = get_agent_info (device);
      maybe_init_omp_async (agent);
      queue_push_copy (agent->omp_async_queue, dst, src, n);
      return true;
    }

  GCN_DEBUG ("Copying %zu bytes from device %d (%p) to device %d (%p)\n", n,
	     device, src, device, dst);
  hsa_status_t status = hsa_fns.hsa_memory_copy_fn (dst, src, n);
  if (status != HSA_STATUS_SUCCESS)
    GOMP_PLUGIN_error ("memory copy failed");
  return true;
}

/* Here <quantity>_size refers to <quantity> multiplied by size -- i.e.
   measured in bytes.  So we have:

   dim1_size: number of bytes to copy on innermost dimension ("row")
   dim0_len: number of rows to copy
   dst: base pointer for destination of copy
   dst_offset1_size: innermost row offset (for dest), in bytes
   dst_offset0_len: offset, number of rows (for dest)
   dst_dim1_size: whole-array dest row length, in bytes (pitch)
   src: base pointer for source of copy
   src_offset1_size: innermost row offset (for source), in bytes
   src_offset0_len: offset, number of rows (for source)
   src_dim1_size: whole-array source row length, in bytes (pitch)
*/

int
GOMP_OFFLOAD_memcpy2d (int dst_ord, int src_ord, size_t dim1_size,
		       size_t dim0_len, void *dst, size_t dst_offset1_size,
		       size_t dst_offset0_len, size_t dst_dim1_size,
		       const void *src, size_t src_offset1_size,
		       size_t src_offset0_len, size_t src_dim1_size)
{
  if (!hsa_fns.hsa_amd_memory_lock_fn
      || !hsa_fns.hsa_amd_memory_unlock_fn
      || !hsa_fns.hsa_amd_memory_async_copy_rect_fn)
    return -1;

  /* GCN hardware requires 4-byte alignment for base addresses & pitches.  Bail
     out quietly if we have anything oddly-aligned rather than letting the
     driver raise an error.  */
  if ((((uintptr_t) dst) & 3) != 0 || (((uintptr_t) src) & 3) != 0)
    return -1;

  if ((dst_dim1_size & 3) != 0 || (src_dim1_size & 3) != 0)
    return -1;

  /* Only handle host to device or device to host transfers here.  */
  if ((dst_ord == -1 && src_ord == -1)
      || (dst_ord != -1 && src_ord != -1))
    return -1;

  hsa_amd_copy_direction_t dir
    = (src_ord == -1) ? hsaHostToDevice : hsaDeviceToHost;
  hsa_agent_t copy_agent;

  /* We need to pin (lock) host memory before we start the transfer.  Try to
     lock the minimum size necessary, i.e. using partial first/last rows of the
     whole array.  Something like this:

	 rows -->
	 ..............
     c | ..#######+++++ <- first row apart from {src,dst}_offset1_size
     o | ++#######+++++ <- whole row
     l | ++#######+++++ <-     "
     s v ++#######..... <- last row apart from trailing remainder
	 ..............

     We could split very large transfers into several rectangular copies, but
     that is unimplemented for now.  */

  size_t bounded_size_host, first_elem_offset_host;
  void *host_ptr;
  if (dir == hsaHostToDevice)
    {
      bounded_size_host = src_dim1_size * (dim0_len - 1) + dim1_size;
      first_elem_offset_host = src_offset0_len * src_dim1_size
			       + src_offset1_size;
      host_ptr = (void *) src;
      struct agent_info *agent = get_agent_info (dst_ord);
      copy_agent = agent->id;
    }
  else
    {
      bounded_size_host = dst_dim1_size * (dim0_len - 1) + dim1_size;
      first_elem_offset_host = dst_offset0_len * dst_dim1_size
			       + dst_offset1_size;
      host_ptr = dst;
      struct agent_info *agent = get_agent_info (src_ord);
      copy_agent = agent->id;
    }

  void *agent_ptr;

  hsa_status_t status
    = hsa_fns.hsa_amd_memory_lock_fn (host_ptr + first_elem_offset_host,
				      bounded_size_host, NULL, 0, &agent_ptr);
  /* We can't lock the host memory: don't give up though, we might still be
     able to use the slow path in our caller.  So, don't make this an
     error.  */
  if (status != HSA_STATUS_SUCCESS)
    return -1;

  hsa_pitched_ptr_t dstpp, srcpp;
  hsa_dim3_t dst_offsets, src_offsets, ranges;

  int retval = 1;

  hsa_signal_t completion_signal;
  status = hsa_fns.hsa_signal_create_fn (1, 0, NULL, &completion_signal);
  if (status != HSA_STATUS_SUCCESS)
    {
      retval = -1;
      goto unlock;
    }

  if (dir == hsaHostToDevice)
    {
      srcpp.base = agent_ptr - first_elem_offset_host;
      dstpp.base = dst;
    }
  else
    {
      srcpp.base = (void *) src;
      dstpp.base = agent_ptr - first_elem_offset_host;
    }

  srcpp.pitch = src_dim1_size;
  srcpp.slice = 0;

  src_offsets.x = src_offset1_size;
  src_offsets.y = src_offset0_len;
  src_offsets.z = 0;

  dstpp.pitch = dst_dim1_size;
  dstpp.slice = 0;

  dst_offsets.x = dst_offset1_size;
  dst_offsets.y = dst_offset0_len;
  dst_offsets.z = 0;

  ranges.x = dim1_size;
  ranges.y = dim0_len;
  ranges.z = 1;

  status
    = hsa_fns.hsa_amd_memory_async_copy_rect_fn (&dstpp, &dst_offsets, &srcpp,
						 &src_offsets, &ranges,
						 copy_agent, dir, 0, NULL,
						 completion_signal);
  /* If the rectangular copy fails, we might still be able to use the slow
     path.  We need to unlock the host memory though, so don't return
     immediately.  */
  if (status != HSA_STATUS_SUCCESS)
    retval = -1;
  else
    hsa_fns.hsa_signal_wait_acquire_fn (completion_signal,
					HSA_SIGNAL_CONDITION_LT, 1, UINT64_MAX,
					HSA_WAIT_STATE_ACTIVE);

  hsa_fns.hsa_signal_destroy_fn (completion_signal);

unlock:
  status = hsa_fns.hsa_amd_memory_unlock_fn (host_ptr + first_elem_offset_host);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not unlock host memory", status);

  return retval;
}

/* As above, <quantity>_size refers to <quantity> multiplied by size -- i.e.
   measured in bytes.  So we have:

   dim2_size: number of bytes to copy on innermost dimension ("row")
   dim1_len: number of rows per slice to copy
   dim0_len: number of slices to copy
   dst: base pointer for destination of copy
   dst_offset2_size: innermost row offset (for dest), in bytes
   dst_offset1_len: offset, number of rows (for dest)
   dst_offset0_len: offset, number of slices (for dest)
   dst_dim2_size: whole-array dest row length, in bytes (pitch)
   dst_dim1_len: whole-array number of rows in slice (for dest)
   src: base pointer for source of copy
   src_offset2_size: innermost row offset (for source), in bytes
   src_offset1_len: offset, number of rows (for source)
   src_offset0_len: offset, number of slices (for source)
   src_dim2_size: whole-array source row length, in bytes (pitch)
   src_dim1_len: whole-array number of rows in slice (for source)
*/

int
GOMP_OFFLOAD_memcpy3d (int dst_ord, int src_ord, size_t dim2_size,
		       size_t dim1_len, size_t dim0_len, void *dst,
		       size_t dst_offset2_size, size_t dst_offset1_len,
		       size_t dst_offset0_len, size_t dst_dim2_size,
		       size_t dst_dim1_len, const void *src,
		       size_t src_offset2_size, size_t src_offset1_len,
		       size_t src_offset0_len, size_t src_dim2_size,
		       size_t src_dim1_len)
{
  if (!hsa_fns.hsa_amd_memory_lock_fn
      || !hsa_fns.hsa_amd_memory_unlock_fn
      || !hsa_fns.hsa_amd_memory_async_copy_rect_fn)
    return -1;

  /* GCN hardware requires 4-byte alignment for base addresses & pitches.  Bail
     out quietly if we have anything oddly-aligned rather than letting the
     driver raise an error.  */
  if ((((uintptr_t) dst) & 3) != 0 || (((uintptr_t) src) & 3) != 0)
    return -1;

  if ((dst_dim2_size & 3) != 0 || (src_dim2_size & 3) != 0)
    return -1;

  /* Only handle host to device or device to host transfers here.  */
  if ((dst_ord == -1 && src_ord == -1)
      || (dst_ord != -1 && src_ord != -1))
    return -1;

  hsa_amd_copy_direction_t dir
    = (src_ord == -1) ? hsaHostToDevice : hsaDeviceToHost;
  hsa_agent_t copy_agent;

  /* We need to pin (lock) host memory before we start the transfer.  Try to
     lock the minimum size necessary, i.e. using partial first/last slices of
     the whole 3D array.  Something like this:

	 slice 0:          slice 1:	     slice 2:
	     __________        __________        __________
       ^    /+++++++++/    :  /+++++++++/    :	/         /
    column /+++##++++/|    | /+++##++++/|    | /+++##    /  # = subarray
     /	  /   ##++++/ |    |/+++##++++/ |    |/+++##++++/   + = area to pin
	 /_________/  :    /_________/  :    /_________/
	row --->

     We could split very large transfers into several rectangular copies, but
     that is unimplemented for now.  */

  size_t bounded_size_host, first_elem_offset_host;
  void *host_ptr;
  if (dir == hsaHostToDevice)
    {
      size_t slice_bytes = src_dim2_size * src_dim1_len;
      bounded_size_host = slice_bytes * (dim0_len - 1)
			  + src_dim2_size * (dim1_len - 1)
			  + dim2_size;
      first_elem_offset_host = src_offset0_len * slice_bytes
			       + src_offset1_len * src_dim2_size
			       + src_offset2_size;
      host_ptr = (void *) src;
      struct agent_info *agent = get_agent_info (dst_ord);
      copy_agent = agent->id;
    }
  else
    {
      size_t slice_bytes = dst_dim2_size * dst_dim1_len;
      bounded_size_host = slice_bytes * (dim0_len - 1)
			  + dst_dim2_size * (dim1_len - 1)
			  + dim2_size;
      first_elem_offset_host = dst_offset0_len * slice_bytes
			       + dst_offset1_len * dst_dim2_size
			       + dst_offset2_size;
      host_ptr = dst;
      struct agent_info *agent = get_agent_info (src_ord);
      copy_agent = agent->id;
    }

  void *agent_ptr;

  hsa_status_t status
    = hsa_fns.hsa_amd_memory_lock_fn (host_ptr + first_elem_offset_host,
				      bounded_size_host, NULL, 0, &agent_ptr);
  /* We can't lock the host memory: don't give up though, we might still be
     able to use the slow path in our caller (maybe even with iterated memcpy2d
     calls).  So, don't make this an error.  */
  if (status != HSA_STATUS_SUCCESS)
    return -1;

  hsa_pitched_ptr_t dstpp, srcpp;
  hsa_dim3_t dst_offsets, src_offsets, ranges;

  int retval = 1;

  hsa_signal_t completion_signal;
  status = hsa_fns.hsa_signal_create_fn (1, 0, NULL, &completion_signal);
  if (status != HSA_STATUS_SUCCESS)
    {
      retval = -1;
      goto unlock;
    }

  if (dir == hsaHostToDevice)
    {
      srcpp.base = agent_ptr - first_elem_offset_host;
      dstpp.base = dst;
    }
  else
    {
      srcpp.base = (void *) src;
      dstpp.base = agent_ptr - first_elem_offset_host;
    }

  /* Pitch is measured in bytes.  */
  srcpp.pitch = src_dim2_size;
  /* Slice is also measured in bytes (i.e. total per-slice).  */
  srcpp.slice = src_dim2_size * src_dim1_len;

  src_offsets.x = src_offset2_size;
  src_offsets.y = src_offset1_len;
  src_offsets.z = src_offset0_len;

  /* As above.  */
  dstpp.pitch = dst_dim2_size;
  dstpp.slice = dst_dim2_size * dst_dim1_len;

  dst_offsets.x = dst_offset2_size;
  dst_offsets.y = dst_offset1_len;
  dst_offsets.z = dst_offset0_len;

  ranges.x = dim2_size;
  ranges.y = dim1_len;
  ranges.z = dim0_len;

  status
    = hsa_fns.hsa_amd_memory_async_copy_rect_fn (&dstpp, &dst_offsets, &srcpp,
						 &src_offsets, &ranges,
						 copy_agent, dir, 0, NULL,
						 completion_signal);
  /* If the rectangular copy fails, we might still be able to use the slow
     path.  We need to unlock the host memory though, so don't return
     immediately.  */
  if (status != HSA_STATUS_SUCCESS)
    retval = -1;
  else
    {
      hsa_signal_value_t sv
	= hsa_fns.hsa_signal_wait_acquire_fn (completion_signal,
					      HSA_SIGNAL_CONDITION_LT, 1,
					      UINT64_MAX,
					      HSA_WAIT_STATE_ACTIVE);
      if (sv < 0)
	{
	  GCN_WARNING ("async copy rect failure");
	  retval = -1;
	}
    }

  hsa_fns.hsa_signal_destroy_fn (completion_signal);

unlock:
  status = hsa_fns.hsa_amd_memory_unlock_fn (host_ptr + first_elem_offset_host);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not unlock host memory", status);

  return retval;
}

/* }}}  */
/* {{{ OpenMP Plugin API  */

/* Run a synchronous OpenMP kernel on DEVICE and pass it an array of pointers
   in VARS as a parameter.  The kernel is identified by FN_PTR which must point
   to a kernel_info structure, and must have previously been loaded to the
   specified device.  */

void
GOMP_OFFLOAD_run (int device, void *fn_ptr, void *vars, void **args)
{
  struct agent_info *agent = get_agent_info (device);
  struct kernel_info *kernel = (struct kernel_info *) fn_ptr;
  struct GOMP_kernel_launch_attributes def;
  struct GOMP_kernel_launch_attributes *kla;
  assert (agent == kernel->agent);

  /* If we get here then the kernel must be OpenMP.  */
  kernel->kind = KIND_OPENMP;

  if (!parse_target_attributes (args, &def, &kla, agent))
    {
      GCN_WARNING ("Will not run GCN kernel because the grid size is zero\n");
      return;
    }
  run_kernel (kernel, vars, kla, NULL, false);
}

/* Run an asynchronous OpenMP kernel on DEVICE.  This is similar to
   GOMP_OFFLOAD_run except that the launch is queued and there is a call to
   GOMP_PLUGIN_target_task_completion when it has finished.  */

void
GOMP_OFFLOAD_async_run (int device, void *tgt_fn, void *tgt_vars,
			void **args, void *async_data)
{
  GCN_DEBUG ("GOMP_OFFLOAD_async_run invoked\n");
  struct agent_info *agent = get_agent_info (device);
  struct kernel_info *kernel = (struct kernel_info *) tgt_fn;
  struct GOMP_kernel_launch_attributes def;
  struct GOMP_kernel_launch_attributes *kla;
  assert (agent == kernel->agent);

  /* If we get here then the kernel must be OpenMP.  */
  kernel->kind = KIND_OPENMP;

  if (!parse_target_attributes (args, &def, &kla, agent))
    {
      GCN_WARNING ("Will not run GCN kernel because the grid size is zero\n");
      return;
    }

  maybe_init_omp_async (agent);
  queue_push_launch (agent->omp_async_queue, kernel, tgt_vars, kla);
  queue_push_callback (agent->omp_async_queue,
		       GOMP_PLUGIN_target_task_completion, async_data);
}

/* }}} */
/* {{{ OpenACC Plugin API  */

/* Run a synchronous OpenACC kernel.  The device number is inferred from the
   already-loaded KERNEL.  */

void
GOMP_OFFLOAD_openacc_exec (void (*fn_ptr) (void *),
			   size_t mapnum __attribute__((unused)),
			   void **hostaddrs __attribute__((unused)),
			   void **devaddrs, unsigned *dims,
			   void *targ_mem_desc)
{
  struct kernel_info *kernel = (struct kernel_info *) fn_ptr;

  gcn_exec (kernel, devaddrs, dims, targ_mem_desc, false, NULL);
}

/* Run an asynchronous OpenACC kernel on the specified queue.  */

void
GOMP_OFFLOAD_openacc_async_exec (void (*fn_ptr) (void *),
				 size_t mapnum __attribute__((unused)),
				 void **hostaddrs __attribute__((unused)),
				 void **devaddrs,
				 unsigned *dims, void *targ_mem_desc,
				 struct goacc_asyncqueue *aq)
{
  struct kernel_info *kernel = (struct kernel_info *) fn_ptr;

  gcn_exec (kernel, devaddrs, dims, targ_mem_desc, true, aq);
}

/* Create a new asynchronous thread and queue for running future kernels.  */

struct goacc_asyncqueue *
GOMP_OFFLOAD_openacc_async_construct (int device)
{
  struct agent_info *agent = get_agent_info (device);

  pthread_mutex_lock (&agent->async_queues_mutex);

  struct goacc_asyncqueue *aq = GOMP_PLUGIN_malloc (sizeof (*aq));
  aq->agent = get_agent_info (device);
  aq->prev = NULL;
  aq->next = agent->async_queues;
  if (aq->next)
    {
      aq->next->prev = aq;
      aq->id = aq->next->id + 1;
    }
  else
    aq->id = 1;
  agent->async_queues = aq;

  aq->queue_first = 0;
  aq->queue_n = 0;
  aq->drain_queue_stop = 0;

  if (pthread_mutex_init (&aq->mutex, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN agent queue mutex");
      return false;
    }
  if (pthread_cond_init (&aq->queue_cond_in, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN agent queue cond");
      return false;
    }
  if (pthread_cond_init (&aq->queue_cond_out, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize a GCN agent queue cond");
      return false;
    }

  hsa_status_t status = hsa_fns.hsa_queue_create_fn (agent->id,
						     ASYNC_QUEUE_SIZE,
						     HSA_QUEUE_TYPE_MULTI,
						     hsa_queue_callback, NULL,
						     UINT32_MAX, UINT32_MAX,
						     &aq->hsa_queue);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Error creating command queue", status);

  int err = pthread_create (&aq->thread_drain_queue, NULL, &drain_queue, aq);
  if (err != 0)
    GOMP_PLUGIN_fatal ("GCN asynchronous thread creation failed: %s",
		       strerror (err));
  GCN_DEBUG ("Async thread %d:%d: created\n", aq->agent->device_id,
	     aq->id);

  pthread_mutex_unlock (&agent->async_queues_mutex);

  return aq;
}

/* Destroy an existing asynchronous thread and queue.  Waits for any
   currently-running task to complete, but cancels any queued tasks.  */

bool
GOMP_OFFLOAD_openacc_async_destruct (struct goacc_asyncqueue *aq)
{
  struct agent_info *agent = aq->agent;

  finalize_async_thread (aq);

  pthread_mutex_lock (&agent->async_queues_mutex);

  int err;
  if ((err = pthread_mutex_destroy (&aq->mutex)))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN async queue mutex: %d", err);
      goto fail;
    }
  if (pthread_cond_destroy (&aq->queue_cond_in))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN async queue cond");
      goto fail;
    }
  if (pthread_cond_destroy (&aq->queue_cond_out))
    {
      GOMP_PLUGIN_error ("Failed to destroy a GCN async queue cond");
      goto fail;
    }
  hsa_status_t status = hsa_fns.hsa_queue_destroy_fn (aq->hsa_queue);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_error ("Error destroying command queue", status);
      goto fail;
    }

  if (aq->prev)
    aq->prev->next = aq->next;
  if (aq->next)
    aq->next->prev = aq->prev;
  if (agent->async_queues == aq)
    agent->async_queues = aq->next;

  GCN_DEBUG ("Async thread %d:%d: destroyed\n", agent->device_id, aq->id);

  free (aq);
  pthread_mutex_unlock (&agent->async_queues_mutex);
  return true;

fail:
  pthread_mutex_unlock (&agent->async_queues_mutex);
  return false;
}

/* Return true if the specified async queue is currently empty.  */

int
GOMP_OFFLOAD_openacc_async_test (struct goacc_asyncqueue *aq)
{
  return queue_empty (aq);
}

/* Block until the specified queue has executed all its tasks and the
   queue is empty.  */

bool
GOMP_OFFLOAD_openacc_async_synchronize (struct goacc_asyncqueue *aq)
{
  wait_queue (aq);
  return true;
}

/* Add a serialization point across two async queues. Any new tasks added to
   AQ2, after this call, will not run until all tasks on AQ1, at the time
   of this call, have completed.  */

bool
GOMP_OFFLOAD_openacc_async_serialize (struct goacc_asyncqueue *aq1,
				      struct goacc_asyncqueue *aq2)
{
  /* For serialize, stream aq2 waits for aq1 to complete work that has been
     scheduled to run on it up to this point.  */
  if (aq1 != aq2)
    {
      struct placeholder *placeholderp = queue_push_placeholder (aq1);
      queue_push_asyncwait (aq2, placeholderp);
    }
  return true;
}

/* Add an opaque callback to the given async queue.  */

void
GOMP_OFFLOAD_openacc_async_queue_callback (struct goacc_asyncqueue *aq,
					   void (*fn) (void *), void *data)
{
  queue_push_callback (aq, fn, data);
}

/* Queue up an asynchronous data copy from host to DEVICE.  */

bool
GOMP_OFFLOAD_openacc_async_host2dev (int device, void *dst, const void *src,
				     size_t n, struct goacc_asyncqueue *aq)
{
  struct agent_info *agent = get_agent_info (device);
  assert (agent == aq->agent);
  queue_push_copy (aq, dst, src, n);
  return true;
}

/* Queue up an asynchronous data copy from DEVICE to host.  */

bool
GOMP_OFFLOAD_openacc_async_dev2host (int device, void *dst, const void *src,
				     size_t n, struct goacc_asyncqueue *aq)
{
  struct agent_info *agent = get_agent_info (device);
  assert (agent == aq->agent);
  queue_push_copy (aq, dst, src, n);
  return true;
}

union goacc_property_value
GOMP_OFFLOAD_openacc_get_property (int device, enum goacc_property prop)
{
  struct agent_info *agent = get_agent_info (device);

  union goacc_property_value propval = { .val = 0 };

  switch (prop)
    {
    case GOACC_PROPERTY_FREE_MEMORY:
      /* Not supported. */
      break;
    case GOACC_PROPERTY_MEMORY:
      {
	size_t size;
	hsa_region_t region = agent->data_region;
	hsa_status_t status =
	  hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_SIZE, &size);
	if (status == HSA_STATUS_SUCCESS)
	  propval.val = size;
	break;
      }
    case GOACC_PROPERTY_NAME:
      propval.ptr = agent->name;
      break;
    case GOACC_PROPERTY_VENDOR:
      propval.ptr = agent->vendor_name;
      break;
    case GOACC_PROPERTY_DRIVER:
      propval.ptr = hsa_context.driver_version_s;
      break;
    }

  return propval;
}

/* Set up plugin-specific thread-local-data (host-side).  */

void *
GOMP_OFFLOAD_openacc_create_thread_data (int ord __attribute__((unused)))
{
  struct gcn_thread *thread_data
    = GOMP_PLUGIN_malloc (sizeof (struct gcn_thread));

  thread_data->async = GOMP_ASYNC_SYNC;

  return (void *) thread_data;
}

/* Clean up plugin-specific thread-local-data.  */

void
GOMP_OFFLOAD_openacc_destroy_thread_data (void *data)
{
  free (data);
}

/* }}} */
