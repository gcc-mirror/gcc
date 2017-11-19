/* Plugin for HSAIL execution.

   Copyright (C) 2013-2017 Free Software Foundation, Inc.

   Contributed by Martin Jambor <mjambor@suse.cz> and
   Martin Liska <mliska@suse.cz>.

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

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <inttypes.h>
#include <stdbool.h>
#include <hsa.h>
#include <plugin/hsa_ext_finalize.h>
#include <dlfcn.h>
#include "libgomp-plugin.h"
#include "gomp-constants.h"
#include "secure_getenv.h"

/* As an HSA runtime is dlopened, following structure defines function
   pointers utilized by the HSA plug-in.  */

struct hsa_runtime_fn_info
{
  /* HSA runtime.  */
  hsa_status_t (*hsa_status_string_fn) (hsa_status_t status,
					const char **status_string);
  hsa_status_t (*hsa_agent_get_info_fn) (hsa_agent_t agent,
					 hsa_agent_info_t attribute,
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
  hsa_status_t (*hsa_memory_free_fn) (void *ptr);
  hsa_status_t (*hsa_signal_destroy_fn) (hsa_signal_t signal);
  hsa_status_t (*hsa_executable_get_symbol_fn)
    (hsa_executable_t executable, const char *module_name,
     const char *symbol_name, hsa_agent_t agent, int32_t call_convention,
     hsa_executable_symbol_t *symbol);
  hsa_status_t (*hsa_executable_symbol_get_info_fn)
    (hsa_executable_symbol_t executable_symbol,
     hsa_executable_symbol_info_t attribute, void *value);
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

  /* HSA finalizer.  */
  hsa_status_t (*hsa_ext_program_add_module_fn) (hsa_ext_program_t program,
						 hsa_ext_module_t module);
  hsa_status_t (*hsa_ext_program_create_fn)
    (hsa_machine_model_t machine_model, hsa_profile_t profile,
     hsa_default_float_rounding_mode_t default_float_rounding_mode,
     const char *options, hsa_ext_program_t *program);
  hsa_status_t (*hsa_ext_program_destroy_fn) (hsa_ext_program_t program);
  hsa_status_t (*hsa_ext_program_finalize_fn)
    (hsa_ext_program_t program,hsa_isa_t isa,
     int32_t call_convention, hsa_ext_control_directives_t control_directives,
     const char *options, hsa_code_object_type_t code_object_type,
     hsa_code_object_t *code_object);
};

/* HSA runtime functions that are initialized in init_hsa_context.  */

static struct hsa_runtime_fn_info hsa_fns;

/* Keep the following GOMP prefixed structures in sync with respective parts of
   the compiler.  */

/* Structure describing the run-time and grid properties of an HSA kernel
   lauch.  */

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

struct GOMP_hsa_kernel_dispatch
{
  /* Pointer to a command queue associated with a kernel dispatch agent.  */
  void *queue;
  /* Pointer to reserved memory for OMP data struct copying.  */
  void *omp_data_memory;
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
  /* Number of children kernel dispatches.  */
  uint64_t kernel_dispatch_count;
  /* Debug purpose argument.  */
  uint64_t debug;
  /* Levels-var ICV.  */
  uint64_t omp_level;
  /* Kernel dispatch structures created for children kernel dispatches.  */
  struct GOMP_hsa_kernel_dispatch **children_dispatches;
  /* Number of threads.  */
  uint32_t omp_num_threads;
};

/* Part of the libgomp plugin interface.  Return the name of the accelerator,
   which is "hsa".  */

const char *
GOMP_OFFLOAD_get_name (void)
{
  return "hsa";
}

/* Part of the libgomp plugin interface.  Return the specific capabilities the
   HSA accelerator have.  */

unsigned int
GOMP_OFFLOAD_get_caps (void)
{
  return GOMP_OFFLOAD_CAP_SHARED_MEM | GOMP_OFFLOAD_CAP_OPENMP_400;
}

/* Part of the libgomp plugin interface.  Identify as HSA accelerator.  */

int
GOMP_OFFLOAD_get_type (void)
{
  return OFFLOAD_TARGET_TYPE_HSA;
}

/* Return the libgomp version number we're compatible with.  There is
   no requirement for cross-version compatibility.  */

unsigned
GOMP_OFFLOAD_version (void)
{
  return GOMP_VERSION;
}

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

/* Initialize debug and suppress_host_fallback according to the environment.  */

static void
init_enviroment_variables (void)
{
  if (secure_getenv ("HSA_DEBUG"))
    debug = true;
  else
    debug = false;

  if (secure_getenv ("HSA_SUPPRESS_HOST_FALLBACK"))
    suppress_host_fallback = true;
  else
    suppress_host_fallback = false;

  hsa_runtime_lib = secure_getenv ("HSA_RUNTIME_LIB");
  if (hsa_runtime_lib == NULL)
    hsa_runtime_lib = HSA_RUNTIME_LIB "libhsa-runtime64.so";

  support_cpu_devices = secure_getenv ("HSA_SUPPORT_CPU_DEVICES");
}

/* Print a logging message with PREFIX to stderr if HSA_DEBUG value
   is set to true.  */

#define HSA_LOG(prefix, ...) \
  do \
  { \
    if (debug) \
      { \
	fprintf (stderr, prefix); \
	fprintf (stderr, __VA_ARGS__); \
      } \
  } \
  while (false)

/* Print a debugging message to stderr.  */

#define HSA_DEBUG(...) HSA_LOG ("HSA debug: ", __VA_ARGS__)

/* Print a warning message to stderr.  */

#define HSA_WARNING(...) HSA_LOG ("HSA warning: ", __VA_ARGS__)

/* Print HSA warning STR with an HSA STATUS code.  */

static void
hsa_warn (const char *str, hsa_status_t status)
{
  if (!debug)
    return;

  const char *hsa_error_msg;
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);

  fprintf (stderr, "HSA warning: %s\nRuntime message: %s", str, hsa_error_msg);
}

/* Report a fatal error STR together with the HSA error corresponding to STATUS
   and terminate execution of the current process.  */

static void
hsa_fatal (const char *str, hsa_status_t status)
{
  const char *hsa_error_msg;
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);
  GOMP_PLUGIN_fatal ("HSA fatal error: %s\nRuntime message: %s", str,
		     hsa_error_msg);
}

/* Like hsa_fatal, except only report error message, and return FALSE
   for propagating error processing to outside of plugin.  */

static bool
hsa_error (const char *str, hsa_status_t status)
{
  const char *hsa_error_msg;
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);
  GOMP_PLUGIN_error ("HSA fatal error: %s\nRuntime message: %s", str,
		     hsa_error_msg);
  return false;
}

struct hsa_kernel_description
{
  const char *name;
  unsigned omp_data_size;
  bool gridified_kernel_p;
  unsigned kernel_dependencies_count;
  const char **kernel_dependencies;
};

struct global_var_info
{
  const char *name;
  void *address;
};

/* Data passed by the static initializer of a compilation unit containing BRIG
   to GOMP_offload_register.  */

struct brig_image_desc
{
  hsa_ext_module_t brig_module;
  const unsigned kernel_count;
  struct hsa_kernel_description *kernel_infos;
  const unsigned global_variable_count;
  struct global_var_info *global_variables;
};

struct agent_info;

/* Information required to identify, finalize and run any given kernel.  */

struct kernel_info
{
  /* Name of the kernel, required to locate it within the brig module.  */
  const char *name;
  /* Size of memory space for OMP data.  */
  unsigned omp_data_size;
  /* The specific agent the kernel has been or will be finalized for and run
     on.  */
  struct agent_info *agent;
  /* The specific module where the kernel takes place.  */
  struct module_info *module;
  /* Mutex enforcing that at most once thread ever initializes a kernel for
     use.  A thread should have locked agent->modules_rwlock for reading before
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
  /* List of all kernel dependencies.  */
  const char **dependencies;
  /* Number of dependencies.  */
  unsigned dependencies_count;
  /* Maximum OMP data size necessary for kernel from kernel dispatches.  */
  unsigned max_omp_data_size;
  /* True if the kernel is gridified.  */
  bool gridified_kernel_p;
};

/* Information about a particular brig module, its image and kernels.  */

struct module_info
{
  /* The next and previous module in the linked list of modules of an agent.  */
  struct module_info *next, *prev;
  /* The description with which the program has registered the image.  */
  struct brig_image_desc *image_desc;

  /* Number of kernels in this module.  */
  int kernel_count;
  /* An array of kernel_info structures describing each kernel in this
     module.  */
  struct kernel_info kernels[];
};

/* Information about shared brig library.  */

struct brig_library_info
{
  char *file_name;
  hsa_ext_module_t image;
};

/* Description of an HSA GPU agent and the program associated with it.  */

struct agent_info
{
  /* The HSA ID of the agent.  Assigned when hsa_context is initialized.  */
  hsa_agent_t id;
  /* Whether the agent has been initialized.  The fields below are usable only
     if it has been.  */
  bool initialized;
  /* The HSA ISA of this agent.  */
  hsa_isa_t isa;
  /* Command queue of the agent.  */
  hsa_queue_t *command_q;
  /* Kernel from kernel dispatch command queue.  */
  hsa_queue_t *kernel_dispatch_command_q;
  /* The HSA memory region from which to allocate kernel arguments.  */
  hsa_region_t kernarg_region;

  /* Read-write lock that protects kernels which are running or about to be run
     from interference with loading and unloading of images.  Needs to be
     locked for reading while a kernel is being run, and for writing if the
     list of modules is manipulated (and thus the HSA program invalidated).  */
  pthread_rwlock_t modules_rwlock;
  /* The first module in a linked list of modules associated with this
     kernel.  */
  struct module_info *first_module;

  /* Mutex enforcing that only one thread will finalize the HSA program.  A
     thread should have locked agent->modules_rwlock for reading before
     acquiring it.  */
  pthread_mutex_t prog_mutex;
  /* Flag whether the HSA program that consists of all the modules has been
     finalized.  */
  bool prog_finalized;
  /* Flag whether the program was finalized but with a failure.  */
  bool prog_finalized_error;
  /* HSA executable - the finalized program that is used to locate kernels.  */
  hsa_executable_t executable;
  /* List of BRIG libraries.  */
  struct brig_library_info **brig_libraries;
  /* Number of loaded shared BRIG libraries.  */
  unsigned brig_libraries_count;
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
};

/* Information about the whole HSA environment and all of its agents.  */

static struct hsa_context_info hsa_context;

#define DLSYM_FN(function) \
  hsa_fns.function##_fn = dlsym (handle, #function); \
  if (hsa_fns.function##_fn == NULL) \
    goto dl_fail;

static bool
init_hsa_runtime_functions (void)
{
  void *handle = dlopen (hsa_runtime_lib, RTLD_LAZY);
  if (handle == NULL)
    goto dl_fail;

  DLSYM_FN (hsa_status_string)
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
  DLSYM_FN (hsa_memory_free)
  DLSYM_FN (hsa_signal_destroy)
  DLSYM_FN (hsa_executable_get_symbol)
  DLSYM_FN (hsa_executable_symbol_get_info)
  DLSYM_FN (hsa_queue_add_write_index_release)
  DLSYM_FN (hsa_queue_load_read_index_acquire)
  DLSYM_FN (hsa_signal_wait_acquire)
  DLSYM_FN (hsa_signal_store_relaxed)
  DLSYM_FN (hsa_signal_store_release)
  DLSYM_FN (hsa_signal_load_acquire)
  DLSYM_FN (hsa_queue_destroy)
  DLSYM_FN (hsa_ext_program_add_module)
  DLSYM_FN (hsa_ext_program_create)
  DLSYM_FN (hsa_ext_program_destroy)
  DLSYM_FN (hsa_ext_program_finalize)
  return true;

 dl_fail:
  HSA_DEBUG ("while loading %s: %s\n", hsa_runtime_lib, dlerror ());
  return false;
}

/* Find kernel for an AGENT by name provided in KERNEL_NAME.  */

static struct kernel_info *
get_kernel_for_agent (struct agent_info *agent, const char *kernel_name)
{
  struct module_info *module = agent->first_module;

  while (module)
    {
      for (unsigned i = 0; i < module->kernel_count; i++)
	if (strcmp (module->kernels[i].name, kernel_name) == 0)
	  return &module->kernels[i];

      module = module->next;
    }

  return NULL;
}

/* Return true if the agent is a GPU and acceptable of concurrent submissions
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

/* Callback of hsa_iterate_agents, if AGENT is a GPU device, increment
   agent_count in hsa_context.  */

static hsa_status_t
count_gpu_agents (hsa_agent_t agent, void *data __attribute__ ((unused)))
{
  if (suitable_hsa_agent_p (agent))
    hsa_context.agent_count++;
  return HSA_STATUS_SUCCESS;
}

/* Callback of hsa_iterate_agents, if AGENT is a GPU device, assign the agent
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
   Return TRUE on success.  */

static bool
init_hsa_context (void)
{
  hsa_status_t status;
  int agent_index = 0;

  if (hsa_context.initialized)
    return true;
  init_enviroment_variables ();
  if (!init_hsa_runtime_functions ())
    {
      HSA_DEBUG ("Run-time could not be dynamically opened\n");
      return false;
    }
  status = hsa_fns.hsa_init_fn ();
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Run-time could not be initialized", status);
  HSA_DEBUG ("HSA run-time initialized\n");
  status = hsa_fns.hsa_iterate_agents_fn (count_gpu_agents, NULL);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("HSA GPU devices could not be enumerated", status);
  HSA_DEBUG ("There are %i HSA GPU devices.\n", hsa_context.agent_count);

  hsa_context.agents
    = GOMP_PLUGIN_malloc_cleared (hsa_context.agent_count
				  * sizeof (struct agent_info));
  status = hsa_fns.hsa_iterate_agents_fn (assign_agent_ids, &agent_index);
  if (agent_index != hsa_context.agent_count)
    {
      GOMP_PLUGIN_error ("Failed to assign IDs to all HSA agents");
      return false;
    }
  hsa_context.initialized = true;
  return true;
}

/* Callback of dispatch queues to report errors.  */

static void
queue_callback (hsa_status_t status,
		hsa_queue_t *queue __attribute__ ((unused)),
		void *data __attribute__ ((unused)))
{
  hsa_fatal ("Asynchronous queue error", status);
}

/* Callback of hsa_agent_iterate_regions.  Determine if a memory REGION can be
   used for kernarg allocations and if so write it to the memory pointed to by
   DATA and break the query.  */

static hsa_status_t
get_kernarg_memory_region (hsa_region_t region, void *data)
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
  if (flags & HSA_REGION_GLOBAL_FLAG_KERNARG)
    {
      hsa_region_t *ret = (hsa_region_t *) data;
      *ret = region;
      return HSA_STATUS_INFO_BREAK;
    }
  return HSA_STATUS_SUCCESS;
}

/* Part of the libgomp plugin interface.  Return the number of HSA devices on
   the system.  */

int
GOMP_OFFLOAD_get_num_devices (void)
{
  if (!init_hsa_context ())
    return 0;
  return hsa_context.agent_count;
}

/* Part of the libgomp plugin interface.  Initialize agent number N so that it
   can be used for computation.  Return TRUE on success.  */

bool
GOMP_OFFLOAD_init_device (int n)
{
  if (!init_hsa_context ())
    return false;
  if (n >= hsa_context.agent_count)
    {
      GOMP_PLUGIN_error ("Request to initialize non-existing HSA device %i", n);
      return false;
    }
  struct agent_info *agent = &hsa_context.agents[n];

  if (agent->initialized)
    return true;

  if (pthread_rwlock_init (&agent->modules_rwlock, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize an HSA agent rwlock");
      return false;
    }
  if (pthread_mutex_init (&agent->prog_mutex, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize an HSA agent program mutex");
      return false;
    }

  uint32_t queue_size;
  hsa_status_t status;
  status = hsa_fns.hsa_agent_get_info_fn (agent->id,
					  HSA_AGENT_INFO_QUEUE_MAX_SIZE,
					  &queue_size);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error requesting maximum queue size of the HSA agent",
    	   	      status);
  status = hsa_fns.hsa_agent_get_info_fn (agent->id, HSA_AGENT_INFO_ISA,
					  &agent->isa);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error querying the ISA of the agent", status);
  status = hsa_fns.hsa_queue_create_fn (agent->id, queue_size,
					HSA_QUEUE_TYPE_MULTI,
					queue_callback, NULL, UINT32_MAX,
					UINT32_MAX,
					&agent->command_q);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error creating command queue", status);

  status = hsa_fns.hsa_queue_create_fn (agent->id, queue_size,
					HSA_QUEUE_TYPE_MULTI,
					queue_callback, NULL, UINT32_MAX,
					UINT32_MAX,
					&agent->kernel_dispatch_command_q);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error creating kernel dispatch command queue", status);

  agent->kernarg_region.handle = (uint64_t) -1;
  status = hsa_fns.hsa_agent_iterate_regions_fn (agent->id,
						 get_kernarg_memory_region,
						 &agent->kernarg_region);
  if (agent->kernarg_region.handle == (uint64_t) -1)
    {
      GOMP_PLUGIN_error ("Could not find suitable memory region for kernel "
			 "arguments");
      return false;
    }
  HSA_DEBUG ("HSA agent initialized, queue has id %llu\n",
	     (long long unsigned) agent->command_q->id);
  HSA_DEBUG ("HSA agent initialized, kernel dispatch queue has id %llu\n",
	     (long long unsigned) agent->kernel_dispatch_command_q->id);
  agent->initialized = true;
  return true;
}

/* Verify that hsa_context has already been initialized and return the
   agent_info structure describing device number N.  Return NULL on error.  */

static struct agent_info *
get_agent_info (int n)
{
  if (!hsa_context.initialized)
    {
      GOMP_PLUGIN_error ("Attempt to use uninitialized HSA context.");
      return NULL;
    }
  if (n >= hsa_context.agent_count)
    {
      GOMP_PLUGIN_error ("Request to operate on anon-existing HSA device %i", n);
      return NULL;
    }
  if (!hsa_context.agents[n].initialized)
    {
      GOMP_PLUGIN_error ("Attempt to use an uninitialized HSA agent.");
      return NULL;
    }
  return &hsa_context.agents[n];
}

/* Insert MODULE to the linked list of modules of AGENT.  */

static void
add_module_to_agent (struct agent_info *agent, struct module_info *module)
{
  if (agent->first_module)
    agent->first_module->prev = module;
  module->next = agent->first_module;
  module->prev = NULL;
  agent->first_module = module;
}

/* Remove MODULE from the linked list of modules of AGENT.  */

static void
remove_module_from_agent (struct agent_info *agent, struct module_info *module)
{
  if (agent->first_module == module)
    agent->first_module = module->next;
  if (module->prev)
    module->prev->next = module->next;
  if (module->next)
    module->next->prev = module->prev;
}

/* Free the HSA program in agent and everything associated with it and set
   agent->prog_finalized and the initialized flags of all kernels to false.
   Return TRUE on success.  */

static bool
destroy_hsa_program (struct agent_info *agent)
{
  if (!agent->prog_finalized || agent->prog_finalized_error)
    return true;

  hsa_status_t status;

  HSA_DEBUG ("Destroying the current HSA program.\n");

  status = hsa_fns.hsa_executable_destroy_fn (agent->executable);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Could not destroy HSA executable", status);

  struct module_info *module;
  for (module = agent->first_module; module; module = module->next)
    {
      int i;
      for (i = 0; i < module->kernel_count; i++)
	module->kernels[i].initialized = false;
    }
  agent->prog_finalized = false;
  return true;
}

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
  kernel->omp_data_size = d->omp_data_size;
  kernel->gridified_kernel_p = d->gridified_kernel_p;
  kernel->dependencies_count = d->kernel_dependencies_count;
  kernel->dependencies = d->kernel_dependencies;
  if (pthread_mutex_init (&kernel->init_mutex, NULL))
    {
      GOMP_PLUGIN_error ("Failed to initialize an HSA kernel mutex");
      return false;
    }
  return true;
}

/* Part of the libgomp plugin interface.  Load BRIG module described by struct
   brig_image_desc in TARGET_DATA and return references to kernel descriptors
   in TARGET_TABLE.  */

int
GOMP_OFFLOAD_load_image (int ord, unsigned version, const void *target_data,
			 struct addr_pair **target_table)
{
  if (GOMP_VERSION_DEV (version) > GOMP_VERSION_HSA)
    {
      GOMP_PLUGIN_error ("Offload data incompatible with HSA plugin"
			 " (expected %u, received %u)",
			 GOMP_VERSION_HSA, GOMP_VERSION_DEV (version));
      return -1;
    }

  struct brig_image_desc *image_desc = (struct brig_image_desc *) target_data;
  struct agent_info *agent;
  struct addr_pair *pair;
  struct module_info *module;
  struct kernel_info *kernel;
  int kernel_count = image_desc->kernel_count;

  agent = get_agent_info (ord);
  if (!agent)
    return -1;

  if (pthread_rwlock_wrlock (&agent->modules_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to write-lock an HSA agent rwlock");
      return -1;
    }
  if (agent->prog_finalized
      && !destroy_hsa_program (agent))
    return -1;

  HSA_DEBUG ("Encountered %d kernels in an image\n", kernel_count);
  pair = GOMP_PLUGIN_malloc (kernel_count * sizeof (struct addr_pair));
  *target_table = pair;
  module = (struct module_info *)
    GOMP_PLUGIN_malloc_cleared (sizeof (struct module_info)
				+ kernel_count * sizeof (struct kernel_info));
  module->image_desc = image_desc;
  module->kernel_count = kernel_count;

  kernel = &module->kernels[0];

  /* Allocate memory for kernel dependencies.  */
  for (unsigned i = 0; i < kernel_count; i++)
    {
      pair->start = (uintptr_t) kernel;
      pair->end = (uintptr_t) (kernel + 1);

      struct hsa_kernel_description *d = &image_desc->kernel_infos[i];
      if (!init_basic_kernel_info (kernel, d, agent, module))
	return -1;
      kernel++;
      pair++;
    }

  add_module_to_agent (agent, module);
  if (pthread_rwlock_unlock (&agent->modules_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to unlock an HSA agent rwlock");
      return -1;
    }
  return kernel_count;
}

/* Add a shared BRIG library from a FILE_NAME to an AGENT.  */

static struct brig_library_info *
add_shared_library (const char *file_name, struct agent_info *agent)
{
  struct brig_library_info *library = NULL;

  void *f = dlopen (file_name, RTLD_NOW);
  void *start = dlsym (f, "__brig_start");
  void *end = dlsym (f, "__brig_end");

  if (start == NULL || end == NULL)
    return NULL;

  unsigned size = end - start;
  char *buf = (char *) GOMP_PLUGIN_malloc (size);
  memcpy (buf, start, size);

  library = GOMP_PLUGIN_malloc (sizeof (struct agent_info));
  library->file_name = (char *) GOMP_PLUGIN_malloc
    ((strlen (file_name) + 1));
  strcpy (library->file_name, file_name);
  library->image = (hsa_ext_module_t) buf;

  return library;
}

/* Release memory used for BRIG shared libraries that correspond
   to an AGENT.  */

static void
release_agent_shared_libraries (struct agent_info *agent)
{
  for (unsigned i = 0; i < agent->brig_libraries_count; i++)
    if (agent->brig_libraries[i])
      {
	free (agent->brig_libraries[i]->file_name);
	free (agent->brig_libraries[i]->image);
	free (agent->brig_libraries[i]);
      }

  free (agent->brig_libraries);
}

/* Create and finalize the program consisting of all loaded modules.  */

static void
create_and_finalize_hsa_program (struct agent_info *agent)
{
  hsa_status_t status;
  hsa_ext_program_t prog_handle;
  int mi = 0;

  if (pthread_mutex_lock (&agent->prog_mutex))
    GOMP_PLUGIN_fatal ("Could not lock an HSA agent program mutex");
  if (agent->prog_finalized)
    goto final;

  status = hsa_fns.hsa_ext_program_create_fn
    (HSA_MACHINE_MODEL_LARGE, HSA_PROFILE_FULL,
     HSA_DEFAULT_FLOAT_ROUNDING_MODE_DEFAULT,
     NULL, &prog_handle);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not create an HSA program", status);

  HSA_DEBUG ("Created a finalized program\n");

  struct module_info *module = agent->first_module;
  while (module)
    {
      status = hsa_fns.hsa_ext_program_add_module_fn
	(prog_handle, module->image_desc->brig_module);
      if (status != HSA_STATUS_SUCCESS)
	hsa_fatal ("Could not add a module to the HSA program", status);
      module = module->next;
      mi++;
    }

  /* Load all shared libraries.  */
  const char *libraries[] = { "libhsamath.so", "libhsastd.so" };
  const unsigned libraries_count = sizeof (libraries) / sizeof (const char *);

  agent->brig_libraries_count = libraries_count;
  agent->brig_libraries = GOMP_PLUGIN_malloc_cleared
    (sizeof (struct brig_library_info) * libraries_count);

  for (unsigned i = 0; i < libraries_count; i++)
    {
      struct brig_library_info *library = add_shared_library (libraries[i],
							      agent);
      if (library == NULL)
	{
	  HSA_WARNING ("Could not open a shared BRIG library: %s\n",
		       libraries[i]);
	  continue;
	}

      status = hsa_fns.hsa_ext_program_add_module_fn (prog_handle,
						      library->image);
      if (status != HSA_STATUS_SUCCESS)
	hsa_warn ("Could not add a shared BRIG library the HSA program",
		  status);
      else
	HSA_DEBUG ("a shared BRIG library has been added to a program: %s\n",
		   libraries[i]);
    }

  hsa_ext_control_directives_t control_directives;
  memset (&control_directives, 0, sizeof (control_directives));
  hsa_code_object_t code_object;
  status = hsa_fns.hsa_ext_program_finalize_fn
    (prog_handle, agent->isa,HSA_EXT_FINALIZER_CALL_CONVENTION_AUTO,
     control_directives, "", HSA_CODE_OBJECT_TYPE_PROGRAM, &code_object);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_warn ("Finalization of the HSA program failed", status);
      goto failure;
    }

  HSA_DEBUG ("Finalization done\n");
  hsa_fns.hsa_ext_program_destroy_fn (prog_handle);

  status
    = hsa_fns.hsa_executable_create_fn (HSA_PROFILE_FULL,
					HSA_EXECUTABLE_STATE_UNFROZEN,
					"", &agent->executable);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not create HSA executable", status);

  module = agent->first_module;
  while (module)
    {
      /* Initialize all global variables declared in the module.  */
      for (unsigned i = 0; i < module->image_desc->global_variable_count; i++)
	{
	  struct global_var_info *var;
	  var = &module->image_desc->global_variables[i];
	  status = hsa_fns.hsa_executable_global_variable_define_fn
	    (agent->executable, var->name, var->address);

	  HSA_DEBUG ("Defining global variable: %s, address: %p\n", var->name,
		     var->address);

	  if (status != HSA_STATUS_SUCCESS)
	    hsa_fatal ("Could not define a global variable in the HSA program",
		       status);
	}

      module = module->next;
    }

  status = hsa_fns.hsa_executable_load_code_object_fn (agent->executable,
						       agent->id,
						       code_object, "");
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not add a code object to the HSA executable", status);
  status = hsa_fns.hsa_executable_freeze_fn (agent->executable, "");
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not freeze the HSA executable", status);

  HSA_DEBUG ("Froze HSA executable with the finalized code object\n");

  /* If all goes good, jump to final.  */
  goto final;

failure:
  agent->prog_finalized_error = true;

final:
  agent->prog_finalized = true;

  if (pthread_mutex_unlock (&agent->prog_mutex))
    GOMP_PLUGIN_fatal ("Could not unlock an HSA agent program mutex");
}

/* Create kernel dispatch data structure for given KERNEL.  */

static struct GOMP_hsa_kernel_dispatch *
create_single_kernel_dispatch (struct kernel_info *kernel,
			       unsigned omp_data_size)
{
  struct agent_info *agent = kernel->agent;
  struct GOMP_hsa_kernel_dispatch *shadow
    = GOMP_PLUGIN_malloc_cleared (sizeof (struct GOMP_hsa_kernel_dispatch));

  shadow->queue = agent->command_q;
  shadow->omp_data_memory
    = omp_data_size > 0 ? GOMP_PLUGIN_malloc (omp_data_size) : NULL;
  unsigned dispatch_count = kernel->dependencies_count;
  shadow->kernel_dispatch_count = dispatch_count;

  shadow->children_dispatches
    = GOMP_PLUGIN_malloc (dispatch_count * sizeof (shadow));

  shadow->object = kernel->object;

  hsa_signal_t sync_signal;
  hsa_status_t status = hsa_fns.hsa_signal_create_fn (1, 0, NULL, &sync_signal);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Error creating the HSA sync signal", status);

  shadow->signal = sync_signal.handle;
  shadow->private_segment_size = kernel->private_segment_size;
  shadow->group_segment_size = kernel->group_segment_size;

  status
    = hsa_fns.hsa_memory_allocate_fn (agent->kernarg_region,
				      kernel->kernarg_segment_size,
				      &shadow->kernarg_address);
  if (status != HSA_STATUS_SUCCESS)
    hsa_fatal ("Could not allocate memory for HSA kernel arguments", status);

  return shadow;
}

/* Release data structure created for a kernel dispatch in SHADOW argument.  */

static void
release_kernel_dispatch (struct GOMP_hsa_kernel_dispatch *shadow)
{
  HSA_DEBUG ("Released kernel dispatch: %p has value: %lu (%p)\n", shadow,
	     shadow->debug, (void *) shadow->debug);

  hsa_fns.hsa_memory_free_fn (shadow->kernarg_address);

  hsa_signal_t s;
  s.handle = shadow->signal;
  hsa_fns.hsa_signal_destroy_fn (s);

  free (shadow->omp_data_memory);

  for (unsigned i = 0; i < shadow->kernel_dispatch_count; i++)
    release_kernel_dispatch (shadow->children_dispatches[i]);

  free (shadow->children_dispatches);
  free (shadow);
}

/* Initialize a KERNEL without its dependencies.  MAX_OMP_DATA_SIZE is used
   to calculate maximum necessary memory for OMP data allocation.  */

static void
init_single_kernel (struct kernel_info *kernel, unsigned *max_omp_data_size)
{
  hsa_status_t status;
  struct agent_info *agent = kernel->agent;
  hsa_executable_symbol_t kernel_symbol;
  status = hsa_fns.hsa_executable_get_symbol_fn (agent->executable, NULL,
						 kernel->name, agent->id,
						 0, &kernel_symbol);
  if (status != HSA_STATUS_SUCCESS)
    {
      hsa_warn ("Could not find symbol for kernel in the code object", status);
      goto failure;
    }
  HSA_DEBUG ("Located kernel %s\n", kernel->name);
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

  HSA_DEBUG ("Kernel structure for %s fully initialized with "
	     "following segment sizes: \n", kernel->name);
  HSA_DEBUG ("  group_segment_size: %u\n",
	     (unsigned) kernel->group_segment_size);
  HSA_DEBUG ("  private_segment_size: %u\n",
	     (unsigned) kernel->private_segment_size);
  HSA_DEBUG ("  kernarg_segment_size: %u\n",
	     (unsigned) kernel->kernarg_segment_size);
  HSA_DEBUG ("  omp_data_size: %u\n", kernel->omp_data_size);
  HSA_DEBUG ("  gridified_kernel_p: %u\n", kernel->gridified_kernel_p);

  if (kernel->omp_data_size > *max_omp_data_size)
    *max_omp_data_size = kernel->omp_data_size;

  for (unsigned i = 0; i < kernel->dependencies_count; i++)
    {
      struct kernel_info *dependency
	= get_kernel_for_agent (agent, kernel->dependencies[i]);

      if (dependency == NULL)
	{
	  HSA_DEBUG ("Could not find a dependency for a kernel: %s, "
		     "dependency name: %s\n", kernel->name,
		     kernel->dependencies[i]);
	  goto failure;
	}

      if (dependency->dependencies_count > 0)
	{
	  HSA_DEBUG ("HSA does not allow kernel dispatching code with "
		     "a depth bigger than one\n");
	  goto failure;
	}

      init_single_kernel (dependency, max_omp_data_size);
    }

  return;

failure:
  kernel->initialization_failed = true;
}

/* Indent stream F by INDENT spaces.  */

static void
indent_stream (FILE *f, unsigned indent)
{
  fprintf (f, "%*s", indent, "");
}

/* Dump kernel DISPATCH data structure and indent it by INDENT spaces.  */

static void
print_kernel_dispatch (struct GOMP_hsa_kernel_dispatch *dispatch, unsigned indent)
{
  indent_stream (stderr, indent);
  fprintf (stderr, "this: %p\n", dispatch);
  indent_stream (stderr, indent);
  fprintf (stderr, "queue: %p\n", dispatch->queue);
  indent_stream (stderr, indent);
  fprintf (stderr, "omp_data_memory: %p\n", dispatch->omp_data_memory);
  indent_stream (stderr, indent);
  fprintf (stderr, "kernarg_address: %p\n", dispatch->kernarg_address);
  indent_stream (stderr, indent);
  fprintf (stderr, "object: %lu\n", dispatch->object);
  indent_stream (stderr, indent);
  fprintf (stderr, "signal: %lu\n", dispatch->signal);
  indent_stream (stderr, indent);
  fprintf (stderr, "private_segment_size: %u\n",
	   dispatch->private_segment_size);
  indent_stream (stderr, indent);
  fprintf (stderr, "group_segment_size: %u\n",
	   dispatch->group_segment_size);
  indent_stream (stderr, indent);
  fprintf (stderr, "children dispatches: %lu\n",
	   dispatch->kernel_dispatch_count);
  indent_stream (stderr, indent);
  fprintf (stderr, "omp_num_threads: %u\n",
	   dispatch->omp_num_threads);
  fprintf (stderr, "\n");

  for (unsigned i = 0; i < dispatch->kernel_dispatch_count; i++)
    print_kernel_dispatch (dispatch->children_dispatches[i], indent + 2);
}

/* Create kernel dispatch data structure for a KERNEL and all its
   dependencies.  */

static struct GOMP_hsa_kernel_dispatch *
create_kernel_dispatch (struct kernel_info *kernel, unsigned omp_data_size)
{
  struct GOMP_hsa_kernel_dispatch *shadow
    = create_single_kernel_dispatch (kernel, omp_data_size);
  shadow->omp_num_threads = 64;
  shadow->debug = 0;
  shadow->omp_level = kernel->gridified_kernel_p ? 1 : 0;

  /* Create kernel dispatch data structures.  We do not allow to have
     a kernel dispatch with depth bigger than one.  */
  for (unsigned i = 0; i < kernel->dependencies_count; i++)
    {
      struct kernel_info *dependency
	= get_kernel_for_agent (kernel->agent, kernel->dependencies[i]);
      shadow->children_dispatches[i]
	= create_single_kernel_dispatch (dependency, omp_data_size);
      shadow->children_dispatches[i]->queue
	= kernel->agent->kernel_dispatch_command_q;
      shadow->children_dispatches[i]->omp_level = 1;
    }

  return shadow;
}

/* Do all the work that is necessary before running KERNEL for the first time.
   The function assumes the program has been created, finalized and frozen by
   create_and_finalize_hsa_program.  */

static void
init_kernel (struct kernel_info *kernel)
{
  if (pthread_mutex_lock (&kernel->init_mutex))
    GOMP_PLUGIN_fatal ("Could not lock an HSA kernel initialization mutex");
  if (kernel->initialized)
    {
      if (pthread_mutex_unlock (&kernel->init_mutex))
	GOMP_PLUGIN_fatal ("Could not unlock an HSA kernel initialization "
			   "mutex");

      return;
    }

  /* Precomputed maximum size of OMP data necessary for a kernel from kernel
     dispatch operation.  */
  init_single_kernel (kernel, &kernel->max_omp_data_size);

  if (!kernel->initialization_failed)
    HSA_DEBUG ("\n");

  kernel->initialized = true;
  if (pthread_mutex_unlock (&kernel->init_mutex))
    GOMP_PLUGIN_fatal ("Could not unlock an HSA kernel initialization "
		       "mutex");
}

/* Parse the target attributes INPUT provided by the compiler and return true
   if we should run anything all.  If INPUT is NULL, fill DEF with default
   values, then store INPUT or DEF into *RESULT.  */

static bool
parse_target_attributes (void **input,
			 struct GOMP_kernel_launch_attributes *def,
			 struct GOMP_kernel_launch_attributes **result)
{
  if (!input)
    GOMP_PLUGIN_fatal ("No target arguments provided");

  bool attrs_found = false;
  while (*input)
    {
      uintptr_t id = (uintptr_t) *input;
      if ((id & GOMP_TARGET_ARG_DEVICE_MASK) == GOMP_DEVICE_HSA
	  && ((id & GOMP_TARGET_ARG_ID_MASK)
	      == GOMP_TARGET_ARG_HSA_KERNEL_ATTRIBUTES))
	{
	  input++;
	  attrs_found = true;
	  break;
	}

      if (id & GOMP_TARGET_ARG_SUBSEQUENT_PARAM)
	input++;
      input++;
    }

  if (!attrs_found)
    {
      def->ndim = 1;
      def->gdims[0] = 1;
      def->gdims[1] = 1;
      def->gdims[2] = 1;
      def->wdims[0] = 1;
      def->wdims[1] = 1;
      def->wdims[2] = 1;
      *result = def;
      HSA_DEBUG ("GOMP_OFFLOAD_run called with no launch attributes\n");
      return true;
    }

  struct GOMP_kernel_launch_attributes *kla;
  kla = (struct GOMP_kernel_launch_attributes *) *input;
  *result = kla;
  if (kla->ndim == 0 || kla->ndim > 3)
    GOMP_PLUGIN_fatal ("Invalid number of dimensions (%u)", kla->ndim);

  HSA_DEBUG ("GOMP_OFFLOAD_run called with %u dimensions:\n", kla->ndim);
  unsigned i;
  for (i = 0; i < kla->ndim; i++)
    {
      HSA_DEBUG ("  Dimension %u: grid size %u and group size %u\n", i,
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

/* Return true if the HSA runtime can run function FN_PTR.  */

bool
GOMP_OFFLOAD_can_run (void *fn_ptr)
{
  struct kernel_info *kernel = (struct kernel_info *) fn_ptr;
  struct agent_info *agent = kernel->agent;
  create_and_finalize_hsa_program (agent);

  if (agent->prog_finalized_error)
    goto failure;

  init_kernel (kernel);
  if (kernel->initialization_failed)
    goto failure;

  return true;

failure:
  if (suppress_host_fallback)
    GOMP_PLUGIN_fatal ("HSA host fallback has been suppressed");
  HSA_DEBUG ("HSA target cannot be launched, doing a host fallback\n");
  return false;
}

/* Atomically store pair of uint16_t values (HEADER and REST) to a PACKET.  */

void
packet_store_release (uint32_t* packet, uint16_t header, uint16_t rest)
{
  __atomic_store_n (packet, header | (rest << 16), __ATOMIC_RELEASE);
}

/* Run KERNEL on its agent, pass VARS to it as arguments and take
   launchattributes from KLA.  */

void
run_kernel (struct kernel_info *kernel, void *vars,
	    struct GOMP_kernel_launch_attributes *kla)
{
  struct agent_info *agent = kernel->agent;
  if (pthread_rwlock_rdlock (&agent->modules_rwlock))
    GOMP_PLUGIN_fatal ("Unable to read-lock an HSA agent rwlock");

  if (!agent->initialized)
    GOMP_PLUGIN_fatal ("Agent must be initialized");

  if (!kernel->initialized)
    GOMP_PLUGIN_fatal ("Called kernel must be initialized");

  struct GOMP_hsa_kernel_dispatch *shadow
    = create_kernel_dispatch (kernel, kernel->max_omp_data_size);

  if (debug)
    {
      fprintf (stderr, "\nKernel has following dependencies:\n");
      print_kernel_dispatch (shadow, 2);
    }

  uint64_t index
    = hsa_fns.hsa_queue_add_write_index_release_fn (agent->command_q, 1);
  HSA_DEBUG ("Got AQL index %llu\n", (long long int) index);

  /* Wait until the queue is not full before writing the packet.   */
  while (index - hsa_fns.hsa_queue_load_read_index_acquire_fn (agent->command_q)
	 >= agent->command_q->size)
    ;

  hsa_kernel_dispatch_packet_t *packet;
  packet = ((hsa_kernel_dispatch_packet_t *) agent->command_q->base_address)
	   + index % agent->command_q->size;

  memset (((uint8_t *) packet) + 4, 0, sizeof (*packet) - 4);
  packet->grid_size_x = kla->gdims[0];
  packet->workgroup_size_x = get_group_size (kla->ndim, kla->gdims[0],
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
      packet->grid_size_z = kla->gdims[2];
      packet->workgroup_size_z = get_group_size (kla->ndim, kla->gdims[2],
					     kla->wdims[2]);
    }
  else
    {
      packet->grid_size_z = 1;
      packet->workgroup_size_z = 1;
    }

  packet->private_segment_size = kernel->private_segment_size;
  packet->group_segment_size = kernel->group_segment_size;
  packet->kernel_object = kernel->object;
  packet->kernarg_address = shadow->kernarg_address;
  hsa_signal_t s;
  s.handle = shadow->signal;
  packet->completion_signal = s;
  hsa_fns.hsa_signal_store_relaxed_fn (s, 1);
  memcpy (shadow->kernarg_address, &vars, sizeof (vars));

  /* PR hsa/70337.  */
  size_t vars_size = sizeof (vars);
  if (kernel->kernarg_segment_size > vars_size)
    {
      if (kernel->kernarg_segment_size != vars_size
	  + sizeof (struct hsa_kernel_runtime *))
	GOMP_PLUGIN_fatal ("Kernel segment size has an unexpected value");
      memcpy (packet->kernarg_address + vars_size, &shadow,
	      sizeof (struct hsa_kernel_runtime *));
    }

  HSA_DEBUG ("Copying kernel runtime pointer to kernarg_address\n");

  uint16_t header;
  header = HSA_PACKET_TYPE_KERNEL_DISPATCH << HSA_PACKET_HEADER_TYPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_ACQUIRE_FENCE_SCOPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_RELEASE_FENCE_SCOPE;

  HSA_DEBUG ("Going to dispatch kernel %s\n", kernel->name);

  packet_store_release ((uint32_t *) packet, header,
			(uint16_t) kla->ndim << HSA_KERNEL_DISPATCH_PACKET_SETUP_DIMENSIONS);

  hsa_fns.hsa_signal_store_release_fn (agent->command_q->doorbell_signal,
				       index);

  /* TODO: GPU agents in Carrizo APUs cannot properly update L2 cache for
     signal wait and signal load operations on their own and we need to
     periodically call the hsa_signal_load_acquire on completion signals of
     children kernels in the CPU to make that happen.  As soon the
     limitation will be resolved, this workaround can be removed.  */

  HSA_DEBUG ("Kernel dispatched, waiting for completion\n");

  /* Root signal waits with 1ms timeout.  */
  while (hsa_fns.hsa_signal_wait_acquire_fn (s, HSA_SIGNAL_CONDITION_LT, 1,
					     1000 * 1000,
					     HSA_WAIT_STATE_BLOCKED) != 0)
    for (unsigned i = 0; i < shadow->kernel_dispatch_count; i++)
      {
	hsa_signal_t child_s;
	child_s.handle = shadow->children_dispatches[i]->signal;

	HSA_DEBUG ("Waiting for children completion signal: %lu\n",
		   shadow->children_dispatches[i]->signal);
	hsa_fns.hsa_signal_load_acquire_fn (child_s);
      }

  release_kernel_dispatch (shadow);

  if (pthread_rwlock_unlock (&agent->modules_rwlock))
    GOMP_PLUGIN_fatal ("Unable to unlock an HSA agent rwlock");
}

/* Part of the libgomp plugin interface.  Run a kernel on device N (the number
   is actually ignored, we assume the FN_PTR has been mapped using the correct
   device) and pass it an array of pointers in VARS as a parameter.  The kernel
   is identified by FN_PTR which must point to a kernel_info structure.  */

void
GOMP_OFFLOAD_run (int n __attribute__((unused)),
		  void *fn_ptr, void *vars, void **args)
{
  struct kernel_info *kernel = (struct kernel_info *) fn_ptr;
  struct GOMP_kernel_launch_attributes def;
  struct GOMP_kernel_launch_attributes *kla;
  if (!parse_target_attributes (args, &def, &kla))
    {
      HSA_DEBUG ("Will not run HSA kernel because the grid size is zero\n");
      return;
    }
  run_kernel (kernel, vars, kla);
}

/* Information to be passed to a thread running a kernel asycnronously.  */

struct async_run_info
{
  int device;
  void *tgt_fn;
  void *tgt_vars;
  void **args;
  void *async_data;
};

/* Thread routine to run a kernel asynchronously.  */

static void *
run_kernel_asynchronously (void *thread_arg)
{
  struct async_run_info *info = (struct async_run_info *) thread_arg;
  int device = info->device;
  void *tgt_fn = info->tgt_fn;
  void *tgt_vars = info->tgt_vars;
  void **args = info->args;
  void *async_data = info->async_data;

  free (info);
  GOMP_OFFLOAD_run (device, tgt_fn, tgt_vars, args);
  GOMP_PLUGIN_target_task_completion (async_data);
  return NULL;
}

/* Part of the libgomp plugin interface.  Run a kernel like GOMP_OFFLOAD_run
   does, but asynchronously and call GOMP_PLUGIN_target_task_completion when it
   has finished.  */

void
GOMP_OFFLOAD_async_run (int device, void *tgt_fn, void *tgt_vars,
			void **args, void *async_data)
{
  pthread_t pt;
  struct async_run_info *info;
  HSA_DEBUG ("GOMP_OFFLOAD_async_run invoked\n");
  info = GOMP_PLUGIN_malloc (sizeof (struct async_run_info));

  info->device = device;
  info->tgt_fn = tgt_fn;
  info->tgt_vars = tgt_vars;
  info->args = args;
  info->async_data = async_data;

  int err = pthread_create (&pt, NULL, &run_kernel_asynchronously, info);
  if (err != 0)
    GOMP_PLUGIN_fatal ("HSA asynchronous thread creation failed: %s",
		       strerror (err));
  err = pthread_detach (pt);
  if (err != 0)
    GOMP_PLUGIN_fatal ("Failed to detach a thread to run HSA kernel "
		       "asynchronously: %s", strerror (err));
}

/* Deinitialize all information associated with MODULE and kernels within
   it.  Return TRUE on success.  */

static bool
destroy_module (struct module_info *module)
{
  int i;
  for (i = 0; i < module->kernel_count; i++)
    if (pthread_mutex_destroy (&module->kernels[i].init_mutex))
      {
	GOMP_PLUGIN_error ("Failed to destroy an HSA kernel initialization "
			   "mutex");
	return false;
      }
  return true;
}

/* Part of the libgomp plugin interface.  Unload BRIG module described by
   struct brig_image_desc in TARGET_DATA from agent number N.  Return
   TRUE on success.  */

bool
GOMP_OFFLOAD_unload_image (int n, unsigned version, const void *target_data)
{
  if (GOMP_VERSION_DEV (version) > GOMP_VERSION_HSA)
    {
      GOMP_PLUGIN_error ("Offload data incompatible with HSA plugin"
			 " (expected %u, received %u)",
			 GOMP_VERSION_HSA, GOMP_VERSION_DEV (version));
      return false;
    }

  struct agent_info *agent;
  agent = get_agent_info (n);
  if (!agent)
    return false;

  if (pthread_rwlock_wrlock (&agent->modules_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to write-lock an HSA agent rwlock");
      return false;
    }
  struct module_info *module = agent->first_module;
  while (module)
    {
      if (module->image_desc == target_data)
	break;
      module = module->next;
    }
  if (!module)
    {
      GOMP_PLUGIN_error ("Attempt to unload an image that has never been "
			 "loaded before");
      return false;
    }

  remove_module_from_agent (agent, module);
  if (!destroy_module (module))
    return false;
  free (module);
  if (!destroy_hsa_program (agent))
    return false;
  if (pthread_rwlock_unlock (&agent->modules_rwlock))
    {
      GOMP_PLUGIN_error ("Unable to unlock an HSA agent rwlock");
      return false;
    }
  return true;
}

/* Part of the libgomp plugin interface.  Deinitialize all information and
   status associated with agent number N.  We do not attempt any
   synchronization, assuming the user and libgomp will not attempt
   deinitialization of a device that is in any way being used at the same
   time.  Return TRUE on success.  */

bool
GOMP_OFFLOAD_fini_device (int n)
{
  struct agent_info *agent = get_agent_info (n);
  if (!agent)
    return false;

  if (!agent->initialized)
    return true;

  struct module_info *next_module = agent->first_module;
  while (next_module)
    {
      struct module_info *module = next_module;
      next_module = module->next;
      if (!destroy_module (module))
	return false;
      free (module);
    }
  agent->first_module = NULL;
  if (!destroy_hsa_program (agent))
    return false;

  release_agent_shared_libraries (agent);

  hsa_status_t status = hsa_fns.hsa_queue_destroy_fn (agent->command_q);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error destroying command queue", status);
  status = hsa_fns.hsa_queue_destroy_fn (agent->kernel_dispatch_command_q);
  if (status != HSA_STATUS_SUCCESS)
    return hsa_error ("Error destroying kernel dispatch command queue", status);
  if (pthread_mutex_destroy (&agent->prog_mutex))
    {
      GOMP_PLUGIN_error ("Failed to destroy an HSA agent program mutex");
      return false;
    }
  if (pthread_rwlock_destroy (&agent->modules_rwlock))
    {
      GOMP_PLUGIN_error ("Failed to destroy an HSA agent rwlock");
      return false;
    }
  agent->initialized = false;
  return true;
}

/* Part of the libgomp plugin interface.  Not implemented as it is not required
   for HSA.  */

void *
GOMP_OFFLOAD_alloc (int ord, size_t size)
{
  GOMP_PLUGIN_error ("HSA GOMP_OFFLOAD_alloc is not implemented because "
		     "it should never be called");
  return NULL;
}

/* Part of the libgomp plugin interface.  Not implemented as it is not required
   for HSA.  */

bool
GOMP_OFFLOAD_free (int ord, void *ptr)
{
  GOMP_PLUGIN_error ("HSA GOMP_OFFLOAD_free is not implemented because "
		     "it should never be called");
  return false;
}

/* Part of the libgomp plugin interface.  Not implemented as it is not required
   for HSA.  */

bool
GOMP_OFFLOAD_dev2host (int ord, void *dst, const void *src, size_t n)
{
  GOMP_PLUGIN_error ("HSA GOMP_OFFLOAD_dev2host is not implemented because "
		     "it should never be called");
  return false;
}

/* Part of the libgomp plugin interface.  Not implemented as it is not required
   for HSA.  */

bool
GOMP_OFFLOAD_host2dev (int ord, void *dst, const void *src, size_t n)
{
  GOMP_PLUGIN_error ("HSA GOMP_OFFLOAD_host2dev is not implemented because "
		     "it should never be called");
  return false;
}

/* Part of the libgomp plugin interface.  Not implemented as it is not required
   for HSA.  */

bool
GOMP_OFFLOAD_dev2dev (int ord, void *dst, const void *src, size_t n)
{
  GOMP_PLUGIN_error ("HSA GOMP_OFFLOAD_dev2dev is not implemented because "
		     "it should never be called");
  return false;
}
