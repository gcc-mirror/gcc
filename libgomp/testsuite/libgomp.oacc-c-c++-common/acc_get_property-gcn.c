/* Test the `acc_get_property' and `acc_get_property_string' library
   functions on amdgcn devices by comparing property values with
   those obtained through the HSA API. */
/* { dg-additional-sources acc_get_property-aux.c } */
/* { dg-additional-options "-ldl" } */
/* { dg-do run { target openacc_radeon_accel_selected } } */

#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openacc.h>

#include <hsa.h>


void expect_device_string_properties (acc_device_t dev_type, int dev_num,
				      const char* expected_vendor,
				      const char* expected_name,
				      const char* expected_driver);

hsa_status_t (*hsa_agent_get_info_fn) (hsa_agent_t agent,
				       hsa_agent_info_t attribute,
				       void *value);
hsa_status_t (*hsa_system_get_info_fn) (hsa_system_info_t attribute,
					void *value);
hsa_status_t (*hsa_iterate_agents_fn)
(hsa_status_t (*callback)(hsa_agent_t agent, void *data), void *data);
hsa_status_t (*hsa_init_fn) (void);

char* support_cpu_devices;

void
test_setup ()
{
  char* env_runtime;
  char* hsa_runtime_lib;
  void *handle;

#define DLSYM_FN(function)						\
  function##_fn = (typeof(function##_fn))dlsym (handle, #function);	\
  if (function##_fn == NULL)						\
    {									\
      fprintf (stderr, "Could not get symbol " #function ".\n");	\
      abort (); 							\
    }

  env_runtime = getenv ("HSA_RUNTIME_LIB");
  hsa_runtime_lib = env_runtime ? env_runtime : (char*)"libhsa-runtime64.so";

  handle = dlopen (hsa_runtime_lib, RTLD_LAZY);
  if (!handle)
    {
      fprintf (stderr, "Could not load %s.\n", hsa_runtime_lib);
      abort ();
    }

  DLSYM_FN (hsa_agent_get_info)
  DLSYM_FN (hsa_system_get_info)
  DLSYM_FN (hsa_iterate_agents)
  DLSYM_FN (hsa_init)

  hsa_init_fn ();

  support_cpu_devices = getenv ("GCN_SUPPORT_CPU_DEVICES");
}

static hsa_status_t
check_agent_properties (hsa_agent_t agent, void *dev_num_arg)
{

  char name[64];
  char vendor_name[64];
  uint16_t minor;
  uint16_t major;
  char driver[60];

  hsa_status_t status;
  hsa_device_type_t device_type;
  int* dev_num = (int*)dev_num_arg;

#define AGENT_GET_INFO(info_type, val)				\
  status = hsa_agent_get_info_fn (agent, info_type, &val);	\
  if (status != HSA_STATUS_SUCCESS)				\
    {								\
      fprintf (stderr, "Failed to obtain " #info_type ".\n");	\
      abort ();							\
    }
#define SYSTEM_GET_INFO(info_type, val)				\
  status = hsa_system_get_info_fn (info_type, &val);		\
  if (status != HSA_STATUS_SUCCESS)				\
    {								\
      fprintf (stderr, "Failed to obtain " #info_type ".\n");	\
      abort ();							\
    }

  AGENT_GET_INFO (HSA_AGENT_INFO_DEVICE, device_type)

    /* Skip unsupported device types.  Mimic the GCN plugin's behavior. */
    if (!(device_type == HSA_DEVICE_TYPE_GPU
	  || (support_cpu_devices && device_type == HSA_DEVICE_TYPE_CPU)))
      return HSA_STATUS_SUCCESS;

  AGENT_GET_INFO (HSA_AGENT_INFO_NAME, name)
  AGENT_GET_INFO (HSA_AGENT_INFO_VENDOR_NAME, vendor_name)

  SYSTEM_GET_INFO (HSA_SYSTEM_INFO_VERSION_MINOR, minor)
  SYSTEM_GET_INFO (HSA_SYSTEM_INFO_VERSION_MAJOR, major)

  snprintf (driver, sizeof driver, "HSA Runtime %hu.%hu",
	    (unsigned short int)major, (unsigned short int)minor);

  expect_device_string_properties(acc_device_radeon, *dev_num,
				  vendor_name, name, driver);

  (*dev_num)++;

  return status;
}

int
main ()
{
  int dev_num = 0;
  test_setup ();

  hsa_status_t status =
    hsa_iterate_agents_fn (&check_agent_properties, &dev_num);

  return status;
}
