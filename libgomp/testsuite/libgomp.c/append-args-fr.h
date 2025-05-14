#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include "../libgomp.c-c++-common/on_device_arch.h"

/* Provides:  */

#define DEFAULT_DEVICE -99

void check_host (omp_interop_t obj);
void check_nvptx (omp_interop_t obj, int dev, omp_interop_fr_t expected_fr, _Bool is_targetsync);
void check_gcn (omp_interop_t obj, int dev, omp_interop_fr_t expected_fr, _Bool is_targetsync);


/* The following assumes that when a nvptx device is available,
   cuda/cuda_driver/hip are supported.
   And that likewise when a gcn device is available that the
   plugin also can not only the HSA but also the HIP library
   such that hsa/hip are supported.
   For the host, omp_interop_none is expected.

   Otherwise, it only does some basic tests without checking
   that the returned result really makes sense.  */

void check_type (omp_interop_t obj)
{
  const char *type;

  type = omp_get_interop_type_desc (obj, omp_ipr_fr_id);
  if (obj != omp_interop_none)
    assert (strcmp (type, "omp_interop_t") == 0);
  else
    assert (type == NULL);

  type = omp_get_interop_type_desc (obj, omp_ipr_fr_name);
  if (obj != omp_interop_none)
    assert (strcmp (type, "const char *") == 0);
  else
    assert (type == NULL);

  type = omp_get_interop_type_desc (obj, omp_ipr_vendor);
  if (obj != omp_interop_none)
    assert (strcmp (type, "int") == 0);
  else
    assert (type == NULL);

  type = omp_get_interop_type_desc (obj, omp_ipr_vendor_name);
  if (obj != omp_interop_none)
    assert (strcmp (type, "const char *") == 0);
  else
    assert (type == NULL);

  type = omp_get_interop_type_desc (obj, omp_ipr_device_num);
  if (obj != omp_interop_none)
    assert (strcmp (type, "int") == 0);
  else
    assert (type == NULL);

  if (obj != omp_interop_none)
    return;
  assert (omp_get_interop_type_desc (obj, omp_ipr_platform) == NULL);
  assert (omp_get_interop_type_desc (obj, omp_ipr_device) == NULL);
  assert (omp_get_interop_type_desc (obj, omp_ipr_device_context) == NULL);
  assert (omp_get_interop_type_desc (obj, omp_ipr_targetsync) == NULL);
}


void
check_host (omp_interop_t obj)
{
  assert (obj == omp_interop_none);
  check_type (obj);
}


void
check_nvptx (omp_interop_t obj, int dev, omp_interop_fr_t expected_fr, _Bool is_targetsync)
{
  assert (obj != omp_interop_none && obj != (omp_interop_t) -1L);

  omp_interop_rc_t ret_code = omp_irc_no_value;
  omp_interop_fr_t fr = (omp_interop_fr_t) omp_get_interop_int (obj, omp_ipr_fr_id, &ret_code);

  assert (ret_code == omp_irc_success);
  assert (fr == expected_fr);

  ret_code = omp_irc_no_value;
  const char *fr_name = omp_get_interop_str (obj, omp_ipr_fr_name, &ret_code);

  assert (ret_code == omp_irc_success);
  if (fr == omp_ifr_cuda)
    assert (strcmp (fr_name, "cuda") == 0);
  else if (fr == omp_ifr_cuda_driver)
    assert (strcmp (fr_name, "cuda_driver") == 0);
  else if (fr == omp_ifr_hip)
    assert (strcmp (fr_name, "hip") == 0);
  else
    assert (0);

  ret_code = omp_irc_no_value;
  int vendor = (int) omp_get_interop_int (obj, omp_ipr_vendor, &ret_code);
  assert (ret_code == omp_irc_success);
  assert (vendor == 11);  /* Nvidia */

  ret_code = omp_irc_no_value;
  const char *vendor_name = omp_get_interop_str (obj, omp_ipr_vendor_name, &ret_code);
  assert (ret_code == omp_irc_success);
  assert (strcmp (vendor_name, "nvidia") == 0);

  ret_code = omp_irc_no_value;
  int dev_num = (int) omp_get_interop_int (obj, omp_ipr_device_num, &ret_code);
  assert (ret_code == omp_irc_success);
  if (dev == DEFAULT_DEVICE)
    assert (dev_num == omp_get_default_device ());
  else
    assert (dev_num == dev);

  /* Platform: N/A.  */
  ret_code = omp_irc_success;
  (void) omp_get_interop_int (obj, omp_ipr_platform, &ret_code);
  assert (ret_code == omp_irc_no_value);
  ret_code = omp_irc_success;
  (void) omp_get_interop_ptr (obj, omp_ipr_platform, &ret_code);
  assert (ret_code == omp_irc_no_value);
  ret_code = omp_irc_success;
  (void) omp_get_interop_str (obj, omp_ipr_platform, &ret_code);
  assert (ret_code == omp_irc_no_value);

  /* Device: int / CUdevice / hipDevice_t -- all internally an 'int'.  */
  ret_code = omp_irc_no_value;
  int fr_device = (int) omp_get_interop_int (obj, omp_ipr_device, &ret_code);

  /* CUDA also starts from 0 and goes to < n with cudaGetDeviceCount(&cn).  */
  assert (ret_code == omp_irc_success);
  assert (fr_device >= 0 && fr_device < omp_get_num_devices ());

  /* Device context: N/A / CUcontext / hipCtx_t -- a pointer.  */
  ret_code = omp_irc_out_of_range;
  void *ctx = omp_get_interop_ptr (obj, omp_ipr_device_context, &ret_code);

  if (fr == omp_ifr_cuda)
    {
      assert (ret_code == omp_irc_no_value);
      assert (ctx == NULL);
    }
  else
    {
      assert (ret_code == omp_irc_success);
      assert (ctx != NULL);
    }

  /* Stream/targetsync: cudaStream_t / CUstream / hipStream_t -- a pointer.  */
  ret_code = omp_irc_out_of_range;
  void *stream = omp_get_interop_ptr (obj, omp_ipr_targetsync, &ret_code);

  if (is_targetsync)  /* no targetsync */
    {
      assert (ret_code == omp_irc_success);
      assert (stream != NULL);
    }
  else
    {
      assert (ret_code == omp_irc_no_value);
      assert (stream == NULL);
    }

  check_type (obj);
  if (fr == omp_ifr_cuda)
    {
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_platform), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device), "int") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device_context), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_targetsync), "cudaStream_t") == 0);
    }
  else if (fr == omp_ifr_cuda_driver)
    {
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_platform), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device), "CUdevice") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device_context), "CUcontext") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_targetsync), "CUstream") == 0);
    }
  else
    {
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_platform), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device), "hipDevice_t") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device_context), "hipCtx_t") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_targetsync), "hipStream_t") == 0);
    }
}


void
check_gcn (omp_interop_t obj, int dev, omp_interop_fr_t expected_fr, _Bool is_targetsync)
{
  assert (obj != omp_interop_none && obj != (omp_interop_t) -1L);

  omp_interop_rc_t ret_code = omp_irc_no_value;
  omp_interop_fr_t fr = (omp_interop_fr_t) omp_get_interop_int (obj, omp_ipr_fr_id, &ret_code);

  assert (ret_code == omp_irc_success);
  assert (fr == expected_fr);

  ret_code = omp_irc_no_value;
  const char *fr_name = omp_get_interop_str (obj, omp_ipr_fr_name, &ret_code);

   assert (ret_code == omp_irc_success);
  if (fr == omp_ifr_hip)
    assert (strcmp (fr_name, "hip") == 0);
  else if (fr == omp_ifr_hsa)
    assert (strcmp (fr_name, "hsa") == 0);
  else
    assert (0);

  ret_code = omp_irc_no_value;
  int vendor = (int) omp_get_interop_int (obj, omp_ipr_vendor, &ret_code);
  assert (ret_code == omp_irc_success);
  assert (vendor == 1);  /* Amd */

  ret_code = omp_irc_no_value;
  const char *vendor_name = omp_get_interop_str (obj, omp_ipr_vendor_name, &ret_code);
  assert (ret_code == omp_irc_success);
  assert (strcmp (vendor_name, "amd") == 0);

  ret_code = omp_irc_no_value;
  int dev_num = (int) omp_get_interop_int (obj, omp_ipr_device_num, &ret_code);
  assert (ret_code == omp_irc_success);
  if (dev == DEFAULT_DEVICE)
    assert (dev_num == omp_get_default_device ());
  else
    assert (dev_num == dev);

  /* Platform: N/A.  */
  ret_code = omp_irc_success;
  (void) omp_get_interop_int (obj, omp_ipr_platform, &ret_code);
  assert (ret_code == omp_irc_no_value);
  ret_code = omp_irc_success;
  (void) omp_get_interop_ptr (obj, omp_ipr_platform, &ret_code);
  assert (ret_code == omp_irc_no_value);
  ret_code = omp_irc_success;
  (void) omp_get_interop_str (obj, omp_ipr_platform, &ret_code);
  assert (ret_code == omp_irc_no_value);

  /* Device: hipDevice_t / hsa_agent_t* -- hip is internally an 'int'.  */
  ret_code = omp_irc_no_value;
  if (fr == omp_ifr_hip)
    {
      /* HIP also starts from 0 and goes to < n as with cudaGetDeviceCount(&cn).  */
      int fr_device = (int) omp_get_interop_int (obj, omp_ipr_device, &ret_code);
      assert (ret_code == omp_irc_success);
      assert (fr_device >= 0 && fr_device < omp_get_num_devices ());
    }
  else
    {
      void *agent = omp_get_interop_ptr (obj, omp_ipr_device, &ret_code);
      assert (ret_code == omp_irc_success);
      assert (agent != NULL);
    }

  /* Device context: hipCtx_t / N/A -- a pointer.  */
  ret_code = omp_irc_out_of_range;
  void *ctx = omp_get_interop_ptr (obj, omp_ipr_device_context, &ret_code);
  if (fr == omp_ifr_hip)
    {
      assert (ret_code == omp_irc_success);
      assert (ctx != NULL);
    }
  else
    {
      assert (ret_code == omp_irc_no_value);
      assert (ctx == NULL);
    }

  /* Stream/targetsync: cudaStream_t / CUstream / hipStream_t -- a pointer.  */
  ret_code = omp_irc_out_of_range;
  void *stream = omp_get_interop_ptr (obj, omp_ipr_targetsync, &ret_code);

  if (is_targetsync)
    {
      assert (ret_code == omp_irc_success);
      assert (stream != NULL);
    }
  else
    {
      assert (ret_code == omp_irc_no_value);
      assert (stream == NULL);
    }

  check_type (obj);
  if (fr == omp_ifr_hip)
    {
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_platform), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device), "hipDevice_t") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device_context), "hipCtx_t") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_targetsync), "hipStream_t") == 0);
    }
  else
    {
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_platform), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device), "hsa_agent_t *") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_device_context), "N/A") == 0);
      assert (strcmp (omp_get_interop_type_desc (obj, omp_ipr_targetsync), "hsa_queue_t *") == 0);
    }
}
