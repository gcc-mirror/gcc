/* { dg-do run } */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include "../libgomp.c-c++-common/on_device_arch.h"

#define DEFAULT_DEVICE -99

/* The following assumes that when a nvptx device is available,
   cuda/cuda_driver/hip are supported.
   And that likewise when a gcn device is available that the
   plugin also can not only the HSA but also the HIP library
   such that hsa/hip are supported.
   For the host, omp_interop_none is expected.

   Otherwise, it only does some basic tests without checking
   that the returned result really makes sense.  */

void check_host (int);
void check_nvptx (int);
void check_gcn (int);

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
do_check (int dev)
{
  int num_dev = omp_get_num_devices ();
  const char *dev_type;
  if (dev != DEFAULT_DEVICE)
    omp_set_default_device (dev);
  int is_nvptx = on_device_arch_nvptx ();
  int is_gcn = on_device_arch_gcn ();
  int is_host;
 
  if (dev != DEFAULT_DEVICE)
    is_host = dev == -1 || dev == num_dev;
  else
    {
      int def_dev = omp_get_default_device ();
      is_host = def_dev == -1 || def_dev == num_dev;
    }

  assert (is_nvptx + is_gcn + is_host == 1);

  if (num_dev > 0 && dev != DEFAULT_DEVICE)
    {
      if (is_host)
	omp_set_default_device (0);
      else
	omp_set_default_device (-1);
    }

  if (is_host)
    dev_type = "host";
  else if (is_nvptx)
    dev_type = "nvptx";
  else if (is_gcn)
    dev_type = "gcn";

  printf ("Running on the %s device (%d)\n", dev_type, dev);
  if (is_host)
    check_host (dev);
  else if (is_nvptx)
    check_nvptx (dev);
  else if (is_gcn)
    check_gcn (dev);
}


void
check_host (int dev)
{
  omp_interop_t obj = (omp_interop_t) -1L;
  if (dev == DEFAULT_DEVICE) {
    #pragma omp interop init(target : obj)
  } else {
    #pragma omp interop init(target : obj) device(dev)
  }
  assert (obj == omp_interop_none);
  check_type (obj);

  obj = (omp_interop_t) -1L;
  if (dev == DEFAULT_DEVICE) {
    #pragma omp interop init(target, prefer_type({attr("ompx_foo")}, {attr("ompx_bar"), fr("cuda"), attr("ompx_foobar")},{fr("cuda_driver")}, {fr("hip")}, {fr("hsa")}) : obj)
  } else {
    #pragma omp interop init(target, prefer_type({attr("ompx_foo")}, {attr("ompx_bar"), fr("cuda"), attr("ompx_foobar")},{fr("cuda_driver")}, {fr("hip")}, {fr("hsa")}) : obj) device(dev)
  }
  assert (obj == omp_interop_none);
  check_type (obj);

  obj = (omp_interop_t) -1L;
  if (dev == DEFAULT_DEVICE) {
    #pragma omp interop init(targetsync : obj)
  } else {
    #pragma omp interop init(targetsync : obj) device(dev)
  }
  assert (obj == omp_interop_none);
  check_type (obj);

  obj = (omp_interop_t) -1L;
  if (dev == DEFAULT_DEVICE) {
    #pragma omp interop init(targetsync, prefer_type("cuda","cuda_driver", "hip", "hsa") : obj)
  } else {
    #pragma omp interop init(targetsync, prefer_type("cuda","cuda_driver", "hip", "hsa") : obj) device(dev)
  }
  assert (obj == omp_interop_none);
  check_type (obj);
}


void
check_nvptx (int dev)
{
  for (int variant = 0; variant <= 7; variant++)
    {
      omp_interop_t obj = (omp_interop_t) -1L;
      switch (variant)
	{
	/* Expect 'cuda'.  */
	case 0:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target : obj)
	  } else {
	    #pragma omp interop init(target : obj) device(dev)
	  }
	  break;
	  }
	case 1:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync : obj)
	  } else {
	    #pragma omp interop init(targetsync : obj) device(dev)
	  }
	  break;
	  }
	case 2:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target, prefer_type({attr("ompx_foo")}, {fr("hsa")}, {attr("ompx_bar"), fr("cuda"), attr("ompx_foobar")},{fr("cuda_driver")}, {fr("hip")}) : obj)
	  } else {
	    #pragma omp interop init(target, prefer_type({attr("ompx_foo")}, {fr("hsa")}, {attr("ompx_bar"), fr("cuda"), attr("ompx_foobar")},{fr("cuda_driver")}, {fr("hip")}) : obj) device(dev)
	  }
	  break;
	  }
	case 3:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync, prefer_type("hsa", "cuda", "cuda_driver", "hip") : obj)
	  } else {
	    #pragma omp interop init(targetsync, prefer_type("hsa", "cuda", "cuda_driver", "hip") : obj) device(dev)
	  }
	  break;
	  }

	/* Expect 'cuda_driver'.  */
	case 4:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target, prefer_type("hsa", "cuda_driver", "hip", "cuda") : obj)
	  } else {
	    #pragma omp interop init(target, prefer_type("hsa", "cuda_driver", "hip", "cuda") : obj) device(dev)
	  }
	  break;
	  }
	case 5:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync, prefer_type("hsa", "cuda_driver", "hip", "cuda") : obj)
	  } else {
	    #pragma omp interop init(targetsync, prefer_type("hsa", "cuda_driver", "hip", "cuda") : obj) device(dev)
	  }
	  break;
	  }

	/* Expect 'hip'.  */
	case 6:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target, prefer_type("hsa", "hip", "cuda", "cuda_driver") : obj)
	  } else {
	    #pragma omp interop init(target, prefer_type("hsa", "hip", "cuda", "cuda_driver") : obj) device(dev)
	  }
	  break;
	  }
	case 7:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync, prefer_type("hsa", "hip", "cuda", "cuda_driver") : obj)
	  } else {
	    #pragma omp interop init(targetsync, prefer_type("hsa", "hip", "cuda", "cuda_driver") : obj) device(dev)
	  }
	  break;
	  }
	default:
	  abort ();
	}
      assert (obj != omp_interop_none && obj != (omp_interop_t) -1L);

      omp_interop_rc_t ret_code = omp_irc_no_value;
      omp_interop_fr_t fr = (omp_interop_fr_t) omp_get_interop_int (obj, omp_ipr_fr_id, &ret_code);

      assert (ret_code == omp_irc_success);
      if (variant >= 0 && variant <= 3)
	assert (fr == omp_ifr_cuda);
      else if (variant <= 5)
	assert (fr == omp_ifr_cuda_driver);
      else if (variant <= 7)
	assert (fr == omp_ifr_hip);
      else
	assert (0);

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

      if (variant % 2 == 0)  /* no targetsync */
	{
	  assert (ret_code == omp_irc_no_value);
	  assert (stream == NULL);
	}
      else
	{
	  assert (ret_code == omp_irc_success);
	  assert (stream != NULL);
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

      if (dev == DEFAULT_DEVICE) {
	#pragma omp interop use(obj)
	#pragma omp interop destroy(obj)
      } else {
	#pragma omp interop use(obj) device(dev)
	#pragma omp interop destroy(obj) device(dev)
      }
    }
}


void
check_gcn (int dev)
{
  for (int variant = 0; variant <= 5; variant++)
    {
      omp_interop_t obj = (omp_interop_t) -1L;
      switch (variant)
	{
	/* Expect 'hip'.  */
	case 0:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target : obj)
	  } else {
	    #pragma omp interop init(target : obj) device(dev)
	  }
	  break;
	  }
	case 1:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync : obj)
	  } else {
	    #pragma omp interop init(targetsync : obj) device(dev)
	  }
	  break;
	  }
	case 2:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target, prefer_type({attr("ompx_foo")}, {fr("cuda")}, {fr("cuda_driver")}, {attr("ompx_bar"), fr("hip"), attr("ompx_foobar")},{fr("hsa")}) : obj)
	  } else {
	    #pragma omp interop init(target, prefer_type({attr("ompx_foo")}, {fr("cuda")}, {fr("cuda_driver")}, {attr("ompx_bar"), fr("hip"), attr("ompx_foobar")},{fr("hsa")}) : obj) device(dev)
	  }
	  break;
	  }
	case 3:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync, prefer_type("cuda", "cuda_driver", "hip", "hsa") : obj)
	  } else {
	    #pragma omp interop init(targetsync, prefer_type("cuda", "cuda_driver", "hip", "hsa") : obj) device(dev)
	  }
	  break;
	  }

	/* Expect 'hsa'.  */
	case 4:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(target, prefer_type("cuda", "cuda_driver", "hsa", "hip") : obj)
	  } else {
	    #pragma omp interop init(target, prefer_type("cuda", "cuda_driver", "hsa", "hip") : obj) device(dev)
	  }
	  break;
	  }
	case 5:
	  {
	  if (dev == DEFAULT_DEVICE) {
	    #pragma omp interop init(targetsync, prefer_type("cuda", "cuda_driver", "hsa", "hip") : obj)
	  } else {
	    #pragma omp interop init(targetsync, prefer_type("cuda", "cuda_driver", "hsa", "hip") : obj) device(dev)
	  }
	  break;
	  }
	default:
	  abort ();
	}
      assert (obj != omp_interop_none && obj != (omp_interop_t) -1L);

      omp_interop_rc_t ret_code = omp_irc_no_value;
      omp_interop_fr_t fr = (omp_interop_fr_t) omp_get_interop_int (obj, omp_ipr_fr_id, &ret_code);

      assert (ret_code == omp_irc_success);
      if (variant >= 0 && variant <= 3)
	assert (fr == omp_ifr_hip);
      else if (variant <= 5)
	assert (fr == omp_ifr_hsa);
      else
	assert (0);

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

      if (variant % 2 == 0)  /* no targetsync */
	{
	  assert (ret_code == omp_irc_no_value);
	  assert (stream == NULL);
	}
      else
	{
	  assert (ret_code == omp_irc_success);
	  assert (stream != NULL);
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

      if (dev == DEFAULT_DEVICE) {
	#pragma omp interop use(obj)
	#pragma omp interop destroy(obj)
      } else {
	#pragma omp interop use(obj) device(dev)
	#pragma omp interop destroy(obj) device(dev)
      }
    }
}


int
main ()
{
  do_check (DEFAULT_DEVICE);
  int ndev = omp_get_num_devices ();
  for (int dev = -1; dev < ndev; dev++)
    do_check (dev);
  for (int dev = -1; dev < ndev; dev++)
    {
      omp_set_default_device (dev);
      do_check (DEFAULT_DEVICE);
    }
}
