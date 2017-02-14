/* Plugin for offload execution on Intel MIC devices.

   Copyright (C) 2014-2016 Free Software Foundation, Inc.

   Contributed by Ilya Verbin <ilya.verbin@intel.com>.

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

/* Host side part of a libgomp plugin.  */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utility>
#include <vector>
#include <map>
#include "libgomp-plugin.h"
#include "compiler_if_host.h"
#include "main_target_image.h"
#include "gomp-constants.h"

#define OFFLOAD_ACTIVE_WAIT_ENV	"OFFLOAD_ACTIVE_WAIT"

#ifdef DEBUG
#define TRACE(...)					    \
{							    \
fprintf (stderr, "HOST:\t%s:%s ", __FILE__, __FUNCTION__);  \
fprintf (stderr, __VA_ARGS__);				    \
fprintf (stderr, "\n");					    \
}
#else
#define TRACE { }
#endif


/* Start/end addresses of functions and global variables on a device.  */
typedef std::vector<addr_pair> AddrVect;

/* Addresses for one image and all devices.  */
typedef std::vector<AddrVect> DevAddrVect;

/* Addresses for all images and all devices.  */
typedef std::map<const void *, DevAddrVect> ImgDevAddrMap;

/* Image descriptor needed by __offload_[un]register_image.  */
struct TargetImageDesc {
  int64_t size;
  /* 10 characters is enough for max int value.  */
  char name[sizeof ("lib0000000000.so")];
  char data[];
};

/* Image descriptors, indexed by a pointer obtained from libgomp.  */
typedef std::map<const void *, TargetImageDesc *> ImgDescMap;


/* Total number of available devices.  */
static int num_devices;

/* Total number of shared libraries with offloading to Intel MIC.  */
static int num_images;

/* Two dimensional array: one key is a pointer to image,
   second key is number of device.  Contains a vector of pointer pairs.  */
static ImgDevAddrMap *address_table;

/* Descriptors of all images, registered in liboffloadmic.  */
static ImgDescMap *image_descriptors;

/* Thread-safe registration of the main image.  */
static pthread_once_t main_image_is_registered = PTHREAD_ONCE_INIT;

static VarDesc vd_host2tgt = {
  { 1, 1 },		      /* dst, src			      */
  { 1, 0 },		      /* in, out			      */
  1,			      /* alloc_if			      */
  1,			      /* free_if			      */
  4,			      /* align				      */
  0,			      /* mic_offset			      */
  { 0, 0, 0, 0, 0, 0, 0, 0 }, /* is_static, is_static_dstn, has_length,
				 is_stack_buf, sink_addr, alloc_disp,
				 is_noncont_src, is_noncont_dst	      */
  0,			      /* offset				      */
  0,			      /* size				      */
  1,			      /* count				      */
  0,			      /* alloc				      */
  0,			      /* into				      */
  0			      /* ptr				      */
};

static VarDesc vd_tgt2host = {
  { 1, 1 },		      /* dst, src			      */
  { 0, 1 },		      /* in, out			      */
  1,			      /* alloc_if			      */
  1,			      /* free_if			      */
  4,			      /* align				      */
  0,			      /* mic_offset			      */
  { 0, 0, 0, 0, 0, 0, 0, 0 }, /* is_static, is_static_dstn, has_length,
				 is_stack_buf, sink_addr, alloc_disp,
				 is_noncont_src, is_noncont_dst	      */
  0,			      /* offset				      */
  0,			      /* size				      */
  1,			      /* count				      */
  0,			      /* alloc				      */
  0,			      /* into				      */
  0			      /* ptr				      */
};


__attribute__((constructor))
static void
init (void)
{
  const char *active_wait = getenv (OFFLOAD_ACTIVE_WAIT_ENV);

  /* Disable active wait by default to avoid useless CPU usage.  */
  if (!active_wait)
    setenv (OFFLOAD_ACTIVE_WAIT_ENV, "0", 0);

  address_table = new ImgDevAddrMap;
  image_descriptors = new ImgDescMap;
  num_devices = _Offload_number_of_devices ();
}

extern "C" const char *
GOMP_OFFLOAD_get_name (void)
{
  const char *res = "intelmic";
  TRACE ("(): return %s", res);
  return res;
}

extern "C" unsigned int
GOMP_OFFLOAD_get_caps (void)
{
  unsigned int res = GOMP_OFFLOAD_CAP_OPENMP_400;
  TRACE ("(): return %x", res);
  return res;
}

extern "C" int
GOMP_OFFLOAD_get_type (void)
{
  enum offload_target_type res = OFFLOAD_TARGET_TYPE_INTEL_MIC;
  TRACE ("(): return %d", res);
  return res;
}

extern "C" int
GOMP_OFFLOAD_get_num_devices (void)
{
  TRACE ("(): return %d", num_devices);
  return num_devices;
}

static bool
offload (const char *file, uint64_t line, int device, const char *name,
	 int num_vars, VarDesc *vars, const void **async_data)
{
  OFFLOAD ofld = __offload_target_acquire1 (&device, file, line);
  if (ofld)
    {
      if (async_data == NULL)
	return __offload_offload1 (ofld, name, 0, num_vars, vars, NULL, 0,
				   NULL, NULL);
      else
	{
	  OffloadFlags flags;
	  flags.flags = 0;
	  flags.bits.omp_async = 1;
	  return __offload_offload3 (ofld, name, 0, num_vars, vars, NULL, 0,
				     NULL, async_data, 0, NULL, flags, NULL);
	}
    }
  else
    {
      GOMP_PLUGIN_error ("%s:%d: Offload target acquire failed\n", file, line);
      return false;
    }
}

static void
register_main_image ()
{
  /* Do not check the return value, because old versions of liboffloadmic did
     not have return values.  */
  __offload_register_image (&main_target_image);

  /* liboffloadmic will call GOMP_PLUGIN_target_task_completion when
     asynchronous task on target is completed.  */
  __offload_register_task_callback (GOMP_PLUGIN_target_task_completion);
}

/* liboffloadmic loads and runs offload_target_main on all available devices
   during a first call to offload ().  */
extern "C" bool
GOMP_OFFLOAD_init_device (int device)
{
  TRACE ("(device = %d)", device);
  pthread_once (&main_image_is_registered, register_main_image);
  return offload (__FILE__, __LINE__, device, "__offload_target_init_proc", 0,
		  NULL, NULL);
}

extern "C" bool
GOMP_OFFLOAD_fini_device (int device)
{
  TRACE ("(device = %d)", device);

  /* liboffloadmic will finalize target processes on all available devices.  */
  __offload_unregister_image (&main_target_image);
  return true;
}

static bool
get_target_table (int device, int &num_funcs, int &num_vars, void **&table)
{
  VarDesc vd1[2] = { vd_tgt2host, vd_tgt2host };
  vd1[0].ptr = &num_funcs;
  vd1[0].size = sizeof (num_funcs);
  vd1[1].ptr = &num_vars;
  vd1[1].size = sizeof (num_vars);

  if (!offload (__FILE__, __LINE__, device, "__offload_target_table_p1", 2,
		vd1, NULL))
    return false;

  int table_size = num_funcs + 2 * num_vars;
  if (table_size > 0)
    {
      table = new void * [table_size];

      VarDesc vd2;
      vd2 = vd_tgt2host;
      vd2.ptr = table;
      vd2.size = table_size * sizeof (void *);

      return offload (__FILE__, __LINE__, device, "__offload_target_table_p2",
		      1, &vd2, NULL);
    }
  return true;
}

/* Offload TARGET_IMAGE to all available devices and fill address_table with
   corresponding target addresses.  */

static bool
offload_image (const void *target_image)
{
  void *image_start = ((void **) target_image)[0];
  void *image_end   = ((void **) target_image)[1];

  TRACE ("(target_image = %p { %p, %p })",
	 target_image, image_start, image_end);

  int64_t image_size = (uintptr_t) image_end - (uintptr_t) image_start;
  TargetImageDesc *image = (TargetImageDesc *) malloc (offsetof (TargetImageDesc, data)
						       + image_size);
  if (!image)
    {
      GOMP_PLUGIN_error ("%s: Can't allocate memory\n", __FILE__);
      return false;
    }

  image->size = image_size;
  sprintf (image->name, "lib%010d.so", num_images++);
  memcpy (image->data, image_start, image->size);

  TRACE ("() __offload_register_image %s { %p, %d }",
	 image->name, image_start, image->size);
  /* Do not check the return value, because old versions of liboffloadmic did
     not have return values.  */
  __offload_register_image (image);

  /* Receive tables for target_image from all devices.  */
  DevAddrVect dev_table;
  bool ret = true;
  for (int dev = 0; dev < num_devices; dev++)
    {
      int num_funcs = 0;
      int num_vars = 0;
      void **table = NULL;

      ret &= get_target_table (dev, num_funcs, num_vars, table);

      AddrVect curr_dev_table;

      for (int i = 0; i < num_funcs; i++)
	{
	  addr_pair tgt_addr;
	  tgt_addr.start = (uintptr_t) table[i];
	  tgt_addr.end = tgt_addr.start + 1;
	  TRACE ("() func %d:\t0x%llx..0x%llx", i,
		 tgt_addr.start, tgt_addr.end);
	  curr_dev_table.push_back (tgt_addr);
	}

      for (int i = 0; i < num_vars; i++)
	{
	  addr_pair tgt_addr;
	  tgt_addr.start = (uintptr_t) table[num_funcs+i*2];
	  tgt_addr.end = tgt_addr.start + (uintptr_t) table[num_funcs+i*2+1];
	  TRACE ("() var %d:\t0x%llx..0x%llx", i, tgt_addr.start, tgt_addr.end);
	  curr_dev_table.push_back (tgt_addr);
	}

      dev_table.push_back (curr_dev_table);
      delete [] table;
    }

  address_table->insert (std::make_pair (target_image, dev_table));
  image_descriptors->insert (std::make_pair (target_image, image));
  return ret;
}

/* Return the libgomp version number we're compatible with.  There is
   no requirement for cross-version compatibility.  */

extern "C" unsigned
GOMP_OFFLOAD_version (void)
{
  return GOMP_VERSION;
}

extern "C" int
GOMP_OFFLOAD_load_image (int device, const unsigned version,
			 const void *target_image, addr_pair **result)
{
  TRACE ("(device = %d, target_image = %p)", device, target_image);

  if (GOMP_VERSION_DEV (version) > GOMP_VERSION_INTEL_MIC)
    {
      GOMP_PLUGIN_error ("Offload data incompatible with intelmic plugin"
			 " (expected %u, received %u)",
			 GOMP_VERSION_INTEL_MIC, GOMP_VERSION_DEV (version));
      return -1;
    }

  /* If target_image is already present in address_table, then there is no need
     to offload it.  */
  if (address_table->count (target_image) == 0)
    {
      /* If fail, return -1 as error code.  */
      if (!offload_image (target_image))
	return -1;
    }

  AddrVect *curr_dev_table = &(*address_table)[target_image][device];
  int table_size = curr_dev_table->size ();
  addr_pair *table = (addr_pair *) malloc (table_size * sizeof (addr_pair));
  if (table == NULL)
    {
      GOMP_PLUGIN_error ("%s: Can't allocate memory\n", __FILE__);
      return -1;
    }

  std::copy (curr_dev_table->begin (), curr_dev_table->end (), table);
  *result = table;
  return table_size;
}

extern "C" bool
GOMP_OFFLOAD_unload_image (int device, unsigned version,
			   const void *target_image)
{
  if (GOMP_VERSION_DEV (version) > GOMP_VERSION_INTEL_MIC)
    {
      GOMP_PLUGIN_error ("Offload data incompatible with intelmic plugin"
			 " (expected %u, received %u)",
			 GOMP_VERSION_INTEL_MIC, GOMP_VERSION_DEV (version));
      return false;
    }

  TRACE ("(device = %d, target_image = %p)", device, target_image);

  /* liboffloadmic unloads the image from all available devices.  */
  if (image_descriptors->count (target_image) > 0)
    {
      TargetImageDesc *image_desc = (*image_descriptors)[target_image];
      __offload_unregister_image (image_desc);
      free (image_desc);

      address_table->erase (target_image);
      image_descriptors->erase (target_image);
    }
  return true;
}

extern "C" void *
GOMP_OFFLOAD_alloc (int device, size_t size)
{
  TRACE ("(device = %d, size = %d)", device, size);

  void *tgt_ptr;
  VarDesc vd[2] = { vd_host2tgt, vd_tgt2host };
  vd[0].ptr = &size;
  vd[0].size = sizeof (size);
  vd[1].ptr = &tgt_ptr;
  vd[1].size = sizeof (void *);

  if (!offload (__FILE__, __LINE__, device, "__offload_target_alloc", 2,
		vd, NULL))
    return NULL;

  return tgt_ptr;
}

extern "C" bool
GOMP_OFFLOAD_free (int device, void *tgt_ptr)
{
  TRACE ("(device = %d, tgt_ptr = %p)", device, tgt_ptr);

  VarDesc vd = vd_host2tgt;
  vd.ptr = &tgt_ptr;
  vd.size = sizeof (void *);

  return offload (__FILE__, __LINE__, device, "__offload_target_free", 1,
		  &vd, NULL);
}

extern "C" bool
GOMP_OFFLOAD_host2dev (int device, void *tgt_ptr, const void *host_ptr,
		       size_t size)
{
  TRACE ("(device = %d, tgt_ptr = %p, host_ptr = %p, size = %d)",
	 device, tgt_ptr, host_ptr, size);
  if (!size)
    return true;

  VarDesc vd1[2] = { vd_host2tgt, vd_host2tgt };
  vd1[0].ptr = &tgt_ptr;
  vd1[0].size = sizeof (void *);
  vd1[1].ptr = &size;
  vd1[1].size = sizeof (size);

  if (!offload (__FILE__, __LINE__, device, "__offload_target_host2tgt_p1", 2,
		vd1, NULL))
    return false;

  VarDesc vd2 = vd_host2tgt;
  vd2.ptr = (void *) host_ptr;
  vd2.size = size;

  return offload (__FILE__, __LINE__, device, "__offload_target_host2tgt_p2", 1,
		  &vd2, NULL);
}

extern "C" bool
GOMP_OFFLOAD_dev2host (int device, void *host_ptr, const void *tgt_ptr,
		       size_t size)
{
  TRACE ("(device = %d, host_ptr = %p, tgt_ptr = %p, size = %d)",
	 device, host_ptr, tgt_ptr, size);
  if (!size)
    return true;

  VarDesc vd1[2] = { vd_host2tgt, vd_host2tgt };
  vd1[0].ptr = &tgt_ptr;
  vd1[0].size = sizeof (void *);
  vd1[1].ptr = &size;
  vd1[1].size = sizeof (size);

  if (!offload (__FILE__, __LINE__, device, "__offload_target_tgt2host_p1", 2,
		vd1, NULL))
    return false;

  VarDesc vd2 = vd_tgt2host;
  vd2.ptr = (void *) host_ptr;
  vd2.size = size;

  return offload (__FILE__, __LINE__, device, "__offload_target_tgt2host_p2", 1,
		  &vd2, NULL);
}

extern "C" bool
GOMP_OFFLOAD_dev2dev (int device, void *dst_ptr, const void *src_ptr,
		      size_t size)
{
  TRACE ("(device = %d, dst_ptr = %p, src_ptr = %p, size = %d)",
	 device, dst_ptr, src_ptr, size);
  if (!size)
    return true;

  VarDesc vd[3] = { vd_host2tgt, vd_host2tgt, vd_host2tgt };
  vd[0].ptr = &dst_ptr;
  vd[0].size = sizeof (void *);
  vd[1].ptr = &src_ptr;
  vd[1].size = sizeof (void *);
  vd[2].ptr = &size;
  vd[2].size = sizeof (size);

  return offload (__FILE__, __LINE__, device, "__offload_target_tgt2tgt", 3,
		  vd, NULL);
}

extern "C" void
GOMP_OFFLOAD_async_run (int device, void *tgt_fn, void *tgt_vars,
			void **, void *async_data)
{
  TRACE ("(device = %d, tgt_fn = %p, tgt_vars = %p, async_data = %p)", device,
	 tgt_fn, tgt_vars, async_data);

  VarDesc vd[2] = { vd_host2tgt, vd_host2tgt };
  vd[0].ptr = &tgt_fn;
  vd[0].size = sizeof (void *);
  vd[1].ptr = &tgt_vars;
  vd[1].size = sizeof (void *);

  offload (__FILE__, __LINE__, device, "__offload_target_run", 2, vd,
	   (const void **) async_data);
}

extern "C" void
GOMP_OFFLOAD_run (int device, void *tgt_fn, void *tgt_vars, void **)
{
  TRACE ("(device = %d, tgt_fn = %p, tgt_vars = %p)", device, tgt_fn, tgt_vars);

  GOMP_OFFLOAD_async_run (device, tgt_fn, tgt_vars, NULL, NULL);
}
