/* Plugin for offload execution on Intel MIC devices.

   Copyright (C) 2014 Free Software Foundation, Inc.

   Contributed by Ilya Verbin <ilya.verbin@intel.com>.

   This file is part of the GNU OpenMP Library (libgomp).

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
#include <libgomp_target.h>
#include "compiler_if_host.h"
#include "main_target_image.h"

#define LD_LIBRARY_PATH_ENV	"LD_LIBRARY_PATH"
#define MIC_LD_LIBRARY_PATH_ENV	"MIC_LD_LIBRARY_PATH"

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


/* Total number of shared libraries with offloading to Intel MIC.  */
static int num_libraries;

/* Pointers to the descriptors, containing pointers to host-side tables and to
   target images.  */
static std::vector< std::pair<void *, void *> > lib_descrs;

/* Thread-safe registration of the main image.  */
static pthread_once_t main_image_is_registered = PTHREAD_ONCE_INIT;


/* Add path specified in LD_LIBRARY_PATH to MIC_LD_LIBRARY_PATH, which is
   required by liboffloadmic.  */
__attribute__((constructor))
static void
set_mic_lib_path (void)
{
  const char *ld_lib_path = getenv (LD_LIBRARY_PATH_ENV);
  const char *mic_lib_path = getenv (MIC_LD_LIBRARY_PATH_ENV);

  if (!ld_lib_path)
    return;

  if (!mic_lib_path)
    setenv (MIC_LD_LIBRARY_PATH_ENV, ld_lib_path, 1);
  else
    {
      size_t len = strlen (mic_lib_path) + strlen (ld_lib_path) + 2;
      bool use_alloca = len <= 2048;
      char *mic_lib_path_new = (char *) (use_alloca ? alloca (len)
						    : malloc (len));
      if (!mic_lib_path_new)
	{
	  fprintf (stderr, "%s: Can't allocate memory\n", __FILE__);
	  exit (1);
	}

      sprintf (mic_lib_path_new, "%s:%s", mic_lib_path, ld_lib_path);
      setenv (MIC_LD_LIBRARY_PATH_ENV, mic_lib_path_new, 1);

      if (!use_alloca)
	free (mic_lib_path_new);
    }
}

extern "C" enum offload_target_type
GOMP_OFFLOAD_get_type (void)
{
  enum offload_target_type res = OFFLOAD_TARGET_TYPE_INTEL_MIC;
  TRACE ("(): return %d", res);
  return res;
}

extern "C" int
GOMP_OFFLOAD_get_num_devices (void)
{
  int res = _Offload_number_of_devices ();
  TRACE ("(): return %d", res);
  return res;
}

/* This should be called from every shared library with offloading.  */
extern "C" void
GOMP_OFFLOAD_register_image (void *host_table, void *target_image)
{
  TRACE ("(host_table = %p, target_image = %p)", host_table, target_image);
  lib_descrs.push_back (std::make_pair (host_table, target_image));
  num_libraries++;
}

static void
offload (const char *file, uint64_t line, int device, const char *name,
	 int num_vars, VarDesc *vars, VarDesc2 *vars2)
{
  OFFLOAD ofld = __offload_target_acquire1 (&device, file, line);
  if (ofld)
    __offload_offload1 (ofld, name, 0, num_vars, vars, vars2, 0, NULL, NULL);
  else
    {
      fprintf (stderr, "%s:%d: Offload target acquire failed\n", file, line);
      exit (1);
    }
}

static void
register_main_image ()
{
  __offload_register_image (&main_target_image);
}

/* Load offload_target_main on target.  */
extern "C" void
GOMP_OFFLOAD_init_device (int device)
{
  TRACE ("");
  pthread_once (&main_image_is_registered, register_main_image);
  offload (__FILE__, __LINE__, device, "__offload_target_init_proc", 0,
	   NULL, NULL);
}

static void
get_target_table (int device, int &num_funcs, int &num_vars, void **&table)
{
  VarDesc vd1[2] = { vd_tgt2host, vd_tgt2host };
  vd1[0].ptr = &num_funcs;
  vd1[0].size = sizeof (num_funcs);
  vd1[1].ptr = &num_vars;
  vd1[1].size = sizeof (num_vars);
  VarDesc2 vd1g[2] = { { "num_funcs", 0 }, { "num_vars", 0 } };

  offload (__FILE__, __LINE__, device, "__offload_target_table_p1", 2,
	   vd1, vd1g);

  int table_size = num_funcs + 2 * num_vars;
  if (table_size > 0)
    {
      table = new void * [table_size];

      VarDesc vd2;
      vd2 = vd_tgt2host;
      vd2.ptr = table;
      vd2.size = table_size * sizeof (void *);
      VarDesc2 vd2g = { "table", 0 };

      offload (__FILE__, __LINE__, device, "__offload_target_table_p2", 1,
	       &vd2, &vd2g);
    }
}

static void
load_lib_and_get_table (int device, int lib_num, mapping_table *&table,
			int &table_size)
{
  struct TargetImage {
    int64_t size;
    /* 10 characters is enough for max int value.  */
    char name[sizeof ("lib0000000000.so")];
    char data[];
  } __attribute__ ((packed));

  void ***host_table_descr = (void ***) lib_descrs[lib_num].first;
  void **host_func_start = host_table_descr[0];
  void **host_func_end   = host_table_descr[1];
  void **host_var_start  = host_table_descr[2];
  void **host_var_end    = host_table_descr[3];

  void **target_image_descr = (void **) lib_descrs[lib_num].second;
  void *image_start = target_image_descr[0];
  void *image_end   = target_image_descr[1];

  TRACE ("() host_table_descr { %p, %p, %p, %p }", host_func_start,
	 host_func_end, host_var_start, host_var_end);
  TRACE ("() target_image_descr { %p, %p }", image_start, image_end);

  int64_t image_size = (uintptr_t) image_end - (uintptr_t) image_start;
  TargetImage *image
    = (TargetImage *) malloc (sizeof (int64_t) + sizeof ("lib0000000000.so")
			      + image_size);
  if (!image)
    {
      fprintf (stderr, "%s: Can't allocate memory\n", __FILE__);
      exit (1);
    }

  image->size = image_size;
  sprintf (image->name, "lib%010d.so", lib_num);
  memcpy (image->data, image_start, image->size);

  TRACE ("() __offload_register_image %s { %p, %d }",
	 image->name, image_start, image->size);
  __offload_register_image (image);

  int tgt_num_funcs = 0;
  int tgt_num_vars = 0;
  void **tgt_table = NULL;
  get_target_table (device, tgt_num_funcs, tgt_num_vars, tgt_table);
  free (image);

  /* The func table contains only addresses, the var table contains addresses
     and corresponding sizes.  */
  int host_num_funcs = host_func_end - host_func_start;
  int host_num_vars  = (host_var_end - host_var_start) / 2;
  TRACE ("() host_num_funcs = %d, tgt_num_funcs = %d",
	 host_num_funcs, tgt_num_funcs);
  TRACE ("() host_num_vars = %d, tgt_num_vars = %d",
	 host_num_vars, tgt_num_vars);
  if (host_num_funcs != tgt_num_funcs)
    {
      fprintf (stderr, "%s: Can't map target functions\n", __FILE__);
      exit (1);
    }
  if (host_num_vars != tgt_num_vars)
    {
      fprintf (stderr, "%s: Can't map target variables\n", __FILE__);
      exit (1);
    }

  table = (mapping_table *) realloc (table, (table_size + host_num_funcs
					     + host_num_vars)
					    * sizeof (mapping_table));
  if (table == NULL)
    {
      fprintf (stderr, "%s: Can't allocate memory\n", __FILE__);
      exit (1);
    }

  for (int i = 0; i < host_num_funcs; i++)
    {
      mapping_table t;
      t.host_start = (uintptr_t) host_func_start[i];
      t.host_end = t.host_start + 1;
      t.tgt_start = (uintptr_t) tgt_table[i];
      t.tgt_end = t.tgt_start + 1;

      TRACE ("() lib %d, func %d:\t0x%llx -- 0x%llx",
	     lib_num, i, t.host_start, t.tgt_start);

      table[table_size++] = t;
    }

  for (int i = 0; i < host_num_vars * 2; i += 2)
    {
      mapping_table t;
      t.host_start = (uintptr_t) host_var_start[i];
      t.host_end = t.host_start + (uintptr_t) host_var_start[i+1];
      t.tgt_start = (uintptr_t) tgt_table[tgt_num_funcs+i];
      t.tgt_end = t.tgt_start + (uintptr_t) tgt_table[tgt_num_funcs+i+1];

      TRACE ("() lib %d, var %d:\t0x%llx (%d) -- 0x%llx (%d)", lib_num, i/2,
	     t.host_start, t.host_end - t.host_start,
	     t.tgt_start, t.tgt_end - t.tgt_start);

      table[table_size++] = t;
    }

  delete [] tgt_table;
}

extern "C" int
GOMP_OFFLOAD_get_table (int device, void *result)
{
  TRACE ("(num_libraries = %d)", num_libraries);

  mapping_table *table = NULL;
  int table_size = 0;

  for (int i = 0; i < num_libraries; i++)
    load_lib_and_get_table (device, i, table, table_size);

  *(void **) result = table;
  return table_size;
}

extern "C" void *
GOMP_OFFLOAD_alloc (int device, size_t size)
{
  TRACE ("(size = %d)", size);

  void *tgt_ptr;
  VarDesc vd1[2] = { vd_host2tgt, vd_tgt2host };
  vd1[0].ptr = &size;
  vd1[0].size = sizeof (size);
  vd1[1].ptr = &tgt_ptr;
  vd1[1].size = sizeof (void *);
  VarDesc2 vd1g[2] = { { "size", 0 }, { "tgt_ptr", 0 } };

  offload (__FILE__, __LINE__, device, "__offload_target_alloc", 2, vd1, vd1g);

  return tgt_ptr;
}

extern "C" void
GOMP_OFFLOAD_free (int device, void *tgt_ptr)
{
  TRACE ("(tgt_ptr = %p)", tgt_ptr);

  VarDesc vd1 = vd_host2tgt;
  vd1.ptr = &tgt_ptr;
  vd1.size = sizeof (void *);
  VarDesc2 vd1g = { "tgt_ptr", 0 };

  offload (__FILE__, __LINE__, device, "__offload_target_free", 1, &vd1, &vd1g);
}

extern "C" void *
GOMP_OFFLOAD_host2dev (int device, void *tgt_ptr, const void *host_ptr,
		       size_t size)
{
  TRACE ("(tgt_ptr = %p, host_ptr = %p, size = %d)", tgt_ptr, host_ptr, size);
  if (!size)
    return tgt_ptr;

  VarDesc vd1[2] = { vd_host2tgt, vd_host2tgt };
  vd1[0].ptr = &tgt_ptr;
  vd1[0].size = sizeof (void *);
  vd1[1].ptr = &size;
  vd1[1].size = sizeof (size);
  VarDesc2 vd1g[2] = { { "tgt_ptr", 0 }, { "size", 0 } };

  offload (__FILE__, __LINE__, device, "__offload_target_host2tgt_p1", 2,
	   vd1, vd1g);

  VarDesc vd2 = vd_host2tgt;
  vd2.ptr = (void *) host_ptr;
  vd2.size = size;
  VarDesc2 vd2g = { "var", 0 };

  offload (__FILE__, __LINE__, device, "__offload_target_host2tgt_p2", 1,
	   &vd2, &vd2g);

  return tgt_ptr;
}

extern "C" void *
GOMP_OFFLOAD_dev2host (int device, void *host_ptr, const void *tgt_ptr,
		       size_t size)
{
  TRACE ("(host_ptr = %p, tgt_ptr = %p, size = %d)", host_ptr, tgt_ptr, size);
  if (!size)
    return host_ptr;

  VarDesc vd1[2] = { vd_host2tgt, vd_host2tgt };
  vd1[0].ptr = &tgt_ptr;
  vd1[0].size = sizeof (void *);
  vd1[1].ptr = &size;
  vd1[1].size = sizeof (size);
  VarDesc2 vd1g[2] = { { "tgt_ptr", 0 }, { "size", 0 } };

  offload (__FILE__, __LINE__, device, "__offload_target_tgt2host_p1", 2,
	   vd1, vd1g);

  VarDesc vd2 = vd_tgt2host;
  vd2.ptr = (void *) host_ptr;
  vd2.size = size;
  VarDesc2 vd2g = { "var", 0 };

  offload (__FILE__, __LINE__, device, "__offload_target_tgt2host_p2", 1,
	   &vd2, &vd2g);

  return host_ptr;
}

extern "C" void
GOMP_OFFLOAD_run (int device, void *tgt_fn, void *tgt_vars)
{
  TRACE ("(tgt_fn = %p, tgt_vars = %p)", tgt_fn, tgt_vars);

  VarDesc vd1[2] = { vd_host2tgt, vd_host2tgt };
  vd1[0].ptr = &tgt_fn;
  vd1[0].size = sizeof (void *);
  vd1[1].ptr = &tgt_vars;
  vd1[1].size = sizeof (void *);
  VarDesc2 vd1g[2] = { { "tgt_fn", 0 }, { "tgt_vars", 0 } };

  offload (__FILE__, __LINE__, device, "__offload_target_run", 2, vd1, vd1g);
}
