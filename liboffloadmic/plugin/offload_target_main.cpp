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

/* Target side part of a libgomp plugin.  */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "compiler_if_target.h"


#ifdef DEBUG
#define TRACE(...)					      \
{							      \
fprintf (stderr, "TARGET:\t%s:%s ", __FILE__, __FUNCTION__);  \
fprintf (stderr, __VA_ARGS__);				      \
fprintf (stderr, "\n");					      \
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

/* Pointer to the descriptor of the last loaded shared library.  */
static void *last_loaded_library = NULL;

/* Pointer and size of the variable, used in __offload_target_host2tgt_p[12]
   and __offload_target_tgt2host_p[12].  */
static void *last_var_ptr = NULL;
static int last_var_size = 0;


/* Override the corresponding functions from libgomp.  */
extern "C" int
omp_is_initial_device (void) __GOMP_NOTHROW
{
  return 0;
}

extern "C" int32_t
omp_is_initial_device_ (void)
{
  return omp_is_initial_device ();
}


/* Dummy function needed for the initialization of target process during the
   first call to __offload_offload1.  */
static void
__offload_target_init_proc (OFFLOAD ofldt)
{
  TRACE ("");
}

/* Collect addresses of the offload functions and of the global variables from
   the library descriptor and send them to host.
   Part 1: Send num_funcs and num_vars to host.  */
static void
__offload_target_table_p1 (OFFLOAD ofldt)
{
  void ***lib_descr = (void ***) last_loaded_library;

  if (lib_descr == NULL)
    {
      TRACE ("");
      fprintf (stderr, "Error! No shared libraries loaded on target.\n");
      return;
    }

  void **func_table_begin = lib_descr[0];
  void **func_table_end   = lib_descr[1];
  void **var_table_begin  = lib_descr[2];
  void **var_table_end    = lib_descr[3];

  /* The func table contains only addresses, the var table contains addresses
     and corresponding sizes.  */
  int num_funcs = func_table_end - func_table_begin;
  int num_vars = (var_table_end - var_table_begin) / 2;
  TRACE ("(num_funcs = %d, num_vars = %d)", num_funcs, num_vars);

  VarDesc vd[2] = { vd_tgt2host, vd_tgt2host };
  vd[0].ptr = &num_funcs;
  vd[0].size = sizeof (num_funcs);
  vd[1].ptr = &num_vars;
  vd[1].size = sizeof (num_vars);

  __offload_target_enter (ofldt, 2, vd, NULL);
  __offload_target_leave (ofldt);
}

/* Part 2: Send the table with addresses to host.  */
static void
__offload_target_table_p2 (OFFLOAD ofldt)
{
  void ***lib_descr = (void ***) last_loaded_library;
  void **func_table_begin = lib_descr[0];
  void **func_table_end   = lib_descr[1];
  void **var_table_begin  = lib_descr[2];
  void **var_table_end    = lib_descr[3];

  int num_funcs = func_table_end - func_table_begin;
  int num_vars = (var_table_end - var_table_begin) / 2;
  int table_size = (num_funcs + 2 * num_vars) * sizeof (void *);
  void **table = (void **) malloc (table_size);
  TRACE ("(table_size = %d)", table_size);

  VarDesc vd = vd_tgt2host;
  vd.ptr = table;
  vd.size = table_size;

  __offload_target_enter (ofldt, 1, &vd, NULL);

  void **p;
  int i = 0;
  for (p = func_table_begin; p < func_table_end; p++, i++)
    table[i] = *p;

  for (p = var_table_begin; p < var_table_end; p++, i++)
    table[i] = *p;

  __offload_target_leave (ofldt);
  free (table);
}

/* Allocate size bytes and send a pointer to the allocated memory to host.  */
static void
__offload_target_alloc (OFFLOAD ofldt)
{
  size_t size = 0;
  void *ptr = NULL;

  VarDesc vd[2] = { vd_host2tgt, vd_tgt2host };
  vd[0].ptr = &size;
  vd[0].size = sizeof (size);
  vd[1].ptr = &ptr;
  vd[1].size = sizeof (void *);

  __offload_target_enter (ofldt, 2, vd, NULL);
  ptr = malloc (size);
  TRACE ("(size = %d): ptr = %p", size, ptr);
  __offload_target_leave (ofldt);
}

/* Free the memory space pointed to by ptr.  */
static void
__offload_target_free (OFFLOAD ofldt)
{
  void *ptr = 0;

  VarDesc vd = vd_host2tgt;
  vd.ptr = &ptr;
  vd.size = sizeof (void *);

  __offload_target_enter (ofldt, 1, &vd, NULL);
  TRACE ("(ptr = %p)", ptr);
  free (ptr);
  __offload_target_leave (ofldt);
}

/* Receive var_size bytes from host and store to var_ptr.
   Part 1: Receive var_ptr and var_size from host.  */
static void
__offload_target_host2tgt_p1 (OFFLOAD ofldt)
{
  void *var_ptr = NULL;
  size_t var_size = 0;

  VarDesc vd[2] = { vd_host2tgt, vd_host2tgt };
  vd[0].ptr = &var_ptr;
  vd[0].size = sizeof (void *);
  vd[1].ptr = &var_size;
  vd[1].size = sizeof (var_size);

  __offload_target_enter (ofldt, 2, vd, NULL);
  TRACE ("(var_ptr = %p, var_size = %d)", var_ptr, var_size);
  last_var_ptr = var_ptr;
  last_var_size = var_size;
  __offload_target_leave (ofldt);
}

/* Part 2: Receive the data from host.  */
static void
__offload_target_host2tgt_p2 (OFFLOAD ofldt)
{
  TRACE ("(last_var_ptr = %p, last_var_size = %d)",
	 last_var_ptr, last_var_size);

  VarDesc vd = vd_host2tgt;
  vd.ptr = last_var_ptr;
  vd.size = last_var_size;

  __offload_target_enter (ofldt, 1, &vd, NULL);
  __offload_target_leave (ofldt);
}

/* Send var_size bytes from var_ptr to host.
   Part 1: Receive var_ptr and var_size from host.  */
static void
__offload_target_tgt2host_p1 (OFFLOAD ofldt)
{
  void *var_ptr = NULL;
  size_t var_size = 0;

  VarDesc vd[2] = { vd_host2tgt, vd_host2tgt };
  vd[0].ptr = &var_ptr;
  vd[0].size = sizeof (void *);
  vd[1].ptr = &var_size;
  vd[1].size = sizeof (var_size);

  __offload_target_enter (ofldt, 2, vd, NULL);
  TRACE ("(var_ptr = %p, var_size = %d)", var_ptr, var_size);
  last_var_ptr = var_ptr;
  last_var_size = var_size;
  __offload_target_leave (ofldt);
}

/* Part 2: Send the data to host.  */
static void
__offload_target_tgt2host_p2 (OFFLOAD ofldt)
{
  TRACE ("(last_var_ptr = %p, last_var_size = %d)",
	 last_var_ptr, last_var_size);

  VarDesc vd = vd_tgt2host;
  vd.ptr = last_var_ptr;
  vd.size = last_var_size;

  __offload_target_enter (ofldt, 1, &vd, NULL);
  __offload_target_leave (ofldt);
}

/* Copy SIZE bytes from SRC_PTR to DST_PTR.  */
static void
__offload_target_tgt2tgt (OFFLOAD ofldt)
{
  void *src_ptr = NULL;
  void *dst_ptr = NULL;
  size_t size = 0;

  VarDesc vd[3] = { vd_host2tgt, vd_host2tgt, vd_host2tgt };
  vd[0].ptr = &dst_ptr;
  vd[0].size = sizeof (void *);
  vd[1].ptr = &src_ptr;
  vd[1].size = sizeof (void *);
  vd[2].ptr = &size;
  vd[2].size = sizeof (size);

  __offload_target_enter (ofldt, 3, vd, NULL);
  TRACE ("(dst_ptr = %p, src_ptr = %p, size = %d)", dst_ptr, src_ptr, size);
  memcpy (dst_ptr, src_ptr, size);
  __offload_target_leave (ofldt);
}

/* Call offload function by the address fn_ptr and pass vars_ptr to it.  */
static void
__offload_target_run (OFFLOAD ofldt)
{
  void *fn_ptr;
  void *vars_ptr;

  VarDesc vd[2] = { vd_host2tgt, vd_host2tgt };
  vd[0].ptr = &fn_ptr;
  vd[0].size = sizeof (void *);
  vd[1].ptr = &vars_ptr;
  vd[1].size = sizeof (void *);

  __offload_target_enter (ofldt, 2, vd, NULL);
  TRACE ("(fn_ptr = %p, vars_ptr = %p)", fn_ptr, vars_ptr);
  void (*fn)(void *) = (void (*)(void *)) fn_ptr;
  fn (vars_ptr);
  __offload_target_leave (ofldt);
}


/* This should be called from every library with offloading.  */
extern "C" void
target_register_lib (const void *target_table)
{
  TRACE ("(target_table = %p { %p, %p, %p, %p })", target_table,
	 ((void **) target_table)[0], ((void **) target_table)[1],
	 ((void **) target_table)[2], ((void **) target_table)[3]);

  last_loaded_library = (void *) target_table;
}

/* Use __offload_target_main from liboffload.  */
int
main (int argc, char **argv)
{
  __offload_target_main ();
  return 0;
}


/* Register offload_target_main's functions in the liboffload.  */

struct Entry {
  const char *name;
  void *func;
};

#define REGISTER(f)				      \
extern "C" const Entry __offload_target_##f##_$entry  \
__attribute__ ((section(".OffloadEntryTable."))) = {  \
  "__offload_target_"#f,			      \
  (void *) __offload_target_##f			      \
}
REGISTER (init_proc);
REGISTER (table_p1);
REGISTER (table_p2);
REGISTER (alloc);
REGISTER (free);
REGISTER (host2tgt_p1);
REGISTER (host2tgt_p2);
REGISTER (tgt2host_p1);
REGISTER (tgt2host_p2);
REGISTER (tgt2tgt);
REGISTER (run);
#undef REGISTER
