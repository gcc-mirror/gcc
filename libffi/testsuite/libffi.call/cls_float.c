/* Area:	closure_call
   Purpose:	Check return value float.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run { xfail mips64*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

static void cls_ret_float_fn(ffi_cif* cif __UNUSED__, void* resp, void** args,
			     void* userdata __UNUSED__)
 {
   *(float *)resp = *(float *)args[0];

   printf("%g: %g\n",*(float *)args[0],
	  *(float *)resp);
 }

typedef float (*cls_ret_float)(float);

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  ffi_type * cl_arg_types[2];
  float res;

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif


  cl_arg_types[0] = &ffi_type_float;
  cl_arg_types[1] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_float, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_float_fn, NULL)  == FFI_OK);
  res = ((((cls_ret_float)pcl)(-2122.12)));
  /* { dg-output "\\-2122.12: \\-2122.12" } */
  printf("res: %.6f\n", res);
  /* { dg-output "\nres: \-2122.120117" } */
  exit(0);
}
