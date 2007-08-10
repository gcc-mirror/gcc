/* Area:	closure_call
   Purpose:	Check return value double.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run } */
#include "ffitest.h"

static void cls_ret_double_fn(ffi_cif* cif __UNUSED__, void* resp, void** args,
			      void* userdata __UNUSED__)
 {
   *(double *)resp = *(double *)args[0];

   printf("%f: %f\n",*(double *)args[0],
	  *(double *)resp);
 }
typedef double (*cls_ret_double)(double);

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  ffi_type * cl_arg_types[2];
  double res;

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif

  cl_arg_types[0] = &ffi_type_double;
  cl_arg_types[1] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_double, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_double_fn, NULL)  == FFI_OK);

  res = (*((cls_ret_double)pcl))(21474.789);
  /* { dg-output "21474.789000: 21474.789000" } */
  printf("res: %.6f\n", res);
  /* { dg-output "\nres: 21474.789000" } */

  exit(0);
}
