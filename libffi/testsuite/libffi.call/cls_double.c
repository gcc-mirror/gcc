/* Area:	closure_call
   Purpose:	Check return value double.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

static void cls_ret_double_fn(ffi_cif* cif,void* resp,void** args,
			     void* userdata)
 {
   *(double *)resp = *(double *)args[0];

   printf("%f: %f\n",*(double *)args[0],
	  *(double *)resp);
 }
typedef double (*cls_ret_double)(double);

int main (void)
{
  ffi_cif cif;
  static ffi_closure cl;
  ffi_closure *pcl = &cl;
  ffi_type * cl_arg_types[2];


  cl_arg_types[0] = &ffi_type_double;
  cl_arg_types[1] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_double, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_double_fn, NULL)  == FFI_OK);

  (*((cls_ret_double)pcl))(21474.789);
  /* { dg-output "21474.789000: 21474.789000" } */
  exit(0);
}
