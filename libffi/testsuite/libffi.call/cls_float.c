/* Area:	closure_call
   Purpose:	Check return value float.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run } */
#include "ffitest.h"

static void cls_ret_float_fn(ffi_cif* cif,void* resp,void** args,
			     void* userdata)
 {
   *(float *)resp = *(float *)args[0];
   
   printf("%g: %g\n",*(float *)args[0], 
	  *(float *)resp);
 }

typedef float (*cls_ret_float)(float);

int main (void)
{
  ffi_cif cif;
  static ffi_closure cl;
  ffi_closure *pcl = &cl;
  ffi_type * cl_arg_types[2];
  
  
  cl_arg_types[0] = &ffi_type_float;
  cl_arg_types[1] = NULL;
  
  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_float, cl_arg_types) == FFI_OK);
  
  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_float_fn, NULL)  == FFI_OK);
  ((((cls_ret_float)pcl)(-2122.12)));
  /* { dg-output "\\-2122.12: \\-2122.12\n" } */
  printf("%f \n",(((cls_ret_float)pcl)(-2122.12)));
  /* { dg-output "\\-2122.12: \\-2122.12" } */
  /* { dg-output "\n\-2122.120117" } */
  exit(0);  
}
