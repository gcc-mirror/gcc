/* Area:	closure_call
   Purpose:	Check return value long long.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run } */
#include "ffitest.h"

static void cls_ret_ulonglong_fn(ffi_cif* cif,void* resp,void** args,
			     void* userdata)
 {
   *(unsigned long long *)resp=  *(unsigned long long *)args[0];
  
   printf("%llu: %llu\n",*(unsigned long long *)args[0], 
	  *(unsigned long long *)resp);
 }
typedef unsigned long long (*cls_ret_ulonglong)(unsigned long long);

int main (void)
{
  ffi_cif cif;
  static ffi_closure cl;
  ffi_closure *pcl = &cl;
  ffi_type * cl_arg_types[2];
  
  cl_arg_types[0] = &ffi_type_uint64;
  cl_arg_types[1] = NULL;
  
  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_uint64, cl_arg_types) == FFI_OK);
  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_ulonglong_fn, NULL)  == FFI_OK);
  (*((cls_ret_ulonglong)pcl))(214LL);
  /* { dg-output "214: 214" } */
  (*((cls_ret_ulonglong)pcl))(9223372035854775808LL);
  /* { dg-output "\n9223372035854775808: 9223372035854775808" } */
  
  exit(0);  
}
