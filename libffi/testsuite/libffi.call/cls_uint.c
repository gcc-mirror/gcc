/* Area:	closure_call
   Purpose:	Check return value uint.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

static void cls_ret_uint_fn(ffi_cif* cif,void* resp,void** args,
			     void* userdata)
 {
   *(unsigned int*)resp = *(unsigned int *)args[0];

   printf("%d: %d\n",*(unsigned int *)args[0],
	  *(unsigned int *)resp);
 }
typedef unsigned int (*cls_ret_uint)(unsigned int);

int main (void)
{
  ffi_cif cif;
  static ffi_closure cl;
  ffi_closure *pcl = &cl;
  ffi_type * cl_arg_types[2];
  unsigned int res;

  cl_arg_types[0] = &ffi_type_uint32;
  cl_arg_types[1] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_uint32, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_uint_fn, NULL)  == FFI_OK);

  res = (*((cls_ret_uint)pcl))(2147483647);
  /* { dg-output "2147483647: 2147483647" } */
  printf("res: %d\n",res);
  /* { dg-output "\nres: 2147483647" } */

  exit(0);
}
