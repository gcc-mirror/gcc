/* Area:	closure_call
   Purpose:	Check return value ushort.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run } */
#include "ffitest.h"

static void cls_ret_ushort_fn(ffi_cif* cif __UNUSED__, void* resp, void** args,
			      void* userdata __UNUSED__)
{
  *(ffi_arg*)resp = *(unsigned short *)args[0];

  printf("%d: %d\n",*(unsigned short *)args[0],
	 (int)*(ffi_arg *)(resp));
}
typedef unsigned short (*cls_ret_ushort)(unsigned short);

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  ffi_type * cl_arg_types[2];
  unsigned short res;

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif

  cl_arg_types[0] = &ffi_type_ushort;
  cl_arg_types[1] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_ushort, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, cls_ret_ushort_fn, NULL)  == FFI_OK);

  res = (*((cls_ret_ushort)pcl))(65535);
  /* { dg-output "65535: 65535" } */
  printf("res: %d\n",res);
  /* { dg-output "\nres: 65535" } */

  exit(0);
}
