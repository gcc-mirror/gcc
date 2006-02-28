/* Area:	ffi_call, closure_call
   Purpose:	Check passing of multiple signed short values.
   Limitations:	none.
   PR:		PR13221.
   Originator:	<andreast@gcc.gnu.org> 20031129  */

/* { dg-do run { xfail mips64*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

signed short test_func_fn(signed short a1, signed short a2)
{
  signed short result;

  result = a1 + a2;

  printf("%d %d: %d\n", a1, a2, result);

  return result;

}

static void test_func_gn(ffi_cif *cif __UNUSED__, void *rval, void **avals,
			 void *data __UNUSED__)
{
  signed short a1, a2;

  a1 = *(signed short *)avals[0];
  a2 = *(signed short *)avals[1];

  *(ffi_arg *)rval = test_func_fn(a1, a2);

}

typedef signed short (*test_type)(signed short, signed short);

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  void * args_dbl[3];
  ffi_type * cl_arg_types[3];
  ffi_arg res_call;
  unsigned short a, b, res_closure;

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif

  a = 2;
  b = 32765;

  args_dbl[0] = &a;
  args_dbl[1] = &b;
  args_dbl[2] = NULL;

  cl_arg_types[0] = &ffi_type_sshort;
  cl_arg_types[1] = &ffi_type_sshort;
  cl_arg_types[2] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 2,
		     &ffi_type_sshort, cl_arg_types) == FFI_OK);

  ffi_call(&cif, FFI_FN(test_func_fn), &res_call, args_dbl);
  /* { dg-output "2 32765: 32767" } */
  printf("res: %d\n", (unsigned short)res_call);
  /* { dg-output "\nres: 32767" } */

  CHECK(ffi_prep_closure(pcl, &cif, test_func_gn, NULL)  == FFI_OK);

  res_closure = (*((test_type)pcl))(2, 32765);
  /* { dg-output "\n2 32765: 32767" } */
  printf("res: %d\n", res_closure);
  /* { dg-output "\nres: 32767" } */

  exit(0);
}
