/* Area:	ffi_call, closure_call
   Purpose:	Check passing of multiple unsigned char values.
   Limitations:	none.
   PR:		PR13221.
   Originator:	<andreast@gcc.gnu.org> 20031129  */

/* { dg-do run { xfail mips64*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

unsigned char test_func_fn(unsigned char a1, unsigned char a2,
			   unsigned char a3, unsigned char a4)
{
  unsigned char result;

  result = a1 + a2 + a3 + a4;

  printf("%d %d %d %d: %d\n", a1, a2, a3, a4, result);

  return result;

}

static void test_func_gn(ffi_cif *cif __UNUSED__, void *rval, void **avals,
			 void *data __UNUSED__)
{
  unsigned char a1, a2, a3, a4;

  a1 = *(unsigned char *)avals[0];
  a2 = *(unsigned char *)avals[1];
  a3 = *(unsigned char *)avals[2];
  a4 = *(unsigned char *)avals[3];

  *(ffi_arg *)rval = test_func_fn(a1, a2, a3, a4);

}

typedef unsigned char (*test_type)(unsigned char, unsigned char,
				   unsigned char, unsigned char);

void test_func(ffi_cif *cif __UNUSED__, void *rval __UNUSED__, void **avals,
	       void *data __UNUSED__)
{
  printf("%d %d %d %d\n", *(unsigned char *)avals[0],
	 *(unsigned char *)avals[1], *(unsigned char *)avals[2],
	 *(unsigned char *)avals[3]);
}
int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  void * args_dbl[5];
  ffi_type * cl_arg_types[5];
  ffi_arg res_call;
  unsigned char a, b, c, d, res_closure;

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif

  a = 1;
  b = 2;
  c = 127;
  d = 125;

  args_dbl[0] = &a;
  args_dbl[1] = &b;
  args_dbl[2] = &c;
  args_dbl[3] = &d;
  args_dbl[4] = NULL;

  cl_arg_types[0] = &ffi_type_uchar;
  cl_arg_types[1] = &ffi_type_uchar;
  cl_arg_types[2] = &ffi_type_uchar;
  cl_arg_types[3] = &ffi_type_uchar;
  cl_arg_types[4] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 4,
		     &ffi_type_uchar, cl_arg_types) == FFI_OK);

  ffi_call(&cif, FFI_FN(test_func_fn), &res_call, args_dbl);
  /* { dg-output "1 2 127 125: 255" } */
  printf("res: %d\n", (unsigned char)res_call);
  /* { dg-output "\nres: 255" } */

  CHECK(ffi_prep_closure(pcl, &cif, test_func_gn, NULL)  == FFI_OK);

  res_closure = (*((test_type)pcl))(1, 2, 127, 125);
  /* { dg-output "\n1 2 127 125: 255" } */
  printf("res: %d\n", res_closure);
  /* { dg-output "\nres: 255" } */

  exit(0);
}
