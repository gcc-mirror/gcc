/* Area:	ffi_call, closure_call
   Purpose:	Check structure passing with different structure size.
		Depending on the ABI. Check overlapping.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

typedef struct cls_struct_16byte {
  int a;
  double b;
  int c;
} cls_struct_16byte;

cls_struct_16byte cls_struct_16byte_fn(struct cls_struct_16byte b1,
			    struct cls_struct_16byte b2)
{
  struct cls_struct_16byte result;

  result.a = b1.a + b2.a;
  result.b = b1.b + b2.b;
  result.c = b1.c + b2.c;

  printf("%d %g %d %d %g %d: %d %g %d\n", b1.a, b1.b, b1.c, b2.a, b2.b, b2.c,
	 result.a, result.b, result.c);

  return result;
}

static void cls_struct_16byte_gn(ffi_cif* cif, void* resp, void** args, void* userdata)
{
  struct cls_struct_16byte b1, b2;

  b1 = *(struct cls_struct_16byte*)(args[0]);
  b2 = *(struct cls_struct_16byte*)(args[1]);

  *(cls_struct_16byte*)resp = cls_struct_16byte_fn(b1, b2);
}

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  void* args_dbl[5];
  ffi_type* cls_struct_fields[4];
  ffi_type cls_struct_type;
  ffi_type* dbl_arg_types[5];

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif

  cls_struct_type.size = 0;
  cls_struct_type.alignment = 0;
  cls_struct_type.type = FFI_TYPE_STRUCT;
  cls_struct_type.elements = cls_struct_fields;

  struct cls_struct_16byte h_dbl = { 7, 8.0, 9 };
  struct cls_struct_16byte j_dbl = { 1, 9.0, 3 };
  struct cls_struct_16byte res_dbl;

  cls_struct_fields[0] = &ffi_type_uint32;
  cls_struct_fields[1] = &ffi_type_double;
  cls_struct_fields[2] = &ffi_type_uint32;
  cls_struct_fields[3] = NULL;

  dbl_arg_types[0] = &cls_struct_type;
  dbl_arg_types[1] = &cls_struct_type;
  dbl_arg_types[2] = NULL;

  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 2, &cls_struct_type,
		     dbl_arg_types) == FFI_OK);

  args_dbl[0] = &h_dbl;
  args_dbl[1] = &j_dbl;
  args_dbl[2] = NULL;

  ffi_call(&cif, FFI_FN(cls_struct_16byte_fn), &res_dbl, args_dbl);
  /* { dg-output "7 8 9 1 9 3: 8 17 12" } */
  printf("res: %d %g %d\n", res_dbl.a, res_dbl.b, res_dbl.c);
  /* { dg-output "\nres: 8 17 12" } */

  CHECK(ffi_prep_closure(pcl, &cif, cls_struct_16byte_gn, NULL) == FFI_OK);

  res_dbl = ((cls_struct_16byte(*)(cls_struct_16byte, cls_struct_16byte))(pcl))(h_dbl, j_dbl);
  /* { dg-output "\n7 8 9 1 9 3: 8 17 12" } */
  printf("res: %d %g %d\n", res_dbl.a, res_dbl.b, res_dbl.c);
  /* { dg-output "\nres: 8 17 12" } */

  exit(0);
}
