/* Area:	ffi_call, closure_call
   Purpose:	Check structure passing with different structure size.
		Especially with small structures which may fit in one
		register. Depending on the ABI.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030902	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

typedef struct cls_struct_1_1byte {
  unsigned char a;
} cls_struct_1_1byte;

cls_struct_1_1byte cls_struct_1_1byte_fn(struct cls_struct_1_1byte a1,
			    struct cls_struct_1_1byte a2)
{
  struct cls_struct_1_1byte result;

  result.a = a1.a + a2.a;

  printf("%d %d: %d\n", a1.a, a2.a, result.a);

  return  result;
}

static void
cls_struct_1_1byte_gn(ffi_cif* cif, void* resp, void** args, void* userdata)
{

  struct cls_struct_1_1byte a1, a2;

  a1 = *(struct cls_struct_1_1byte*)(args[0]);
  a2 = *(struct cls_struct_1_1byte*)(args[1]);

  *(cls_struct_1_1byte*)resp = cls_struct_1_1byte_fn(a1, a2);
}

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  void* args_dbl[5];
  ffi_type* cls_struct_fields[2];
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

  struct cls_struct_1_1byte g_dbl = { 12 };
  struct cls_struct_1_1byte f_dbl = { 178 };
  struct cls_struct_1_1byte res_dbl;

  cls_struct_fields[0] = &ffi_type_uchar;
  cls_struct_fields[1] = NULL;

  dbl_arg_types[0] = &cls_struct_type;
  dbl_arg_types[1] = &cls_struct_type;
  dbl_arg_types[2] = NULL;

  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 2, &cls_struct_type,
		     dbl_arg_types) == FFI_OK);

  args_dbl[0] = &g_dbl;
  args_dbl[1] = &f_dbl;
  args_dbl[2] = NULL;

  ffi_call(&cif, FFI_FN(cls_struct_1_1byte_fn), &res_dbl, args_dbl);
  /* { dg-output "12 178: 190" } */
  printf("res: %d\n", res_dbl.a);
  /* { dg-output "\nres: 190" } */

  CHECK(ffi_prep_closure(pcl, &cif, cls_struct_1_1byte_gn, NULL) == FFI_OK);

  res_dbl = ((cls_struct_1_1byte(*)(cls_struct_1_1byte, cls_struct_1_1byte))(pcl))(g_dbl, f_dbl);
  /* { dg-output "\n12 178: 190" } */
  printf("res: %d\n", res_dbl.a);
  /* { dg-output "\nres: 190" } */

  exit(0);
}
