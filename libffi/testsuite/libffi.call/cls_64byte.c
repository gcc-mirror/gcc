/* Area:	ffi_call, closure_call
   Purpose:	Check structure passing with different structure size.
		Depending on the ABI. Check bigger struct which overlaps
		the gp and fp register count on Darwin/AIX/ppc64.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20030828	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* } } */
#include "ffitest.h"

typedef struct cls_struct_64byte {
  double a;
  double b;
  double c;
  double d;
  double e;
  double f;
  double g;
  double h;
} cls_struct_64byte;

cls_struct_64byte cls_struct_64byte_fn(struct cls_struct_64byte b0,
			    struct cls_struct_64byte b1,
			    struct cls_struct_64byte b2,
			    struct cls_struct_64byte b3)
{
  struct cls_struct_64byte result;

  result.a = b0.a + b1.a + b2.a + b3.a;
  result.b = b0.b + b1.b + b2.b + b3.b;
  result.c = b0.c + b1.c + b2.c + b3.c;
  result.d = b0.d + b1.d + b2.d + b3.d;
  result.e = b0.e + b1.e + b2.e + b3.e;
  result.f = b0.f + b1.f + b2.f + b3.f;
  result.g = b0.g + b1.g + b2.g + b3.g;
  result.h = b0.h + b1.h + b2.h + b3.h;

  printf("%g %g %g %g %g %g %g %g\n", result.a, result.b, result.c,
	 result.d, result.e, result.f, result.g, result.h);

  return result;
}

static void
cls_struct_64byte_gn(ffi_cif* cif, void* resp, void** args, void* userdata)
{
  struct cls_struct_64byte b0, b1, b2, b3;

  b0 = *(struct cls_struct_64byte*)(args[0]);
  b1 = *(struct cls_struct_64byte*)(args[1]);
  b2 = *(struct cls_struct_64byte*)(args[2]);
  b3 = *(struct cls_struct_64byte*)(args[3]);

  *(cls_struct_64byte*)resp = cls_struct_64byte_fn(b0, b1, b2, b3);
}

int main (void)
{
  ffi_cif cif;
  static ffi_closure cl;
  ffi_closure *pcl = &cl;
  void* args_dbl[5];
  ffi_type* cls_struct_fields[9];
  ffi_type cls_struct_type;
  ffi_type* dbl_arg_types[5];

  cls_struct_type.size = 0;
  cls_struct_type.alignment = 0;
  cls_struct_type.type = FFI_TYPE_STRUCT;
  cls_struct_type.elements = cls_struct_fields;

  struct cls_struct_64byte e_dbl = { 9.0, 2.0, 6.0, 5.0, 3.0, 4.0, 8.0, 1.0 };
  struct cls_struct_64byte f_dbl = { 1.0, 2.0, 3.0, 7.0, 2.0, 5.0, 6.0, 7.0 };
  struct cls_struct_64byte g_dbl = { 4.0, 5.0, 7.0, 9.0, 1.0, 1.0, 2.0, 9.0 };
  struct cls_struct_64byte h_dbl = { 8.0, 6.0, 1.0, 4.0, 0.0, 3.0, 3.0, 1.0 };
  struct cls_struct_64byte res_dbl;

  cls_struct_fields[0] = &ffi_type_double;
  cls_struct_fields[1] = &ffi_type_double;
  cls_struct_fields[2] = &ffi_type_double;
  cls_struct_fields[3] = &ffi_type_double;
  cls_struct_fields[4] = &ffi_type_double;
  cls_struct_fields[5] = &ffi_type_double;
  cls_struct_fields[6] = &ffi_type_double;
  cls_struct_fields[7] = &ffi_type_double;
  cls_struct_fields[8] = NULL;

  dbl_arg_types[0] = &cls_struct_type;
  dbl_arg_types[1] = &cls_struct_type;
  dbl_arg_types[2] = &cls_struct_type;
  dbl_arg_types[3] = &cls_struct_type;
  dbl_arg_types[4] = NULL;

  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 4, &cls_struct_type,
		     dbl_arg_types) == FFI_OK);

  args_dbl[0] = &e_dbl;
  args_dbl[1] = &f_dbl;
  args_dbl[2] = &g_dbl;
  args_dbl[3] = &h_dbl;
  args_dbl[4] = NULL;

  ffi_call(&cif, FFI_FN(cls_struct_64byte_fn), &res_dbl, args_dbl);
  /* { dg-output "22 15 17 25 6 13 19 18" } */
  CHECK( res_dbl.a == (e_dbl.a + f_dbl.a + g_dbl.a + h_dbl.a));
  CHECK( res_dbl.b == (e_dbl.b + f_dbl.b + g_dbl.b + h_dbl.b));
  CHECK( res_dbl.c == (e_dbl.c + f_dbl.c + g_dbl.c + h_dbl.c));
  CHECK( res_dbl.d == (e_dbl.d + f_dbl.d + g_dbl.d + h_dbl.d));
  CHECK( res_dbl.e == (e_dbl.e + f_dbl.e + g_dbl.e + h_dbl.e));
  CHECK( res_dbl.f == (e_dbl.f + f_dbl.f + g_dbl.f + h_dbl.f));
  CHECK( res_dbl.g == (e_dbl.g + f_dbl.g + g_dbl.g + h_dbl.g));
  CHECK( res_dbl.h == (e_dbl.h + f_dbl.h + g_dbl.h + h_dbl.h));

  CHECK(ffi_prep_closure(pcl, &cif, cls_struct_64byte_gn, NULL) == FFI_OK);

  res_dbl = ((cls_struct_64byte(*)(cls_struct_64byte,
				     cls_struct_64byte,
				     cls_struct_64byte,
				     cls_struct_64byte))
	     (pcl))(e_dbl, f_dbl, g_dbl, h_dbl);
  /* { dg-output "\n22 15 17 25 6 13 19 18" } */
  CHECK( res_dbl.a == (e_dbl.a + f_dbl.a + g_dbl.a + h_dbl.a));
  CHECK( res_dbl.b == (e_dbl.b + f_dbl.b + g_dbl.b + h_dbl.b));
  CHECK( res_dbl.c == (e_dbl.c + f_dbl.c + g_dbl.c + h_dbl.c));
  CHECK( res_dbl.d == (e_dbl.d + f_dbl.d + g_dbl.d + h_dbl.d));
  CHECK( res_dbl.e == (e_dbl.e + f_dbl.e + g_dbl.e + h_dbl.e));
  CHECK( res_dbl.f == (e_dbl.f + f_dbl.f + g_dbl.f + h_dbl.f));
  CHECK( res_dbl.g == (e_dbl.g + f_dbl.g + g_dbl.g + h_dbl.g));
  CHECK( res_dbl.h == (e_dbl.h + f_dbl.h + g_dbl.h + h_dbl.h));

  exit(0);
}
