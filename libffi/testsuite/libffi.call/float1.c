/* Area:	ffi_call
   Purpose:	Check return value double.
   Limitations:	none.
   PR:		none.
   Originator:	From the original ffitest.c  */

/* { dg-do run } */
#include "ffitest.h"
#include "float.h"

static double dblit(float f)
{
  return f/3.0;
}

int main (void)
{
  ffi_cif cif;
  ffi_type *args[MAX_ARGS];
  void *values[MAX_ARGS];
  float f;
  double d;


  args[0] = &ffi_type_float;
  values[0] = &f;
  
  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, 
		     &ffi_type_double, args) == FFI_OK);
  
  f = 3.14159;
  
  ffi_call(&cif, FFI_FN(dblit), &d, values);
  
  /* These are not always the same!! Check for a reasonable delta */
 
  CHECK(d - dblit(f) < DBL_EPSILON);

  exit(0);

}
