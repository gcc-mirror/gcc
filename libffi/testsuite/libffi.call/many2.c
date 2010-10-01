/* Area:        ffi_call
   Purpose:     Check unsigned char arguments.
   Limitations: none.
   PR:          PR45677.
   Originator:  Dan Witte <dwitte@gmail.com> 20100916  */

/* { dg-do run } */

#include "ffitest.h"

#define NARGS 7

typedef unsigned char u8;

__attribute__((noinline)) u8
foo (u8 a, u8 b, u8 c, u8 d, u8 e, u8 f, u8 g)
{
  return a + b + c + d + e + f + g;
}

u8
bar (u8 a, u8 b, u8 c, u8 d, u8 e, u8 f, u8 g)
{
  return foo (a, b, c, d, e, f, g);
}

int
main (void)
{
  ffi_type *ffitypes[NARGS];
  int i;
  ffi_cif cif;
  ffi_arg result = 0;
  u8 args[NARGS];
  void *argptrs[NARGS];

  for (i = 0; i < NARGS; ++i)
    ffitypes[i] = &ffi_type_uchar;

  CHECK (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, NARGS,
		       &ffi_type_uint8, ffitypes) == FFI_OK);

  for (i = 0; i < NARGS; ++i)
    {
      args[i] = i;
      argptrs[i] = &args[i];
    }
  ffi_call (&cif, FFI_FN (bar), &result, argptrs);

  CHECK (result == 21);
  return 0;
}
