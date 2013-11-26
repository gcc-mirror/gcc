/* FIXME: rename this file */

/*
   Artificially create _GLOBAL_AIX[ID]_shr_o symbols in libgcc.a.

   This means that libstdc++.a can invoke these symbols and they are resolved
   regardless of whether libstdc++.a is linked against libgcc_s.a or libgcc.a.

   The symbols are created in libgcc_s.a by collect2 as there are exception
   frames to register for LIB2_DIVMOD_FUNCS.

   The symbols are NOT created by collect2 for libgcc.a, because libgcc.a is
   a 'real' archive containing objects and collect2 is not invoked.

   libstdc++.a is linked against libgcc.a when handling the command line
   options '-static-libgcc -static-libstdc++'.
*/

void _GLOBAL__AIXI_shr_o (void);
void _GLOBAL__AIXD_shr_o (void);

void
_GLOBAL__AIXI_shr_o (void)
{
  return;
}

void
_GLOBAL__AIXD_shr_o (void)
{
  return;
}

