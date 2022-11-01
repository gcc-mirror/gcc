/* Based on newlib/libm/mathfp/s_ispos.c in Newlib.  */

#include "amdgcnmach.h"

v64si 
v64df_ispos (v64df x)
{
  // Explicitly create mask for internal function.
  v64si __mask = VECTOR_INIT (-1);
  FUNCTION_INIT (v64si);

  v64si hx;
  GET_HIGH_WORD (hx, x, NO_COND);

  VECTOR_RETURN ((hx & 0x80000000) == 0, NO_COND);

  FUNCTION_RETURN;
}
