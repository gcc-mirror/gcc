/* Based on newlib/libm/mathfp/s_numtest.c in Newlib.  */

#include "amdgcnmach.h"

v64si
v64df_numtest (v64df x)
{
  // Explicitly create mask for internal function.
  v64si __mask = VECTOR_INIT (-1);
  FUNCTION_INIT (v64si);

  v64si hx, lx;
  EXTRACT_WORDS (hx, lx, x);
  v64si exp = (hx & 0x7ff00000) >> 20;

  /* Check for a zero input. */
  VECTOR_RETURN (VECTOR_INIT (0), x == 0.0);

  /* Check for not a number or infinity. */
  VECTOR_IF (exp == 0x7ff, cond)
    VECTOR_RETURN (VECTOR_MERGE (VECTOR_INIT (NAN),
				 VECTOR_INIT (INF),
				 ((hx & 0xf0000) != 0) | (lx != 0)),
		   cond);
  /* Otherwise it's a finite value. */
  VECTOR_ELSE (cond)
    VECTOR_RETURN (VECTOR_INIT (NUM), cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}
