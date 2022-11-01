/* Based on newlib/libm/mathfp/sf_numtest.c in Newlib.  */

#include "amdgcnmach.h"

v64si
v64sf_numtestf (v64sf x)
{
  // Explicitly create mask for internal function.
  v64si __mask = VECTOR_INIT (-1);
  FUNCTION_INIT (v64si);

  v64si wx;
  GET_FLOAT_WORD (wx, x, NO_COND);
  v64si exp = (wx & 0x7f800000) >> 23;

  /* Check for a zero input. */
  VECTOR_RETURN (VECTOR_INIT (0), x == 0.0);

  /* Check for not a number or infinity. */
  VECTOR_IF (exp == 0xff, cond)
    VECTOR_RETURN (VECTOR_MERGE (VECTOR_INIT (NAN), VECTOR_INIT (INF),
                                 wx & 0x7fffff),
		   cond);
  /* Otherwise it's a finite value. */
  VECTOR_ELSE (cond)
    VECTOR_RETURN (VECTOR_INIT (NUM), cond);
  VECTOR_ENDIF

  FUNCTION_RETURN;
}
