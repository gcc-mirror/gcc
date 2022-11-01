/* Based on newlib/libm/mathfp/sf_ispos.c in Newlib.  */

#include "amdgcnmach.h"

v64si
v64sf_isposf (v64sf x)
{
  v64si wx = CAST_VECTOR (v64si, x);

  return (wx & 0x80000000) == 0;
}
