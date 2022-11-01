/* Based on newlib/libm/mathfp/s_mathcnst.c in Newlib.  */

#include "amdgcnmach.h"

double BIGX = 7.09782712893383973096e+02;
double SMALLX = -7.45133219101941108420e+02;
double z_rooteps = 7.4505859692e-9;
float  z_rooteps_f = 1.7263349182589107e-4;

ufloat z_hugeval_f  = { 0x7f800000 };
ufloat z_infinity_f = { 0x7f800000 };
ufloat z_notanum_f  = { 0x7fd00000 };

#ifdef __IEEE_BIG_ENDIAN
udouble z_hugeval  = { 0x7ff00000, 0 };
udouble z_infinity = { 0x7ff00000, 0 };
udouble z_notanum  = { 0xeff80000, 0 };
#else /* __IEEE_LITTLE_ENDIAN  */
udouble z_hugeval  = { 0, 0x7ff00000 };
udouble z_infinity = { 0, 0x7ff00000 };
udouble z_notanum  = { 0, 0x7ff80000 };
#endif /* __IEEE_LITTLE_ENDIAN */

