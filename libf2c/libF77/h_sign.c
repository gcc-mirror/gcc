#include "f2c.h"

#ifdef KR_headers
shortint h_sign(a,b) shortint *a, *b;
#else
shortint h_sign(shortint *a, shortint *b)
#endif
{
shortint x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}
