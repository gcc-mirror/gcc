#include "f2c.h"

#ifdef KR_headers
integer i_dim(a,b) integer *a, *b;
#else
integer i_dim(integer *a, integer *b)
#endif
{
return( *a > *b ? *a - *b : 0);
}
