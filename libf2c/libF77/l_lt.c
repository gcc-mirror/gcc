#include "f2c.h"

#ifdef KR_headers
extern integer s_cmp();
logical l_lt(a,b,la,lb) char *a, *b; ftnlen la, lb;
#else
extern integer s_cmp(char *, char *, ftnlen, ftnlen);
logical l_lt(char *a, char *b, ftnlen la, ftnlen lb)
#endif
{
return(s_cmp(a,b,la,lb) < 0);
}
