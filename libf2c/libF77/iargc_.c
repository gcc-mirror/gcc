#include "f2c.h"

#ifdef KR_headers
ftnint G77_iargc_0 ()
#else
ftnint G77_iargc_0 (void)
#endif
{
extern int f__xargc;
return ( f__xargc - 1 );
}
