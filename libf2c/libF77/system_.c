/* f77 interface to system routine */

#include "f2c.h"

#ifdef KR_headers
extern char *F77_aloc();

 integer
G77_system_0 (s, n) register char *s; ftnlen n;
#else
#undef abs
#undef min
#undef max
#include <stdlib.h>
extern char *F77_aloc(ftnlen, char*);

 integer
G77_system_0 (register char *s, ftnlen n)
#endif
{
	char buff0[256], *buff;
	register char *bp, *blast;
	integer rv;

	buff = bp = n < sizeof(buff0)
			? buff0 : F77_aloc(n+1, "system_");
	blast = bp + n;

	while(bp < blast && *s)
		*bp++ = *s++;
	*bp = 0;
	rv = system(buff);
	if (buff != buff0)
		free(buff);
	return rv;
	}
