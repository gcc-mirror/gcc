#include "f2c.h"
#include "fio.h"
#include "lio.h"

 integer
#ifdef KR_headers
s_wsne(a) cilist *a;
#else
s_wsne(cilist *a)
#endif
{
	int n;

	if(n=c_le(a))
		return(n);
	f__reading=0;
	f__external=1;
	f__formatted=1;
	f__putn = x_putc;
	L_len = LINE;
	f__donewrec = x_wSL;
	if(f__curunit->uwrt != 1 && f__nowwriting(f__curunit))
		err(a->cierr, errno, "namelist output start");
	x_wsne(a);
	return e_wsle();
	}
