#include "f2c.h"
#include "fio.h"
#include "fmt.h"
#include "lio.h"

#ifdef KR_headers
integer s_wsle(a) cilist *a;
#else
integer s_wsle(cilist *a)
#endif
{
	int n;
	if(n=c_le(a)) return(n);
	f__reading=0;
	f__external=1;
	f__formatted=1;
	f__putn = t_putc;
	f__lioproc = l_write;
	L_len = LINE;
	f__donewrec = x_wSL;
	if(f__curunit->uwrt != 1 && f__nowwriting(f__curunit))
		err(a->cierr, errno, "list output start");
	return(0);
	}

integer e_wsle(Void)
{
	f__init = 1;
	t_putc('\n');
	f__recpos=0;
#ifdef ALWAYS_FLUSH
	if (fflush(f__cf))
		err(f__elist->cierr, errno, "write end");
#else
	if (f__cf == stdout)
		fflush(stdout);
	else if (f__cf == stderr)
		fflush(stderr);
#endif
	return(0);
	}
