#include "config.h"
#include "f2c.h"
#include "fio.h"
#include "lio.h"
extern char *f__icptr;
extern char *f__icend;
extern icilist *f__svic;
extern int f__icnum;
#ifdef KR_headers
extern void z_putc();
#else
extern void z_putc(int);
#endif

 static int
z_wSL(Void)
{
	while(f__recpos < f__svic->icirlen)
		z_putc(' ');
	return z_rnew();
	}

 static void
#ifdef KR_headers
c_liw(a) icilist *a;
#else
c_liw(icilist *a)
#endif
{
	f__reading = 0;
	f__external = 0;
	f__formatted = 1;
	f__putn = z_putc;
	L_len = a->icirlen;
	f__donewrec = z_wSL;
	f__svic = a;
	f__icnum = f__recpos = 0;
	f__cursor = 0;
	f__cf = 0;
	f__curunit = 0;
	f__icptr = a->iciunit;
	f__icend = f__icptr + a->icirlen*a->icirnum;
	f__elist = (cilist *)a;
	}

 integer
#ifdef KR_headers
s_wsni(a) icilist *a;
#else
s_wsni(icilist *a)
#endif
{
	cilist ca;

	if(f__init != 1) f_init();
	f__init = 3;
	c_liw(a);
	ca.cifmt = a->icifmt;
	x_wsne(&ca);
	z_wSL();
	return 0;
	}

 integer
#ifdef KR_headers
s_wsli(a) icilist *a;
#else
s_wsli(icilist *a)
#endif
{
	if(f__init != 1) f_init();
	f__init = 3;
	f__lioproc = l_write;
	c_liw(a);
	return(0);
	}

integer e_wsli(Void)
{
	f__init = 1;
	z_wSL();
	return(0);
	}
