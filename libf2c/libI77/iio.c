#include "f2c.h"
#include "fio.h"
#include "fmt.h"
extern char *f__icptr;
char *f__icend;
extern icilist *f__svic;
int f__icnum;
extern int f__hiwater;
z_getc(Void)
{
	if(f__recpos++ < f__svic->icirlen) {
		if(f__icptr >= f__icend) err(f__svic->iciend,(EOF),"endfile");
		return(*(unsigned char *)f__icptr++);
		}
	return '\n';
}

 void
#ifdef KR_headers
z_putc(c)
#else
z_putc(int c)
#endif
{
	if (f__icptr < f__icend && f__recpos++ < f__svic->icirlen)
		*f__icptr++ = c;
}
z_rnew(Void)
{
	f__icptr = f__svic->iciunit + (++f__icnum)*f__svic->icirlen;
	f__recpos = 0;
	f__cursor = 0;
	f__hiwater = 0;
	return 1;
}

 static int
z_endp(Void)
{
	(*f__donewrec)();
	return 0;
	}

#ifdef KR_headers
c_si(a) icilist *a;
#else
c_si(icilist *a)
#endif
{
	if (f__init & 2)
		f__fatal (131, "I/O recursion");
	f__init |= 2;
	f__elist = (cilist *)a;
	f__fmtbuf=a->icifmt;
	f__curunit = 0;
	f__sequential=f__formatted=1;
	f__external=0;
	if(pars_f(f__fmtbuf)<0)
		err(a->icierr,100,"startint");
	fmt_bg();
	f__cblank=f__cplus=f__scale=0;
	f__svic=a;
	f__icnum=f__recpos=0;
	f__cursor = 0;
	f__hiwater = 0;
	f__icptr = a->iciunit;
	f__icend = f__icptr + a->icirlen*a->icirnum;
	f__cf = 0;
	return(0);
}

 int
iw_rev(Void)
{
	if(f__workdone)
		z_endp();
	f__hiwater = f__recpos = f__cursor = 0;
	return(f__workdone=0);
	}

#ifdef KR_headers
integer s_rsfi(a) icilist *a;
#else
integer s_rsfi(icilist *a)
#endif
{	int n;
	if(n=c_si(a)) return(n);
	f__reading=1;
	f__doed=rd_ed;
	f__doned=rd_ned;
	f__getn=z_getc;
	f__dorevert = z_endp;
	f__donewrec = z_rnew;
	f__doend = z_endp;
	return(0);
}

z_wnew(Void)
{
	if (f__recpos < f__hiwater) {
		f__icptr += f__hiwater - f__recpos;
		f__recpos = f__hiwater;
		}
	while(f__recpos++ < f__svic->icirlen)
		*f__icptr++ = ' ';
	f__recpos = 0;
	f__cursor = 0;
	f__hiwater = 0;
	f__icnum++;
	return 1;
}
#ifdef KR_headers
integer s_wsfi(a) icilist *a;
#else
integer s_wsfi(icilist *a)
#endif
{	int n;
	if(n=c_si(a)) return(n);
	f__reading=0;
	f__doed=w_ed;
	f__doned=w_ned;
	f__putn=z_putc;
	f__dorevert = iw_rev;
	f__donewrec = z_wnew;
	f__doend = z_endp;
	return(0);
}
integer e_rsfi(Void)
{	int n;
	f__init &= ~2;
	n = en_fio();
	f__fmtbuf = NULL;
	return(n);
}
integer e_wsfi(Void)
{
	int n;
	f__init &= ~2;
	n = en_fio();
	f__fmtbuf = NULL;
	if(f__svic->icirnum != 1
	 && (f__icnum >  f__svic->icirnum
	 || (f__icnum == f__svic->icirnum && (f__recpos | f__hiwater))))
		err(f__svic->icierr,110,"inwrite");
	if (f__recpos < f__hiwater)
		f__recpos = f__hiwater;
	if (f__recpos >= f__svic->icirlen)
		err(f__svic->icierr,110,"recend");
	if (!f__recpos && f__icnum)
		return n;
	while(f__recpos++ < f__svic->icirlen)
		*f__icptr++ = ' ';
	return n;
}
