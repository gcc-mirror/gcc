#include "f2c.h"
#include "fio.h"
#include "fmt.h"

y_rsk(Void)
{
	if(f__curunit->uend || f__curunit->url <= f__recpos
		|| f__curunit->url == 1) return 0;
	do {
		getc(f__cf);
	} while(++f__recpos < f__curunit->url);
	return 0;
}
y_getc(Void)
{
	int ch;
	if(f__curunit->uend) return(-1);
	if((ch=getc(f__cf))!=EOF)
	{
		f__recpos++;
		if(f__curunit->url>=f__recpos ||
			f__curunit->url==1)
			return(ch);
		else	return(' ');
	}
	if(feof(f__cf))
	{
		f__curunit->uend=1;
		errno=0;
		return(-1);
	}
	err(f__elist->cierr,errno,"readingd");
}

 static int
y_rev(Void)
{
	if (f__recpos < f__hiwater)
		f__recpos = f__hiwater;
	if (f__curunit->url > 1)
		while(f__recpos < f__curunit->url)
			(*f__putn)(' ');
	if (f__recpos)
		f__putbuf(0);
	f__recpos = 0;
	return(0);
}

 static int
y_err(Void)
{
	err(f__elist->cierr, 110, "dfe");
}

 static int
y_newrec(Void)
{
	y_rev();
	f__hiwater = f__cursor = 0;
	return(1);
}

#ifdef KR_headers
c_dfe(a) cilist *a;
#else
c_dfe(cilist *a)
#endif
{
	f__sequential=0;
	f__formatted=f__external=1;
	f__elist=a;
	f__cursor=f__scale=f__recpos=0;
	f__curunit = &f__units[a->ciunit];
	if(a->ciunit>MXUNIT || a->ciunit<0)
		err(a->cierr,101,"startchk");
	if(f__curunit->ufd==NULL && fk_open(DIR,FMT,a->ciunit))
		err(a->cierr,104,"dfe");
	f__cf=f__curunit->ufd;
	if(!f__curunit->ufmt) err(a->cierr,102,"dfe");
	if(!f__curunit->useek) err(a->cierr,104,"dfe");
	f__fmtbuf=a->cifmt;
	if(a->cirec <= 0)
		err(a->cierr,130,"dfe");
	fseek(f__cf,(long)f__curunit->url * (a->cirec-1),SEEK_SET);
	f__curunit->uend = 0;
	return(0);
}
#ifdef KR_headers
integer s_rdfe(a) cilist *a;
#else
integer s_rdfe(cilist *a)
#endif
{
	int n;
	if(f__init != 1) f_init();
	f__init = 3;
	f__reading=1;
	if(n=c_dfe(a))return(n);
	if(f__curunit->uwrt && f__nowreading(f__curunit))
		err(a->cierr,errno,"read start");
	f__getn = y_getc;
	f__doed = rd_ed;
	f__doned = rd_ned;
	f__dorevert = f__donewrec = y_err;
	f__doend = y_rsk;
	if(pars_f(f__fmtbuf)<0)
		err(a->cierr,100,"read start");
	fmt_bg();
	return(0);
}
#ifdef KR_headers
integer s_wdfe(a) cilist *a;
#else
integer s_wdfe(cilist *a)
#endif
{
	int n;
	if(f__init != 1) f_init();
	f__init = 3;
	f__reading=0;
	if(n=c_dfe(a)) return(n);
	if(f__curunit->uwrt != 1 && f__nowwriting(f__curunit))
		err(a->cierr,errno,"startwrt");
	f__putn = x_putc;
	f__doed = w_ed;
	f__doned= w_ned;
	f__dorevert = y_err;
	f__donewrec = y_newrec;
	f__doend = y_rev;
	if(pars_f(f__fmtbuf)<0)
		err(a->cierr,100,"startwrt");
	fmt_bg();
	return(0);
}
integer e_rdfe(Void)
{
	f__init = 1;
	en_fio();
	return(0);
}
