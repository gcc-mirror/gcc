#include "config.h"
#include "f2c.h"
#include "fio.h"
#include <string.h>
#ifdef KR_headers
integer f_inqu(a) inlist *a;
#else
#if defined (MSDOS) && !defined (GO32)
#undef abs
#undef min
#undef max
#include "io.h"
#endif
integer f_inqu(inlist *a)
#endif
{	flag byfile;
	int i, n;
	unit *p;
	char buf[256];
	long x;
	if (f__init & 2)
		f__fatal (131, "I/O recursion");
	if(a->infile!=NULL)
	{	byfile=1;
		g_char(a->infile,a->infilen,buf);
#ifdef NON_UNIX_STDIO
		x = access(buf,0) ? -1 : 0;
		for(i=0,p=NULL;i<MXUNIT;i++)
			if(f__units[i].ufd != NULL
			 && f__units[i].ufnm != NULL
			 && !strcmp(f__units[i].ufnm,buf)) {
				p = &f__units[i];
				break;
				}
#else
		x=f__inode(buf, &n);
		for(i=0,p=NULL;i<MXUNIT;i++)
			if(f__units[i].uinode==x
			&& f__units[i].ufd!=NULL
			&& f__units[i].udev == n) {
				p = &f__units[i];
				break;
				}
#endif
	}
	else
	{
		byfile=0;
		if(a->inunit<MXUNIT && a->inunit>=0)
		{
			p= &f__units[a->inunit];
		}
		else
		{
			p=NULL;
		}
	}
	if(a->inex!=NULL)
		if(byfile && x != -1 || !byfile && p!=NULL)
			*a->inex=1;
		else *a->inex=0;
	if(a->inopen!=NULL)
		if(byfile) *a->inopen=(p!=NULL);
		else *a->inopen=(p!=NULL && p->ufd!=NULL);
	if(a->innum!=NULL) *a->innum= p-f__units;
	if(a->innamed!=NULL)
		if(byfile || p!=NULL && p->ufnm!=NULL)
			*a->innamed=1;
		else	*a->innamed=0;
	if(a->inname!=NULL)
		if(byfile)
			b_char(buf,a->inname,a->innamlen);
		else if(p!=NULL && p->ufnm!=NULL)
			b_char(p->ufnm,a->inname,a->innamlen);
	if(a->inacc!=NULL && p!=NULL && p->ufd!=NULL)
		if(p->url)
			b_char("DIRECT",a->inacc,a->inacclen);
		else	b_char("SEQUENTIAL",a->inacc,a->inacclen);
	if(a->inseq!=NULL)
		if(p!=NULL && p->url)
			b_char("NO",a->inseq,a->inseqlen);
		else	b_char("YES",a->inseq,a->inseqlen);
	if(a->indir!=NULL)
		if(p==NULL || p->url)
			b_char("YES",a->indir,a->indirlen);
		else	b_char("NO",a->indir,a->indirlen);
	if(a->infmt!=NULL)
		if(p!=NULL && p->ufmt==0)
			b_char("UNFORMATTED",a->infmt,a->infmtlen);
		else	b_char("FORMATTED",a->infmt,a->infmtlen);
	if(a->inform!=NULL)
		if(p!=NULL && p->ufmt==0)
		b_char("NO",a->inform,a->informlen);
		else b_char("YES",a->inform,a->informlen);
	if(a->inunf)
		if(p!=NULL && p->ufmt==0)
			b_char("YES",a->inunf,a->inunflen);
		else if (p!=NULL) b_char("NO",a->inunf,a->inunflen);
		else b_char("UNKNOWN",a->inunf,a->inunflen);
	if(a->inrecl!=NULL && p!=NULL)
		*a->inrecl=p->url;
	if(a->innrec!=NULL && p!=NULL && p->url>0)
		*a->innrec=ftell(p->ufd)/p->url+1;
	if(a->inblank && p!=NULL && p->ufmt)
		if(p->ublnk)
			b_char("ZERO",a->inblank,a->inblanklen);
		else	b_char("NULL",a->inblank,a->inblanklen);
	return(0);
}
