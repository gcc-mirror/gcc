#include "f2c.h"
#include "fio.h"

#ifdef KR_headers
extern char *strcpy();
extern FILE *tmpfile();
#else
#undef abs
#undef min
#undef max
#include <stdlib.h>
#include <string.h>
#endif

extern char *f__r_mode[], *f__w_mode[];

#ifdef KR_headers
integer f_end(a) alist *a;
#else
integer f_end(alist *a)
#endif
{
	unit *b;
	FILE *tf;

	if (f__init & 2)
		f__fatal (131, "I/O recursion");
	if(a->aunit>=MXUNIT || a->aunit<0) err(a->aerr,101,"endfile");
	b = &f__units[a->aunit];
	if(b->ufd==NULL) {
		char nbuf[10];
		sprintf(nbuf,"fort.%ld",a->aunit);
		if (tf = fopen(nbuf, f__w_mode[0]))
			fclose(tf);
		return(0);
		}
	b->uend=1;
	return(b->useek ? t_runc(a) : 0);
}

 static int
#ifdef KR_headers
copy(from, len, to) FILE *from, *to; register long len;
#else
copy(FILE *from, register long len, FILE *to)
#endif
{
	int len1;
	char buf[BUFSIZ];

	while(fread(buf, len1 = len > BUFSIZ ? BUFSIZ : (int)len, 1, from)) {
		if (!fwrite(buf, len1, 1, to))
			return 1;
		if ((len -= len1) <= 0)
			break;
		}
	return 0;
	}

 int
#ifdef KR_headers
t_runc(a) alist *a;
#else
t_runc(alist *a)
#endif
{
	long loc, len;
	unit *b;
	FILE *bf, *tf;
	int rc = 0;

	b = &f__units[a->aunit];
	if(b->url)
		return(0);	/*don't truncate direct files*/
	loc=ftell(bf = b->ufd);
	fseek(bf,0L,SEEK_END);
	len=ftell(bf);
	if (loc >= len || b->useek == 0 || b->ufnm == NULL)
		return(0);
	fclose(b->ufd);
	if (!loc) {
		if (!(bf = fopen(b->ufnm, f__w_mode[b->ufmt])))
			rc = 1;
		if (b->uwrt)
			b->uwrt = 1;
		goto done;
		}
	if (!(bf = fopen(b->ufnm, f__r_mode[0]))
	 || !(tf = tmpfile())) {
#ifdef NON_UNIX_STDIO
 bad:
#endif
		rc = 1;
		goto done;
		}
	if (copy(bf, loc, tf)) {
 bad1:
		rc = 1;
		goto done1;
		}
	if (!(bf = freopen(b->ufnm, f__w_mode[0], bf)))
		goto bad1;
	rewind(tf);
	if (copy(tf, loc, bf))
		goto bad1;
	b->uwrt = 1;
	b->urw = 2;
#ifdef NON_UNIX_STDIO
	if (b->ufmt) {
		fclose(bf);
		if (!(bf = fopen(b->ufnm, f__w_mode[3])))
			goto bad;
		fseek(bf,0L,SEEK_END);
		b->urw = 3;
		}
#endif
done1:
	fclose(tf);
done:
	f__cf = b->ufd = bf;
	if (rc)
		err(a->aerr,111,"endfile");
	return 0;
	}
