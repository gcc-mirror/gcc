#include "f2c.h"
#include "fio.h"
#include "lio.h"
#include "fmt.h"

extern int f__Aquote;

 static VOID
nl_donewrec(Void)
{
	(*f__donewrec)();
	PUT(' ');
	}

#ifdef KR_headers
x_wsne(a) cilist *a;
#else
#include <string.h>

 VOID
x_wsne(cilist *a)
#endif
{
	Namelist *nl;
	char *s;
	Vardesc *v, **vd, **vde;
	ftnint *number, type;
	ftnlen *dims;
	ftnlen size;
	static ftnint one = 1;
	extern ftnlen f__typesize[];

	nl = (Namelist *)a->cifmt;
	PUT('&');
	for(s = nl->name; *s; s++)
		PUT(*s);
	PUT(' ');
	f__Aquote = 1;
	vd = nl->vars;
	vde = vd + nl->nvars;
	while(vd < vde) {
		v = *vd++;
		s = v->name;
#ifdef No_Extra_Namelist_Newlines
		if (f__recpos+strlen(s)+2 >= L_len)
#endif
			nl_donewrec();
		while(*s)
			PUT(*s++);
		PUT(' ');
		PUT('=');
		number = (dims = v->dims) ? dims + 1 : &one;
		type = v->type;
		if (type < 0) {
			size = -type;
			type = TYCHAR;
			}
		else
			size = f__typesize[type];
		l_write(number, v->addr, size, type);
		if (vd < vde) {
			if (f__recpos+2 >= L_len)
				nl_donewrec();
			PUT(',');
			PUT(' ');
			}
		else if (f__recpos+1 >= L_len)
			nl_donewrec();
		}
	f__Aquote = 0;
	PUT('/');
	}
