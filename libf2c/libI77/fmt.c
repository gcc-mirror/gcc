#include "f2c.h"
#include "fio.h"
#include "fmt.h"
#define skip(s) while(*s==' ') s++
#ifdef interdata
#define SYLMX 300
#endif
#ifdef pdp11
#define SYLMX 300
#endif
#ifdef vax
#define SYLMX 300
#endif
#ifndef SYLMX
#define SYLMX 300
#endif
#define GLITCH '\2'
	/* special quote character for stu */
extern int f__cursor,f__scale;
extern flag f__cblank,f__cplus;	/*blanks in I and compulsory plus*/
static struct syl f__syl[SYLMX];
int f__parenlvl,f__pc,f__revloc;

 static
#ifdef KR_headers
char *ap_end(s) char *s;
#else
char *ap_end(char *s)
#endif
{	char quote;
	quote= *s++;
	for(;*s;s++)
	{	if(*s!=quote) continue;
		if(*++s!=quote) return(s);
	}
	if(f__elist->cierr) {
		errno = 100;
		return(NULL);
	}
	f__fatal(100, "bad string");
	/*NOTREACHED*/ return 0;
}
 static
#ifdef KR_headers
op_gen(a,b,c,d)
#else
op_gen(int a, int b, int c, int d)
#endif
{	struct syl *p= &f__syl[f__pc];
	if(f__pc>=SYLMX)
	{	fprintf(stderr,"format too complicated:\n");
		sig_die(f__fmtbuf, 1);
	}
	p->op=a;
	p->p1=b;
	p->p2.i[0]=c;
	p->p2.i[1]=d;
	return(f__pc++);
}
#ifdef KR_headers
static char *f_list();
static char *gt_num(s,n,n1) char *s; int *n, n1;
#else
static char *f_list(char*);
static char *gt_num(char *s, int *n, int n1)
#endif
{	int m=0,f__cnt=0;
	char c;
	for(c= *s;;c = *s)
	{	if(c==' ')
		{	s++;
			continue;
		}
		if(c>'9' || c<'0') break;
		m=10*m+c-'0';
		f__cnt++;
		s++;
	}
	if(f__cnt==0) {
		if (!n1)
			s = 0;
		*n=n1;
		}
	else *n=m;
	return(s);
}

 static
#ifdef KR_headers
char *f_s(s,curloc) char *s;
#else
char *f_s(char *s, int curloc)
#endif
{
	skip(s);
	if(*s++!='(')
	{
		return(NULL);
	}
	if(f__parenlvl++ ==1) f__revloc=curloc;
	if(op_gen(RET1,curloc,0,0)<0 ||
		(s=f_list(s))==NULL)
	{
		return(NULL);
	}
	return(s);
}

 static
#ifdef KR_headers
ne_d(s,p) char *s,**p;
#else
ne_d(char *s, char **p)
#endif
{	int n,x,sign=0;
	struct syl *sp;
	switch(*s)
	{
	default:
		return(0);
	case ':': (void) op_gen(COLON,0,0,0); break;
	case '$':
		(void) op_gen(NONL, 0, 0, 0); break;
	case 'B':
	case 'b':
		if(*++s=='z' || *s == 'Z') (void) op_gen(BZ,0,0,0);
		else (void) op_gen(BN,0,0,0);
		break;
	case 'S':
	case 's':
		if(*(s+1)=='s' || *(s+1) == 'S')
		{	x=SS;
			s++;
		}
		else if(*(s+1)=='p' || *(s+1) == 'P')
		{	x=SP;
			s++;
		}
		else x=S;
		(void) op_gen(x,0,0,0);
		break;
	case '/': (void) op_gen(SLASH,0,0,0); break;
	case '-': sign=1;
	case '+':	s++;	/*OUTRAGEOUS CODING TRICK*/
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		if (!(s=gt_num(s,&n,0))) {
 bad:			*p = 0;
			return 1;
			}
		switch(*s)
		{
		default:
			return(0);
		case 'P':
		case 'p': if(sign) n= -n; (void) op_gen(P,n,0,0); break;
		case 'X':
		case 'x': (void) op_gen(X,n,0,0); break;
		case 'H':
		case 'h':
			sp = &f__syl[op_gen(H,n,0,0)];
			sp->p2.s = s + 1;
			s+=n;
			break;
		}
		break;
	case GLITCH:
	case '"':
	case '\'':
		sp = &f__syl[op_gen(APOS,0,0,0)];
		sp->p2.s = s;
		if((*p = ap_end(s)) == NULL)
			return(0);
		return(1);
	case 'T':
	case 't':
		if(*(s+1)=='l' || *(s+1) == 'L')
		{	x=TL;
			s++;
		}
		else if(*(s+1)=='r'|| *(s+1) == 'R')
		{	x=TR;
			s++;
		}
		else x=T;
		if (!(s=gt_num(s+1,&n,0)))
			goto bad;
		s--;
		(void) op_gen(x,n,0,0);
		break;
	case 'X':
	case 'x': (void) op_gen(X,1,0,0); break;
	case 'P':
	case 'p': (void) op_gen(P,1,0,0); break;
	}
	s++;
	*p=s;
	return(1);
}

 static
#ifdef KR_headers
e_d(s,p) char *s,**p;
#else
e_d(char *s, char **p)
#endif
{	int i,im,n,w,d,e,found=0,x=0;
	char *sv=s;
	s=gt_num(s,&n,1);
	(void) op_gen(STACK,n,0,0);
	switch(*s++)
	{
	default: break;
	case 'E':
	case 'e':	x=1;
	case 'G':
	case 'g':
		found=1;
		if (!(s=gt_num(s,&w,0))) {
 bad:
			*p = 0;
			return 1;
			}
		if(w==0) break;
		if(*s=='.') {
			if (!(s=gt_num(s+1,&d,0)))
				goto bad;
			}
		else d=0;
		if(*s!='E' && *s != 'e')
			(void) op_gen(x==1?E:G,w,d,0);	/* default is Ew.dE2 */
		else {
			if (!(s=gt_num(s+1,&e,0)))
				goto bad;
			(void) op_gen(x==1?EE:GE,w,d,e);
			}
		break;
	case 'O':
	case 'o':
		i = O;
		im = OM;
		goto finish_I;
	case 'Z':
	case 'z':
		i = Z;
		im = ZM;
		goto finish_I;
	case 'L':
	case 'l':
		found=1;
		if (!(s=gt_num(s,&w,0)))
			goto bad;
		if(w==0) break;
		(void) op_gen(L,w,0,0);
		break;
	case 'A':
	case 'a':
		found=1;
		skip(s);
		if(*s>='0' && *s<='9')
		{	s=gt_num(s,&w,1);
			if(w==0) break;
			(void) op_gen(AW,w,0,0);
			break;
		}
		(void) op_gen(A,0,0,0);
		break;
	case 'F':
	case 'f':
		if (!(s=gt_num(s,&w,0)))
			goto bad;
		found=1;
		if(w==0) break;
		if(*s=='.') {
			if (!(s=gt_num(s+1,&d,0)))
				goto bad;
			}
		else d=0;
		(void) op_gen(F,w,d,0);
		break;
	case 'D':
	case 'd':
		found=1;
		if (!(s=gt_num(s,&w,0)))
			goto bad;
		if(w==0) break;
		if(*s=='.') {
			if (!(s=gt_num(s+1,&d,0)))
				goto bad;
			}
		else d=0;
		(void) op_gen(D,w,d,0);
		break;
	case 'I':
	case 'i':
		i = I;
		im = IM;
 finish_I:
		if (!(s=gt_num(s,&w,0)))
			goto bad;
		found=1;
		if(w==0) break;
		if(*s!='.')
		{	(void) op_gen(i,w,0,0);
			break;
		}
		if (!(s=gt_num(s+1,&d,0)))
			goto bad;
		(void) op_gen(im,w,d,0);
		break;
	}
	if(found==0)
	{	f__pc--; /*unSTACK*/
		*p=sv;
		return(0);
	}
	*p=s;
	return(1);
}
 static
#ifdef KR_headers
char *i_tem(s) char *s;
#else
char *i_tem(char *s)
#endif
{	char *t;
	int n,curloc;
	if(*s==')') return(s);
	if(ne_d(s,&t)) return(t);
	if(e_d(s,&t)) return(t);
	s=gt_num(s,&n,1);
	if((curloc=op_gen(STACK,n,0,0))<0) return(NULL);
	return(f_s(s,curloc));
}

 static
#ifdef KR_headers
char *f_list(s) char *s;
#else
char *f_list(char *s)
#endif
{
	for(;*s!=0;)
	{	skip(s);
		if((s=i_tem(s))==NULL) return(NULL);
		skip(s);
		if(*s==',') s++;
		else if(*s==')')
		{	if(--f__parenlvl==0)
			{
				(void) op_gen(REVERT,f__revloc,0,0);
				return(++s);
			}
			(void) op_gen(GOTO,0,0,0);
			return(++s);
		}
	}
	return(NULL);
}

#ifdef KR_headers
pars_f(s) char *s;
#else
pars_f(char *s)
#endif
{
	char *e;

	f__parenlvl=f__revloc=f__pc=0;
	if((e=f_s(s,0)) == NULL)
	{
		/* Try and delimit the format string.  Parens within
		   hollerith and quoted strings have to match for this
		   to work, but it's probably adequate for most needs.
		   Note that this is needed because a valid CHARACTER
		   variable passed for FMT= can contain '(I)garbage',
		   where `garbage' is billions and billions of junk
		   characters, and it's up to the run-time library to
		   know where the format string ends by counting parens.
		   Meanwhile, still treat NUL byte as "hard stop", since
		   f2c still appends that at end of FORMAT-statement
		   strings.  */

		int level=0;

		for (f__fmtlen=0;
			((*s!=')') || (--level > 0))
				&& (*s!='\0')
				&& (f__fmtlen<80);
			++s, ++f__fmtlen)
		{
			if (*s=='(')
				++level;
		}
		if (*s==')')
			++f__fmtlen;
		return(-1);
	}
	f__fmtlen = e - s;
	return(0);
}
#define STKSZ 10
int f__cnt[STKSZ],f__ret[STKSZ],f__cp,f__rp;
flag f__workdone, f__nonl;

 static
#ifdef KR_headers
type_f(n)
#else
type_f(int n)
#endif
{
	switch(n)
	{
	default:
		return(n);
	case RET1:
		return(RET1);
	case REVERT: return(REVERT);
	case GOTO: return(GOTO);
	case STACK: return(STACK);
	case X:
	case SLASH:
	case APOS: case H:
	case T: case TL: case TR:
		return(NED);
	case F:
	case I:
	case IM:
	case A: case AW:
	case O: case OM:
	case L:
	case E: case EE: case D:
	case G: case GE:
	case Z: case ZM:
		return(ED);
	}
}
#ifdef KR_headers
integer do_fio(number,ptr,len) ftnint *number; ftnlen len; char *ptr;
#else
integer do_fio(ftnint *number, char *ptr, ftnlen len)
#endif
{	struct syl *p;
	int n,i;
	for(i=0;i<*number;i++,ptr+=len)
	{
loop:	switch(type_f((p= &f__syl[f__pc])->op))
	{
	default:
		fprintf(stderr,"unknown code in do_fio: %d\n%.*s\n",
			p->op,f__fmtlen,f__fmtbuf);
		err(f__elist->cierr,100,"do_fio");
	case NED:
		if((*f__doned)(p))
		{	f__pc++;
			goto loop;
		}
		f__pc++;
		continue;
	case ED:
		if(f__cnt[f__cp]<=0)
		{	f__cp--;
			f__pc++;
			goto loop;
		}
		if(ptr==NULL)
			return((*f__doend)());
		f__cnt[f__cp]--;
		f__workdone=1;
		if((n=(*f__doed)(p,ptr,len))>0)
			errfl(f__elist->cierr,errno,"fmt");
		if(n<0)
			err(f__elist->ciend,(EOF),"fmt");
		continue;
	case STACK:
		f__cnt[++f__cp]=p->p1;
		f__pc++;
		goto loop;
	case RET1:
		f__ret[++f__rp]=p->p1;
		f__pc++;
		goto loop;
	case GOTO:
		if(--f__cnt[f__cp]<=0)
		{	f__cp--;
			f__rp--;
			f__pc++;
			goto loop;
		}
		f__pc=1+f__ret[f__rp--];
		goto loop;
	case REVERT:
		f__rp=f__cp=0;
		f__pc = p->p1;
		if(ptr==NULL)
			return((*f__doend)());
		if(!f__workdone) return(0);
		if((n=(*f__dorevert)()) != 0) return(n);
		goto loop;
	case COLON:
		if(ptr==NULL)
			return((*f__doend)());
		f__pc++;
		goto loop;
	case NONL:
		f__nonl = 1;
		f__pc++;
		goto loop;
	case S:
	case SS:
		f__cplus=0;
		f__pc++;
		goto loop;
	case SP:
		f__cplus = 1;
		f__pc++;
		goto loop;
	case P:	f__scale=p->p1;
		f__pc++;
		goto loop;
	case BN:
		f__cblank=0;
		f__pc++;
		goto loop;
	case BZ:
		f__cblank=1;
		f__pc++;
		goto loop;
	}
	}
	return(0);
}
en_fio(Void)
{	ftnint one=1;
	return(do_fio(&one,(char *)NULL,(ftnint)0));
}
 VOID
fmt_bg(Void)
{
	f__workdone=f__cp=f__rp=f__pc=f__cursor=0;
	f__cnt[0]=f__ret[0]=0;
}
