#include "f2c.h"

#ifdef KR_headers
extern VOID sig_die();
VOID c_div(resx, a, b)
complex *a, *b, *resx;
#else
extern void sig_die(char*,int);
void c_div(complex *resx, complex *a, complex *b)
#endif
{
double ratio, den;
double abr, abi;
complex res;

if( (abr = b->r) < 0.)
	abr = - abr;
if( (abi = b->i) < 0.)
	abi = - abi;
if( abr <= abi )
	{
	if(abi == 0)
		sig_die("complex division by zero", 1);
	ratio = (double)b->r / b->i ;
	den = b->i * (1 + ratio*ratio);
	res.r = (a->r*ratio + a->i) / den;
	res.i = (a->i*ratio - a->r) / den;
	}

else
	{
	ratio = (double)b->i / b->r ;
	den = b->r * (1 + ratio*ratio);
	res.r = (a->r + a->i*ratio) / den;
	res.i = (a->i - a->r*ratio) / den;
	}

resx->r = res.r;
resx->i = res.i;
}
