#include "f2c.h"

extern void sig_die (char *, int);
void
z_div (doublecomplex * c, doublecomplex * a, doublecomplex * b)
{
  double ratio, den;
  double abr, abi, cr;

  if ((abr = b->r) < 0.)
    abr = -abr;
  if ((abi = b->i) < 0.)
    abi = -abi;
  if (abr <= abi)
    {
      if (abi == 0)
	{
#ifdef IEEE_COMPLEX_DIVIDE
	  if (a->i != 0 || a->r != 0)
	    abi = 1.;
	  c->i = c->r = abi / abr;
	  return;
#else
	  sig_die ("complex division by zero", 1);
#endif
	}
      ratio = b->r / b->i;
      den = b->i * (1 + ratio * ratio);
      cr = (a->r * ratio + a->i) / den;
      c->i = (a->i * ratio - a->r) / den;
    }

  else
    {
      ratio = b->i / b->r;
      den = b->r * (1 + ratio * ratio);
      cr = (a->r + a->i * ratio) / den;
      c->i = (a->i - a->r * ratio) / den;
    }
  c->r = cr;
}
