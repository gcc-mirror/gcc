#include "f2c.h"

extern void z_div (doublecomplex *, doublecomplex *, doublecomplex *);
void
pow_zi (doublecomplex * p, doublecomplex * a, integer * b)	/* p = a**b  */
{
  integer n;
  unsigned long u;
  double t;
  doublecomplex q, x;
  static doublecomplex one = { 1.0, 0.0 };

  n = *b;
  q.r = 1;
  q.i = 0;

  if (n == 0)
    goto done;
  if (n < 0)
    {
      n = -n;
      z_div (&x, &one, a);
    }
  else
    {
      x.r = a->r;
      x.i = a->i;
    }

  for (u = n;;)
    {
      if (u & 01)
	{
	  t = q.r * x.r - q.i * x.i;
	  q.i = q.r * x.i + q.i * x.r;
	  q.r = t;
	}
      if (u >>= 1)
	{
	  t = x.r * x.r - x.i * x.i;
	  x.i = 2 * x.r * x.i;
	  x.r = t;
	}
      else
	break;
    }
done:
  p->i = q.i;
  p->r = q.r;
}
