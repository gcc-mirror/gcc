#include "f2c.h"

#ifdef KR_headers
double log(), f__cabs(), atan2();
VOID z_log(r, z) doublecomplex *r, *z;
#else
#undef abs
#include "math.h"
extern double f__cabs(double, double);
void z_log(doublecomplex *r, doublecomplex *z)
#endif
{
	double s, s0, t, t2, u, v;
	double zi = z->i, zr = z->r;

	r->i = atan2(zi, zr);
#ifdef Pre20000310
	r->r = log( f__cabs( zr, zi ) );
#else
	if (zi < 0)
		zi = -zi;
	if (zr < 0)
		zr = -zr;
	if (zr < zi) {
		t = zi;
		zi = zr;
		zr = t;
		}
	t = zi/zr;
	s = zr * sqrt(1 + t*t);
	/* now s = f__cabs(zi,zr), and zr = |zr| >= |zi| = zi */
	if ((t = s - 1) < 0)
		t = -t;
	if (t > .01)
		r->r = log(s);
	else {

#ifdef Comment

	log(1+x) = x - x^2/2 + x^3/3 - x^4/4 + - ...

		 = x(1 - x/2 + x^2/3 -+...)

	[sqrt(y^2 + z^2) - 1] * [sqrt(y^2 + z^2) + 1] = y^2 + z^2 - 1, so

	sqrt(y^2 + z^2) - 1 = (y^2 + z^2 - 1) / [sqrt(y^2 + z^2) + 1]

#endif /*Comment*/

		t = ((zr*zr - 1.) + zi*zi) / (s + 1);
		t2 = t*t;
		s = 1. - 0.5*t;
		u = v = 1;
		do {
			s0 = s;
			u *= t2;
			v += 2;
			s += u/v - t*u/(v+1);
			} while(s > s0);
		r->r = s*t;
		}
#endif
	}
