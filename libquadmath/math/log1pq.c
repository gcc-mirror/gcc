/*							log1pq.c
 *
 *      Relative error logarithm
 *	Natural logarithm of 1+x, 128-bit long double precision
 *
 *
 *
 * SYNOPSIS:
 *
 * long double x, y, log1pq();
 *
 * y = log1pq( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns the base e (2.718...) logarithm of 1+x.
 *
 * The argument 1+x is separated into its exponent and fractional
 * parts.  If the exponent is between -1 and +1, the logarithm
 * of the fraction is approximated by
 *
 *     log(1+x) = x - 0.5 x^2 + x^3 P(x)/Q(x).
 *
 * Otherwise, setting  z = 2(w-1)/(w+1),
 *
 *     log(w) = z + z^3 P(z)/Q(z).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      -1, 8       100000      1.9e-34     4.3e-35
 */

/* Copyright 2001 by Stephen L. Moshier

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, see
    <http://www.gnu.org/licenses/>.  */

#include "quadmath-imp.h"

/* Coefficients for log(1+x) = x - x^2 / 2 + x^3 P(x)/Q(x)
 * 1/sqrt(2) <= 1+x < sqrt(2)
 * Theoretical peak relative error = 5.3e-37,
 * relative peak error spread = 2.3e-14
 */
static const __float128
  P12 = 1.538612243596254322971797716843006400388E-6Q,
  P11 = 4.998469661968096229986658302195402690910E-1Q,
  P10 = 2.321125933898420063925789532045674660756E1Q,
  P9 = 4.114517881637811823002128927449878962058E2Q,
  P8 = 3.824952356185897735160588078446136783779E3Q,
  P7 = 2.128857716871515081352991964243375186031E4Q,
  P6 = 7.594356839258970405033155585486712125861E4Q,
  P5 = 1.797628303815655343403735250238293741397E5Q,
  P4 = 2.854829159639697837788887080758954924001E5Q,
  P3 = 3.007007295140399532324943111654767187848E5Q,
  P2 = 2.014652742082537582487669938141683759923E5Q,
  P1 = 7.771154681358524243729929227226708890930E4Q,
  P0 = 1.313572404063446165910279910527789794488E4Q,
  /* Q12 = 1.000000000000000000000000000000000000000E0L, */
  Q11 = 4.839208193348159620282142911143429644326E1Q,
  Q10 = 9.104928120962988414618126155557301584078E2Q,
  Q9 = 9.147150349299596453976674231612674085381E3Q,
  Q8 = 5.605842085972455027590989944010492125825E4Q,
  Q7 = 2.248234257620569139969141618556349415120E5Q,
  Q6 = 6.132189329546557743179177159925690841200E5Q,
  Q5 = 1.158019977462989115839826904108208787040E6Q,
  Q4 = 1.514882452993549494932585972882995548426E6Q,
  Q3 = 1.347518538384329112529391120390701166528E6Q,
  Q2 = 7.777690340007566932935753241556479363645E5Q,
  Q1 = 2.626900195321832660448791748036714883242E5Q,
  Q0 = 3.940717212190338497730839731583397586124E4Q;

/* Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2),
 * where z = 2(x-1)/(x+1)
 * 1/sqrt(2) <= x < sqrt(2)
 * Theoretical peak relative error = 1.1e-35,
 * relative peak error spread 1.1e-9
 */
static const __float128
  R5 = -8.828896441624934385266096344596648080902E-1Q,
  R4 = 8.057002716646055371965756206836056074715E1Q,
  R3 = -2.024301798136027039250415126250455056397E3Q,
  R2 = 2.048819892795278657810231591630928516206E4Q,
  R1 = -8.977257995689735303686582344659576526998E4Q,
  R0 = 1.418134209872192732479751274970992665513E5Q,
  /* S6 = 1.000000000000000000000000000000000000000E0L, */
  S5 = -1.186359407982897997337150403816839480438E2Q,
  S4 = 3.998526750980007367835804959888064681098E3Q,
  S3 = -5.748542087379434595104154610899551484314E4Q,
  S2 = 4.001557694070773974936904547424676279307E5Q,
  S1 = -1.332535117259762928288745111081235577029E6Q,
  S0 = 1.701761051846631278975701529965589676574E6Q;

/* C1 + C2 = ln 2 */
static const __float128 C1 = 6.93145751953125E-1Q;
static const __float128 C2 = 1.428606820309417232121458176568075500134E-6Q;

static const __float128 sqrth = 0.7071067811865475244008443621048490392848Q;
/* ln (2^16384 * (1 - 2^-113)) */
static const __float128 zero = 0;

__float128
log1pq (__float128 xm1)
{
  __float128 x, y, z, r, s;
  ieee854_float128 u;
  int32_t hx;
  int e;

  /* Test for NaN or infinity input. */
  u.value = xm1;
  hx = u.words32.w0;
  if ((hx & 0x7fffffff) >= 0x7fff0000)
    return xm1 + fabsq (xm1);

  /* log1p(+- 0) = +- 0.  */
  if (((hx & 0x7fffffff) == 0)
      && (u.words32.w1 | u.words32.w2 | u.words32.w3) == 0)
    return xm1;

  if ((hx & 0x7fffffff) < 0x3f8e0000)
    {
      math_check_force_underflow (xm1);
      if ((int) xm1 == 0)
	return xm1;
    }

  if (xm1 >= 0x1p113Q)
    x = xm1;
  else
    x = xm1 + 1;

  /* log1p(-1) = -inf */
  if (x <= 0)
    {
      if (x == 0)
	return (-1 / zero);  /* log1p(-1) = -inf */
      else
	return (zero / (x - x));
    }

  /* Separate mantissa from exponent.  */

  /* Use frexp used so that denormal numbers will be handled properly.  */
  x = frexpq (x, &e);

  /* Logarithm using log(x) = z + z^3 P(z^2)/Q(z^2),
     where z = 2(x-1)/x+1).  */
  if ((e > 2) || (e < -2))
    {
      if (x < sqrth)
	{			/* 2( 2x-1 )/( 2x+1 ) */
	  e -= 1;
	  z = x - 0.5Q;
	  y = 0.5Q * z + 0.5Q;
	}
      else
	{			/*  2 (x-1)/(x+1)   */
	  z = x - 0.5Q;
	  z -= 0.5Q;
	  y = 0.5Q * x + 0.5Q;
	}
      x = z / y;
      z = x * x;
      r = ((((R5 * z
	      + R4) * z
	     + R3) * z
	    + R2) * z
	   + R1) * z
	+ R0;
      s = (((((z
	       + S5) * z
	      + S4) * z
	     + S3) * z
	    + S2) * z
	   + S1) * z
	+ S0;
      z = x * (z * r / s);
      z = z + e * C2;
      z = z + x;
      z = z + e * C1;
      return (z);
    }


  /* Logarithm using log(1+x) = x - .5x^2 + x^3 P(x)/Q(x). */

  if (x < sqrth)
    {
      e -= 1;
      if (e != 0)
	x = 2 * x - 1;	/*  2x - 1  */
      else
	x = xm1;
    }
  else
    {
      if (e != 0)
	x = x - 1;
      else
	x = xm1;
    }
  z = x * x;
  r = (((((((((((P12 * x
		 + P11) * x
		+ P10) * x
	       + P9) * x
	      + P8) * x
	     + P7) * x
	    + P6) * x
	   + P5) * x
	  + P4) * x
	 + P3) * x
	+ P2) * x
       + P1) * x
    + P0;
  s = (((((((((((x
		 + Q11) * x
		+ Q10) * x
	       + Q9) * x
	      + Q8) * x
	     + Q7) * x
	    + Q6) * x
	   + Q5) * x
	  + Q4) * x
	 + Q3) * x
	+ Q2) * x
       + Q1) * x
    + Q0;
  y = x * (z * r / s);
  y = y + e * C2;
  z = y - 0.5Q * z;
  z = z + x;
  z = z + e * C1;
  return (z);
}
