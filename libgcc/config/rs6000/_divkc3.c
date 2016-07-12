typedef float KFtype __attribute__ ((mode (KF)));
typedef __complex float KCtype __attribute__ ((mode (KC)));

#define COPYSIGN(x,y) __builtin_copysignq (x, y)
#define INFINITY __builtin_infq ()
#define FABS __builtin_fabsq
#define isnan __builtin_isnan
#define isinf __builtin_isinf
#define isfinite __builtin_isfinite

KCtype
__divkc3 (KFtype a, KFtype b, KFtype c, KFtype d)
{
  KFtype denom, ratio, x, y;
  KCtype res;

  /* ??? We can get better behavior from logarithmic scaling instead of
     the division.  But that would mean starting to link libgcc against
     libm.  We could implement something akin to ldexp/frexp as gcc builtins
     fairly easily...  */
  if (FABS (c) < FABS (d))
    {
      ratio = c / d;
      denom = (c * ratio) + d;
      x = ((a * ratio) + b) / denom;
      y = ((b * ratio) - a) / denom;
    }
  else
    {
      ratio = d / c;
      denom = (d * ratio) + c;
      x = ((b * ratio) + a) / denom;
      y = (b - (a * ratio)) / denom;
    }

  /* Recover infinities and zeros that computed as NaN+iNaN; the only cases
     are nonzero/zero, infinite/finite, and finite/infinite.  */
  if (isnan (x) && isnan (y))
    {
      if (c == 0.0 && d == 0.0 && (!isnan (a) || !isnan (b)))
	{
	  x = COPYSIGN (INFINITY, c) * a;
	  y = COPYSIGN (INFINITY, c) * b;
	}
      else if ((isinf (a) || isinf (b)) && isfinite (c) && isfinite (d))
	{
	  a = COPYSIGN (isinf (a) ? 1 : 0, a);
	  b = COPYSIGN (isinf (b) ? 1 : 0, b);
	  x = INFINITY * (a * c + b * d);
	  y = INFINITY * (b * c - a * d);
	}
      else if ((isinf (c) || isinf (d)) && isfinite (a) && isfinite (b))
	{
	  c = COPYSIGN (isinf (c) ? 1 : 0, c);
	  d = COPYSIGN (isinf (d) ? 1 : 0, d);
	  x = 0.0 * (a * c + b * d);
	  y = 0.0 * (b * c - a * d);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}
