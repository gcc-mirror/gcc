typedef float KFtype __attribute__ ((mode (KF)));
typedef __complex float KCtype __attribute__ ((mode (KC)));

#define COPYSIGN(x,y) __builtin_copysignq (x, y)
#define INFINITY __builtin_infq ()
#define isnan __builtin_isnan
#define isinf __builtin_isinf

KCtype
__mulkc3 (KFtype a, KFtype b, KFtype c, KFtype d)
{
  KFtype ac, bd, ad, bc, x, y;
  KCtype res;

  ac = a * c;
  bd = b * d;
  ad = a * d;
  bc = b * c;

  x = ac - bd;
  y = ad + bc;

  if (isnan (x) && isnan (y))
    {
      /* Recover infinities that computed as NaN + iNaN.  */
      _Bool recalc = 0;
      if (isinf (a) || isinf (b))
	{
	  /* z is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  a = COPYSIGN (isinf (a) ? 1 : 0, a);
	  b = COPYSIGN (isinf (b) ? 1 : 0, b);
	  if (isnan (c)) c = COPYSIGN (0, c);
	  if (isnan (d)) d = COPYSIGN (0, d);
          recalc = 1;
	}
     if (isinf (c) || isinf (d))
	{
	  /* w is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  c = COPYSIGN (isinf (c) ? 1 : 0, c);
	  d = COPYSIGN (isinf (d) ? 1 : 0, d);
	  if (isnan (a)) a = COPYSIGN (0, a);
	  if (isnan (b)) b = COPYSIGN (0, b);
	  recalc = 1;
	}
     if (!recalc
	  && (isinf (ac) || isinf (bd)
	      || isinf (ad) || isinf (bc)))
	{
	  /* Recover infinities from overflow by changing NaNs to 0.  */
	  if (isnan (a)) a = COPYSIGN (0, a);
	  if (isnan (b)) b = COPYSIGN (0, b);
	  if (isnan (c)) c = COPYSIGN (0, c);
	  if (isnan (d)) d = COPYSIGN (0, d);
	  recalc = 1;
	}
      if (recalc)
	{
	  x = INFINITY * (a * c - b * d);
	  y = INFINITY * (a * d + b * c);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}

