/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* Based on newlib/libm/mathfp/sf_fmod.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VS_MATH_FUNC (v64sf, fmodf, v64sf x, v64sf y)
{
  FUNCTION_INIT(v64sf);

  v64si hx, hy, hz;
  GET_FLOAT_WORD (hx, x, NO_COND);
  GET_FLOAT_WORD (hy, y, NO_COND);
  v64si sx = hx & 0x80000000;	/* sign of x */
  hx ^=sx;		/* |x| */
  hy &= 0x7fffffff;	/* |y| */

  v64sf zeroes = VECTOR_MERGE (VECTOR_INIT (-0.0f),
			       VECTOR_INIT (0.0f),
			       sx != 0);

  /* purge off exception values */
  VECTOR_IF ((hy == 0) | (hx >= 0x7f800000)
	     | (hy > 0x7f800000), cond)	// y=0, or x not finite or y is NaN
    VECTOR_RETURN ((x * y) / (x * y), cond);
  VECTOR_ENDIF
  VECTOR_IF (hx < hy, cond)		// |x|<|y| return x
    VECTOR_RETURN (x, cond);
  VECTOR_ENDIF
  VECTOR_IF (hx == hy, cond)
    VECTOR_RETURN (zeroes, hx == hy);	// |x|=|y| return x*0
  VECTOR_ENDIF

  /* determine ix = ilogb(x) */
  v64si ix;
  VECTOR_IF (hx < 0x00800000, cond)	// subnormal x
    ix = VECTOR_INIT (-126);
    for (v64si i = (hx << 8);
	 !ALL_ZEROES_P (cond & (i > 0));
	 i <<= 1)
      VECTOR_COND_MOVE (ix, ix - 1, cond & (i > 0));
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (ix, (hx >> 23) - 127, cond);
  VECTOR_ENDIF

  /* determine iy = ilogb(y) */
  v64si iy;
  VECTOR_IF (hy < 0x00800000, cond)	// subnormal y
    iy = VECTOR_INIT (-126);
    for (v64si i = (hy << 8); !ALL_ZEROES_P (cond & (i >= 0)); i <<= 1)
      VECTOR_COND_MOVE (iy, iy - 1, cond & (i >= 0));
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (iy, (hy >> 23) - 127, cond);
  VECTOR_ENDIF

/* set up {hx,lx}, {hy,ly} and align y to x */
  VECTOR_IF (ix >= -126, cond)
    VECTOR_COND_MOVE (hx, 0x00800000 | (0x007fffff & hx), cond);
  VECTOR_ELSE (cond)		// subnormal x, shift x to normal
    {
      v64si n = -126 - ix;
      VECTOR_COND_MOVE (hx, hx << n, cond);
    }
  VECTOR_ENDIF
  VECTOR_IF (iy >= -126, cond)
    VECTOR_COND_MOVE (hy, 0x00800000 | (0x007fffff & hy), cond);
  VECTOR_ELSE (cond)		// subnormal y, shift y to normal
    {
      v64si n = -126 - iy;
      VECTOR_COND_MOVE (hy, hy << n, cond);
    }
  VECTOR_ENDIF

/* fix point fmod */
  v64si n = ix - iy;
  v64si cond = n != 0;

  while (!ALL_ZEROES_P (cond))
    {
      hz = hx - hy;
      VECTOR_IF2 (hz < 0, cond2, cond)
	VECTOR_COND_MOVE (hx, hx + hx, cond2);
      VECTOR_ELSE2 (cond2, cond)
	VECTOR_IF2 (hz == 0, cond3, cond2)		// return sign(x)*0
	  VECTOR_RETURN (zeroes, cond3);
	VECTOR_ELSE2 (cond3, cond2)
	  VECTOR_COND_MOVE (hx, hz + hz, cond2);
	VECTOR_ENDIF
      VECTOR_ENDIF

      n += cond;	// Active lanes should be -1
      cond &= (n != 0);
    }

  hz = hx - hy;
  VECTOR_COND_MOVE (hx, hz, hz >= 0);

  /* convert back to floating value and restore the sign */
  VECTOR_RETURN (zeroes, hx == 0);	// return sign(x)*0

  cond = hx < 0x00800000;
  while (!ALL_ZEROES_P (cond))		// normalize x
    {
      VECTOR_COND_MOVE (hx, hx + hx, cond);
      iy += cond;	// Active lanes should be -1

      cond &= (hx < 0x00800000);
    }
  VECTOR_IF (iy >= -126, cond)		// normalize output
    VECTOR_COND_MOVE (hx, (hx - 0x00800000) | ((iy + 127) << 23), cond);
    SET_FLOAT_WORD (x, hx | sx, cond);
  VECTOR_ELSE (cond)		// subnormal output */
    n = -126 - iy;
    hx >>= n;
    SET_FLOAT_WORD (x, hx | sx, cond);
    x *= VECTOR_INIT (1.0f);		/* create necessary signal */
  VECTOR_ENDIF

  VECTOR_RETURN (x, NO_COND);	/* exact output */

  FUNCTION_RETURN;
}

DEF_VARIANTS2 (fmodf, sf, sf)
