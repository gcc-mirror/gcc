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

/* Based on newlib/libm/mathfp/s_fmod.c in Newlib.  */

#include "amdgcnmach.h"

DEF_VD_MATH_FUNC (v64df, fmod, v64df x, v64df y)
{
  FUNCTION_INIT(v64df);

  v64si hx, hy, hz;
  v64usi lx, ly, lz;
  EXTRACT_WORDS (hx, lx, x);
  EXTRACT_WORDS (hy, ly, y);
  v64si sx = hx & 0x80000000;	/* sign of x */
  hx ^=sx;		/* |x| */
  hy &= 0x7fffffff;	/* |y| */

  v64df zeroes = VECTOR_MERGE (VECTOR_INIT (-0.0),
			       VECTOR_INIT (0.0),
			       sx != 0);

  /* purge off exception values */
  VECTOR_IF (((hy | ly) == 0) | (hx >= 0x7ff00000)
	     | ((hy | ((ly | -ly) >> 31)) > 0x7ff00000), cond)	// y=0, or x not finite or y is NaN
    VECTOR_RETURN ((x * y) / (x * y), cond);
  VECTOR_ENDIF
  VECTOR_IF (hx <= hy, cond)		// |x|<|y| return x
    VECTOR_IF2 ((hx < hy) | (lx < ly), cond2, cond)
      VECTOR_RETURN (x, cond);
    VECTOR_ENDIF
    VECTOR_IF2 (lx == ly, cond2, cond)
      VECTOR_RETURN (zeroes, cond2);
    VECTOR_ENDIF
  VECTOR_ENDIF

  /* determine ix = ilogb(x) */
  v64si ix;
  VECTOR_IF (hx < 0x00100000, cond)	// subnormal x
    VECTOR_IF2 (hx == 0, cond2, cond)
      ix = VECTOR_INIT (-1043);
      for (v64si i = __builtin_convertvector (lx, v64si);
	   !ALL_ZEROES_P (cond2 & (i > 0));
	   i <<= 1)
	VECTOR_COND_MOVE (ix, ix - 1, cond2 & (i > 0));
    VECTOR_ELSE2 (cond2, cond)
      ix = VECTOR_INIT (-1022);
      for (v64si i = __builtin_convertvector (hx << 11, v64si);
	   !ALL_ZEROES_P (cond2 & (i > 0));
	   i <<= 1)
	VECTOR_COND_MOVE (ix, ix - 1, cond2 & (i > 0));
    VECTOR_ENDIF
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (ix, (hx >> 20) - 1023, cond);
  VECTOR_ENDIF

  /* determine iy = ilogb(y) */
  v64si iy;
  VECTOR_IF (hy < 0x00100000, cond)	// subnormal y
    VECTOR_IF2 (hy == 0, cond2, cond)
      iy = VECTOR_INIT (-1043);
      for (v64si i = __builtin_convertvector (ly, v64si);
	   !ALL_ZEROES_P (cond2 & (i > 0));
	   i <<= 1)
	VECTOR_COND_MOVE (iy, iy - 1, cond2 & (i > 0));
    VECTOR_ELSE2 (cond2, cond)
      iy = VECTOR_INIT (-1022);
      for (v64si i = __builtin_convertvector (hy << 11, v64si);
	   !ALL_ZEROES_P (cond2 & (i > 0));
	   i <<= 1)
	VECTOR_COND_MOVE (iy, iy - 1, cond2 & (i > 0));
    VECTOR_ENDIF
  VECTOR_ELSE (cond)
    VECTOR_COND_MOVE (iy, (hy >> 20) - 1023, cond);
  VECTOR_ENDIF


/* set up {hx,lx}, {hy,ly} and align y to x */
  VECTOR_IF (ix >= -1022, cond)
    VECTOR_COND_MOVE (hx, 0x00100000 | (0x000fffff & hx), cond);
  VECTOR_ELSE (cond)		// subnormal x, shift x to normal
    {
      v64si n = -1022 - ix;
      VECTOR_IF2 (n <= 31, cond2, cond)
	VECTOR_COND_MOVE (hx, (hx << n) | (lx >> (32 - n)), cond2);
	VECTOR_COND_MOVE (lx, lx << n, cond2);
      VECTOR_ELSE2 (cond2, cond)
	VECTOR_COND_MOVE (hx, __builtin_convertvector (lx << (n - 32), v64si), cond2);
	VECTOR_COND_MOVE (lx, VECTOR_INIT (0U), cond2);
      VECTOR_ENDIF
    }
  VECTOR_ENDIF
  VECTOR_IF (iy >= -1022, cond)
    VECTOR_COND_MOVE (hy, 0x00100000 | (0x000fffff & hy), cond);
  VECTOR_ELSE (cond)		// subnormal y, shift y to normal
    {
      v64si n = -1022 - iy;
      VECTOR_IF2 (n <= 31, cond2, cond)
	VECTOR_COND_MOVE (hy, (hy << n) | (ly >> (32 - n)), cond2);
	VECTOR_COND_MOVE (ly, ly << n, cond2);
      VECTOR_ELSE2 (cond2, cond)
	VECTOR_COND_MOVE (hy, __builtin_convertvector (ly << (n - 32), v64si), cond2);
	VECTOR_COND_MOVE (ly, VECTOR_INIT (0U), cond2);
      VECTOR_ENDIF
    }
  VECTOR_ENDIF

/* fix point fmod */
  v64si n = ix - iy;
  v64si cond = n != 0;

  while (!ALL_ZEROES_P (cond))
    {
      hz = hx - hy;
      lz = lx - ly;
      VECTOR_IF2 (lx < ly, cond2, cond)
	VECTOR_COND_MOVE (hz, hz - 1, cond2);
      VECTOR_ENDIF
      VECTOR_IF2 (hz < 0, cond2, cond)
	VECTOR_COND_MOVE (hx, hx + hx + (__builtin_convertvector(lx, v64usi) >> 31), cond2);
        VECTOR_COND_MOVE (lx, lx + lx, cond2);
      VECTOR_ELSE2 (cond2, cond)
	VECTOR_IF2 ((hz | lz) == 0, cond3, cond2)		// return sign(x)*0
	  VECTOR_RETURN (zeroes, cond3);
	VECTOR_ENDIF
        VECTOR_COND_MOVE (hx, hz + hz + (__builtin_convertvector(lz, v64usi) >> 31), cond2);
        VECTOR_COND_MOVE (lx, lz + lz, cond2);
      VECTOR_ENDIF

      n += cond;	// Active lanes should be -1
      cond &= (n != 0);
    }

  hz = hx - hy;
  lz = lx - ly;
  VECTOR_COND_MOVE (hz, hz - 1, lx < ly);
  VECTOR_IF (hz >= 0, cond)
    VECTOR_COND_MOVE (hx, hz, cond);
    VECTOR_COND_MOVE (lx, lz, cond);
  VECTOR_ENDIF

  /* convert back to floating value and restore the sign */
  VECTOR_RETURN (zeroes, (hx | lx) == 0);	// return sign(x)*0
  cond = hx < 0x00100000;
  while (!ALL_ZEROES_P (cond))		// normalize x
    {
      VECTOR_COND_MOVE (hx, hx + hx + (lx >> 31), cond);
      VECTOR_COND_MOVE (lx, lx + lx, cond);
      iy += cond;	// Active lanes should be -1

      cond &= (hx < 0x00100000);
    }
  VECTOR_IF (iy >= -1022, cond) // normalize output
    VECTOR_COND_MOVE (hx, (hx - 0x00100000) | ((iy + 1023) << 20), cond);
    INSERT_WORDS (x, hx | sx, lx, cond);
  VECTOR_ELSE (cond)		// subnormal output */
    n = -1022 - iy;
    VECTOR_IF2 (n <= 20, cond2, cond)
      VECTOR_COND_MOVE (lx, (lx >> n) | (hx << (32 - n)), cond2);
      VECTOR_COND_MOVE (hx, hx >> n, cond2);
    VECTOR_ELSEIF2 (n <= 31, cond2, cond)
      VECTOR_COND_MOVE (lx, __builtin_convertvector ((hx << (32 - n)) | (lx >> n), v64usi), cond2);
      VECTOR_COND_MOVE (hx, sx, cond2);
    VECTOR_ELSE2 (cond2, cond)
      VECTOR_COND_MOVE (lx, __builtin_convertvector (hx >> (n - 32), v64usi), cond2);
      VECTOR_COND_MOVE (hx, sx, cond2);
    VECTOR_ENDIF
    INSERT_WORDS (x, hx | sx, lx, cond);
    x *= VECTOR_INIT (1.0);		/* create necessary signal */
  VECTOR_ENDIF

  VECTOR_RETURN (x, NO_COND);	/* exact output */
  FUNCTION_RETURN;
}

DEF_VARIANTS2 (fmod, df, df)
