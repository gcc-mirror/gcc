 * software is freely granted, provided that this notice
 *	2. Perform y*log2(x) = n+y' by simulating muti-precision
 *	always returns the correct integer provided it is
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
static const double
static double
	int32_t i,j,k,yisint,n;
	int32_t hx,hy,ix,iy;
	uint32_t lx,ly;
	if((iy|ly)==0) return one;
	   iy > 0x7ff00000 || ((iy==0x7ff00000)&&(ly!=0)))
		return x+y;
	if(hx<0) {
		    if((uint32_t)(j<<(52-k))==ly) yisint = 2-(j&1);
	    }
	}
	if(ly==0) {
	    }
		return __ieee754_sqrt(x);
		    } else if(yisint==1)

	if(((((uint32_t)hx>>31)-1)|yisint)==0) return (x-x)/(x-x);
	/* now |1-x| is tiny <= 2**-20, suffice to compute
	if(((((uint32_t)hx>>31)-1)|(yisint-1))==0)
	}
