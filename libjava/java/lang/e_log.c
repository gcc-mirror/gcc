 * software is freely granted, provided that this notice
 * Method :
 *   1. Argument Reduction: find k and f such that
 *			x = 2^k * (1+f),
 *      We use a special Reme algorithm on [0,0.1716] to generate
 * 	a polynomial of degree 14 to approximate R The maximum error
 *	    | Lg1*s +...+Lg7*s    -  R(z) | <= 2
 *
 *	3. Finally,  log(x) = k*ln2 + log(1+f).
 *	   Here ln2 is split into two floating point number:
 *	log(x) is NaN with signal if x < 0 (including -INF) ;
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
	int32_t k,hx,i,j;
	uint32_t lx;
	    if (((hx&0x7fffffff)|lx)==0)
	}
 	s = f/(2.0+f);
	t1= w*(Lg2+w*(Lg4+w*Lg6));
	t2= z*(Lg1+w*(Lg3+w*(Lg5+w*Lg7)));
