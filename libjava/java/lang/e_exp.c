 * software is freely granted, provided that this notice
 *               x = k*ln2 + r,  |r| <= 0.5*ln2.
 *      Here r will be represented as r = hi-lo for better
 *      We use a special Reme algorithm on [0,0.34658] to generate
 * 	a polynomial of degree 5 to approximate R. The maximum error
 *	    | 2.0+P1*z+...+P5*z   -  R(z) | <= 2
 *                                 r*R1(r)
 *
 *	For IEEE double
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
	int32_t k,xsb;
	uint32_t hx;
	        uint32_t lx;
		if(((hx&0xfffff)|lx)!=0)
	if(hx > 0x3fd62e42) {		/* if  |x| > 0.5 ln2 */
	}
	if(k==0) 	return one-((x*c)/(c-2.0)-x);
	    uint32_t hy;
	    uint32_t hy;
