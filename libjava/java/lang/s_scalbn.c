 * software is freely granted, provided that this notice
/*
 * scalbn(x,n) returns x* 2**n  computed by  exponent
 * manipulation rather than by actually performing an
	int32_t  k,hx,lx;
	    x *= two54;
	    k = ((hx&0x7ff00000)>>20) - 54;
        k = k+n;
