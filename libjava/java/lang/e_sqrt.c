 * software is freely granted, provided that this notice
 * Method:
 *   Bit by bit method using integer arithmetic. (Slow, but portable)
 *	Scale x to y in [1,4) with even powers of 2:
 *
 *	To compute q    from q , one checks whether
 *		    i+1       i
 *	that (2) is equivalent to
 *	The advantage of (3) is that s  and y  can be computed by
 *
 *	One may easily use induction to prove (4) and (5).
 *	      it does not necessary to do a full (53-bit) comparison
 *
	int32_t sign = (int)0x80000000;
	uint32_t r,t1,s1,ix1,q1;
	int32_t ix0,s0,q,m,t,i;
	if((ix0&0x7ff00000)==0x7ff00000) {
	}
	    t = s0+r;
	    if(t<=ix0) {
		s0   = t+r;
		ix0 -= t;
		q   += r;
	    }
	    t1 = s1+r;
	    if((t<ix0)||((t==ix0)&&(t1<=ix1))) {
		if(((t1&sign)==(uint32_t)sign)&&(s1&sign)==0) s0 += 1;
	        if (q1==(uint32_t)0xffffffff) { q1=0; q += 1;}
		    if (q1==(uint32_t)0xfffffffe) q+=1;
		    q1+=2;

(This is a copy of a drafted paper by Prof W. Kahan
	Two algorithms are given here to implement sqrt(x)
	to chop results of arithmetic operations instead of round them,
	is executed exactly with no roundoff error, all part of the
	a floating point number x (in IEEE double format) respectively

	Apply Heron's rule three times to y, we have y approximates
	is slow. If division is very slow, then one should use the
	By twiddling y's last bit it is possible to force y to be

	Here k is a 32-bit integer and T2[] is an integer array
	to about 1 ulp. To be exact, we will have

	(a) the term z*y in the final iteration is always less than 1;
	By twiddling y's last bit it is possible to force y to be
	    k := z1 >> 26;		... get z's 25-th and 26-th
	If multiplication is cheaper then the foregoing red tape, the
	Note that z*z can overwrite I; this value must be sensed if it is
		z1: |        f2        |
    (4)	Special cases (see (4) of Section A).

