 * software is freely granted, provided that this notice
 *
 * __kernel_rem_pio2 return the last three digits of N with
 * The method is to compute the integer (mod 8) and fraction parts of
 * 	x[]	The input value (must be positive) is broken into nx
 *		x[i] will be the i-th 24 bit of x. The scaled exponent
 *		of x[0] is given in input parameter e0 (i.e., x[0]*2^e0
 *		integer array, contains the (24*i)-th to (24*i+23)-th
 *		bit of 2/pi after binary point. The corresponding
 * 	jz	local integer variable indicating the number of
 *		terms of ipio2[] used.
 *		into 24 bits chunks.
 *	f[]	ipio2[] in floating point
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
static int init_jk[] = {2,3,4,6};
static const double
static double
	int __kernel_rem_pio2(double *x, double *y, int e0, int nx, int prec, const int32_t *ipio2)
	int __kernel_rem_pio2(x,y,e0,nx,prec,ipio2)
	double x[], y[]; int e0,nx,prec; int32_t ipio2[];
	int32_t jz,jx,jv,jp,jk,carry,n,iq[20],i,j,k,m,q0,ih;
	    fw    =  (double)((int32_t)(twon24* z));
	    iq[i] =  (int32_t)(z-two24*fw);
	n  = (int32_t) z;
	}
	    if(z>=two24) {
		fw = (double)((int32_t)(twon24*z));
		iq[jz] = (int32_t)(z-two24*fw);
		iq[jz] = (int32_t) fw;
	    } else iq[jz] = (int32_t) z ;
		y[0] = (ih==0)? fw: -fw;
		for (i=jz;i>=0;i--) fw += fq[i];
		y[0] = (ih==0)? fw: -fw;
		y[1] = (ih==0)? fw: -fw;
		    fw      = fq[i-1]+fq[i];
		    fw      = fq[i-1]+fq[i];
		for (fw=0.0,i=jz;i>=2;i--) fw += fq[i];
