 * software is freely granted, provided that this notice
 *
 * return the remainder of x rem pi/2 in y[0]+y[1]
 * Table of constants for 2/pi, 396 Hex digits (476 decimal) of 2/pi
static const int32_t two_over_pi[] = {
static int32_t two_over_pi[] = {
0xA2F983, 0x6E4E44, 0x1529FC, 0x2757D1, 0xF534DD, 0xC0DB62,
0x95993C, 0x439041, 0xFE5163, 0xABDEBB, 0xC561B7, 0x246E3A,
0x424DD2, 0xE00649, 0x2EEA09, 0xD1921C, 0xFE1DEB, 0x1CB129,
0xA73EE8, 0x8235F5, 0x2EBB44, 0x84E99C, 0x7026B4, 0x5F7E41,
0x3991D6, 0x398353, 0x39F49C, 0x845F8B, 0xBDF928, 0x3B1FF8,
0x97FFDE, 0x05980F, 0xEF2F11, 0x8B5A0A, 0x6D1F6D, 0x367ECF,
0x27CB09, 0xB74F46, 0x3F669E, 0x5FEA2D, 0x7527BA, 0xC7EBE5,
0xF17B3D, 0x0739F7, 0x8A5292, 0xEA6BFB, 0x5FB11F, 0x8D5D08,
0x560330, 0x46FC7B, 0x6BABF0, 0xCFBC20, 0x9AF436, 0x1DA9E3,
0x91615E, 0xE61B08, 0x659985, 0x5F14A0, 0x68408D, 0xFFD880,
0x4D7327, 0x310606, 0x1556CA, 0x73A8C9, 0x60E27B, 0xC08C6B,
static const int32_t npio2_hw[] = {
static int32_t npio2_hw[] = {
static const double
static double
	int32_t __ieee754_rem_pio2(double x, double *y)
	int32_t __ieee754_rem_pio2(x,y)
	int32_t i,j,n,ix,hx;
	uint32_t low;
	    if(hx>0) {
	    n  = (int32_t) (t*invpio2+half);
	    if(n<32&&ix!=npio2_hw[n-1]) {
	        uint32_t high;
	        y[0] = r-w;
		    w  = fn*pio2_2;
		    w  = fn*pio2_2t-((t-r)-w);
		    	w  = fn*pio2_3;
		    	w  = fn*pio2_3t-((t-r)-w);
    /*
	SET_HIGH_WORD(z, ix - ((int32_t)e0<<20));
		tx[i] = (double)((int32_t)(z));
