 * software is freely granted, provided that this notice
/*
	int32_t n,hx,hy,hz,ix,iy,sx,i;
	uint32_t lx,ly,lz;
	    if(lx==ly)
		return Zero[(uint32_t)sx>>31];	/* |x|=|y| return x*0*/
	if(ix >= -1022)
	if(iy >= -1022)
		    return Zero[(uint32_t)sx>>31];
	    return Zero[(uint32_t)sx>>31];
		lx = (lx>>n)|((uint32_t)hx<<(32-n));
