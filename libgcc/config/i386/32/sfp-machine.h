#define _FP_W_TYPE_SIZE		32
#define _FP_W_TYPE		unsigned int
#define _FP_WS_TYPE		signed int
#define _FP_I_TYPE		int

#define __FP_FRAC_ADD_4(r3,r2,r1,r0,x3,x2,x1,x0,y3,y2,y1,y0)	\
  __asm__ ("add{l} {%11,%3|%3,%11}\n\t"				\
	   "adc{l} {%9,%2|%2,%9}\n\t"				\
	   "adc{l} {%7,%1|%1,%7}\n\t"				\
	   "adc{l} {%5,%0|%0,%5}"				\
	   : "=r" ((USItype) (r3)),				\
	     "=&r" ((USItype) (r2)),				\
	     "=&r" ((USItype) (r1)),				\
	     "=&r" ((USItype) (r0))				\
	   : "%0" ((USItype) (x3)),				\
	     "g" ((USItype) (y3)),				\
	     "%1" ((USItype) (x2)),				\
	     "g" ((USItype) (y2)),				\
	     "%2" ((USItype) (x1)),				\
	     "g" ((USItype) (y1)),				\
	     "%3" ((USItype) (x0)),				\
	     "g" ((USItype) (y0)))
#define __FP_FRAC_ADD_3(r2,r1,r0,x2,x1,x0,y2,y1,y0)		\
  __asm__ ("add{l} {%8,%2|%2,%8}\n\t"				\
	   "adc{l} {%6,%1|%1,%6}\n\t"				\
	   "adc{l} {%4,%0|%0,%4}"				\
	   : "=r" ((USItype) (r2)),				\
	     "=&r" ((USItype) (r1)),				\
	     "=&r" ((USItype) (r0))				\
	   : "%0" ((USItype) (x2)),				\
	     "g" ((USItype) (y2)),				\
	     "%1" ((USItype) (x1)),				\
	     "g" ((USItype) (y1)),				\
	     "%2" ((USItype) (x0)),				\
	     "g" ((USItype) (y0)))
#define __FP_FRAC_SUB_4(r3,r2,r1,r0,x3,x2,x1,x0,y3,y2,y1,y0)	\
  __asm__ ("sub{l} {%11,%3|%3,%11}\n\t"				\
	   "sbb{l} {%9,%2|%2,%9}\n\t"				\
	   "sbb{l} {%7,%1|%1,%7}\n\t"				\
	   "sbb{l} {%5,%0|%0,%5}"				\
	   : "=r" ((USItype) (r3)),				\
	     "=&r" ((USItype) (r2)),				\
	     "=&r" ((USItype) (r1)),				\
	     "=&r" ((USItype) (r0))				\
	   : "0" ((USItype) (x3)),				\
	     "g" ((USItype) (y3)),				\
	     "1" ((USItype) (x2)),				\
	     "g" ((USItype) (y2)),				\
	     "2" ((USItype) (x1)),				\
	     "g" ((USItype) (y1)),				\
	     "3" ((USItype) (x0)),				\
	     "g" ((USItype) (y0)))
#define __FP_FRAC_SUB_3(r2,r1,r0,x2,x1,x0,y2,y1,y0)		\
  __asm__ ("sub{l} {%8,%2|%2,%8}\n\t"				\
	   "sbb{l} {%6,%1|%1,%6}\n\t"				\
	   "sbb{l} {%4,%0|%0,%4}"				\
	   : "=r" ((USItype) (r2)),				\
	     "=&r" ((USItype) (r1)),				\
	     "=&r" ((USItype) (r0))				\
	   : "0" ((USItype) (x2)),				\
	     "g" ((USItype) (y2)),				\
	     "1" ((USItype) (x1)),				\
	     "g" ((USItype) (y1)),				\
	     "2" ((USItype) (x0)),				\
	     "g" ((USItype) (y0)))


#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		_FP_QNANBIT_S
#define _FP_NANFRAC_D		_FP_QNANBIT_D, 0
/* Even if XFmode is 12byte,  we have to pad it to
   16byte since soft-fp emulation is done in 16byte.  */
#define _FP_NANFRAC_E		_FP_QNANBIT_E, 0, 0, 0
#define _FP_NANFRAC_Q		_FP_QNANBIT_Q, 0, 0, 0

#ifndef _SOFT_FLOAT
#define FP_EX_SHIFT 0

#define _FP_DECL_EX \
  unsigned short _fcw __attribute__ ((unused)) = FP_RND_NEAREST;

#define FP_RND_NEAREST		0
#define FP_RND_ZERO		0xc00
#define FP_RND_PINF		0x800
#define FP_RND_MINF		0x400

#define FP_RND_MASK		0xc00

#define FP_INIT_ROUNDMODE				\
  do {							\
    __asm__ __volatile__ ("fnstcw\t%0" : "=m" (_fcw));	\
  } while (0)
#endif
