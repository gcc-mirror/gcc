#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long long
#define _FP_WS_TYPE		signed long long
#define _FP_I_TYPE		long long

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));

#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))

#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_H		_FP_QNANBIT_H
#define _FP_NANFRAC_S		_FP_QNANBIT_S
#define _FP_NANFRAC_D		_FP_QNANBIT_D
#define _FP_NANFRAC_E		_FP_QNANBIT_E, 0
#define _FP_NANFRAC_Q		_FP_QNANBIT_Q, 0

#ifndef _SOFT_FLOAT
#define FP_EX_SHIFT 7

#define _FP_DECL_EX \
  unsigned int _fcw __attribute__ ((unused)) = FP_RND_NEAREST;

#define FP_RND_NEAREST		0
#define FP_RND_ZERO		0x6000
#define FP_RND_PINF		0x4000
#define FP_RND_MINF		0x2000

#define FP_RND_MASK		0x6000

#define FP_INIT_ROUNDMODE					\
  do {								\
    __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (_fcw));	\
  } while (0)
#endif
