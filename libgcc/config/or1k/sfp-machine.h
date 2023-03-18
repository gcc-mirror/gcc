#define _FP_W_TYPE_SIZE		32
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

#define _FP_MUL_MEAT_S(R,X,Y)				\
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)	_FP_DIV_MEAT_1_loop(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)	_FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1), -1
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1, -1, -1
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP 1
#define _FP_QNANNEGATEDP 0

/* Someone please check this.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if ((_FP_FRAC_HIGH_RAW_##fs(X) & _FP_QNANBIT_##fs)		\
	&& !(_FP_FRAC_HIGH_RAW_##fs(Y) & _FP_QNANBIT_##fs))	\
      {								\
	R##_s = Y##_s;						\
	_FP_FRAC_COPY_##wc(R,Y);				\
      }								\
    else							\
      {								\
	R##_s = X##_s;						\
	_FP_FRAC_COPY_##wc(R,X);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

/* Handle getting and setting rounding mode for soft fp operations.  */

#define FP_RND_NEAREST		(0x0 << 1)
#define FP_RND_ZERO		(0x1 << 1)
#define FP_RND_PINF		(0x2 << 1)
#define FP_RND_MINF		(0x3 << 1)
#define FP_RND_MASK		(0x3 << 1)

#define FP_EX_OVERFLOW		1 << 3
#define FP_EX_UNDERFLOW		1 << 4
#define FP_EX_INEXACT		1 << 8
#define FP_EX_INVALID		1 << 9
#define FP_EX_DIVZERO		1 << 11
#define FP_EX_ALL \
	(FP_EX_INVALID | FP_EX_DIVZERO | FP_EX_OVERFLOW | FP_EX_UNDERFLOW \
	 | FP_EX_INEXACT)

#define _FP_DECL_EX \
  unsigned int _fpcsr __attribute__ ((unused)) = FP_RND_NEAREST

#define FP_ROUNDMODE (_fpcsr & FP_RND_MASK)

#ifdef __or1k_hard_float__
#define FP_INIT_ROUNDMODE					\
do {								\
  __asm__ volatile ("l.mfspr %0,r0,20" : "=r" (_fpcsr));	\
} while (0)

#define FP_HANDLE_EXCEPTIONS					\
do {								\
  if (__builtin_expect (_fex, 0))				\
    {								\
      _fpcsr |= _fex;						\
      __asm__ volatile ("l.mtspr r0,%0,20" : : "r" (_fpcsr));	\
    }								\
} while (0)
#endif

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#define __BYTE_ORDER __BIG_ENDIAN

#define _FP_TININESS_AFTER_ROUNDING 0

/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
