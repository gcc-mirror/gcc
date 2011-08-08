#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));

#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))

/* The type of the result of a floating point comparison.  This must
   match `__libgcc_cmp_return__' in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_MUL_MEAT_Q(R,X,Y)                           \
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		_FP_QNANBIT_S
#define _FP_NANFRAC_D		_FP_QNANBIT_D
#define _FP_NANFRAC_E		_FP_QNANBIT_E, 0
#define _FP_NANFRAC_Q		_FP_QNANBIT_Q, 0
#define _FP_NANSIGN_S		1
#define _FP_NANSIGN_D		1
#define _FP_NANSIGN_E		1
#define _FP_NANSIGN_Q		1

#define _FP_KEEPNANFRACP 1

/* Here is something Intel misdesigned: the specs don't define
   the case where we have two NaNs with same mantissas, but
   different sign. Different operations pick up different NaNs.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if (_FP_FRAC_GT_##wc(X, Y)					\
	|| (_FP_FRAC_EQ_##wc(X,Y) && (OP == '+' || OP == '*')))	\
      {								\
	R##_s = X##_s;						\
        _FP_FRAC_COPY_##wc(R,X);				\
      }								\
    else							\
      {								\
	R##_s = Y##_s;						\
        _FP_FRAC_COPY_##wc(R,Y);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

#define FP_EX_INVALID		0x01
#define FP_EX_DENORM		0x02
#define FP_EX_DIVZERO		0x04
#define FP_EX_OVERFLOW		0x08
#define FP_EX_UNDERFLOW		0x10
#define FP_EX_INEXACT		0x20

#define FP_HANDLE_EXCEPTIONS						\
  do {									\
    double tmp, dummy;							\
    if (_fex & FP_EX_INVALID)						\
      {									\
	tmp = 0.0;							\
	__asm__ __volatile__ ("frcpa.s0 %0,p1=f0,f0"			\
			      : "=f" (tmp) : : "p1" );			\
      }									\
    if (_fex & FP_EX_DIVZERO)						\
      {									\
	__asm__ __volatile__ ("frcpa.s0 %0,p1=f1,f0"			\
			      : "=f" (tmp) : : "p1" );			\
      }									\
    if (_fex & FP_EX_OVERFLOW)						\
      {									\
	dummy = __DBL_MAX__;						\
	__asm__ __volatile__ ("fadd.d.s0 %0=%1,%1"			\
			      : "=f" (dummy) : "0" (dummy));		\
      }									\
    if (_fex & FP_EX_UNDERFLOW)						\
      {									\
	dummy = __DBL_MIN__;						\
	__asm__ __volatile__ ("fnma.d.s0 %0=%1,%1,f0"			\
			      : "=f" (tmp) : "f" (dummy));		\
      }									\
    if (_fex & FP_EX_INEXACT)						\
      {									\
	dummy = __DBL_MAX__;						\
	__asm__ __volatile__ ("fsub.d.s0 %0=%1,f1"			\
			      : "=f" (dummy) : "0" (dummy));		\
      }									\
  } while (0)

#define FP_RND_NEAREST		0
#define FP_RND_ZERO		0xc00L
#define FP_RND_PINF		0x800L
#define FP_RND_MINF		0x400L

#define _FP_DECL_EX \
  unsigned long int _fpsr __attribute__ ((unused)) = FP_RND_NEAREST

#define FP_INIT_ROUNDMODE			\
  do {						\
    __asm__ __volatile__ ("mov.m %0=ar.fpsr"	\
			  : "=r" (_fpsr));	\
  } while (0)

#define FP_ROUNDMODE		(_fpsr & 0xc00L)

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#define __BYTE_ORDER __LITTLE_ENDIAN

/* Define ALIASNAME as a strong alias for NAME.  */
#define strong_alias(name, aliasname) _strong_alias(name, aliasname)
#define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
