/* libquadmath uses soft-fp only for sqrtq and only for
   the positive finite case, so it doesn't care about
   NaN representation, nor tininess after rounding vs.
   before rounding, all it cares about is current rounding
   mode and raising inexact exceptions.  */
#if __SIZEOF_LONG__ == 8
#define _FP_W_TYPE_SIZE 64
#define _FP_I_TYPE long long
#define _FP_NANFRAC_Q _FP_QNANBIT_Q, 0
#else
#define _FP_W_TYPE_SIZE 32
#define _FP_I_TYPE int
#define _FP_NANFRAC_Q _FP_QNANBIT_Q, 0, 0, 0
#endif
#define _FP_W_TYPE unsigned _FP_I_TYPE
#define _FP_WS_TYPE signed _FP_I_TYPE
#define _FP_QNANNEGATEDP 0
#define _FP_NANSIGN_Q 1
#define _FP_KEEPNANFRACP 1
#define _FP_TININESS_AFTER_ROUNDING 0
#ifndef __BYTE_ORDER
#define __LITTLE_ENDIAN __ORDER_LITTLE_ENDIAN__
#define __BIG_ENDIAN __ORDER_BIG_ENDIAN__
#define __BYTE_ORDER __BYTE_ORDER__
#endif
#define _FP_DECL_EX \
  unsigned int fp_roundmode __attribute__ ((unused)) = FP_RND_NEAREST;
#define FP_ROUNDMODE fp_roundmode
#define FP_INIT_ROUNDMODE \
  do					\
    {					\
      switch (fegetround ())		\
        {				\
        case FE_UPWARD:			\
          fp_roundmode = FP_RND_PINF;	\
          break;			\
        case FE_DOWNWARD:		\
          fp_roundmode = FP_RND_MINF;	\
          break;			\
        case FE_TOWARDZERO:		\
          fp_roundmode = FP_RND_ZERO;	\
          break;			\
	default:			\
	  break;			\
        }				\
    }					\
  while (0)
#define FP_HANDLE_EXCEPTIONS \
  do					\
    {					\
      if (_fex & FP_EX_INEXACT)		\
	{				\
	  volatile double eight = 8.0;	\
	  volatile double eps		\
	    = DBL_EPSILON;		\
	  eight += eps;			\
	}				\
    }					\
  while (0)
