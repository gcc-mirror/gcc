#define _FP_W_TYPE_SIZE		32
#define _FP_W_TYPE		unsigned int
#define _FP_WS_TYPE		signed int
#define _FP_I_TYPE		int

/* The type of the result of a floating point comparison.  This must
   match `__libgcc_cmp_return__' in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

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


#define _FP_MUL_MEAT_Q(R,X,Y)					\
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		_FP_QNANBIT_S
#define _FP_NANFRAC_D		_FP_QNANBIT_D, 0
/* Even if XFmode is 12byte,  we have to pad it to
   16byte since soft-fp emulation is done in 16byte.  */
#define _FP_NANFRAC_E		_FP_QNANBIT_E, 0, 0, 0
#define _FP_NANFRAC_Q		_FP_QNANBIT_Q, 0, 0, 0
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

#ifndef _SOFT_FLOAT
#define FP_EX_INVALID		0x01
#define FP_EX_DENORM		0x02
#define FP_EX_DIVZERO		0x04
#define FP_EX_OVERFLOW		0x08
#define FP_EX_UNDERFLOW		0x10
#define FP_EX_INEXACT		0x20

struct fenv
{
  unsigned short int __control_word;
  unsigned short int __unused1;
  unsigned short int __status_word;
  unsigned short int __unused2;
  unsigned short int __tags;
  unsigned short int __unused3;
  unsigned int __eip;
  unsigned short int __cs_selector;
  unsigned int __opcode:11;
  unsigned int __unused4:5;
  unsigned int __data_offset;
  unsigned short int __data_selector;
  unsigned short int __unused5;
};

#define FP_HANDLE_EXCEPTIONS						\
  do {									\
    if (_fex & FP_EX_INVALID)						\
      {									\
	float f = 0.0;							\
	__asm__ __volatile__ ("fdiv {%y0, %0|%0, %y0}" : "+t" (f));	\
	__asm__ __volatile__ ("fwait");					\
      }									\
    if (_fex & FP_EX_DENORM)						\
      {									\
	struct fenv temp;						\
	__asm__ __volatile__ ("fnstenv %0" : "=m" (temp));		\
	temp.__status_word |= FP_EX_DENORM;				\
	__asm__ __volatile__ ("fldenv %0" : : "m" (temp));		\
	__asm__ __volatile__ ("fwait");					\
      }									\
    if (_fex & FP_EX_DIVZERO)						\
      {									\
	float f = 1.0, g = 0.0;						\
	__asm__ __volatile__ ("fdivp {%0, %y1|%y1, %0}"			\
				      : "+t" (f) : "u" (g)		\
				      : "st(1)");			\
	__asm__ __volatile__ ("fwait");					\
      }									\
    if (_fex & FP_EX_OVERFLOW)						\
      {									\
	struct fenv temp;						\
	__asm__ __volatile__ ("fnstenv %0" : "=m" (temp));		\
	temp.__status_word |= FP_EX_OVERFLOW;				\
	__asm__ __volatile__ ("fldenv %0" : : "m" (temp));		\
	__asm__ __volatile__ ("fwait");					\
      }									\
    if (_fex & FP_EX_UNDERFLOW)						\
      {									\
	struct fenv temp;						\
	__asm__ __volatile__ ("fnstenv %0" : "=m" (temp));		\
	temp.__status_word |= FP_EX_UNDERFLOW;				\
	__asm__ __volatile__ ("fldenv %0" : : "m" (temp));		\
	__asm__ __volatile__ ("fwait");					\
      }									\
    if (_fex & FP_EX_INEXACT)						\
      {									\
	struct fenv temp;						\
	__asm__ __volatile__ ("fnstenv %0" : "=m" (temp));		\
	temp.__status_word |= FP_EX_INEXACT;				\
	__asm__ __volatile__ ("fldenv %0" : : "m" (temp));		\
	__asm__ __volatile__ ("fwait");					\
      }									\
  } while (0)

#define FP_RND_NEAREST		0
#define FP_RND_ZERO		0xc00
#define FP_RND_PINF		0x800
#define FP_RND_MINF		0x400

#define _FP_DECL_EX \
  unsigned short _fcw __attribute__ ((unused)) = FP_RND_NEAREST

#define FP_INIT_ROUNDMODE			\
  do {						\
    __asm__ ("fnstcw %0" : "=m" (_fcw));	\
  } while (0)

#define FP_ROUNDMODE		(_fcw & 0xc00)
#endif

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#define __BYTE_ORDER __LITTLE_ENDIAN

/* Define ALIASNAME as a strong alias for NAME.  */
#if defined __MACH__
/* Mach-O doesn't support aliasing.  If these functions ever return
   anything but CMPtype we need to revisit this... */
#define strong_alias(name, aliasname) \
  CMPtype aliasname (TFtype a, TFtype b) { return name(a, b); }
#else
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
#endif
