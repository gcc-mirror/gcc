#if defined HAVE_STDINT_H
#include <stdint.h>
#elif defined HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if defined HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if defined HAVE_SYS_CONFIG_H
#include <sys/config.h>

/* ISO C9X int type declarations */

#if !defined HAVE_INT32_DEFINED && defined HAVE_BSD_INT32_DEFINED
typedef u_int32_t uint32_t;
#endif

#if !defined HAVE_BSD_INT32_DEFINED && !defined HAVE_INT32_DEFINED
// FIXME -- this could have problems with systems that don't define SI to be 4
typedef int int32_t __attribute__((mode(SI)));
typedef unsigned int uint32_t __attribute__((mode(SI)));
#endif

  /* These typedefs are true for the targets running Java. */

#define Sign_Extend(a,b) if (b < 0) a |= (uint32_t)0xffff0000;
  uint32_t i[2];
#if defined (_DOUBLE_IS_32BITS)
#define Exp_msk1    ((uint32_t)0x00800000L)
#define Exp_msk11   ((uint32_t)0x00800000L)
#define Exp_mask    ((uint32_t)0x7f800000L)
#define Exp_1       ((uint32_t)0x3f800000L)
#define Exp_11      ((uint32_t)0x3f800000L)
#define Frac_mask   ((uint32_t)0x007fffffL)
#define Frac_mask1  ((uint32_t)0x007fffffL)
#define Sign_bit    ((uint32_t)0x80000000L)
#define Bndry_mask  ((uint32_t)0x007fffffL)
#define Bndry_mask1 ((uint32_t)0x007fffffL)
#define Sign_bit    ((uint32_t)0x80000000L)
#define Infinite(x) (word0(x) == ((uint32_t)0x7f800000L))
#define Exp_msk1    ((uint32_t)0x100000L)
#define Exp_msk11   ((uint32_t)0x100000L)
#define Exp_mask  ((uint32_t)0x7ff00000L)
#define Exp_1  ((uint32_t)0x3ff00000L)
#define Exp_11 ((uint32_t)0x3ff00000L)
#define Frac_mask  ((uint32_t)0xfffffL)
#define Frac_mask1 ((uint32_t)0xfffffL)
#define Bndry_mask  ((uint32_t)0xfffffL)
#define Bndry_mask1 ((uint32_t)0xfffffL)
#define Sign_bit ((uint32_t)0x80000000L)
#define Infinite(x) (word0(x) == ((uint32_t)0x7ff00000L)) /* sufficient test for here */
#define Exp_msk1   ((uint32_t)0x1000000L)
#define Exp_msk11  ((uint32_t)0x1000000L)
#define Exp_mask  ((uint32_t)0x7f000000L)
#define Exp_1  ((uint32_t)0x41000000L)
#define Exp_11 ((uint32_t)0x41000000L)
#define Frac_mask  ((uint32_t)0xffffffL)
#define Frac_mask1 ((uint32_t)0xffffffL)
#define Bndry_mask  ((uint32_t)0xefffffL)
#define Bndry_mask1 ((uint32_t)0xffffffL)
#define Sign_bit ((uint32_t)0x80000000L)
#define Tiny0 ((uint32_t)0x100000L)

/* This is a blatant hack: on Solaris 2.5, pthread.h defines uint32_t
   in pthread.h, which we sometimes include.  We protect our
   definition the same way Solaris 2.5 does, to avoid redefining it.  */
#  ifndef _UINT32_T
#define Exp_msk11   ((uint32_t)0x800000L)
#  endif
#define Exp_mask  ((uint32_t)0x7f80L)
#define Exp_1  ((uint32_t)0x40800000L)
#define Exp_11 ((uint32_t)0x4080L)
#define Frac_mask  ((uint32_t)0x7fffffL)
#define Frac_mask1 ((uint32_t)0xffff007fL)
#define Bndry_mask  ((uint32_t)0xffff007fL)
#define Bndry_mask1 ((uint32_t)0xffff007fL)
#define LSB ((uint32_t)0x10000L)
#define Sign_bit ((uint32_t)0x8000L)
#define Big1 ((uint32_t)0xffffffffL)
struct _Jv_Bigint
extern char* _EXFUN(_dtoa_r, (struct _Jv_reent *ptr, double d,
			      int mode, int ndigits, int *decpt, int *sign,
void _EXFUN(_dtoa, (double d, int mode, int ndigits, int *decpt, int *sign,
