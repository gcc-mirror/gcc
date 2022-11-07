/* Optimization at -O2 and above currently result in ICEs when converting
   between vector types.  */
#pragma GCC optimize ("O1")

#include <errno.h>
#include <sys/types.h>
#include <machine/ieeefp.h>

typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));
typedef float v16sf __attribute__ ((vector_size (64)));
typedef float v32sf __attribute__ ((vector_size (128)));
typedef float v64sf __attribute__ ((vector_size (256)));

typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));
typedef double v8df __attribute__ ((vector_size (64)));
typedef double v16df __attribute__ ((vector_size (128)));
typedef double v32df __attribute__ ((vector_size (256)));
typedef double v64df __attribute__ ((vector_size (512)));

typedef int v2si __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef int v16si __attribute__ ((vector_size (64)));
typedef int v32si __attribute__ ((vector_size (128)));
typedef int v64si __attribute__ ((vector_size (256)));

typedef unsigned int v64usi __attribute__ ((vector_size (256)));

typedef long v2di __attribute__ ((vector_size (16)));
typedef long v4di __attribute__ ((vector_size (32)));
typedef long v8di __attribute__ ((vector_size (64)));
typedef long v16di __attribute__ ((vector_size (128)));
typedef long v32di __attribute__ ((vector_size (256)));
typedef long v64di __attribute__ ((vector_size (512)));

typedef union {
  v2sf t_v2sf;
  v4sf t_v4sf;
  v8sf t_v8sf;
  v16sf t_v16sf;
  v32sf t_v32sf;
  v64sf t_v64sf;

  v2df t_v2df;
  v4df t_v4df;
  v8df t_v8df;
  v16df t_v16df;
  v32df t_v32df;
  v64df t_v64df;

  v2si t_v2si;
  v4si t_v4si;
  v8si t_v8si;
  v16si t_v16si;
  v32si t_v32si;
  v64si t_v64si;

  v64usi t_v64usi;

  v2di t_v2di;
  v4di t_v4di;
  v8di t_v8di;
  v16di t_v16di;
  v32di t_v32di;
  v64di t_v64di;
} vector_union;

/* Cast between vectors with a different number of elements.  */

#define RESIZE_VECTOR(to_t, from) \
({ \
  __auto_type __from = (from); \
  *((to_t *) &__from); \
})

/* Bit-wise cast vector FROM to type TO_T.  */

#define CAST_VECTOR(to_t, from) \
({ \
  _Static_assert (sizeof (to_t) == sizeof (from)); \
  union { \
    typeof (from) __from; \
    to_t __to; \
  } __tmp; \
  __tmp.__from = (from); \
  __tmp.__to; \
})

#define EXTRACT_WORDS(hi, lo, x) \
do { \
  vector_union __tmp; \
  __tmp.t_v64df = (x); \
  hi = __builtin_convertvector (__tmp.t_v64di >> 32, typeof (hi)); \
  lo = __builtin_convertvector (__tmp.t_v64di & 0xffffffff, typeof (lo)); \
} while (0)

#define INSERT_WORDS(x, hi, lo, cond) \
do { \
  vector_union __tmp; \
  __tmp.t_v64di = __builtin_convertvector (hi, v64di) << 32 | \
    __builtin_convertvector (lo, v64di) & 0xffffffff; \
  VECTOR_COND_MOVE (x, __tmp.t_v64df, cond); \
} while (0)

#define GET_HIGH_WORD(x, y, cond) \
do { \
  vector_union __tmp; \
  __tmp.t_v64df = (y); \
  VECTOR_COND_MOVE (x, __builtin_convertvector (__tmp.t_v64di >> 32, v64si), \
		    (cond)); \
} while (0)

#define GET_LOW_WORD(x, y, cond) \
do { \
  vector_union __tmp; \
  __tmp.t_v64df = (y); \
  VECTOR_COND_MOVE (x, __builtin_convertvector (__tmp.t_v64di & 0xffffffff, \
						v64si), (cond)); \
} while (0)

#define SET_HIGH_WORD(x, y, cond) \
do { \
  vector_union __tmp; \
  __tmp.t_v64df = x; \
  __tmp.t_v64di &= 0xffffffff; \
  __tmp.t_v64di |= __builtin_convertvector (y, v64di) << 32; \
  VECTOR_COND_MOVE (x, __tmp.t_v64df, (cond)); \
} while (0)

#define SET_LOW_WORD(x, y, cond) \
do { \
  vector_union __tmp; \
  __tmp.t_v64df = x; \
  __tmp.t_v64di &= 0xffffffff00000000ULL; \
  __tmp.t_v64di |= __builtin_convertvector (y, v64di); \
  VECTOR_COND_MOVE (x, __tmp.t_v64df, (cond)); \
 } while (0)

#define GET_FLOAT_WORD(x, y, cond) \
  VECTOR_COND_MOVE(x, CAST_VECTOR(v64si, (y)), (cond))

#define SET_FLOAT_WORD(x, y, cond) \
  VECTOR_COND_MOVE(x, CAST_VECTOR(v64sf, (y)), (cond))

#define NO_COND __mask

/* Note - __mask is _not_ accounted for in VECTOR_MERGE!  */
#define VECTOR_MERGE(vec1, vec2, cond) \
({ \
  _Static_assert (__builtin_types_compatible_p (typeof (vec1), typeof (vec2))); \
  union { \
    typeof (vec1) val; \
    v64si t_v64si; \
    v64di t_v64di; \
  } __vec1, __vec2, __res; \
  __vec1.val = (vec1); \
  __vec2.val = (vec2); \
  __builtin_choose_expr ( \
        sizeof (vec1) == sizeof (v64si), \
        ({ \
          v64si __bitmask = __builtin_convertvector ((cond), v64si); \
          __res.t_v64si = (__vec1.t_v64si & __bitmask) \
                          | (__vec2.t_v64si & ~__bitmask); \
        }), \
        ({ \
          v64di __bitmask = __builtin_convertvector ((cond), v64di); \
          __res.t_v64di = (__vec1.t_v64di & __bitmask) \
                          | (__vec2.t_v64di & ~__bitmask); \
        })); \
  __res.val; \
})

#define VECTOR_RETURN(retval, cond) \
do { \
  _Static_assert (__builtin_types_compatible_p (typeof (retval), typeof (__ret))); \
  __auto_type __cond = __builtin_convertvector ((cond), typeof (__mask)); \
  __auto_type __retval = (retval); \
  VECTOR_COND_MOVE (__ret, __retval, __cond); \
  __mask &= ~__cond; \
} while (0)

#define VECTOR_COND_MOVE(var, val, cond) \
do { \
  _Static_assert (__builtin_types_compatible_p (typeof (var), typeof (val))); \
  __auto_type __cond = __builtin_convertvector ((cond), typeof (__mask)); \
  var = VECTOR_MERGE ((val), var, __cond & __mask); \
} while (0)

#define VECTOR_IF(cond, cond_var) \
{ \
  __auto_type cond_var = (cond); \
  __auto_type __inv_cond = ~cond_var; \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSEIF(cond, cond_var) \
  } \
  cond_var = __inv_cond & (cond); \
  __inv_cond &= ~(cond); \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSE(cond_var) \
  } \
  cond_var = __inv_cond; \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_IF2(cond, cond_var, prev_cond_var) \
{ \
  __auto_type cond_var = (cond) & __builtin_convertvector (prev_cond_var, typeof (cond)); \
  __auto_type __inv_cond = ~(cond); \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSEIF2(cond, cond_var, prev_cond_var) \
  } \
  cond_var = (cond) & __inv_cond & __builtin_convertvector (prev_cond_var, typeof (cond)); \
  __inv_cond &= ~(cond); \
  if (!ALL_ZEROES_P (cond_var)) \
  {

#define VECTOR_ELSE2(cond_var, prev_cond_var) \
  } \
  cond_var = __inv_cond & __builtin_convertvector (prev_cond_var, typeof (__inv_cond)); \
  if (!ALL_ZEROES_P (cond_var)) \
  {


#define VECTOR_ENDIF \
  } \
}

#define VECTOR_INIT_AUX(x, type) \
({ \
  typeof (x) __e = (x); \
  type __tmp = { \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e, \
    __e, __e, __e, __e, __e, __e, __e, __e }; \
  __tmp; \
})

#define VECTOR_INIT(x) \
  (_Generic ((x), int: VECTOR_INIT_AUX ((x), v64si), \
                  unsigned: VECTOR_INIT_AUX ((x), v64usi), \
                  long: VECTOR_INIT_AUX ((x), v64di), \
                  float: VECTOR_INIT_AUX ((x), v64sf), \
                  double: VECTOR_INIT_AUX ((x), v64df)))

#define VECTOR_WIDTH(TYPE) (sizeof (TYPE) / (V_SF_SI_P (TYPE) ? 4 : 8))

#define V_SF_SI_P(TYPE) \
  (__builtin_types_compatible_p (TYPE, v2sf) \
   || __builtin_types_compatible_p (TYPE, v4sf) \
   || __builtin_types_compatible_p (TYPE, v8sf) \
   || __builtin_types_compatible_p (TYPE, v16sf) \
   || __builtin_types_compatible_p (TYPE, v32sf) \
   || __builtin_types_compatible_p (TYPE, v64sf) \
   || __builtin_types_compatible_p (TYPE, v2si) \
   || __builtin_types_compatible_p (TYPE, v4si) \
   || __builtin_types_compatible_p (TYPE, v8si) \
   || __builtin_types_compatible_p (TYPE, v16si) \
   || __builtin_types_compatible_p (TYPE, v32si) \
   || __builtin_types_compatible_p (TYPE, v64si))

#define VECTOR_INIT_MASK(TYPE) \
({ \
  vector_union __mask; \
  __mask.t_v64di = VECTOR_INIT (0L); \
  for (int i = 0; i < VECTOR_WIDTH (TYPE); i++) \
    __mask.t_v64di[i] = -1; \
  __builtin_choose_expr (V_SF_SI_P (TYPE), __mask.t_v64si, __mask.t_v64di); \
})

#define ALL_ZEROES_P(x) (COND_TO_BITMASK(x) == 0)

#define COND_TO_BITMASK(x) \
({ \
  long __tmp = 0; \
  __auto_type __x = __builtin_convertvector((x), typeof (__mask)) & __mask; \
  __builtin_choose_expr (sizeof (__mask) == 256, \
                         ({ asm ("v_cmp_ne_u32_e64 %0, %1, 0" \
                                 : "=Sg" (__tmp) \
                                 : "v" (__x)); }), \
                         ({ asm ("v_cmp_ne_u64_e64 %0, %1, 0" \
                                 : "=Sg" (__tmp) \
                                 : "v" (__x)); })); \
  __tmp; \
})

#define VECTOR_WHILE(cond, cond_var, prev_cond_var) \
{ \
  __auto_type cond_var = prev_cond_var; \
  for (;;) { \
    cond_var &= (cond); \
    if (ALL_ZEROES_P (cond_var)) \
      break;

#define VECTOR_ENDWHILE \
  } \
}

#define DEF_VS_MATH_FUNC(rettype, name, args...) \
    rettype v64sf##_##name##_aux (args, v64si __mask)

#define DEF_VD_MATH_FUNC(rettype, name, args...) \
    rettype v64df##_##name##_aux (args, v64di __mask)

/* Use this for predicate functions that take a vector of doubles but
   return a vector of ints.  */
#define DEF_VD_MATH_PRED(rettype, name, args...) \
    rettype v64df##_##name##_aux (args, v64si __mask)

#define FUNCTION_INIT(rettype) \
  rettype __ret

#define FUNCTION_RETURN \
  return __ret

#define DEF_VARIANT(FUN, TRET, TARG, COUNT) \
v##COUNT##TRET \
v##COUNT##TARG##_##FUN (v##COUNT##TARG __arg) \
{ \
  __auto_type __upsized_arg = RESIZE_VECTOR (v64##TARG, __arg); \
  __auto_type __mask = VECTOR_INIT_MASK (v##COUNT##TRET); \
  __auto_type __result = v64##TARG##_##FUN##_aux (__upsized_arg, __mask); \
  return RESIZE_VECTOR (v##COUNT##TRET, __result); \
}

#define DEF_VARIANT2(FUN, TRET, TARG, COUNT) \
v##COUNT##TRET \
v##COUNT##TARG##_##FUN (v##COUNT##TARG __arg1, v##COUNT##TARG __arg2) \
{ \
  __auto_type __upsized_arg1 = RESIZE_VECTOR (v64##TARG, __arg1); \
  __auto_type __upsized_arg2 = RESIZE_VECTOR (v64##TARG, __arg2); \
  __auto_type __mask = VECTOR_INIT_MASK (v##COUNT##TRET); \
  __auto_type __result = v64##TARG##_##FUN##_aux (__upsized_arg1, __upsized_arg2, __mask); \
  return RESIZE_VECTOR (v##COUNT##TRET, __result); \
}

#define DEF_VARIANTS(FUN, RETTYPE, ARGTYPE) \
  DEF_VARIANT (FUN, RETTYPE, ARGTYPE, 2) \
  DEF_VARIANT (FUN, RETTYPE, ARGTYPE, 4) \
  DEF_VARIANT (FUN, RETTYPE, ARGTYPE, 8) \
  DEF_VARIANT (FUN, RETTYPE, ARGTYPE, 16) \
  DEF_VARIANT (FUN, RETTYPE, ARGTYPE, 32) \
  DEF_VARIANT (FUN, RETTYPE, ARGTYPE, 64)

#define DEF_VARIANTS2(FUN, RETTYPE, ARGTYPE) \
  DEF_VARIANT2 (FUN, RETTYPE, ARGTYPE, 2) \
  DEF_VARIANT2 (FUN, RETTYPE, ARGTYPE, 4) \
  DEF_VARIANT2 (FUN, RETTYPE, ARGTYPE, 8) \
  DEF_VARIANT2 (FUN, RETTYPE, ARGTYPE, 16) \
  DEF_VARIANT2 (FUN, RETTYPE, ARGTYPE, 32) \
  DEF_VARIANT2 (FUN, RETTYPE, ARGTYPE, 64)

/* From fdlibm.h */

#ifdef _FLT_LARGEST_EXPONENT_IS_NORMAL
#define FLT_UWORD_IS_FINITE(x) ((x) == (x))
#define FLT_UWORD_IS_NAN(x) ((x) != (x))
#define FLT_UWORD_IS_INFINITE(x) ((x) != (x))
#define FLT_UWORD_MAX 0x7fffffff
#define FLT_UWORD_EXP_MAX 0x43010000
#define FLT_UWORD_LOG_MAX 0x42b2d4fc
#define FLT_UWORD_LOG_2MAX 0x42b437e0
#define HUGE ((float)0X1.FFFFFEP128)
#else
#define FLT_UWORD_IS_FINITE(x) ((x)<0x7f800000)
#define FLT_UWORD_IS_NAN(x) ((x)>0x7f800000)
#define FLT_UWORD_IS_INFINITE(x) ((x)==0x7f800000)
#define FLT_UWORD_MAX 0x7f7fffffL
#define FLT_UWORD_EXP_MAX 0x43000000
#define FLT_UWORD_LOG_MAX 0x42b17217
#define FLT_UWORD_LOG_2MAX 0x42b2d4fc
#define HUGE ((float)3.40282346638528860e+38)
#endif
#define FLT_UWORD_HALF_MAX (FLT_UWORD_MAX-(1L<<23))
#define FLT_LARGEST_EXP (FLT_UWORD_MAX>>23)

#ifdef _FLT_NO_DENORMALS
#define FLT_UWORD_IS_ZERO(x) ((x)<0x00800000)
#define FLT_UWORD_IS_SUBNORMAL(x) ((x) != (x))
#define FLT_UWORD_MIN 0x00800000
#define FLT_UWORD_EXP_MIN 0x42fc0000
#define FLT_UWORD_LOG_MIN 0x42aeac50
#define FLT_SMALLEST_EXP 1
#else
#define FLT_UWORD_IS_ZERO(x) ((x)==0)
#define FLT_UWORD_IS_SUBNORMAL(x) ((x)<0x00800000)
#define FLT_UWORD_MIN 0x00000001
#define FLT_UWORD_EXP_MIN 0x43160000
#define FLT_UWORD_LOG_MIN 0x42cff1b5
#define FLT_SMALLEST_EXP -22
#endif

/* From zmath.h */

#define NUM 3
#define NAN 2
#define INF 1

#define __PI 3.14159265358979323846
#define __SQRT_HALF 0.70710678118654752440
#define __PI_OVER_TWO 1.57079632679489661923132
#define __INV_PI_OVER_TWO_2_24 10680707.430881743590348355907974

typedef const union
{
  unsigned int l[2];
  double d;
} udouble;

typedef const union
{
  unsigned int l;
  float f;
} ufloat;

extern double BIGX;
extern double SMALLX;

extern udouble z_infinity;
extern udouble z_notanum;
extern double  z_rooteps;

extern ufloat  z_infinity_f;
extern ufloat  z_notanum_f;
extern float   z_rooteps_f;

/* From math_errf.c */

static v64sf v64sf_math_oflowf (v64si sign)
{
  errno = ERANGE;
  return VECTOR_MERGE (VECTOR_INIT (-0x1p97f),
                       VECTOR_INIT (0x1p97f), sign) * 0x1p97f;
}

static v64sf v64sf_math_uflowf (v64si sign)
{
  errno = ERANGE;
  return VECTOR_MERGE (VECTOR_INIT (-0x1p-95f),
                       VECTOR_INIT (0x1p-95f), sign) * 0x1p-95f;
}

/* From math_config.h */

static v64si v64sf_issignalingf_inline (v64sf x)
{
  v64si __mask = VECTOR_INIT (-1);
  v64si ix;
  GET_FLOAT_WORD (ix, x, NO_COND);
  /* Use IEEE-754 2008 encoding - i.e. exponent bits all 1, MSB of
     significand is 0 for signalling NaN.  */
  return ((ix & 0x7f800000) == 0x7f800000) & ((ix & 0x00400000) == 0);
}

/* Vector extensions to sys/reent.h */

struct v64_reent {
  v64si _v64si_gamma_signgam;
};

extern struct v64_reent *_v64_reent;
#define _V64_REENT _v64_reent

#define _REENT_V64SI_SIGNGAM(ptr)      ((ptr)->_v64si_gamma_signgam)

/* Vector extensions to math.h */

#define v64si_signgam (*__v64si_signgam())
extern v64si* __v64si_signgam (void);
#define __v64si_signgam_r(ptr) _REENT_V64SI_SIGNGAM(ptr)

