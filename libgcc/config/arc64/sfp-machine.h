#ifdef __ARC64_ARCH32__

#define _FP_W_TYPE_SIZE         32
#define _FP_W_TYPE              unsigned long
#define _FP_WS_TYPE             signed long
#define _FP_I_TYPE              long

#define _FP_MUL_MEAT_S(R,X,Y)                           \
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)                           \
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)                           \
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)   _FP_DIV_MEAT_1_udiv_norm(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)   _FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S           _FP_QNANBIT_S
#define _FP_NANFRAC_D           _FP_QNANBIT_D, 0
#define _FP_NANFRAC_Q           _FP_QNANBIT_Q, 0, 0, 0

#else

#define _FP_W_TYPE_SIZE         64
#define _FP_W_TYPE              unsigned long long
#define _FP_WS_TYPE             signed long long
#define _FP_I_TYPE              long long

#define _FP_MUL_MEAT_S(R,X,Y)                                   \
  _FP_MUL_MEAT_1_imm(_FP_WFRACBITS_S,R,X,Y)
#define _FP_MUL_MEAT_D(R,X,Y)                                   \
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)                                   \
  _FP_MUL_MEAT_2_wide_3mul(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)   _FP_DIV_MEAT_1_imm(S,R,X,Y,_FP_DIV_HELP_imm)
#define _FP_DIV_MEAT_D(R,X,Y)   _FP_DIV_MEAT_1_udiv_norm(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_H		_FP_QNANBIT_H
#define _FP_NANFRAC_S           _FP_QNANBIT_S
#define _FP_NANFRAC_D           _FP_QNANBIT_D
#define _FP_NANFRAC_Q           _FP_QNANBIT_Q, 0
#endif /* !__ARC64_ARC32__ */

#ifdef __ARC64_ARCH64__
typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))
#endif

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_NANSIGN_H           0
#define _FP_NANSIGN_S           0
#define _FP_NANSIGN_D           0
#define _FP_NANSIGN_Q           0

#define _FP_KEEPNANFRACP 0
#define _FP_QNANNEGATEDP 0

#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)      \
  do {                                          \
    R##_s = _FP_NANSIGN_##fs;                   \
    _FP_FRAC_SET_##wc(R,_FP_NANFRAC_##fs);      \
    R##_c = FP_CLS_NAN;                         \
  } while (0)

/* Not checked.  */
#define _FP_TININESS_AFTER_ROUNDING 0

#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN    4321

# define __BYTE_ORDER __LITTLE_ENDIAN

/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
