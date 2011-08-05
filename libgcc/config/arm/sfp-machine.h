#define _FP_W_TYPE_SIZE		32
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

/* The type of the result of a floating point comparison.  This must
   match `__libgcc_cmp_return__' in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_MUL_MEAT_S(R,X,Y)				\
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)	_FP_DIV_MEAT_1_loop(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)	_FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_4_udiv(Q,R,X,Y)

#define _FP_NANFRAC_H		((_FP_QNANBIT_H << 1) - 1)
#define _FP_NANFRAC_S		((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1), -1
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1, -1, -1
#define _FP_NANSIGN_H		0
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP 1

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

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#if defined __ARMEB__
# define __BYTE_ORDER __BIG_ENDIAN
#else
# define __BYTE_ORDER __LITTLE_ENDIAN
#endif


/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

#ifdef __ARM_EABI__
/* Rename functions to their EABI names.  */
/* The comparison functions need wrappers for EABI semantics, so
   leave them unmolested.  */
#define __negsf2	__aeabi_fneg
#define __subsf3	__aeabi_fsub
#define __addsf3	__aeabi_fadd
#define __floatunsisf	__aeabi_ui2f
#define __floatsisf	__aeabi_i2f
#define __floatundisf	__aeabi_ul2f
#define __floatdisf	__aeabi_l2f
#define __mulsf3	__aeabi_fmul
#define __divsf3	__aeabi_fdiv
#define __unordsf2	__aeabi_fcmpun
#define __fixsfsi	__aeabi_f2iz
#define __fixunssfsi	__aeabi_f2uiz
#define __fixsfdi	__aeabi_f2lz
#define __fixunssfdi	__aeabi_f2ulz
#define __floatdisf	__aeabi_l2f

#define __negdf2	__aeabi_dneg
#define __subdf3	__aeabi_dsub
#define __adddf3	__aeabi_dadd
#define __floatunsidf	__aeabi_ui2d
#define __floatsidf	__aeabi_i2d
#define __extendsfdf2	__aeabi_f2d
#define __truncdfsf2	__aeabi_d2f
#define __floatundidf	__aeabi_ul2d
#define __floatdidf	__aeabi_l2d
#define __muldf3	__aeabi_dmul
#define __divdf3	__aeabi_ddiv
#define __unorddf2	__aeabi_dcmpun
#define __fixdfsi	__aeabi_d2iz
#define __fixunsdfsi	__aeabi_d2uiz
#define __fixdfdi	__aeabi_d2lz
#define __fixunsdfdi	__aeabi_d2ulz
#define __floatdidf	__aeabi_l2d
#define __extendhfsf2	__gnu_h2f_ieee
#define __truncsfhf2	__gnu_f2h_ieee

#endif /* __ARM_EABI__ */
