# f7_c_*.o modules from libf7.c.
F7_C_PARTS += set_s16 set_u16 set_s32 set_u32 init
F7_C_PARTS += get_s16 get_u16 get_s32 get_u32 get_s64 get_u64
F7_C_PARTS += lrint ldexp frexp madd_msub madd msub hypot
F7_C_PARTS += addsub add sub mulx mul square divx div div1 fmod sqrt cbrt
F7_C_PARTS += Ineg Iadd Isub Imul Idiv IRsub Isquare Ildexp Isqrt
F7_C_PARTS += set_float get_float get_double set_double set_pdouble
F7_C_PARTS += fabs neg fmin fmax minmax truncx trunc floor ceil round lround
F7_C_PARTS += horner logx log log10 log2 exp pow10 pow powi
F7_C_PARTS += sin cos tan cotan sincos sinh cosh tanh sinhcosh
F7_C_PARTS += asinacos asin acos atan atan2 fdim
F7_C_PARTS += abscmp_msb_ge cmp cmp_abs cmp_unordered

F7_C_PARTS += const_1 const_1_2 const_1_3
F7_C_PARTS += const_pi const_ln2 const_1_ln2 const_ln10 const_1_ln10 const_sqrt2
F7_C_PARTS += # const_m1 const_2 const_sqrt2

# f7_asm_*.o modules from libf7-asm.sx.
F7_ASM_PARTS += classify clr mul_mant cmp_mant set_u64
F7_ASM_PARTS += copy copy_P copy_mant clr_mant_lsbs
F7_ASM_PARTS += addsub_mant_scaled store load
F7_ASM_PARTS += to_integer to_unsigned clz normalize_with_carry normalize
F7_ASM_PARTS += store_expo sqrt16 sqrt_approx div

F7_ASM_PARTS += D_class D_fma D_powi
F7_ASM_PARTS += D_isnan D_isinf D_isfinite D_signbit D_copysign D_neg D_fabs

F7_ASM_PARTS += call_dd call_ddd

# Stuff that will be wrapped in f7-wraps.h (included by libf7-asm.sx)
# and give f7_asm_D_*.o modules.
g_ddd += add sub mul div
g_xdd_cmp += le lt ge gt ne eq unord
g_dx += floatunsidf floatsidf extendsfdf2
g_xd += fixdfsi fixdfdi fixunsdfdi fixunsdfsi truncdfsf2

m_ddd += pow fmin fmax fmod hypot atan2 fdim
m_ddx += ldexp frexp
m_dd += sqrt cbrt exp exp10 pow10 log log10 log2 sin cos tan cotan asin acos atan
m_dd += ceil floor trunc round sinh cosh tanh
m_xd += lrint lround

# -mcall-prologues
CALL_PROLOGUES += divx sqrt cbrt get_double set_double logx exp exp10 pow10
CALL_PROLOGUES += put_C truncx round minmax sincos tan cotan pow powi fmod
CALL_PROLOGUES += atan atan2 asinacos madd_msub hypot init horner sinhcosh tanh

# -mstrict-X
STRICT_X += log addsub truncx ldexp exp

# Renames used when building f7-renames.h.
F7F += fabs neg add sub addsub div div1 divx fmod sqrt cbrt
F7F += square mul mulx madd_msub madd msub hypot
F7F += Ineg Iadd Isub Imul Idiv IRsub Isquare Ildexp Isqrt
F7F += le lt gt ge ne eq cmp cmp_abs ordered unordered cmp_unordered
F7F += lt_impl gt_impl le_impl ge_impl eq_impl ne_impl unord_impl

F7F += lrint ldexp frexp exp logx log log10 log2
F7F += minmax fmax fmin floor ceil round lround trunc truncx
F7F += horner pow10 exp10 pow powi
F7F += sin cos tan cotan sincos sinh cosh tanh sinhcosh
F7F += asinacos asin acos atan atan2 fdim
F7F += mul_noround sqrt16_round sqrt16_floor
F7F += clr_mant_lsbs abscmp_msb_ge lshrdi3 ashldi3
F7F += assert

F7F += classify

F7F += class_inf class_nan class_number class_zero class_nonzero class_sign
F7F += signbit set_sign set_nan set_inf
F7F += is_inf is_nan is_number is_zero is_nonzero
F7F += clr copy copy_P copy_mant msbit is0 cmp_mant store_expo
F7F += abs

F7F += set_s64 set_s32 set_s16 set_s16_impl set_u16_worker
F7F += set_u64 set_u32 set_u16 set_u16_impl
F7F += set_float set_pdouble set_double_impl set_double init_impl init
F7F += get_s16 get_s32 get_s64 get_float
F7F += get_u16 get_u32 get_u64 get_double

F7F += set_eps set_1pow2

# Renames for ALIASes without own module.
F7F += min max exp10
F7F += floatunsidf floatsidf extendsfdf2
F7F += fixdfsi fixdfdi fixunsdfdi fixunsdfsi truncdfsf2

# Renames for f7-const.def.
F7F_cst += 1 2 1_2 1_3 m1 pi ln2 ln10 1_ln2 1_ln10 sqrt2

F7F_asm += classify
F7F_asm += store_expo clr copy copy_P copy_mant
F7F_asm += cmp_mant normalize store_expo
F7F_asm += set_u64 set_s64 addsub_mant_scaled mul_mant
F7F_asm += to_integer to_unsigned clr_mant_lsbs
F7F_asm += div sqrt_approx sqrt16_round sqrt16_floor
F7F_asm += lshrdi3 ashldi3

F7F_asm += class_D

F7F_asm += call_ddd call_xdd call_ddx
F7F_asm += call_dd  call_xd  call_dx
