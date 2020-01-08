/* Copyright (C) 2019-2020 Free Software Foundation, Inc.

   This file is part of LIBF7, which is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libf7.h"

#ifndef __AVR_TINY__

#define ALIAS(X, Y) \
  F7_WEAK __attribute__((__alias__(F7_STRINGY(X)))) __typeof__(X) Y;

#define DALIAS(...) // empty
#define LALIAS(...) // empty

#ifndef IN_LIBGCC2

#include <stdio.h>
#include <assert.h>

#define in_libgcc false

_Static_assert (sizeof (f7_t) == 10 && F7_MANT_BYTES == 7,
		"libf7 will only work with 7-byte mantissa.");
#else

#define in_libgcc true

#if __SIZEOF_DOUBLE__ == 8
#undef  DALIAS
#define DALIAS(X,Y) \
  F7_WEAK __attribute__((__alias__(F7_STRINGY(X)))) __typeof__(X) Y;
#endif

#if __SIZEOF_LONG_DOUBLE__ == 8
#undef  LALIAS
#define LALIAS(X,Y) \
  F7_WEAK __attribute__((__alias__(F7_STRINGY(X)))) __typeof__(X) Y;
#endif

#endif // in libgcc

static F7_INLINE
void f7_assert (bool x)
{
  if (!in_libgcc && !x)
    __builtin_abort();
}

static F7_INLINE
int16_t abs_ssat16 (int16_t a)
{
  _Sat _Fract sa = __builtin_avr_rbits (a);
  return __builtin_avr_bitsr (__builtin_avr_absr (sa));
}

static F7_INLINE
int16_t add_ssat16 (int16_t a, int16_t b)
{
  _Sat _Fract sa = __builtin_avr_rbits (a);
  _Sat _Fract sb = __builtin_avr_rbits (b);
  return __builtin_avr_bitsr (sa + sb);
}

static F7_INLINE
int16_t sub_ssat16 (int16_t a, int16_t b)
{
  _Sat _Fract sa = __builtin_avr_rbits (a);
  _Sat _Fract sb = __builtin_avr_rbits (b);
  return __builtin_avr_bitsr (sa - sb);
}

static F7_INLINE
int8_t ssat8_range (int16_t a, int8_t range)
{
  if (a >= range)
    return range;
  if (a <= -range)
    return -range;
  return a;
}


#define IN_LIBF7_H
  #define F7_CONST_DEF(NAME, FLAGS, M6, M5, M4, M3, M2, M1, M0, EXPO) \
    F7_UNUSED static const uint8_t F7_(const_##NAME##_msb)  = M6;     \
    F7_UNUSED static const int16_t F7_(const_##NAME##_expo) = EXPO;
  #include "libf7-const.def"
  #undef F7_CONST_DEF
#undef IN_LIBF7_H


/*
  libgcc naming converntions for conversions:

   __float<fmode><fmode>  : Convert float modes.
   __floatun<imode><fmode>: Convert unsigned integral to float.
   __fix<fmode><imode>    : Convert float to signed integral.
   __fixuns<fmode><imode> : Convert float to unsigned integral.
*/


#ifdef F7MOD_floatundidf_
F7_WEAK
f7_double_t __floatundidf (uint64_t x)
{
  f7_t xx;
  f7_set_u64 (&xx, x);
  return f7_get_double (&xx);
}
#endif // F7MOD_floatundidf_


#ifdef F7MOD_floatdidf_
F7_WEAK
f7_double_t __floatdidf (int64_t x)
{
  f7_t xx;
  f7_set_s64 (&xx, x);
  return f7_get_double (&xx);
}
#endif // F7MOD_floatdidf_


#ifdef F7MOD_init_
f7_t* f7_init_impl (uint64_t mant, uint8_t flags, f7_t *cc, int16_t expo)
{
  flags &= F7_FLAGS;
  if (f7_class_number (flags))
    {
      uint8_t msb;
      while ((__builtin_memcpy (&msb, (uint8_t*) &mant + 7, 1), msb))
	{
	  mant >>= 1;
	  expo = add_ssat16 (expo, 1);
	}
      *(uint64_t*) cc->mant = mant;
      cc->expo = add_ssat16 (expo, F7_MANT_BITS-1);

      cc = f7_normalize_asm (cc);
    }

  cc->flags = flags;

  return cc;
}
#endif // F7MOD_init_


#ifdef F7MOD_set_s16_
f7_t* f7_set_s16_impl (f7_t *cc, int16_t i16)
{
  uint16_t u16 = (uint16_t) i16;
  uint8_t flags = 0;
  if (i16 < 0)
    {
      u16 = -u16;
      flags = F7_FLAG_sign;
    }
  f7_set_u16_impl (cc, u16);
  cc->flags = flags;
  return cc;
}
#endif // F7MOD_set_s16_


#ifdef F7MOD_set_u16_
f7_t* f7_set_u16_impl (f7_t *cc, uint16_t u16)
{
  f7_clr (cc);
  F7_MANT_HI2 (cc) = u16;
  cc->expo = 15;
  return f7_normalize_asm (cc);
}
#endif // F7MOD_set_u16_


#ifdef F7MOD_set_s32_
f7_t* f7_set_s32 (f7_t *cc, int32_t i32)
{
  uint32_t u32 = (uint32_t) i32;
  uint8_t flags = 0;
  if (i32 < 0)
    {
      u32 = -u32;
      flags = F7_FLAG_sign;
    }
  cc = f7_set_u32 (cc, u32);
  cc->flags = flags;
  return cc;
}
ALIAS (f7_set_s32, f7_floatsidf)
#endif // F7MOD_set_s32_


#ifdef F7MOD_set_u32_
f7_t* f7_set_u32 (f7_t *cc, uint32_t u32)
{
  f7_clr (cc);
  F7_MANT_HI4 (cc) = u32;
  cc->expo = 31;
  return f7_normalize_asm (cc);
}
ALIAS (f7_set_u32, f7_floatunsidf)
#endif // F7MOD_set_u32_


// IEEE 754 single
// float =  s  bbbbbbbb mmmmmmmmmmmmmmmmmmmmmmm
//	   31
// s = sign
// b = biased exponent, bias = 127
// m = mantissa

// +0	      =	 0 0 0
// -0	      =	 1 0 0
// Inf	      =	 S B 0	=  S * Inf, B = 0xff
// NaN	      =	 S B M,	   B = 0xff, M != 0
// Sub-normal =	 S 0 M	=  S * 0.M * 2^{1 - bias}, M != 0
// Normal     =  S B M  =  S * 1.M * 2^{B - bias}, B = 1 ... 0xfe

#define FLT_DIG_EXP   8
#define FLT_DIG_MANT  (31 - FLT_DIG_EXP)
#define FLT_MAX_EXP   ((1 << FLT_DIG_EXP) - 1)
#define FLT_EXP_BIAS  (FLT_MAX_EXP >> 1)

#ifdef F7MOD_set_float_
F7_WEAK
void f7_set_float (f7_t *cc, float f)
{
  uint32_t val32;

  _Static_assert (__SIZEOF_FLOAT__ == 4, "");
  _Static_assert (__FLT_MANT_DIG__ == 24, "");
  __builtin_memcpy (&val32, &f, __SIZEOF_FLOAT__);

  uint16_t val16 = val32 >> 16;
  val16 >>= FLT_DIG_MANT - 16;

  uint8_t expo_biased = val16 & FLT_MAX_EXP;
  bool sign = val16 & (1u << FLT_DIG_EXP);

  f7_clr (cc);

  uint32_t mant = val32 & ((1ul << FLT_DIG_MANT) -1);

  if (mant == 0)
    {
      if (expo_biased == 0)
	return;
      if (expo_biased >= FLT_MAX_EXP)
	return f7_set_inf (cc, sign);
    }

  if (expo_biased == 0)
    expo_biased = 1;   // Sub-normal: biased expo of 1 was encoded as 0.
  else if (expo_biased < FLT_MAX_EXP)
    mant |= (1ul << FLT_DIG_MANT);
  else
    return f7_set_nan (cc);

  __builtin_memcpy (& F7_MANT_HI4 (cc), &mant, 4);

  cc->expo = expo_biased - FLT_EXP_BIAS + 31 - FLT_DIG_MANT;
  f7_normalize_asm (cc);
  f7_set_sign (cc, sign);
}
ALIAS (f7_set_float, f7_extendsfdf2)
#endif // F7MOD_set_float_


#ifdef F7MOD_get_float_
static F7_INLINE
float make_float (uint32_t x)
{
  float ff;
  __builtin_memcpy (&ff, &x, 4);
  return ff;

}

F7_WEAK
float f7_get_float (const f7_t *aa)
{
  uint8_t a_class = f7_classify (aa);

  if (f7_class_nan (a_class))
    return make_float (0xffc00000 /* NaN: Biased expo = 0xff, mant != 0 */);

  uint32_t mant;
  __builtin_memcpy (&mant, &F7_MANT_CONST_HI4 (aa), 4);

  uint8_t expo8 = 0;
  uint8_t mant_offset = FLT_DIG_EXP;
  int16_t c_expo = add_ssat16 (aa->expo, FLT_EXP_BIAS);

  if (f7_class_zero (a_class) || c_expo <= -FLT_DIG_MANT)
    {
      // Zero or tiny.
      return 0.0f;
    }
  else if (c_expo >= FLT_MAX_EXP || f7_class_inf (a_class))
    {
      // Inf or overflow.
      expo8 = FLT_MAX_EXP;
      mant = 0;
    }
  else if (c_expo > 0)
    {
      // Normal.
      expo8 = c_expo;
    }
  else
    {
      // Sub-normal:  -DIG_MANT < c_expo <= 0.
      // Encoding of 0 represents a biased exponent of 1.
      // mant_offset in 9...31.
      expo8 = 0;
      mant_offset += 1 - c_expo;
    }

  uint16_t expo16 = expo8 << (FLT_DIG_MANT - 16);

  if (f7_class_sign (a_class))
    expo16 |= 1u << (FLT_DIG_EXP + FLT_DIG_MANT - 16);

  mant >>= mant_offset;

  __asm ("cbr %T0%t2, 1 << (7 & %2)"  "\n\t"
	 "or  %C0, %A1"		      "\n\t"
	 "or  %D0, %B1"
	 : "+d" (mant)
	 : "r" (expo16), "n" (FLT_DIG_MANT));

  return make_float (mant);
}
F7_PURE ALIAS (f7_get_float, f7_truncdfsf2)
#endif // F7MOD_get_float_

#define DBL_DIG_EXP   11
#define DBL_DIG_MANT  (63 - DBL_DIG_EXP)
#define DBL_MAX_EXP   ((1 << DBL_DIG_EXP) - 1)
#define DBL_EXP_BIAS  (DBL_MAX_EXP >> 1)


#ifdef F7MOD_set_double_
void f7_set_double_impl (f7_double_t val64, f7_t *cc)
{
  f7_clr (cc);
  register uint64_t mant __asm ("r18") = val64 & ((1ull << DBL_DIG_MANT) -1);

  uint16_t val16 = 3[(uint16_t*) & val64];
  val16 >>= DBL_DIG_MANT - 48;

  uint16_t expo_biased = val16 & DBL_MAX_EXP;
  bool sign = val16 & (1u << DBL_DIG_EXP);

  if (mant == 0)
    {
      if (expo_biased == 0)
	return;
      if (expo_biased >= DBL_MAX_EXP)
	return f7_set_inf (cc, sign);
    }
  __asm ("" : "+r" (mant));

  if (expo_biased == 0)
    expo_biased = 1;   // Sub-normal: biased expo of 1 was encoded as 0.
  else if (expo_biased < DBL_MAX_EXP)
    mant |= (1ull << DBL_DIG_MANT);
  else
    return f7_set_nan (cc);

  *(uint64_t*) & cc->mant = mant;

  cc->expo = expo_biased - DBL_EXP_BIAS + 63 - DBL_DIG_MANT - 8;
  f7_normalize_asm (cc);
  f7_set_sign (cc, sign);
}
#endif // F7MOD_set_double_


#ifdef F7MOD_set_pdouble_
void f7_set_pdouble (f7_t *cc, const f7_double_t *val64)
{
  f7_set_double (cc, *val64);
}
#endif // F7MOD_set_pdouble_


#ifdef F7MOD_get_double_
static F7_INLINE
uint64_t clr_r18 (void)
{
  extern void __clr_8 (void);
  register uint64_t r18 __asm ("r18");
  __asm ("%~call %x[f]" : "=r" (r18) : [f] "i" (__clr_8));
  return r18;
}

static F7_INLINE
f7_double_t make_double (uint64_t x)
{
  register f7_double_t r18 __asm ("r18") = x;
  __asm ("" : "+r" (r18));
  return r18;
}

F7_WEAK
f7_double_t f7_get_double (const f7_t *aa)
{
  uint8_t a_class = f7_classify (aa);

  if (f7_class_nan (a_class))
    {
      uint64_t nan = clr_r18() | (0x7fffull << 48);
      return make_double (nan);
    }

  uint64_t mant;
  __builtin_memcpy (&mant, & aa->mant, 8);

  mant &= 0x00ffffffffffffff;

  // FIXME: For subnormals, rounding is premature and should be
  //	    done *after* the mantissa has been shifted into place
  //	    (or the round value be shifted left accordingly).
  // Round.
  mant += 1u << (F7_MANT_BITS - (1 + DBL_DIG_MANT) - 1);

  uint8_t dex;
  register uint64_t r18 __asm ("r18") = mant;
  // dex = Overflow ? 1 : 0.
  __asm ("bst %T[mant]%T[bitno]"  "\n\t"
	 "clr %0"		  "\n\t"
	 "bld %0,0"
	 : "=r" (dex), [mant] "+r" (r18)
	 : [bitno] "n" (64 - 8));

  mant = r18 >> dex;

  uint16_t expo16 = 0;
  uint16_t mant_offset = DBL_DIG_EXP - 8;
  int16_t c_expo = add_ssat16 (aa->expo, DBL_EXP_BIAS + dex);

  if (f7_class_zero (a_class) || c_expo <= -DBL_DIG_MANT)
    {
      // Zero or tiny.
      return make_double (clr_r18());
    }
  else if (c_expo >= DBL_MAX_EXP || f7_class_inf (a_class))
    {
      // Inf or overflow.
      expo16 = DBL_MAX_EXP;
      mant = clr_r18();
    }
  else if (c_expo > 0)
    {
      // Normal.
      expo16 = c_expo;
    }
  else
    {
      // Sub-normal:  -DIG_MANT < c_expo <= 0.
      // Encoding expo of 0 represents a biased exponent of 1.
      // mant_offset in 5...55 = 63-8.
      mant_offset += 1 - c_expo;
    }

  expo16 <<= (DBL_DIG_MANT - 48);

  if (f7_class_sign (a_class))
    expo16 |= 1u << (DBL_DIG_EXP + DBL_DIG_MANT - 48);

  // mant >>= mant_offset;
  mant = f7_lshrdi3 (mant, mant_offset);

  r18 = mant;
  __asm ("cbr %T0%t2, 1 << (7 & %2)"  "\n\t"
	 "or  %r0+6, %A1"	      "\n\t"
	 "or  %r0+7, %B1"
	 : "+r" (r18)
	 : "r" (expo16), "n" (DBL_DIG_MANT));

  return make_double (r18);
}
#endif // F7MOD_get_double_


#ifdef F7MOD_fabs_
F7_WEAK
void f7_fabs (f7_t *cc, const f7_t *aa)
{
  f7_abs (cc, aa);
}
#endif // F7MOD_fabs_


#ifdef F7MOD_neg_
F7_WEAK
f7_t* f7_neg (f7_t *cc, const f7_t *aa)
{
  f7_copy (cc, aa);

  uint8_t c_class = f7_classify (cc);

  if (! f7_class_zero (c_class))
    cc->sign = ! f7_class_sign (c_class);

  return cc;
}
#endif // F7MOD_neg_


#ifdef F7MOD_frexp_
F7_WEAK
void f7_frexp (f7_t *cc, const f7_t *aa, int *expo)
{
  uint8_t a_class = f7_classify (aa);

  if (f7_class_nan (a_class))
    return f7_set_nan (cc);

  if (f7_class_inf (a_class) || aa->expo == INT16_MAX)
    return f7_set_inf (cc, f7_class_sign (a_class));

  if (! f7_msbit (aa))
    {
      *expo = 0;
      return f7_clr (cc);
    }

  *expo = 1 + aa->expo;
  cc->flags = a_class & F7_FLAG_sign;
  cc->expo = -1;
  f7_copy_mant (cc, aa);
}
#endif // F7MOD_frexp_

#ifdef F7MOD_get_s16_
F7_WEAK
int16_t f7_get_s16 (const f7_t *aa)
{
  extern int16_t to_s16 (const f7_t*, uint8_t) F7ASM(f7_to_integer_asm);
  return to_s16 (aa, 0xf);
}
#endif // F7MOD_get_s16_


#ifdef F7MOD_get_s32_
F7_WEAK
int32_t f7_get_s32 (const f7_t *aa)
{
  extern int32_t to_s32 (const f7_t*, uint8_t) F7ASM(f7_to_integer_asm);
  return to_s32 (aa, 0x1f);
}
F7_PURE ALIAS (f7_get_s32, f7_fixdfsi)
#endif // F7MOD_get_s32_


#ifdef F7MOD_get_s64_
  F7_WEAK
  int64_t f7_get_s64 (const f7_t *aa)
{
  extern int64_t to_s64 (const f7_t*, uint8_t) F7ASM(f7_to_integer_asm);
  return to_s64 (aa, 0x3f);
}
F7_PURE ALIAS (f7_get_s64, f7_fixdfdi)
#endif // F7MOD_get_s64_

#ifdef F7MOD_get_u16_
  F7_WEAK
  uint16_t f7_get_u16 (const f7_t *aa)
{
  extern uint16_t to_u16 (const f7_t*, uint8_t) F7ASM(f7_to_unsigned_asm);
  return to_u16 (aa, 0xf);
}
#endif // F7MOD_get_u16_


#ifdef F7MOD_get_u32_
F7_WEAK
uint32_t f7_get_u32 (const f7_t *aa)
{
  extern uint32_t to_u32 (const f7_t*, uint8_t) F7ASM(f7_to_unsigned_asm);
  return to_u32 (aa, 0x1f);
}
F7_PURE ALIAS (f7_get_u32, f7_fixunsdfsi)
#endif // F7MOD_get_u32_


#ifdef F7MOD_get_u64_
F7_WEAK
uint64_t f7_get_u64 (const f7_t *aa)
{
  extern int64_t to_u64 (const f7_t*, uint8_t) F7ASM(f7_to_unsigned_asm);
  return to_u64 (aa, 0x3f);
}
F7_PURE ALIAS (f7_get_u64, f7_fixunsdfdi)
#endif // F7MOD_get_u64_


#ifdef F7MOD_cmp_unordered_
F7_NOINLINE
static int8_t cmp_u8 (uint8_t a_class, uint8_t b_class, bool sign_a);

F7_WEAK
int8_t f7_cmp_unordered (const f7_t *aa, const f7_t *bb, bool with_sign)
{
  uint8_t a_class = f7_classify (aa);
  uint8_t b_class = f7_classify (bb);

  uint8_t a_sign = f7_class_sign (a_class) & with_sign;
  uint8_t b_sign = f7_class_sign (b_class) & with_sign;
  uint8_t ab_class = a_class | b_class;
  ab_class &= with_sign - 2;

  if (f7_class_nan (ab_class))
    return INT8_MIN;

  if (a_sign != b_sign)
    return b_sign - a_sign;

  if (f7_class_inf (ab_class))
    return cmp_u8 (a_class, b_class, a_sign);

  if (f7_class_zero (ab_class))
    return cmp_u8 (b_class, a_class, a_sign);

  if (aa->expo < bb->expo)
    return a_sign ? 1 : -1;

  if (aa->expo > bb->expo)
    return a_sign ? -1 : 1;

  return cmp_u8 (1 + f7_cmp_mant (aa, bb), 1, a_sign);
}


int8_t cmp_u8 (uint8_t a_class, uint8_t b_class, bool sign_a)
{
  int8_t c;
  __asm ("sub  %[a], %[b]"    "\n\t"
	 "breq 1f"	      "\n\t"
	 "sbc  %[c], %[c]"    "\n\t"
	 "sbci %[c], -1"      "\n\t"
	 "sbrc %[s], 0"	      "\n\t"
	 "neg  %[c]"	      "\n\t"
	 "1:"
	 : [c] "=d" (c)
	 : [a] "0" (a_class), [b] "r" (b_class), [s] "r" (sign_a));
  return c;
}
#endif // F7MOD_cmp_unordered_


#ifdef F7MOD_cmp_abs_
F7_WEAK
int8_t f7_cmp_abs (const f7_t *aa, const f7_t *bb)
{
  return f7_cmp_unordered (aa, bb, false /* no signs */);
}
#endif // F7MOD_cmp_abs_


#ifdef F7MOD_cmp_
F7_WEAK
int8_t f7_cmp (const f7_t *aa, const f7_t *bb)
{
  return f7_cmp_unordered (aa, bb, true /* with signs */);
}
#endif // F7MOD_cmp_


#ifdef F7MOD_abscmp_msb_ge_
// Compare absolute value of Number aa against a f7_t represented
// by msb and expo.
F7_WEAK
bool f7_abscmp_msb_ge (const f7_t *aa, uint8_t msb, int16_t expo)
{
  uint8_t a_msb = aa->mant[F7_MANT_BYTES - 1];

  if (0 == (0x80 & a_msb))
    // 0 or subnormal.
    return false;

  return aa->expo == expo
    ? a_msb >= msb
    : aa->expo > expo;
}
#endif // F7MOD_abscmp_msb_ge_

#ifdef F7MOD_lt_
F7_WEAK
bool f7_lt_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_lt (aa, bb);
}
#endif // F7MOD_lt_

#ifdef F7MOD_le_
F7_WEAK
bool f7_le_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_le (aa, bb);
}
#endif // F7MOD_le_

#ifdef F7MOD_gt_
F7_WEAK
bool f7_gt_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_gt (aa, bb);
}
#endif // F7MOD_gt_

#ifdef F7MOD_ge_
F7_WEAK
bool f7_ge_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_ge (aa, bb);
}
#endif // F7MOD_ge_

#ifdef F7MOD_ne_
F7_WEAK
bool f7_ne_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_ne (aa, bb);
}
#endif // F7MOD_ne_

#ifdef F7MOD_eq_
F7_WEAK
bool f7_eq_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_eq (aa, bb);
}
#endif // F7MOD_eq_


#ifdef F7MOD_unord_
F7_WEAK
bool f7_unord_impl (const f7_t *aa, const f7_t *bb)
{
  return f7_unordered (aa, bb);
}
#endif // F7MOD_unord_


#ifdef F7MOD_minmax_
F7_WEAK
f7_t* f7_minmax (f7_t *cc, const f7_t *aa, const f7_t *bb, bool do_min)
{
  int8_t cmp = f7_cmp_unordered (aa, bb, true /* with signs */);

  if (cmp == INT8_MIN)
    return (f7_set_nan (cc), cc);

  if (do_min)
    cmp = -cmp;

  return f7_copy (cc, cmp >= 0 ? aa : bb);
}
#endif // F7MOD_minmax_


#ifdef F7MOD_fmax_
F7_WEAK
f7_t* f7_fmax (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  return f7_minmax (cc, aa, bb, false);
}
ALIAS (f7_fmax, f7_max)
#endif // F7MOD_fmax_


#ifdef F7MOD_fmin_
F7_WEAK
f7_t* f7_fmin (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  return f7_minmax (cc, aa, bb, true);
}
ALIAS (f7_fmin, f7_min)
#endif // F7MOD_fmin_


#ifdef F7MOD_mulx_
F7_WEAK
uint8_t f7_mulx (f7_t *cc, const f7_t *aa, const f7_t *bb, bool no_rounding)
{
  uint8_t a_class = f7_classify (aa);
  uint8_t b_class = f7_classify (bb);
  // From this point on, no more access aa->flags or bb->flags
  // to avoid early-clobber when writing cc->flags.

  uint8_t ab_class = a_class | b_class;
  // If either value is NaN, return NaN.
  if (f7_class_nan (ab_class)
      // Any combination of Inf and 0.
      || (f7_class_zero (ab_class) && f7_class_inf (ab_class)))
    {
      cc->flags = F7_FLAG_nan;
      return 0;
    }
  // If either value is 0.0, return 0.0.
  if (f7_class_zero (ab_class))
    {
      f7_clr (cc);
      return 0;
    }
  // We have 2 non-zero numbers-or-INF.

  uint8_t c_sign = (a_class ^ b_class) & F7_FLAG_sign;
  uint8_t c_inf  = ab_class & F7_FLAG_inf;
  cc->flags = c_sign | c_inf;
  if (c_inf)
    return 0;

  int16_t expo = add_ssat16 (aa->expo, bb->expo);
  // Store expo and handle expo = INT16_MIN  and INT16_MAX.
  if (f7_store_expo (cc, expo))
    return 0;

  return f7_mul_mant_asm (cc, aa, bb, no_rounding);
}
#endif // F7MOD_mulx_


#ifdef F7MOD_square_
F7_WEAK
void f7_square (f7_t *cc, const f7_t *aa)
{
  f7_mul (cc, aa, aa);
}
#endif // F7MOD_square_


#ifdef F7MOD_mul_
F7_WEAK
void f7_mul (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  f7_mulx (cc, aa, bb, false);
}
#endif // F7MOD_mul_


#ifdef F7MOD_Iadd_
F7_WEAK void f7_Iadd (f7_t *cc, const f7_t *aa) { f7_add (cc, cc, aa); }
#endif

#ifdef F7MOD_Isub_
F7_WEAK void f7_Isub (f7_t *cc, const f7_t *aa) { f7_sub (cc, cc, aa); }
#endif

#ifdef F7MOD_Imul_
F7_WEAK void f7_Imul (f7_t *cc, const f7_t *aa) { f7_mul (cc, cc, aa); }
#endif

#ifdef F7MOD_Idiv_
F7_WEAK void f7_Idiv (f7_t *cc, const f7_t *aa) { f7_div (cc, cc, aa); }
#endif

#ifdef F7MOD_IRsub_
F7_WEAK void f7_IRsub (f7_t *cc, const f7_t *aa) { f7_sub (cc, aa, cc); }
#endif

#ifdef F7MOD_Ineg_
F7_WEAK void f7_Ineg (f7_t *cc) { f7_neg (cc, cc); }
#endif

#ifdef F7MOD_Isqrt_
F7_WEAK void f7_Isqrt (f7_t *cc) { f7_sqrt (cc, cc); }
#endif

#ifdef F7MOD_Isquare_
F7_WEAK void f7_Isquare (f7_t *cc) { f7_square (cc, cc); }
#endif

#ifdef F7MOD_Ildexp_
F7_WEAK f7_t* f7_Ildexp (f7_t *cc, int ex) { return f7_ldexp (cc, cc, ex); }
#endif


#ifdef F7MOD_add_
F7_WEAK
void f7_add (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  f7_addsub (cc, aa, bb, false);
}
#endif // F7MOD_add_


#ifdef F7MOD_sub_
F7_WEAK
void f7_sub (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  f7_addsub (cc, aa, bb, true);
}
#endif // F7MOD_sub_


#ifdef F7MOD_addsub_
static void return_with_sign (f7_t *cc, const f7_t *aa, int8_t c_sign)
{
  __asm (";;; return with sign");
  f7_copy (cc, aa);
  if (c_sign != -1)
    f7_set_sign (cc, c_sign);
}

F7_WEAK
void f7_addsub (f7_t *cc, const f7_t *aa, const f7_t *bb, bool neg_b)
{
  uint8_t a_class = f7_classify (aa);
  uint8_t b_class = f7_classify (bb);
  // From this point on, no more access aa->flags or bb->flags
  // to avoid early-clobber when writing cc->flags.

  // Hande NaNs.
  if (f7_class_nan (a_class | b_class))
    return f7_set_nan (cc);

  bool a_sign = f7_class_sign (a_class);
  bool b_sign = f7_class_sign (b_class) ^ neg_b;

  // Add the mantissae?
  bool do_add = a_sign == b_sign;

  // Handle +Infs and -Infs.
  bool a_inf = f7_class_inf (a_class);
  bool b_inf = f7_class_inf (b_class);

  if (a_inf && b_inf)
    {
      if (do_add)
	return f7_set_inf (cc, a_sign);
      else
	return f7_set_nan (cc);
    }
  else if (a_inf)
    return f7_set_inf (cc, a_sign);
  else if (b_inf)
    return f7_set_inf (cc, b_sign);

  int16_t shift16 = sub_ssat16 (aa->expo, bb->expo);

  // aa + 0 = aa.
  // Also check MSBit to get rid of Subnormals and 0.
  if (shift16 > F7_MANT_BITS || f7_is0 (bb))
    return return_with_sign (cc, aa, -1);

  // 0 + bb = bb.
  // 0 - bb = -bb.
  // Also check MSBit to get rid of Subnormals and 0.
  if (shift16 < -F7_MANT_BITS || f7_is0 (aa))
    return return_with_sign (cc, bb, b_sign);

  // Now aa and bb are non-zero, non-NaN, non-Inf.
  // shift > 0 ==> |a| > |b|
  // shift < 0 ==> |a| < |b|
  int8_t shift = (int8_t) shift16;
  bool c_sign = a_sign;
  if (shift < 0
      || (shift == 0 && f7_cmp_mant (aa, bb) < 0))
    {
      const f7_t *p = aa; aa = bb; bb = p;
      c_sign = b_sign;
      shift = -shift;
    }
  uint8_t shift2 = (uint8_t) (shift << 1);

  cc->expo = aa->expo;
  // From this point on, no more access aa->expo or bb->expo
  // to avoid early-clobber when writing cc->expo.

  cc->flags = c_sign;  _Static_assert (F7_FLAGNO_sign == 0, "");

  // This function uses neither .expo nor .flags from either aa or bb,
  // hence there is early-clobber for cc->expo and cc->flags.
  f7_addsub_mant_scaled_asm (cc, aa, bb, shift2 | do_add);
}
#endif // F7MOD_addsub_


#ifdef F7MOD_madd_msub_
F7_WEAK
void f7_madd_msub (f7_t *cc, const f7_t *aa, const f7_t *bb, const f7_t *dd,
                   bool neg_d)
{
  f7_t xx7, *xx = &xx7;
  uint8_t x_lsb = f7_mulx (xx, aa, bb, true /* no rounding */);
  uint8_t x_sign = f7_signbit (xx);
  int16_t x_expo = xx->expo;
  f7_addsub (xx, xx, dd, neg_d);
  // Now add LSB.  If cancellation occured in the add / sub, then we have the
  // chance of extra 8 bits of precision.  Turn LSByte into f7_t.
  f7_clr (cc);
  cc->expo = sub_ssat16 (x_expo, F7_MANT_BITS);
  cc->mant[F7_MANT_BYTES - 1] = x_lsb;
  cc = f7_normalize_asm (cc);
  cc->flags = x_sign;
  f7_Iadd (cc, xx);
}
#endif // F7MOD_madd_msub_

#ifdef F7MOD_madd_
F7_WEAK
void f7_madd (f7_t *cc, const f7_t *aa, const f7_t *bb, const f7_t *dd)
{
  f7_madd_msub (cc, aa, bb, dd, false);
}
#endif // F7MOD_madd_

#ifdef F7MOD_msub_
F7_WEAK
void f7_msub (f7_t *cc, const f7_t *aa, const f7_t *bb, const f7_t *dd)
{
  f7_madd_msub (cc, aa, bb, dd, true);
}
#endif // F7MOD_msub_


#ifdef F7MOD_ldexp_
F7_WEAK
f7_t* f7_ldexp (f7_t *cc, const f7_t *aa, int delta)
{
  uint8_t a_class = f7_classify (aa);

  cc->flags = a_class;

  // Inf and NaN.
  if (! f7_class_number (a_class))
    return cc;

  if (f7_msbit (aa) == 0)
    return (f7_clr (cc), cc);

  int16_t expo = add_ssat16 (delta, aa->expo);
  // Store expo and handle expo = INT16_MIN  and INT16_MAX.
  if (! f7_store_expo (cc, expo))
    f7_copy_mant (cc, aa);

  return cc;
}
#endif // F7MOD_ldexp_


#if USE_LPM
#elif USE_LD
#else
#error need include "asm-defs.h"
#endif // USE_LPM

/*
  Handling constants:

  F7_PCONST (PVAR, X)

      Set  f7_t [const] *PVAR  to an LD address for one
      of the  f7_const_X[_P]  constants.
      PVAR might be set to point to a local auto that serves
      as temporary storage for f7_const_X_P.  PVAR is only
      valid in the current block.

  const f7_t* F7_PCONST_U16 (PVAR, <ident> X)       // USE_LD
  f7_t*       F7_PCONST_U16 (PVAR, uint16_t X)      // USE_LPM

      Set  f7_t [const] *PVAR  to an LD address for one of the
      f7_const_X[_P]  constants.  PVAR might be set to point to a
      local auto that serves as temporary storage for X.  PVAR is
      only valid in the current block.

  F7_PCONST_VAR (PVAR, VAR)

      VAR is a pointer variable holding the address of some f7_const_X[_P]
      constant.  Set  [const] f7_t *PVAR  to a respective LD address.
      PVAR might be set to point to a local auto that serves
      as temporary storage for f7_const_X_P.  PVAR is only
      valid in the current block.

  F7_CONST_ADDR (<ident> CST, f7_t* PTMP)

      Return an LD address to for some f7_const_X[_P] constant.
      *PTMP might be needed to hold a copy of f7_const_X_P in RAM.

  f7_t*       F7_U16_ADDR (uint16_t     X, f7_t* PTMP)   // USE_LPM
  const f7_t* F7_U16_ADDR (<cst-ident>  X, <unused>)     // USE_LD

      Return an LD address to compile-time constant  uint16_t X  which is
      also known as  f7_const_X[_P].  *PTMP might be set to  (f7_t) X.

  f7_t* f7_const (f7_t *PVAR, <cst-ident> X)

      Copy  f7_const_X[_P]  to *PVAR.

  f7_t* f7_copy_flash (f7_t *DST, const f7_t *SRC)

      Copy to *DST with LD (from .rodata in flash) if the address
      space is linear, or with  LPM (from .progmem.data) if the
      address space is not linear.

  f7_t* f7_copy (f7_t *DST, const f7_t* SRC)

      Copy to RAM using LD.

  f7_t* f7_copy_P (f7_t *DST, const f7_t *SRC)

      Copy to RAM using LPM.
*/

#if USE_LPM
  #define F7_RAW_CONST_ADDR(CST) \
      & F7_(const_##CST##_P)

  #define F7_PCONST(PVAR, CST)				    \
      f7_t _var_for_##CST;				    \
      f7_copy_P (& _var_for_##CST, & F7_(const_##CST##_P)); \
      PVAR = & _var_for_##CST

  #define F7_PCONST_U16(PVAR, CST)		\
      f7_t _var_for_##CST;			\
      PVAR = f7_set_u16 (& _var_for_##CST, CST)

  #define F7_PCONST_VAR(PVAR, VAR)		\
      f7_t _var_for_##VAR;			\
      f7_copy_P (& _var_for_##VAR, VAR);	\
      PVAR = & _var_for_##VAR

  #define MAYBE_const // empty

  #define F7_CONST_ADDR(CST, PTMP) \
      f7_copy_P ((PTMP), & F7_(const_##CST##_P))

  #define F7_U16_ADDR(CST, PTMP)   \
      f7_set_u16 ((PTMP), CST)

#elif USE_LD
  #define F7_RAW_CONST_ADDR(CST)   \
      & F7_(const_##CST)

  #define F7_PCONST(PVAR, CST)	   \
      PVAR = & F7_(const_##CST)

  #define F7_PCONST_U16(PVAR, CST) \
      PVAR = & F7_(const_##CST)

  #define F7_PCONST_VAR(PVAR, VAR) \
      PVAR = (VAR)

  #define F7_CONST_ADDR(CST, PTMP) \
      (& F7_(const_##CST))

  #define F7_U16_ADDR(CST, PTMP)   \
      (& F7_(const_##CST))

  #define MAYBE_const const
#endif



#define DD(str, X)		\
  do {				\
    LOG_PSTR (PSTR (str));	\
    f7_dump (X);		\
  } while (0)

#undef DD
#define DD(...) (void) 0


#ifdef F7MOD_sqrt_
static void sqrt_worker (f7_t *cc, const f7_t *rr)
{
  f7_t tmp7, *tmp = &tmp7;
  f7_t aa7, *aa = &aa7;

  // aa in  [1/2, 2)  =>  aa->expo in { -1, 0 }.
  int16_t a_expo = -(rr->expo & 1);
  int16_t c_expo = (rr->expo - a_expo) >> 1;  // FIXME: r_expo = INT_MAX???

  __asm ("" : "+r" (aa));

  f7_copy (aa, rr);
  aa->expo = a_expo;

  // No use of rr or *cc past this point:  We may use cc as temporary.
  // Approximate square-root of  A  by  X <-- (X + A / X) / 2.

  f7_sqrt_approx_asm (cc, aa);

  // Iterate  X <-- (X + A / X) / 2.
  // 3 Iterations with 16, 32, 58 bits of precision for the quotient.

  for (uint8_t prec = 16; (prec & 0x80) == 0; prec <<= 1)
    {
      f7_divx (tmp, aa, cc, (prec & 64) ? 2 + F7_MANT_BITS : prec);
      f7_Iadd (cc, tmp);
      // This will never underflow because |c_expo| is small.
      cc->expo--;
    }

  // Similar: |c_expo| is small, hence no ldexp needed.
  cc->expo += c_expo;
}

F7_WEAK
void f7_sqrt (f7_t *cc, const f7_t *aa)
{
  uint8_t a_class = f7_classify (aa);

  if (f7_class_nan (a_class) || f7_class_sign (a_class))
    return f7_set_nan (cc);

  if (f7_class_inf (a_class))
    return f7_set_inf (cc, 0);

  if (f7_class_zero (a_class))
    return f7_clr (cc);

  sqrt_worker (cc, aa);
}
#endif // F7MOD_sqrt_


#ifdef F7MOD_hypot_
F7_WEAK
void f7_hypot (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  f7_t xx7, *xx = &xx7;

  f7_square (xx, aa);
  f7_square (cc, bb);
  f7_Iadd (cc, xx);
  f7_Isqrt (cc);
}
#endif // F7MOD_hypot_


#ifdef F7MOD_const_m1_
#include "libf7-constdef.h"
#endif // -1

#ifdef F7MOD_const_1_2_
#include "libf7-constdef.h"
#endif // 1/2

#ifdef F7MOD_const_1_3_
#include "libf7-constdef.h"
#endif // 1/3

#ifdef F7MOD_const_ln2_
#include "libf7-constdef.h"
#endif // ln2

#ifdef F7MOD_const_1_ln2_
#include "libf7-constdef.h"
#endif // 1_ln2

#ifdef F7MOD_const_ln10_
#include "libf7-constdef.h"
#endif // ln10

#ifdef F7MOD_const_1_ln10_
#include "libf7-constdef.h"
#endif // 1_ln10

#ifdef F7MOD_const_1_
#include "libf7-constdef.h"
#endif // 1

#ifdef F7MOD_const_sqrt2_
#include "libf7-constdef.h"
#endif // sqrt2

#ifdef F7MOD_const_2_
#include "libf7-constdef.h"
#endif // 2

#ifdef F7MOD_const_pi_
#include "libf7-constdef.h"
#endif // pi


#ifdef F7MOD_divx_

// C /= A
extern void f7_div_asm (f7_t*, const f7_t*, uint8_t);

F7_WEAK
void f7_divx (f7_t *cc, const f7_t *aa, const f7_t *bb, uint8_t quot_bits)
{
  uint8_t a_class = f7_classify (aa);
  uint8_t b_class = f7_classify (bb);
  // From this point on, no more access aa->flags or bb->flags
  // to avoid early-clobber when writing cc->flags.

  // If either value is NaN, return NaN.
  if (f7_class_nan (a_class | b_class)
      // If both values are Inf or both are 0, return NaN.
      || f7_class_zero (a_class & b_class)
      || f7_class_inf (a_class & b_class)
      // Inf / 0 = NaN.
      || (f7_class_inf (a_class) && f7_class_zero (b_class)))
    {
      return f7_set_nan (cc);
    }

  // 0 / B   = 0  for non-zero, non-NaN B.
  // A / Inf = 0  for non-zero numbers A.
  if (f7_class_zero (a_class) || f7_class_inf (b_class))
    return f7_clr (cc);

  uint8_t c_sign = (a_class ^ b_class) & F7_FLAG_sign;

  if (f7_class_inf (a_class) || f7_class_zero (b_class))
    return f7_set_inf (cc, c_sign);

  cc->flags = c_sign;     _Static_assert (F7_FLAGNO_sign == 0, "");
  int16_t expo = sub_ssat16 (aa->expo, bb->expo);

  // Store expo and handle expo = INT16_MIN  and INT16_MAX.
  if (f7_store_expo (cc, expo))
    return;

  f7_t ss7, *ss = &ss7;
  ss->flags = cc->flags;
  ss->expo  = cc->expo;

  f7_copy_mant (ss, aa);
  f7_div_asm (ss, bb, quot_bits);
  f7_copy (cc, ss);
}
#endif // F7MOD_divx_


#ifdef F7MOD_div_
F7_WEAK
void f7_div (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  /* When f7_divx calls f7_div_asm, dividend and divisor are valid
     mantissae, i.e. their MSBit is set.  Therefore, the quotient will
     be in  [0x0.ff..., 0x0.40...]  and to adjust it, at most 1 left-shift
     is needed.  Compute F7_MANT_BITS + 2 bits of the quotient:
     One bit is used for rounding, and one bit might be consumed by the
     mentioned left-shift.  */

  f7_divx (cc, aa, bb, 2 + F7_MANT_BITS);
}
#endif // F7MOD_div_


#ifdef F7MOD_div1_
F7_WEAK
void f7_div1 (f7_t *cc, const f7_t *aa)
{
  F7_PCONST_U16 (const f7_t *one, 1);
  f7_div (cc, one, aa);
}
#endif // F7MOD_div_


#ifdef F7MOD_fmod_
F7_WEAK
void f7_fmod (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
  uint8_t a_class = f7_classify (aa);
  uint8_t b_class = f7_classify (bb);

  if (! f7_class_number (a_class)
      || f7_class_nan (b_class)
      || f7_class_zero (b_class))
    {
      return f7_set_nan (cc);
    }

  // A == 0 and B != 0  =>  0.
  if (f7_class_zero (a_class))
    return f7_clr (cc);

  f7_t zz7, *zz = & zz7;

  f7_div (zz, aa, bb);

  // Z in Z,  |Z| <= |A/B|.
  f7_trunc (zz, zz);

  // C = A - Z * B.
  f7_msub (cc, zz, bb, aa);
  cc->flags ^= F7_FLAG_sign;
}
#endif // F7MOD_fmod_


#ifdef F7MOD_truncx_
F7_WEAK
f7_t* f7_truncx (f7_t *cc, const f7_t *aa, bool do_floor)
{
  uint8_t a_class = f7_classify (aa);

  if (! f7_class_nonzero (a_class))
    return f7_copy (cc, aa);

  bool sign = f7_class_sign (a_class);

  int16_t a_expo = aa->expo;

  if (a_expo < 0)
    {
      // |A| < 1.
      if (sign & do_floor)
	return f7_set_s16 (cc, -1);

      f7_clr (cc);
      return cc;
    }
  else if (a_expo >= F7_MANT_BITS - 1)
    // A == floor (A).
    return f7_copy (cc, aa);

  f7_t tmp7, *tmp = &tmp7;

  // Needed if aa === cc.
  f7_copy (tmp, aa);

  cc->flags = sign;
  cc->expo = a_expo;
  f7_clr_mant_lsbs (cc, aa, F7_MANT_BITS - 1 - a_expo);

  if (do_floor && cc->sign && f7_cmp_mant (cc, tmp) != 0)
    {
      F7_PCONST_U16 (const f7_t *one, 1);
      f7_Isub (cc, one);
    }

  return cc;
}
#endif // F7MOD_truncx_


#ifdef F7MOD_floor_
F7_WEAK
f7_t* f7_floor (f7_t *cc, const f7_t *aa)
{
  return f7_truncx (cc, aa, true);
}
#endif // F7MOD_floor_


#ifdef F7MOD_trunc_
F7_WEAK
f7_t* f7_trunc (f7_t *cc, const f7_t *aa)
{
  return f7_truncx (cc, aa, false);
}
#endif // F7MOD_trunc_


#ifdef F7MOD_ceil_
F7_WEAK
void f7_ceil (f7_t *cc, const f7_t *aa)
{
  cc = f7_copy (cc, aa);
  cc->flags ^= F7_FLAG_sign;
  cc = f7_floor (cc, cc);
  f7_Ineg (cc);
}
#endif // F7MOD_ceil_


#ifdef F7MOD_round_
F7_WEAK
void f7_round (f7_t *cc, const f7_t *aa)
{
  f7_t tmp;
  (void) tmp;
  const f7_t *half = F7_CONST_ADDR (1_2, &tmp);

  f7_addsub   (cc, aa, half, f7_signbit (aa));
  f7_trunc (cc, cc);
}
#endif // F7MOD_round_


#ifdef F7MOD_horner_

// Assertion when using this function is that either cc != xx,
// or if cc == xx, then tmp1 must be non-NULL and tmp1 != xx.
// In General, the calling functions have a spare f7_t object available
// and can pass it down to save some stack.
// Moreover, the power series must have degree 1 at least.

F7_WEAK
void f7_horner (f7_t *cc, const f7_t *xx, uint8_t n_coeff, const f7_t *coeff,
                f7_t *tmp1)
{
  f7_assert (n_coeff > 1);

  if (cc != xx)
    tmp1 = cc;
  else
    f7_assert (tmp1 != NULL && tmp1 != xx);

  f7_t *yy = tmp1;
  f7_t tmp27, *tmp2 = &tmp27;

  n_coeff--;
  const f7_t *pcoeff = coeff + n_coeff;

  f7_copy_flash (yy, pcoeff);

  while (1)
    {
      --pcoeff;
#if 1
      f7_Imul (yy, xx);
      const f7_t *cst = USE_LD ? pcoeff : f7_copy_P (tmp2, pcoeff);
      if (coeff == pcoeff)
	return f7_add (cc, yy, cst);
      f7_Iadd (yy, cst);
#else
      const f7_t *cst = USE_LD ? pcoeff : f7_copy_P (tmp2, pcoeff);
      f7_madd (yy, yy, xx, cst);
      if (coeff == pcoeff)
        {
	  f7_copy (cc, yy);
	  return;
        }
#endif
    }

  __builtin_unreachable();
}
#endif // F7MOD_horner_


#ifdef F7MOD_log_
F7_WEAK
void f7_log (f7_t *cc, const f7_t *aa)
{
    f7_logx (cc, aa, NULL);
}
#endif // F7MOD_log_


#ifdef F7MOD_log2_
F7_WEAK
void f7_log2 (f7_t *cc, const f7_t *aa)
{
  f7_logx (cc, aa, F7_RAW_CONST_ADDR (1_ln2));
}
#endif // F7MOD_log2_


#ifdef F7MOD_log10_
F7_WEAK
void f7_log10 (f7_t *cc, const f7_t *aa)
{
  f7_logx (cc, aa, F7_RAW_CONST_ADDR (1_ln10));
}
#endif // F7MOD_log10_


#ifdef F7MOD_logx_

#define ARRAY_NAME coeff_artanh
#include "libf7-array.def"
#undef ARRAY_NAME

// Compute P * ln(A)  if P != NULL and ln(A), otherwise.
// P is a LD-address if USE_LD and a LPM-address if USE_LPM.
// Assumption is that P > 0.

F7_WEAK
void f7_logx (f7_t *cc, const f7_t *aa, const f7_t *p)
{
  uint8_t a_class = f7_classify (aa);

  if (f7_class_nan (a_class) || f7_class_sign (a_class))
    return f7_set_nan (cc);

  if (f7_class_inf (a_class))
    return f7_set_inf (cc, 0);

  if (f7_class_zero (a_class))
    return f7_set_inf (cc, 1);

  f7_t *yy = cc;
  f7_t xx7, *xx = &xx7;
  f7_t tmp7, *tmp = &tmp7;

  // Y in [1, 2]  =  A * 2 ^ (-a_expo).
  int16_t a_expo = aa->expo;
  f7_copy (yy, aa);
  yy->expo = 0;

  // Y in [1 / sqrt2, sqrt2].

  if (f7_abscmp_msb_ge (yy, F7_(const_sqrt2_msb), F7_(const_sqrt2_expo)))
    {
      yy->expo = -1;
      a_expo = add_ssat16 (a_expo, 1);
    }

  const f7_t *one = F7_U16_ADDR (1, & tmp7);

  // X := (Y - 1) / (Y + 1),  |X| <= (sqrt2 - 1) / (sqrt2 + 1)  ~  0.172.
  f7_sub (xx, yy, one);
  f7_Iadd (yy, one);
  f7_Idiv (xx, yy);

  // Y := X^2,  |Y| < 0.03.
  f7_square (yy, xx);

  // Y := artanh (X^2) / X
  f7_horner (yy, yy, n_coeff_artanh, coeff_artanh, tmp);

  // C = X * Y = ln A - a_expo * ln2.
  f7_mul (cc, xx, yy);

  // X := a_expo * ln2.
  f7_set_s16 (xx, a_expo);
  f7_Imul (xx, F7_CONST_ADDR (ln2, & tmp7));

  // C = ln A.
  f7_Iadd (cc, xx);

  if (p && USE_LPM)
    f7_Imul (cc, f7_copy_P (tmp, p));
  if (p && USE_LD)
    f7_Imul (cc, p);
}
#endif // F7MOD_logx_


#ifdef F7MOD_exp_

#define ARRAY_NAME coeff_exp
#include "libf7-array.def"
#undef ARRAY_NAME

#define STATIC static
#include "libf7-constdef.h" // ln2_low
#undef STATIC

F7_WEAK
void f7_exp (f7_t *cc, const f7_t *aa)
{
  uint8_t a_class = f7_classify (aa);

  if (f7_class_nan (a_class))
    return f7_set_nan (cc);

  /* The maximal exponent of 2 for a double is 1023, hence we may limit
     to  |A| < 1023 * ln2 ~ 709.  We limit to  1024 ~ 1.99 * 2^9  */

  if (f7_class_inf (a_class)
      || (f7_class_nonzero (a_class) && aa->expo >= 9))
    {
      if (f7_class_sign (a_class))
	return f7_clr (cc);
      else
	return f7_set_inf (cc, 0);
    }

  f7_t const *cst;
  f7_t qq7, *qq = &qq7;

  F7_PCONST (cst, ln2);

  // We limited |A| to 1024 and are now dividing by ln2, hence Q will
  // be at most 1024 / ln2 ~ 1477 and fit into 11 bits.  We will
  // round Q anyway, hence only request 11 bits from the division and
  // one additional bit that might be needed to normalize the quotient.
  f7_divx (qq, aa, cst, 1 + 11);

  // Use the smallest (by absolute value) remainder system.
  f7_round (qq, qq);
  int16_t q = f7_get_s16 (qq);

  // Reducing A mod ln2 gives |C| <= ln2 / 2,  C = -A mod ln2.
  f7_msub (cc, qq, cst, aa);

  // Corrigendum:  We added Q * ln2; now add Q times the low part of ln2
  // for better precision.  Due to |C| < |A| this is not a no-op in general.
  const f7_t *yy = F7_CONST_ADDR (ln2_low, &_var_for_ln2);
  f7_madd (cc, qq, yy, cc);

  // Because we computed C = -A mod ...
  cc->flags ^= F7_FLAG_sign;

  // Reduce further to |C| < ln2 / 8 which is the range of our MiniMax poly.
  const uint8_t MAX_LN2_RED = 3;
  int8_t scal2 = 0;

  while (f7_abscmp_msb_ge (cc, F7_(const_ln2_msb),
			   F7_(const_ln2_expo) - MAX_LN2_RED))
    {
      scal2++;
      cc->expo--;
    }

  f7_horner (cc, cc, n_coeff_exp, coeff_exp, qq);

  while (--scal2 >= 0)
    f7_Isquare (cc);

  f7_Ildexp (cc, q);
}
#endif // F7MOD_exp_


#ifdef F7MOD_pow10_
F7_WEAK
void f7_pow10 (f7_t *cc, const f7_t *aa)
{
  const f7_t *p_ln10;
  F7_PCONST (p_ln10, ln10);
  f7_mul (cc, aa, p_ln10);
  f7_exp (cc, cc);
}
ALIAS (f7_pow10, f7_exp10)
#endif // F7MOD_pow10_


#ifdef F7MOD_cbrt_
F7_WEAK
void f7_cbrt (f7_t *cc, const f7_t *aa)
{
  f7_copy (cc, aa);
  const f7_t *p_1_3;
  uint8_t c_flags = cc->flags;
  cc->flags &= ~F7_FLAG_sign;
  f7_log (cc, cc);
  F7_PCONST (p_1_3, 1_3);
  f7_Imul (cc, p_1_3);
  f7_exp (cc, cc);

  if (c_flags & F7_FLAG_sign)
    cc->flags |= F7_FLAG_sign;
}
#endif // F7MOD_cbrt_


#ifdef F7MOD_pow_
F7_WEAK
void f7_pow (f7_t *cc, const f7_t *aa, const f7_t *bb)
{
#if 0
  f7_t slots[cc == bb];
  f7_t *yy = cc == bb ? slots : cc;
#else
  f7_t yy7, *yy = &yy7;
#endif
  f7_log (yy, aa);
  f7_Imul (yy, bb);
  f7_exp (cc, yy);
}
#endif // F7MOD_pow_


#ifdef F7MOD_powi_
F7_WEAK
void f7_powi (f7_t *cc, const f7_t *aa, int ii)
{
  uint16_t u16 = ii;
  f7_t xx27, *xx2 = &xx27;

  if (ii < 0)
    u16 = -u16;

  f7_copy (xx2, aa);

  f7_set_u16 (cc, 1);

  while (1)
    {
      if (u16 & 1)
	f7_Imul (cc, xx2);

      if (! f7_is_nonzero (cc))
	break;

      u16 >>= 1;
      if (u16 == 0)
	break;
      f7_Isquare (xx2);
    }

  if (ii < 0)
    f7_div1 (xx2, aa);
}
#endif // F7MOD_powi_


#ifdef F7MOD_sincos_

#define ARRAY_NAME coeff_sin
  #define FOR_SIN
  #include "libf7-array.def"
  #undef  FOR_SIN
#undef ARRAY_NAME

#define ARRAY_NAME coeff_cos
  #define FOR_COS
  #include "libf7-array.def"
  #undef  FOR_COS
#undef ARRAY_NAME

#define STATIC static
#include "libf7-constdef.h" // pi_low
#undef STATIC

typedef union
{
  struct
  {
    bool    neg_sin : 1; // Must be bit F7_FLAGNO_sign.
    bool    neg_cos : 1;
    bool    do_sin: 1;
    bool    do_cos: 1;
    bool    swap_sincos : 1;
    uint8_t res : 3;
  };
  uint8_t bits;
} sincos_t;


F7_WEAK
void f7_sincos (f7_t *ss, f7_t *cc, const f7_t *aa)
{
  uint8_t a_class = f7_classify (aa);

  sincos_t sc = { .bits = a_class & F7_FLAG_sign };
  if (ss != NULL) sc.do_sin = 1;
  if (cc != NULL) sc.do_cos = 1;

  if (f7_class_nan (a_class) || f7_class_inf (a_class))
    {
      if (sc.do_sin)  f7_set_nan (ss);
      if (sc.do_cos)  f7_set_nan (cc);
      return;
    }

  f7_t pi7, *pi = &pi7;
  f7_t xx7, *xx = &xx7;
  f7_t yy7, *yy = &yy7;
  f7_t *hh = sc.do_sin ? ss : cc;

  // X = |A|
  f7_copy (xx, aa);
  xx->flags = 0;

  // Y is how often we subtract PI from X.
  f7_clr (yy);
  f7_const (pi, pi);

  if (f7_abscmp_msb_ge (xx, F7_(const_pi_msb), F7_(const_pi_expo) + 1))
    {
      pi->expo = 1 + F7_(const_pi_expo);  // 2*pi

      // Y = X / 2pi.
      f7_div (yy, xx, pi);

      // The integral part of |A| / pi mod 2 is bit 55 - x_expo.
      if (yy->expo >= F7_MANT_BITS && !f7_is_zero (yy))
        {
	  // Too big for sensible calculation:  Should this be NaN instead?
	  if (sc.do_sin)  f7_clr (ss);
	  if (sc.do_cos)  f7_clr (cc);
	  return;
        }

      // X -= 2pi * [ X / 2pi ]
      f7_floor (yy, yy);

      f7_msub (xx, yy, pi, xx);
      xx->flags ^= F7_FLAG_sign;

      // We divided by 2pi, but Y should count times we subtracted pi.
      yy->expo++;
    }

  pi->expo = F7_(const_pi_expo); // pi
  f7_sub (hh, xx, pi);
  if (!f7_signbit (hh))
    {
      // H = X - pi >= 0  =>  X >= pi
      // sin(x) = -sin(x - pi)
      // cos(x) = -cos(x - pi)
      f7_copy (xx, hh);
      // Y: We subtracted pi one more time.
      f7_Iadd (yy, f7_set_u16 (hh, 1));
      sc.neg_sin ^= 1;
      sc.neg_cos ^= 1;
    }

  pi->expo = F7_(const_pi_expo) - 1; // pi/2
  if (f7_gt (xx, pi))
    {
      // x > pi/2
      // sin(x) =  sin(pi - x)
      // cos(x) = -cos(pi - x)
      pi->expo = F7_(const_pi_expo); // pi
      f7_IRsub (xx, pi);
      // Y: We subtracted pi one more time (and then negated).
      f7_Iadd (yy, f7_set_u16 (hh, 1));
      yy->flags ^= F7_FLAG_sign;
      sc.neg_cos ^= 1;
    }

  pi->expo = F7_(const_pi_expo) - 2; // pi/4
  if (f7_gt (xx, pi))
    {
      // x > pi/4
      // sin(x) = cos(pi/2 - x)
      // cos(x) = sin(pi/2 - x)
      pi->expo = F7_(const_pi_expo) - 1; // pi/2
      f7_IRsub (xx, pi);
      // Y: We subtracted pi/2 one more time (and then negated).
      f7_Iadd (yy, f7_set_1pow2 (hh, -1, 0));
      yy->flags ^= F7_FLAG_sign;
      sc.swap_sincos = 1;
    }

  if (!f7_is0 (yy))
    {
      // Y counts how often we subtracted pi from X in order to
      // get 0 <= X < pi/4 as small as possible (Y is 0 mod 0.5).
      // Now also subtract the low part of pi:
      // f7_const_pi_low = pi - f7_const_pi  in order to get more precise
      // results in the cases where the final result is close to 0.
      const f7_t *pi_low = F7_CONST_ADDR (pi_low, pi);
      //f7_const (pi, pi_low);
      f7_Imul (yy, pi_low);
      f7_Isub (xx, yy);
    }

  // X   in [0, pi/4].
  // X^2 in [0, pi^2/16] ~ [0, 0.6169]

  f7_square (yy, xx);

  f7_t *x_sin = xx;
  f7_t *x_cos = yy;

  if ((sc.do_sin && !sc.swap_sincos)
      || (sc.do_cos && sc.swap_sincos))
    {
      f7_horner (hh, yy, n_coeff_sin, coeff_sin, NULL);
      f7_mul (x_sin, hh, xx);
    }

  if ((sc.do_cos && !sc.swap_sincos)
      || (sc.do_sin && sc.swap_sincos))
    {
      f7_horner (x_cos, yy, n_coeff_cos, coeff_cos, hh);
    }

  if (sc.swap_sincos)
    {
      x_sin = yy;
      x_cos = xx;
    }

  if (sc.do_sin)
    {
      x_sin->flags ^= sc.bits;
      x_sin->flags &= F7_FLAG_sign;
      f7_copy (ss, x_sin);
    }

  if (sc.do_cos)
    {
      x_cos->flags = sc.neg_cos;
      f7_copy (cc, x_cos);
    }
}
#endif // F7MOD_sincos_

#ifdef F7MOD_sin_
F7_WEAK
void f7_sin (f7_t *ss, const f7_t *aa)
{
  f7_sincos (ss, NULL, aa);
}
#endif // F7MOD_sin_

#ifdef F7MOD_cos_
F7_WEAK
void f7_cos (f7_t *cc, const f7_t *aa)
{
  f7_sincos (NULL, cc, aa);
}
#endif // F7MOD_cos_


#ifdef F7MOD_tan_
F7_WEAK
void f7_tan (f7_t *tt, const f7_t *aa)
{
  f7_t xcos;
  f7_sincos (tt, & xcos, aa);
  f7_Idiv (tt, & xcos);
}
#endif // F7MOD_tan_


#ifdef F7MOD_cotan_
F7_WEAK
void f7_cotan (f7_t *ct, const f7_t *aa)
{
  f7_t xcos;
  f7_sincos (ct, & xcos, aa);
  f7_div (ct, & xcos, ct);
}
#endif // F7MOD_cotan_


#ifdef F7MOD_sinhcosh_
F7_WEAK
void f7_sinhcosh (f7_t *cc, const f7_t *aa, bool do_sinh)
{
  f7_t xx7, *xx = &xx7;
  // C = exp(A)
  f7_exp (cc, aa);
  // X = exp(-A)
  f7_div (xx, f7_set_u16 (xx, 1), cc);
  // sinh(A) = (exp(A) - exp(-A)) / 2
  // cosh(A) = (exp(A) + exp(-A)) / 2
  f7_addsub (cc, cc, xx, do_sinh);
  cc->expo--;
}
#endif // F7MOD_sinhcosh_


#ifdef F7MOD_sinh_
F7_WEAK
void f7_sinh (f7_t *cc, const f7_t *aa)
{
  f7_sinhcosh (cc, aa, true);
}
#endif // F7MOD_sinh_


#ifdef F7MOD_cosh_
F7_WEAK
void f7_cosh (f7_t *cc, const f7_t *aa)
{
  f7_sinhcosh (cc, aa, false);
}
#endif // F7MOD_cosh_


#ifdef F7MOD_tanh_
F7_WEAK
void f7_tanh (f7_t *cc, const f7_t *aa)
{
  // tanh(A) = (exp(2A) - 1) / (exp(2A) + 1)
  f7_t xx7, *xx = &xx7;
  F7_PCONST_U16 (const f7_t *one, 1);
  // C = 2A
  f7_copy (cc, aa);
  cc->expo++;
  // C = exp(2A)
  f7_exp (cc, cc);
  // X = exp(2A) + 1
  f7_add (xx, cc, one);
  // C = exp(2A) - 1
  f7_Isub (cc, one);
  // C = tanh(A)
  f7_Idiv (cc, xx);
}
#endif // F7MOD_tanh_


#ifdef F7MOD_atan_

#define MINIMAX_6_6_IN_0_1

#define ARRAY_NAME coeff_atan_zahler
#define FOR_NUMERATOR
#include "libf7-array.def"
#undef FOR_NUMERATOR
#undef ARRAY_NAME

#define ARRAY_NAME coeff_atan_nenner
#define FOR_DENOMINATOR
#include "libf7-array.def"
#undef FOR_DENOMINATOR
#undef ARRAY_NAME

#include "libf7-constdef.h"

F7_WEAK
void f7_atan (f7_t *cc, const f7_t *aa)
{
  uint8_t a_class = f7_classify (aa);
  uint8_t flags = a_class & F7_FLAG_sign;

  if (f7_class_nan (a_class))
    return f7_set_nan (cc);

  f7_t yy7, *yy = &yy7;
  f7_t zz7, *zz = &zz7;

  if (f7_class_inf (a_class))
    {
      f7_set_u16 (cc, 0);
      goto do_Inf;
    }

  // C = |A|
  f7_copy (cc, aa);
  cc->flags = 0;

  if (!f7_class_zero (a_class) && cc->expo >= 0)
    {
      // C >= 1:  use  atan (x) + atan (1/x) = pi / 2  to reduce to [0, 1].
      flags |= 1 << 1;
      f7_div (cc, f7_set_u16 (yy, 1), cc);
    }
#if !defined (MINIMAX_6_6_IN_0_1)
  const uint8_t const_a_msb = 0x89;
  const int16_t const_a_expo = -2;
  if (f7_abscmp_msb_ge (cc, const_a_msb, const_a_expo))
    {
      // We have C in [0, 1] and we want to use argument reduction by means
      // of addition theorem  atan(x) - atan(y) = atan((x - y) / (1 + xy)).
      // We want to split [0, 1] into  [0, a] u [a, 1]  in such a way that
      // the upper interval will be mapped to  [-a, a].  The system is easy
      // to solve and yiels
      //    y = 1 / sqrt (3)       ~  0.57735     atan(y) = pi / 6
      //    a = (1 - y) / (1 + y)  ~  0.26795  ~  0x0.8930A2F * 2^-1.
      flags |= 1 << 2;
      // C <- (C - Y) / (1 + C * Y)  in  [-a, a]
      const f7_t *cst = F7_CONST_ADDR (1_sqrt3, zz);
      f7_mul (yy, cc, cst);
      f7_Isub (cc, cst);
      f7_Iadd (yy, F7_U16_ADDR (1, zz));
      f7_Idiv (cc, yy);
    }
#endif
  // C <- C * p(C^2) / q(C^2)
  f7_square (yy, cc);
  f7_horner (zz, yy, n_coeff_atan_zahler, coeff_atan_zahler, NULL);
  f7_Imul (zz, cc);
  f7_horner (cc, yy, n_coeff_atan_nenner, coeff_atan_nenner, NULL);
  f7_div (cc, zz, cc);

#if !defined (MINIMAX_6_6_IN_0_1)
  if (flags & (1 << 2))
    f7_Iadd (cc, F7_CONST_ADDR (pi_6, yy));
#endif

  if (flags & (1 << 1))
    {
    do_Inf:;
      // Y = pi / 2
      f7_const (yy, pi);
      yy->expo = F7_(const_pi_expo) - 1;
      f7_IRsub (cc, yy);
    }

  cc->flags = a_class & F7_FLAG_sign;
}
#undef MINIMAX_6_6_IN_0_1
#endif // F7MOD_atan_


#ifdef F7MOD_asinacos_

#define ARRAY_NAME coeff_func_a_zahler
#define FOR_NUMERATOR
#include "libf7-array.def"
#undef  FOR_NUMERATOR
#undef  ARRAY_NAME

#define ARRAY_NAME coeff_func_a_nenner
#define FOR_DENOMINATOR
#include "libf7-array.def"
#undef  FOR_DENOMINATOR
#undef  ARRAY_NAME

typedef union
{
  struct
  {
    bool    sign : 1;       // Must be bit F7_FLAGNO_sign.
    bool    do_acos : 1;    // From caller.
    bool    have_acos : 1;  // What we compute from rational approx p/q.
    uint8_t res : 5;
  };
  uint8_t bits;
} asinacos_t;

F7_WEAK
void f7_asinacos (f7_t *cc, const f7_t *aa, uint8_t what)
{
  f7_t xx7, *xx = &xx7;
  f7_t xx27, *xx2 = &xx27;

  asinacos_t flags = { .bits = what | f7_signbit (aa) };

  f7_abs (xx, aa);

  int8_t cmp = f7_cmp (xx, f7_set_u16 (cc, 1));

  if (cmp == INT8_MIN
      || cmp > 0)
    {
      return f7_set_nan (cc);
    }

  if (xx->expo <= -2 || f7_is_zero (xx))
    {
      // |A| < 1/2:  asin(x) = x * a(2*x^2)
      f7_square (xx2, xx);
      xx2->expo ++;
    }
  else
    {
      // |A| > 1/2: acos (1-x) = sqrt(2*x) * a(x)
      // C is 1 from above.
      f7_IRsub (xx, cc);
      f7_copy (xx2, xx);
      flags.have_acos = 1;
    }

  // MiniMax [5/4] numerator.
  f7_horner (cc, xx2, n_coeff_func_a_zahler, coeff_func_a_zahler, NULL);

  if (flags.have_acos)
    {
      xx->expo ++;
      f7_Isqrt (xx);
    }
  f7_Imul (cc, xx);

  // MiniMax [5/4] denominator.
  f7_horner (xx, xx2, n_coeff_func_a_nenner, coeff_func_a_nenner, NULL);

  f7_Idiv (cc, xx);

  /*
      With the current value of C, we have:

		|	     |	      do_asin	    |	    do_acos
		|     C	     |	A <= 0	 |  A >= 0  |  A <= 0  |  A >= 0
      ----------+------------+-----------+----------+----------+----------
      have_asin | asin (|A|) |	  -C	 |    C	    | pi/2 + C | pi/2 - C
      have_acos | acos (|A|) | -pi/2 + C | pi/2 - C |  pi - C  |     C

      Result = n_pi2 * pi/2 + C * (c_sign ? -1 : 1)
      Result (A, do_asin) = asin (A)
      Result (A, do_acos) = acos (A)

      with
	  c_sign = do_acos ^ have_acos ^ a_sign
	  n_pi2	 = do_acos + have_acos * (a_sign ^ do_acos) ? (-1 : 1)
	  n_pi2 in { -1, 0, 1, 2 }
  */

  // All what matters for c_sign is bit 0.
  uint8_t c_sign = flags.bits;
  int8_t n_pi2 = flags.do_acos;
  c_sign ^= flags.do_acos;
  if (flags.have_acos)
    {
      n_pi2++;
      __asm ("" : "+r" (n_pi2));
      if (c_sign & 1)  // c_sign & 1  =  a_sign ^ do_acos
	n_pi2 -= 2;
      c_sign++;
    }

  cc->flags = c_sign & F7_FLAG_sign;

  if (n_pi2)
    {
      f7_const (xx, pi);
      if (n_pi2 < 0)
	xx->sign = 1;
      if (n_pi2 != 2)
	xx->expo = F7_(const_pi_expo) - 1;

      f7_Iadd (cc, xx);
    }
}
#endif // F7MOD_asinacos_


#ifdef F7MOD_asin_
F7_WEAK
void f7_asin (f7_t *cc, const f7_t *aa)
{
  f7_asinacos (cc, aa, 0);
}
#endif // F7MOD_asin_


#ifdef F7MOD_acos_
F7_WEAK
void f7_acos (f7_t *cc, const f7_t *aa)
{
  f7_asinacos (cc, aa, 1 << 1);
}
#endif // F7MOD_acos_


#ifndef IN_LIBGCC2

#ifdef F7MOD_put_C_

#include <stdio.h>
#include <avr/pgmspace.h>

static F7_INLINE
uint8_t f7_hex_digit (uint8_t nibble)
{
  nibble = (uint8_t) (nibble + '0');
  if (nibble > '9')
    nibble = (uint8_t) (nibble + ('a' - '0' - 10));
  return nibble;
}

static void f7_put_hex2 (uint8_t x, FILE *stream)
{
  putc ('0', stream);
  if (x)
    {
      putc ('x', stream);
      putc (f7_hex_digit (x >> 4), stream);
      putc (f7_hex_digit (x & 0xf), stream);
    }
}

#define XPUT(str) \
  fputs_P (PSTR (str), stream)

// Write to STREAM a line that is appropriate for usage in libf7-const.def.

F7_WEAK
void f7_put_CDEF (const char *name, const f7_t *aa, FILE *stream)
{
  char buf[7];
  XPUT ("F7_CONST_DEF (");
  fputs (name, stream);
  XPUT (",\t");
  uint8_t a_class = f7_classify (aa);
  if (! f7_class_nonzero (a_class))
    {
      f7_put_hex2 (a_class & F7_FLAGS, stream);
      XPUT (",\t0,0,0,0,0,0,0,\t0)");
      return;
    }
  putc ('0' + (a_class & F7_FLAGS), stream);
  XPUT (",\t");

  for (uint8_t i = 0; i < F7_MANT_BYTES; i++)
    {
      f7_put_hex2 (aa->mant[F7_MANT_BYTES-1 - i], stream);
      putc (',', stream);
    }
  putc ('\t', stream);

  itoa (aa->expo, buf, 10);
  fputs (buf, stream);
  XPUT (")");
}

void f7_put_C (const f7_t *aa, FILE *stream)
{
  char buf[7];

  uint8_t a_class = f7_classify (aa);
  if (f7_class_nan (a_class))
    {
      XPUT ("{ .is_nan = 1 }");
      return;
    }
  bool sign = a_class & F7_FLAG_sign;

  if (f7_class_inf (a_class))
    {
      XPUT ("{ .is_nan = 1, .sign = ");
      putc ('0' + sign, stream);
      XPUT (" }");
      return;
    }

  XPUT ("{ .sign = ");
  putc ('0' + sign, stream);

  XPUT (", .mant = { ");
  for (uint8_t i = 0; i < F7_MANT_BYTES; i++)
    {
      f7_put_hex2 (aa->mant[F7_MANT_BYTES-1 - i], stream);
      if (i != F7_MANT_BYTES - 1)
	putc (',', stream);
    }

  XPUT (" }, .expo = ");
  itoa (aa->expo, buf, 10);
  fputs (buf, stream);
  XPUT (" }");
}
#endif //F7MOD_put_C_


#ifdef F7MOD_dump_

#include <avr/pgmspace.h>

#ifndef AVRTEST_H

#include <stdio.h>

static void LOG_PSTR (const char *str)
{
  fputs_P (str, stdout);
}

static void LOG_PFMT_U16 (const char *fmt, uint16_t x)
{
  printf_P (fmt, x);
}

static void LOG_PFMT_FLOAT (const char *fmt, float x)
{
  printf_P (fmt, x);
}

#define LOG_X8(X)               LOG_PFMT_U16 (PSTR (" 0x%02x "), (uint8_t)(X))
#define LOG_PFMT_S16(FMT, X)    LOG_PFMT_U16 (FMT, (unsigned)(X))
#define LOG_PFMT_ADDR(FMT, X)   LOG_PFMT_U16 (FMT, (unsigned)(X))

#endif // AVRTEST_H

static void dump_byte (uint8_t b)
{
  LOG_PSTR (PSTR (" "));
  for (uint8_t i = 0; i < 8; i++)
    {
      LOG_PSTR ((b & 0x80) ? PSTR ("1") : PSTR ("0"));
      b = (uint8_t) (b << 1);
    }
}

void f7_dump_mant (const f7_t *aa)
{
  LOG_PSTR (PSTR ("\tmant   ="));
  for (int i = F7_MANT_BYTES - 1; i >= 0; i--)
    LOG_X8 (aa->mant[i]);
  LOG_PSTR (PSTR ("\n\t       ="));

  for (int i = F7_MANT_BYTES - 1; i >= 0; i--)
    dump_byte (aa->mant[i]);
  LOG_PSTR (PSTR ("\n"));
}

void f7_dump (const f7_t *aa)
{
  LOG_PFMT_ADDR (PSTR ("\n0x%04x\tflags  = "), aa);
  dump_byte (aa->flags);
  uint8_t a_class = f7_classify (aa);
  LOG_PSTR (PSTR ("  = "));
  LOG_PSTR (f7_class_sign (a_class) ? PSTR ("-") : PSTR ("+"));
  if (f7_class_inf (a_class))    LOG_PSTR (PSTR ("Inf "));
  if (f7_class_nan (a_class))    LOG_PSTR (PSTR ("NaN "));
  if (f7_class_zero (a_class))   LOG_PSTR (PSTR ("0 "));
  if (f7_class_number (a_class)) LOG_PSTR (PSTR ("Number "));

  LOG_PFMT_FLOAT (PSTR (" = %.10g\n"), f7_get_float (aa));
  LOG_PFMT_S16 (PSTR ("\texpo   = %d\n"), aa->expo);

  f7_dump_mant (aa);
}
#endif // F7MOD_dump_

#endif // ! libgcc

#endif // !AVR_TINY
