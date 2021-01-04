/* Copyright (C) 2019-2021 Free Software Foundation, Inc.

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

#ifndef LIBF7_H
#define LIBF7_H
#define IN_LIBF7_H

#include "f7-renames.h"

#define F7_MANT_BYTES 7
#define F7_MANT_BITS (8 * F7_MANT_BYTES)

/*  Using the following GCC features:
    --  Unnamed structs / unions (GNU-C)
    --  Fixed-point types (GNU-C)
    --  Inline asm
    --  Setting assembler names by means of __asm (GNU-C).
    --  Attributes: alias, always_inline, const, noinline, unused,
                    progmem, pure, weak, warning
    --  GCC built-ins: __builtin_abort, __builtin_constant_p
    --  AVR built-ins: __builtin_avr_bitsr, __builtin_avr_rbits
*/

/* We have 2 kinds of flags:

   A)  The flags that are stored in f7_t.flags:
       --  f7_t.is_nan (NaN)
       --  f7_t.is_inf (+Inf or -Inf)
       --  f7_t.sign (negative or -Inf).

   B)  The flags that are returned by f7_classify().  This are the
       flags from A) together with
       --  _zero: indicate that a number is zero.
*/

#define F7_FLAGNO_sign  0
#define F7_FLAGNO_zero  1
#define F7_FLAGNO_nan   2
#define F7_FLAGNO_inf   7

#define F7_HAVE_Inf 1

// Flags that might be set by f7_classify().
#define F7_FLAG_sign            (1 << F7_FLAGNO_sign)
#define F7_FLAG_zero            (1 << F7_FLAGNO_zero)
#define F7_FLAG_nan             (1 << F7_FLAGNO_nan)
#define F7_FLAG_inf   (F7_HAVE_Inf << F7_FLAGNO_inf)

// Flags that might be set in f7_t.flags.
#define F7_FLAGS (F7_FLAG_inf | F7_FLAG_nan | F7_FLAG_sign)

#if !defined __ASSEMBLER__

#ifndef IN_LIBGCC2
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#else
/* Do not assume that we have std headers when we build libgcc.  */

typedef __UINT64_TYPE__ uint64_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT8_TYPE__  uint8_t;
typedef __INT64_TYPE__ int64_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT8_TYPE__  int8_t;
typedef _Bool bool;
#define false 0
#define true  1
#define INT8_MIN  (-1 - __INT8_MAX__)
#define INT16_MAX __INT16_MAX__
#define NULL ((void*) 0)
#endif /* IN_LIBGCC2 */

#include "asm-defs.h"

#ifdef __cplusplus
extern "C" {
#define _Static_assert(X, Y) static_assert (X)
#endif // C++

#define F7_INLINE   inline __attribute__((__always_inline__))
#define F7_NOINLINE __attribute__((__noinline__))
#define F7_WEAK     __attribute__((__weak__))
#define F7_PURE     __attribute__((__pure__))
#define F7_UNUSED   __attribute__((__unused__))
#define F7_CONST    __attribute__((__const__))

#define F7_STRINGY2(X)  #X
#define F7_STRINGY(X)   F7_STRINGY2(X)
#define F7ASM(X)        __asm (F7_STRINGY2(X))

typedef struct f7_t
{
  union
  {
    struct
    {
      uint8_t sign        :1;
      uint8_t reserved1   :1;
      uint8_t is_nan      :1;
      uint8_t reserved2   :4;
      uint8_t is_inf      :1;
    };
    uint8_t flags;
  };

  uint8_t mant[7];
  int16_t expo;
} f7_t;

typedef uint64_t f7_double_t;

#define F7_MANT_HI4(X) \
  (*(uint32_t*) & (X)->mant[F7_MANT_BYTES - 4])

#define F7_MANT_CONST_HI4(X) \
  (*(const uint32_t*) & (X)->mant[F7_MANT_BYTES - 4])

#define F7_MANT_HI2(X) \
  (*(uint16_t*) & (X)->mant[F7_MANT_BYTES - 2])

static F7_INLINE F7_PURE
uint8_t f7_classify (const f7_t *aa)
{
  extern void f7_classify_asm (void);
  register uint8_t rclass __asm ("r24");
  __asm ("%~call %x[f]"
	 : "=r" (rclass)
	 : [f] "i" (f7_classify_asm), "z" (aa));
  return rclass;
}


// +Inf or -Inf
static F7_INLINE
bool f7_class_inf (uint8_t c)
{
#if defined (F7_HAVE_Inf) && F7_HAVE_Inf == 1
  return c >= F7_FLAG_inf;
#elif defined (F7_HAVE_Inf) && F7_HAVE_Inf == 0
  (void) c;
  return false;
#else
#error macro F7_HAVE_Inf must be defined to 0 or to 1.
#endif // Have Inf
}

static F7_INLINE
bool f7_is_inf (const f7_t *aa)
{
  return f7_class_inf (aa->flags);
}

// Not-a-Number (NaN).
static F7_INLINE
bool f7_class_nan (uint8_t c)
{
  return c & F7_FLAG_nan;
}

static F7_INLINE
bool f7_is_nan (const f7_t *aa)
{
  return f7_class_nan (aa->flags);
}

// Some number
static F7_INLINE
bool f7_class_number (uint8_t c)
{
  return c <= (F7_FLAG_sign | F7_FLAG_zero);
}

static F7_INLINE
bool f7_is_number (const f7_t *aa)
{
  return f7_class_number (f7_classify (aa));
}

// Zero
static F7_INLINE
bool f7_class_zero (uint8_t c)
{
  return c & F7_FLAG_zero;
}

static F7_INLINE
bool f7_is_zero (const f7_t *aa)
{
  return f7_class_zero (f7_classify (aa));
}

// A non-zero number.
static F7_INLINE
bool f7_class_nonzero (uint8_t c)
{
  return c <= F7_FLAG_sign;
}

static F7_INLINE
bool f7_is_nonzero (const f7_t *aa)
{
  return f7_class_nonzero (f7_classify (aa));
}

static F7_INLINE
bool f7_class_sign (uint8_t c)
{
  return c & F7_FLAG_sign;
}

static F7_INLINE
bool f7_signbit (const f7_t *aa)
{
  return aa->flags & F7_FLAG_sign;
}

static F7_INLINE
void f7_set_sign (f7_t *cc, bool sign)
{
  _Static_assert (F7_FLAGNO_sign == 0, "");
  cc->flags &= ~F7_FLAG_sign;
  cc->flags |= sign;
}

static F7_INLINE
void f7_set_nan (f7_t *cc)
{
  cc->flags = F7_FLAG_nan;
}

static F7_INLINE
void f7_clr (f7_t *cc)
{
  extern void f7_clr_asm (void);
  __asm ("%~call %x[f]"
	 :
	 : [f] "i" (f7_clr_asm), "z" (cc)
	 : "memory");
}

static F7_INLINE
f7_t* f7_copy (f7_t *cc, const f7_t *aa)
{
  extern void f7_copy_asm (void);
  __asm ("%~call %x[f]"
	 :
	 : [f] "i" (f7_copy_asm), "z" (cc), "x" (aa)
	 : "memory");
  return cc;
}

static F7_INLINE
f7_t* f7_copy_P (f7_t *cc, const f7_t *aa)
{
  extern void f7_copy_P_asm (void);
  __asm ("%~call %x[f]"
	 :
	 : [f] "i" (f7_copy_P_asm), "x" (cc), "z" (aa)
	 : "memory");
  return cc;
}

static F7_INLINE
void f7_copy_mant (f7_t *cc, const f7_t *aa)
{
  extern void f7_copy_mant_asm (void);
  __asm ("%~call %x[f]"
	 :
	 : [f] "i" (f7_copy_mant_asm), "z" (cc), "x" (aa)
	 : "memory");
}

static F7_INLINE
void f7_set_inf (f7_t *cc, bool sign)
{
#if F7_HAVE_Inf == 1
  cc->flags = F7_FLAG_inf | sign;
#else
  (void) sign;
  cc->flags = F7_FLAG_nan;
#endif // Have Inf
}


static F7_INLINE
bool f7_msbit (const f7_t *aa)
{
  return aa->mant[F7_MANT_BYTES - 1] & 0x80;
}

// Quick test against 0 if A is known to be a number (neither NaN nor Inf).
static F7_INLINE
bool f7_is0 (const f7_t *aa)
{
  return 0 == f7_msbit (aa);
}


static F7_INLINE
int8_t f7_cmp_mant (const f7_t *aa, const f7_t *bb)
{
  extern void f7_cmp_mant_asm (void);
  register int8_t r24 __asm ("r24");
  __asm ("%~call %x[f] ;; %1 %3"
	 : "=r" (r24)
	 : [f] "i" (f7_cmp_mant_asm), "x" (aa), "z" (bb));
  return r24;
}

static F7_INLINE
bool f7_store_expo (f7_t *cc, int16_t expo)
{
  extern void f7_store_expo_asm (void);
  register bool r24 __asm ("r24");
  register int16_t rexpo __asm ("r24") = expo;
  __asm ("%~call %x[f] ;; %0 %2 %3"
	 : "=r" (r24)
	 : [f] "i" (f7_store_expo_asm), "z" (cc), "r" (rexpo));
  return r24;
}

static F7_INLINE
f7_t* f7_abs (f7_t *cc, const f7_t *aa)
{
  f7_copy (cc, aa);
  f7_set_sign (cc, 0);

  return cc;
}


F7_PURE extern int8_t f7_cmp (const f7_t*, const f7_t*);
F7_PURE extern bool f7_lt_impl (const f7_t*, const f7_t*);
F7_PURE extern bool f7_le_impl (const f7_t*, const f7_t*);
F7_PURE extern bool f7_gt_impl (const f7_t*, const f7_t*);
F7_PURE extern bool f7_ge_impl (const f7_t*, const f7_t*);
F7_PURE extern bool f7_ne_impl (const f7_t*, const f7_t*);
F7_PURE extern bool f7_eq_impl (const f7_t*, const f7_t*);
F7_PURE extern bool f7_unord_impl (const f7_t*, const f7_t*);

static F7_INLINE
bool f7_lt (const f7_t *aa, const f7_t *bb)
{
  return 2 & f7_cmp (aa, bb);
}

static F7_INLINE
bool f7_gt (const f7_t *aa, const f7_t *bb)
{
  return 1 == f7_cmp (aa, bb);
}

static F7_INLINE
bool f7_le (const f7_t *aa, const f7_t *bb)
{
  int8_t c = f7_cmp (aa, bb);
  return (uint8_t) (c + 1) <= 1;
}

static F7_INLINE
bool f7_ge (const f7_t *aa, const f7_t *bb)
{
  return f7_cmp (aa, bb) >= 0;
}

static F7_INLINE
bool f7_unordered (const f7_t *aa, const f7_t *bb)
{
  return INT8_MIN == f7_cmp (aa, bb);
}

static F7_INLINE
bool f7_ordered (const f7_t *aa, const f7_t *bb)
{
  return INT8_MIN != f7_cmp (aa, bb);
}

static F7_INLINE
bool f7_eq (const f7_t *aa, const f7_t *bb)
{
  return 0 == f7_cmp (aa, bb);
}

static F7_INLINE
bool f7_ne (const f7_t *aa, const f7_t *bb)
{
  return 1 & f7_cmp (aa, bb);
}

extern void f7_clr (f7_t*);

__attribute__((warning ("foo_u16"))) void foo_u16 (void);
__attribute__((warning ("foo_s16"))) void foo_s16 (void);

extern f7_t* f7_set_s16_impl (f7_t*, int16_t);
extern f7_t* f7_set_u16_impl (f7_t*, uint16_t);

static F7_INLINE
f7_t* f7_set_u16_worker (f7_t *cc, uint16_t u16)
{
  if (__builtin_constant_p (u16))
    {
      if (u16 == 0)
	return cc;

      uint8_t off = __builtin_clz (u16);
      if (15 - off)
	* (uint8_t*) & cc->expo = (uint8_t) (15 - off);
      u16 <<= off;
      if (u16 & 0xff)
	cc->mant[5] = (uint8_t) u16;
      if (u16 & 0xff00)
	cc->mant[6] = (uint8_t) (u16 >> 8);

      return cc;
    }
  else
    {
      foo_u16();
      __builtin_abort();
      return NULL;
    }
}

static F7_INLINE
f7_t* f7_set_u16 (f7_t *cc, uint16_t u16)
{
  if (__builtin_constant_p (u16))
    {
      f7_clr (cc);
      return f7_set_u16_worker (cc, u16);
    }

  return f7_set_u16_impl (cc, u16);
}

static F7_INLINE
f7_t* f7_set_s16 (f7_t *cc, int16_t s16)
{
  if (__builtin_constant_p (s16))
    {
      f7_clr (cc);

      uint16_t u16 = (uint16_t) s16;

      if (s16 < 0)
        {
	  u16 = -u16;
	  cc->flags = F7_FLAG_sign;
        }

      return f7_set_u16_worker (cc, u16);
    }

  return f7_set_s16_impl (cc, s16);
}

static F7_INLINE
void f7_set_eps (f7_t *cc, uint8_t eps, bool sign)
{
  cc = f7_set_u16 (cc, 1);
  if (!__builtin_constant_p (sign) || sign)
    cc->flags = sign;
  cc->mant[0] = eps;
}

static F7_INLINE
f7_t* f7_set_1pow2 (f7_t *cc, int16_t expo, bool sign)
{
  cc = f7_set_u16 (cc, 1);
  cc->expo = expo;
  if (!__builtin_constant_p (sign) || sign)
    cc->flags = sign;
  return cc;
}

static F7_INLINE
f7_t* f7_set_u64 (f7_t *cc, uint64_t u64)
{
  extern f7_t* f7_set_u64_asm (uint64_t, f7_t*);
  return f7_set_u64_asm (u64, cc);
}

static F7_INLINE
f7_t* f7_set_s64 (f7_t *cc, int64_t s64)
{
  extern f7_t* f7_set_s64_asm (int64_t, f7_t*);
  return f7_set_s64_asm (s64, cc);
}

extern void f7_set_double_impl (f7_double_t, f7_t*);
static F7_INLINE
void f7_set_double (f7_t *cc, f7_double_t val64)
{
  f7_set_double_impl (val64, cc);
}

extern f7_t* f7_init_impl (uint64_t, uint8_t, f7_t*, int16_t);

static F7_INLINE
f7_t* f7_init (f7_t *cc, uint8_t flags, uint64_t mant, int16_t expo)
{
  return f7_init_impl (mant, flags, cc, expo);
}

extern f7_t* f7_set_s32 (f7_t*, int32_t);
extern f7_t* f7_set_u16 (f7_t*, uint16_t);
extern f7_t* f7_set_u32 (f7_t*, uint32_t);
extern void f7_set_float (f7_t*, float);
extern void f7_set_pdouble (f7_t*, const f7_double_t*);

F7_PURE extern int16_t f7_get_s16 (const f7_t*);
F7_PURE extern int32_t f7_get_s32 (const f7_t*);
F7_PURE extern int64_t f7_get_s64 (const f7_t*);
F7_PURE extern uint16_t f7_get_u16 (const f7_t*);
F7_PURE extern uint32_t f7_get_u32 (const f7_t*);
F7_PURE extern uint64_t f7_get_u64 (const f7_t*);
F7_PURE extern float f7_get_float (const f7_t*);
F7_PURE extern f7_double_t f7_get_double (const f7_t*);

#if USE_LPM == 1
  #define F7_PGMSPACE     __attribute__((__progmem__))
  #define f7_copy_flash   f7_copy_P

  #define f7_const(X, NAME) \
    f7_copy_P ((X), & F7_(const_ ## NAME ## _P))

  #define F7_CONST_DEF(NAME, FLAGS, M0, M1, M2, M3, M4, M5, M6, EXPO) \
    extern const f7_t F7_(const_ ## NAME ## _P);
  #include "libf7-const.def"
  #undef F7_CONST_DEF
#else
  #define F7_PGMSPACE     // Empty
  #define f7_copy_flash   f7_copy

  #define f7_const(X, NAME) \
    f7_copy ((X), & F7_(const_ ## NAME))

  #define F7_CONST_DEF(NAME, FLAGS, M0, M1, M2, M3, M4, M5, M6, EXPO) \
    extern const f7_t F7_(const_ ## NAME);
  #include "libf7-const.def"
  #undef F7_CONST_DEF
#endif // USE_LPM


// Basic floating point arithmetic:
// double output <=> f7_t*
// double input  <=> const f7_t*
extern f7_t* f7_neg (f7_t*, const f7_t*);
extern void f7_add (f7_t*, const f7_t*, const f7_t*);
extern void f7_sub (f7_t*, const f7_t*, const f7_t*);
extern void f7_mul (f7_t*, const f7_t*, const f7_t*);
extern void f7_div (f7_t*, const f7_t*, const f7_t*);

// Analogies of functions from math.h:
// double output <=> f7_t*
// double input  <=> const f7_t*
extern void f7_fabs (f7_t*, const f7_t*);
extern void f7_fmod (f7_t*, const f7_t*, const f7_t*);
extern void f7_frexp (f7_t*, const f7_t*, int*);
extern void f7_exp (f7_t*, const f7_t*);
extern void f7_log (f7_t*, const f7_t*);
extern void f7_pow (f7_t*, const f7_t*, const f7_t*);
extern void f7_sqrt (f7_t*, const f7_t*);
extern void f7_cbrt (f7_t*, const f7_t*);
extern void f7_hypot (f7_t*, const f7_t*, const f7_t*);
extern f7_t* f7_ldexp (f7_t*, const f7_t*, int);
extern f7_t* f7_fmax (f7_t*, const f7_t*, const f7_t*);
extern f7_t* f7_fmin (f7_t*, const f7_t*, const f7_t*);
extern f7_t* f7_trunc (f7_t*, const f7_t*);
extern f7_t* f7_floor (f7_t*, const f7_t*);
extern void f7_ceil (f7_t*, const f7_t*);
extern void f7_round (f7_t*, const f7_t*);
extern void f7_sin (f7_t*, const f7_t*);
extern void f7_cos (f7_t*, const f7_t*);
extern void f7_tan (f7_t*, const f7_t*);
extern void f7_atan (f7_t*, const f7_t*);
extern void f7_asin (f7_t*, const f7_t*);
extern void f7_acos (f7_t*, const f7_t*);
extern void f7_tanh (f7_t*, const f7_t*);
extern void f7_sinh (f7_t*, const f7_t*);
extern void f7_cosh (f7_t*, const f7_t*);
extern void f7_log2 (f7_t*, const f7_t*);
extern void f7_log10 (f7_t*, const f7_t*);
extern void f7_exp10 (f7_t*, const f7_t*);
extern void f7_pow10 (f7_t*, const f7_t*);

// Just prototypes, not implemented yet.
extern void f7_atan2 (f7_t*, const f7_t*, const f7_t*);
extern long f7_lrint (const f7_t*);
extern long f7_lround (const f7_t*);

// Helper functions, aliases, convenience.
extern void f7_div1 (f7_t*, const f7_t*);
extern void f7_square (f7_t*, const f7_t*);

extern void f7_powi (f7_t*, const f7_t*, int);
extern f7_t* f7_max (f7_t*, const f7_t*, const f7_t*);
extern f7_t* f7_min (f7_t*, const f7_t*, const f7_t*);
extern f7_t* f7_truncx (f7_t*, const f7_t*, bool);
extern void f7_cotan (f7_t*, const f7_t*);
extern void f7_sincos (f7_t*, f7_t*, const f7_t*);
extern void f7_asinacos (f7_t*, const f7_t*, uint8_t);
extern void f7_sinhcosh (f7_t*, const f7_t*, bool);

extern void f7_horner (f7_t*, const f7_t*, uint8_t, const f7_t *coeff, f7_t*);
extern void f7_mul_noround (f7_t*, const f7_t*, const f7_t*);
extern void f7_clr_mant_lsbs (f7_t*, const f7_t*, uint8_t) F7ASM(f7_clr_mant_lsbs_asm);

F7_PURE extern int8_t f7_cmp_unordered (const f7_t*, const f7_t*, bool);
F7_PURE extern int8_t f7_cmp_abs (const f7_t*, const f7_t*);

F7_PURE extern bool f7_abscmp_msb_ge (const f7_t*, uint8_t msb, int16_t expo);
extern void f7_addsub (f7_t*, const f7_t*, const f7_t*, bool neg_b);
extern void f7_madd_msub (f7_t*, const f7_t*, const f7_t*, const f7_t*, bool);
extern void f7_madd (f7_t*, const f7_t*, const f7_t*, const f7_t*);
extern void f7_msub (f7_t*, const f7_t*, const f7_t*, const f7_t*);
extern uint8_t f7_mulx (f7_t*, const f7_t*, const f7_t*, bool);
extern void f7_divx (f7_t*, const f7_t*, const f7_t*, uint8_t);
extern void f7_logx (f7_t*, const f7_t*, const f7_t*);
extern f7_t* f7_minmax (f7_t*, const f7_t*, const f7_t*, bool);

// Idem:
//    f7_Ifunc (y)    = f7_func (y, y)
//    f7_Ifunc (y, x) = f7_func (y, y, x)
extern void f7_Iadd (f7_t*, const f7_t*);
extern void f7_Isub (f7_t*, const f7_t*);
extern void f7_Imul (f7_t*, const f7_t*);
extern void f7_Idiv (f7_t*, const f7_t*);
extern void f7_IRsub (f7_t*, const f7_t*);
extern void f7_Ineg (f7_t*);
extern void f7_Isqrt (f7_t*);
extern void f7_Isquare (f7_t*);
extern f7_t* f7_Ildexp (f7_t*, int);

// Protoypes for some functions from libf7-asm.sx.
F7_CONST extern uint16_t f7_sqrt16_round (uint16_t) F7ASM(f7_sqrt16_round_asm);
F7_CONST extern uint8_t  f7_sqrt16_floor (uint16_t) F7ASM(f7_sqrt16_floor_asm);
extern void f7_addsub_mant_scaled_asm (f7_t*, const f7_t*, const f7_t*, uint8_t);
extern uint8_t f7_mul_mant_asm (f7_t*, const f7_t*, const f7_t*, uint8_t);
extern void f7_sqrt_approx_asm (f7_t*, const f7_t*);
extern uint64_t f7_lshrdi3 (uint64_t, uint8_t) F7ASM(f7_lshrdi3_asm);
extern uint64_t f7_ashldi3 (uint64_t, uint8_t) F7ASM(f7_ashldi3_asm);
// Normalize a non-Inf, non-NaN value.  Sets .sign to 0.
extern f7_t* f7_normalize_asm (f7_t*);

// Dumping.
#ifndef IN_LIBGCC2
extern void f7_dump (const f7_t*);
extern void f7_dump_mant (const f7_t*);
extern void f7_put_C (const f7_t*, FILE*);
extern void f7_put_CDEF (const char *name, const f7_t*, FILE*);
#endif /* IN_LIBGCC2 */

#ifdef __cplusplus
} // extern "C"
#include "libf7-class.h"
#endif // C++

#endif /* __ASSEMBLER__ */
#undef IN_LIBF7_H
#endif /* LIBF7_H */
