/* Copyright (C) 2013-2021 Free Software Foundation, Inc.

   This file is part of GCC.

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


/* This file supplies implementations for some AVR-specific builtin
   functions so that code like the following works as expected:

   int (*f (void))(_Fract)
   {
       return __builtin_avr_countlsr;
   }

   In this specific case, the generated code is:

   f:
       ldi r24,lo8(gs(__countlsHI))
       ldi r25,hi8(gs(__countlsHI))
       ret
*/

/* Map fixed-point suffix to the corresponding fixed-point type.  */

typedef short _Fract fx_hr_t;
typedef _Fract fx_r_t;
typedef long _Fract fx_lr_t;
typedef long long _Fract fx_llr_t;

typedef unsigned short _Fract fx_uhr_t;
typedef unsigned _Fract fx_ur_t;
typedef unsigned long _Fract fx_ulr_t;
typedef unsigned long long _Fract fx_ullr_t;

typedef short _Accum fx_hk_t;
typedef _Accum fx_k_t;
typedef long _Accum fx_lk_t;
typedef long long _Accum fx_llk_t;

typedef unsigned short _Accum fx_uhk_t;
typedef unsigned _Accum fx_uk_t;
typedef unsigned long _Accum fx_ulk_t;
typedef unsigned long long _Accum fx_ullk_t;

/* Map fixed-point suffix to the corresponding natural integer type.  */

typedef char int_hr_t;
typedef int int_r_t;
typedef long int_lr_t;
typedef long long int_llr_t;

typedef unsigned char int_uhr_t;
typedef unsigned int int_ur_t;
typedef unsigned long int_ulr_t;
typedef unsigned long long int_ullr_t;

typedef int int_hk_t;
typedef long int_k_t;
typedef long long int_lk_t;
typedef long long int_llk_t;

typedef unsigned int int_uhk_t;
typedef unsigned long int_uk_t;
typedef unsigned long long int_ulk_t;
typedef unsigned long long int_ullk_t;

/* Map mode to the corresponding integer type.  */

typedef char int_qi_t;
typedef int int_hi_t;
typedef long int_si_t;
typedef long long int_di_t;

typedef unsigned char uint_qi_t;
typedef unsigned int uint_hi_t;
typedef unsigned long uint_si_t;
typedef unsigned long long uint_di_t;



/************************************************************************/

/* Supply implementations / symbols for __builtin_roundFX ASM_NAME.  */

#ifdef L_round

#define ROUND1(FX)                              \
  ROUND2 (FX)

#define ROUND2(FX)                                                      \
  extern fx_## FX ##_t __round## FX (fx_## FX ##_t x, int rpoint);      \
                                                                        \
  fx_## FX ##_t                                                         \
  __round## FX (fx_## FX ##_t x, int rpoint)                            \
  {                                                                     \
    return __builtin_avr_round ##FX (x, rpoint);                        \
  }

ROUND1(L_LABEL)

#endif /* L_round */



/*********************************************************************/

/* Implement some count-leading-redundant-sign-bits to be used with
   coundlsFX implementation.  */

#ifdef L__clrsbqi
extern int __clrsbqi2 (char x);

int
__clrsbqi2 (char x)
{
  int ret;

  if (x < 0)
    x = ~x;

  if (x == 0)
    return 8 * sizeof (x) -1;

  ret = __builtin_clz (x << 8);
  return ret - 1;
}
#endif /* L__clrsbqi */


#ifdef L__clrsbdi
extern int __clrsbdi2 (long long x);

int
__clrsbdi2 (long long x)
{
  int ret;

  if (x < 0LL)
    x = ~x;

  if (x == 0LL)
    return 8 * sizeof (x) -1;

  ret = __builtin_clzll ((unsigned long long) x);
  return ret - 1;
}
#endif /* L__clrsbdi */



/*********************************************************************/

/* Supply implementations / symbols for __builtin_avr_countlsFX.  */

/* Signed */

#ifdef L_countls

#define COUNTLS1(MM)                            \
  COUNTLS2 (MM)

#define COUNTLS2(MM)                                                    \
  extern int __countls## MM ##2 (int_## MM ##_t);                       \
  extern int __clrsb## MM ##2 (int_## MM ##_t);                         \
                                                                        \
  int                                                                   \
  __countls## MM ##2 (int_## MM ##_t x)                                 \
  {                                                                     \
    if (x == 0)                                                         \
      return __INT8_MAX__;                                              \
                                                                        \
    return __clrsb## MM ##2 (x);                                        \
  }

COUNTLS1(L_LABEL)

#endif /* L_countls */

/* Unsigned */

#ifdef L_countlsu

#define clz_qi2 __builtin_clz /* unused, avoid warning */
#define clz_hi2 __builtin_clz
#define clz_si2 __builtin_clzl
#define clz_di2 __builtin_clzll

#define COUNTLS1(MM)                            \
  COUNTLS2 (MM)

#define COUNTLS2(MM)                                                    \
  extern int __countlsu## MM ##2 (uint_## MM ##_t);                     \
                                                                        \
  int                                                                   \
  __countlsu## MM ##2 (uint_## MM ##_t x)                               \
  {                                                                     \
    if (x == 0)                                                         \
      return __INT8_MAX__;                                              \
                                                                        \
    if (sizeof (x) == 1)                                                \
      return clz_hi2 (x << 8);                                          \
    else                                                                \
      return clz_## MM ##2 (x);                                         \
  }

COUNTLS1(L_LABEL)

#endif /* L_countlsu */
