/* Stub for defining degree-valued trigonometric functions in libgfortran.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargl@gcc.gnu.org>
   and Fritz Reese <foreese@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
This replaces all GMP/MPFR functions used by trigd.inc with native versions.
The precision is defined by FTYPE defined before including this file.
The module which includes this file must define the following:

FTYPE             -- floating point type
SIND, COSD, TAND  -- names of the functions to define
SUFFIX(x)         -- add a literal suffix for floating point constants (f, ...)

COSD_SMALL [optional] -- for x <= COSD_SMALL, COSD(x) = 1 if set
TINY       [optional] -- subtract from 1 under the above condition if set
SIND_SMALL [optional] -- for x <= SIND_SMALL, SIND(x) = D2R(x) if set
COSD30                -- literal value of COSD(30) to the precision of FTYPE
PIO180H               -- upper bits of pi/180 for FMA
PIO180L               -- lower bits of pi/180 for FMA

 */

#define ITYPE int
#define GFC_RND_MODE 0
#define RETTYPE FTYPE
#define RETURN(x) return (x)

#define ISFINITE(x) isfinite(x)
#define mpfr_init(x) do { } while (0)
#define mpfr_init_set_ui(x, v, rnd) (x = (v))
#define mpfr_clear(x) do { } while (0)
#define mpfr_swap(x, y) do { FTYPE z = y; y = x; x = z; } while (0)
#define mpfr_copysign(rop, op1, op2, rnd) rop = SUFFIX(copysign)((op1), (op2))
#define mpfr_fmod(rop, x, d, rnd) (rop = SUFFIX(fmod)((x), (d)))
#define mpfr_abs(rop, op, rnd) (rop = SUFFIX(fabs)(op))
#define mpfr_cmp_ld(x, y) ((x) - (y))
#define mpfr_cmp_ui(x, n) ((x) - (n))
#define mpfr_zero_p(x) ((x) == 0)
#define mpfr_set(rop, x, rnd) (rop = (x))
#define mpfr_set_zero(rop, s) (rop = SUFFIX(copysign)(0, (s)))
#define mpfr_set_inf(rop, s) (rop = ((s)*-2 + 1) * INFINITY)
#define mpfr_set_ui(rop, n, rnd) (rop = (n))
#define mpfr_set_si(rop, n, rnd) (rop = (n))
#define mpfr_set_ld(rop, x, rnd) (rop = (x))
#define mpfr_set_si_2exp(rop, op, exp, rnd) (rop = (0x1.p##exp))
#define mpfr_get_z(rop, x, rnd) ((rop = (int)(x)), (rop - (x)))
#define mpfr_mul(rop, op1, op2, rnd)    (rop = ((op1) * (op2)))
#define mpfr_sub_d(rop, op1, op2, rnd)  (rop = ((op1) - (op2)))
#define mpfr_sub_ui(rop, op1, op2, rnd) (rop = ((op1) - (op2)))
#define mpfr_sub(rop, op1, op2, rnd)    (rop = ((op1) - (op2)))
#define mpfr_ui_sub(rop, op1, op2, rnd) (rop = ((op1) - (op2)))
#define mpfr_neg(rop, op, rnd) (rop = -(op))
#define mpfr_sin(rop, x, rnd) (rop = SUFFIX(sin)(x))
#define mpfr_cos(rop, x, rnd) (rop = SUFFIX(cos)(x))
#define mpfr_tan(rop, x, rnd) (rop = SUFFIX(tan)(x))

#define mpz_init(n) do { } while (0)
#define mpz_clear(x) do { } while (0)
#define mpz_cmp_ui(x, y) ((x) - (y))
#define mpz_divisible_ui_p(n, d) ((n) % (d) == 0)

#define FMA(x,y,z)  SUFFIX(fma)((x), (y), (z))
#define D2R(x) (x = FMA((x), PIO180H, (x) * PIO180L))

#define SET_COSD30(x) (x = COSD30)


extern FTYPE SIND (FTYPE);
export_proto (SIND);

extern FTYPE COSD (FTYPE);
export_proto (COSD);

extern FTYPE TAND (FTYPE);
export_proto (TAND);

#include "trigd.inc"

#undef ITYPE
#undef GFC_RND_MODE
#undef RETTYPE
#undef RETURN

#undef ISFINITE
#undef mpfr_signbit

#undef mpfr_init
#undef mpfr_init_set_ui
#undef mpfr_clear
#undef mpfr_swap
#undef mpfr_fmod
#undef mpfr_abs
#undef mpfr_cmp_ld
#undef mpfr_cmp_ui
#undef mpfr_zero_p
#undef mpfr_set
#undef mpfr_set_zero
#undef mpfr_set_inf
#undef mpfr_set_ui
#undef mpfr_set_si
#undef mpfr_set_ld
#undef mpfr_set_si_2exp
#undef mpfr_get_z
#undef mpfr_mul_si
#undef mpfr_sub_d
#undef mpfr_sub_ui
#undef mpfr_sub
#undef mpfr_ui_sub
#undef mpfr_neg
#undef mpfr_sin
#undef mpfr_cos
#undef mpfr_tan

#undef mpz_init
#undef mpz_clear
#undef mpz_cmp_ui
#undef mpz_divisible_ui_p

#undef FMA
#undef D2R

#undef SET_COSD30


/* vim: set ft=c: */
