!   Copyright 2002, 2005 Free Software Foundation, Inc.
!   Contributed by Tobias Schl"uter
!
!This file is part of the GNU Fortran 95 runtime library (libgfortran).
!
!GNU libgfortran is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public
!License as published by the Free Software Foundation; either
!version 2 of the License, or (at your option) any later version.

!In addition to the permissions in the GNU General Public License, the
!Free Software Foundation gives you unlimited permission to link the
!compiled version of this file into combinations with other programs,
!and to distribute those combinations without any restriction coming
!from the use of this file.  (The General Public License restrictions
!do apply in other respects; for example, they cover modification of
!the file, and distribution when not linked into a combine
!executable.)
!
!GNU libgfortran is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public
!License along with libgfortran; see the file COPYING.  If not,
!write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
!Boston, MA 02110-1301, USA.
!
! Specifics for the intrinsics whose calling conventions change if
! -ff2c is used.
!
! There are two annoyances WRT the preprocessor:
!  - we're using -traditional-cpp, so we can't use the ## operator.
!  - macros expand to a single line, and Fortran lines can't be wider
!    than 132 characters, therefore we use two macros to split the lines
!
! The cases we need to implement are functions returning default REAL
! or COMPLEX.  The former need to return DOUBLE PRECISION instead of REAL,
! the latter become subroutines returning via a hidden first argument.

! one argument functions
#define REAL_HEAD(NAME) \
elemental function f2c_specific__/**/NAME/**/_r4 (parm) result(res);

#define REAL_BODY(NAME) \
  REAL, intent (in) :: parm; \
  DOUBLE PRECISION :: res; \
  res = NAME (parm); \
end function

#define COMPLEX_HEAD(NAME) \
subroutine f2c_specific__/**/NAME/**/_c4 (res, parm);

#define COMPLEX_BODY(NAME) \
  COMPLEX, intent (in) :: parm; \
  COMPLEX, intent (out) :: res; \
  res = NAME (parm); \
end subroutine

#define DCOMPLEX_HEAD(NAME) \
subroutine f2c_specific__/**/NAME/**/_c8 (res, parm);

#define DCOMPLEX_BODY(NAME) \
  DOUBLE COMPLEX, intent (in) :: parm; \
  DOUBLE COMPLEX, intent (out) :: res; \
  res = NAME (parm); \
end subroutine

REAL_HEAD(abs)
REAL_BODY(abs)
! abs is special in that the result is real
elemental function f2c_specific__abs_c4 (parm) result (res)
  COMPLEX, intent(in) :: parm
  DOUBLE PRECISION :: res
  res = abs(parm)
end function

REAL_HEAD(exp)
REAL_BODY(exp)
COMPLEX_HEAD(exp)
COMPLEX_BODY(exp)
DCOMPLEX_HEAD(exp)
DCOMPLEX_BODY(exp)

REAL_HEAD(log)
REAL_BODY(log)
COMPLEX_HEAD(log)
COMPLEX_BODY(log)
DCOMPLEX_HEAD(log)
DCOMPLEX_BODY(log)

REAL_HEAD(log10)
REAL_BODY(log10)

REAL_HEAD(sqrt)
REAL_BODY(sqrt)
COMPLEX_HEAD(sqrt)
COMPLEX_BODY(sqrt)
DCOMPLEX_HEAD(sqrt)
DCOMPLEX_BODY(sqrt)

REAL_HEAD(asin)
REAL_BODY(asin)

REAL_HEAD(acos)
REAL_BODY(acos)

REAL_HEAD(atan)
REAL_BODY(atan)

REAL_HEAD(sin)
REAL_BODY(sin)
COMPLEX_HEAD(sin)
COMPLEX_BODY(sin)
DCOMPLEX_HEAD(sin)
DCOMPLEX_BODY(sin)

REAL_HEAD(cos)
REAL_BODY(cos)
COMPLEX_HEAD(cos)
COMPLEX_BODY(cos)
DCOMPLEX_HEAD(cos)
DCOMPLEX_BODY(cos)

REAL_HEAD(tan)
REAL_BODY(tan)

REAL_HEAD(sinh)
REAL_BODY(sinh)

REAL_HEAD(cosh)
REAL_BODY(cosh)

REAL_HEAD(tanh)
REAL_BODY(tanh)

COMPLEX_HEAD(conjg)
COMPLEX_BODY(conjg)
DCOMPLEX_HEAD(conjg)
DCOMPLEX_BODY(conjg)

REAL_HEAD(aint)
REAL_BODY(aint)

REAL_HEAD(anint)
REAL_BODY(anint)

! two argument functions
#define REAL2_HEAD(NAME) \
elemental function f2c_specific__/**/NAME/**/_r4 (p1, p2) result(res);

#define REAL2_BODY(NAME) \
  REAL, intent (in) :: p1, p2; \
  DOUBLE PRECISION :: res; \
  res = NAME (p1, p2); \
end function

REAL2_HEAD(sign)
REAL2_BODY(sign)

REAL2_HEAD(dim)
REAL2_BODY(dim)

REAL2_HEAD(atan2)
REAL2_BODY(atan2)

REAL2_HEAD(mod)
REAL2_BODY(mod)
