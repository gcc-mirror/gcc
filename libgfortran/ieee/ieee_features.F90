!    Implementation of the IEEE_FEATURES standard intrinsic module
!    Copyright (C) 2013-2021 Free Software Foundation, Inc.
!    Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
! 
! This file is part of the GNU Fortran runtime library (libgfortran).
! 
! Libgfortran is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation; either
! version 3 of the License, or (at your option) any later version.
! 
! Libgfortran is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
! 
! You should have received a copy of the GNU General Public License and
! a copy of the GCC Runtime Library Exception along with this program;
! see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
! <http://www.gnu.org/licenses/>.  */

module IEEE_FEATURES

  implicit none
  private

  type, public :: IEEE_FEATURES_TYPE
    private
    integer :: hidden
  end type

  type(IEEE_FEATURES_TYPE), parameter, public :: &
    IEEE_DATATYPE       = IEEE_FEATURES_TYPE(0), &
    IEEE_DENORMAL       = IEEE_FEATURES_TYPE(1), &
    IEEE_SUBNORMAL      = IEEE_FEATURES_TYPE(1), &
    IEEE_DIVIDE         = IEEE_FEATURES_TYPE(2), &
    IEEE_HALTING        = IEEE_FEATURES_TYPE(3), &
    IEEE_INEXACT_FLAG   = IEEE_FEATURES_TYPE(4), &
    IEEE_INF            = IEEE_FEATURES_TYPE(5), &
    IEEE_INVALID_FLAG   = IEEE_FEATURES_TYPE(6), &
    IEEE_NAN            = IEEE_FEATURES_TYPE(7), &
    IEEE_ROUNDING       = IEEE_FEATURES_TYPE(8), &
    IEEE_SQRT           = IEEE_FEATURES_TYPE(9), &
    IEEE_UNDERFLOW_FLAG = IEEE_FEATURES_TYPE(10)

end module IEEE_FEATURES
