! Copyright (C) 2018-2025 Free Software Foundation, Inc.
! Contributed by Steven G. Kargl <kargl@gcc.gnu.org>
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
! <http://www.gnu.org/licenses/>.
!
! WARNING:  This file should never be compiled with an option that changes
! default logical kind from 4 to some other value or changes default integer
! kind from 4 to some other value.
!
! There are four combinations of repeatable and image_distinct.  The
! language below is from the F2018 standard (actually, J3/18-007r1).
!
! This routine is only used for non-coarray programs or with programs
! compiled with -fcoarray=single.  Use of -fcoarray=lib or -fcoarray=shared
! requires different routines due to the need for communication between
! images under case(iv).
!
! Technically, neither image_distinct nor image_num are now needed.  The
! interface to _gfortran_random_init() is maintained for libgfortran ABI.
! Note, the Fortran standard requires the image_distinct argument, so
! it will always have a valid value, and the frontend generates an value
! of 0 for image_num.
!
impure subroutine _gfortran_random_init(repeatable, image_distinct, image_num) 

   implicit none

   logical, value, intent(in) :: repeatable
   logical, value, intent(in) :: image_distinct
   integer, value, intent(in) :: image_num

   logical, save :: once = .true.
   integer :: nseed, lcg_seed
   integer, save, allocatable :: seed(:)

   if (repeatable) then
      if (once) then
         once = .false.
         call random_seed(size=nseed)
         allocate(seed(nseed))
         lcg_seed = 57911963
         call _gfortran_lcg(seed)
      end if
      call random_seed(put=seed)
   else
      call random_seed()
      !
      ! This cannot happen; but, prevent gfortran complaining about
      ! unused variables.
      !
      if (image_num > 2) then
         block
            use iso_fortran_env, only : error_unit
            write(error_unit, '(A)') 'whoops: random_init(.false., .false.)'
            if (image_distinct) error stop image_num + 1
            error stop image_num
         end block
      end if
   end if

   contains
      !
      ! SK Park and KW Miller, ``Random number generators: good ones are hard
      ! to find,'' Comm. ACM, 31(10), 1192--1201, (1988).
      !
      ! Implementation of a prime modulus multiplicative linear congruential
      ! generator, which avoids overflow and provides the full period.
      !
      impure elemental subroutine _gfortran_lcg(i)
         implicit none
         integer, intent(out) :: i
         integer, parameter :: a = 16807     ! Multiplier
         integer, parameter :: m = huge(a)   ! Modulus
         integer, parameter :: q = 127773    ! Quotient to avoid overflow
         integer, parameter :: r = 2836      ! Remainder to avoid overflow
         lcg_seed = a * mod(lcg_seed, q) - r * (lcg_seed / q)
         if (lcg_seed <= 0) lcg_seed = lcg_seed + m
         i = lcg_seed
      end subroutine _gfortran_lcg

end subroutine _gfortran_random_init
