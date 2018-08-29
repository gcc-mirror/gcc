! Copyright (C) 2018 Free Software Foundation, Inc.
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
!
! WARNING:  This file should never be compiled with an option that changes
! default logical kind from 4 to some other value or changes default integer
! kind from from 4 to some other value.
!
!
! There are four combinations of repeatable and image_distinct.  If a program
! is compiled without the -fcoarray= option or with -fcoarray=single, then
! execution of the compiled executable does not use image_distinct as it is
! irrelevant (although required).  The behavior is as follows:
!
! call random_init(.true., .true.)
!
! The sequence of random numbers is repeatable within an instance of program
! execution.  That is, calls to random_init(.true., .true.) during the
! execution will reset the sequence of RN to the same sequence.  If the
! program is compiled with -fcoarray=lib and multiple images are instantiated,
! then each image accesses a repeatable distinct sequence of random numbers.
! There are no guarantees that multiple execution of the program will access
! the same sequence.
!
! call random_init(.false., .false.)
! call random_init(.false., .true.)
!
! The sequence of random numbers is determined from process-dependent seeds.
! On each execution of the executable, different seeds will be used.  For
! -fcoarray=lib and multiple instantiated images, each image will use
! process-dependent seeds.  In other words, the two calls have identical
! behavior.
!
! call random_init(.true., .false.)
! 
! For a program compiled without the -fcoarray= option or with
! -fcoarray=single, a single image is instantiated when the executable is
! run.  If the executable causes multiple images to be instantiated, then
! image_distinct=.false. in one image cannot affect the sequence of random
! numbers in another image.  As gfortran gives each image its own independent
! PRNG, this condition is automatically satisfied.
!
impure subroutine _gfortran_random_init(repeatable, image_distinct, hidden) 

   implicit none

   logical, value, intent(in) :: repeatable
   logical, value, intent(in) :: image_distinct
   integer, value, intent(in) :: hidden

   logical, save :: once = .true.
   integer :: nseed
   integer, save, allocatable :: seed(:)

   if (once) then
      once = .false.
      call random_seed(size=nseed)
      allocate(seed(nseed))
      call random_seed(get=seed)
      !
      ! To guarantee that seed is distinct on multiple images, add the hidden
      ! argument (which is the image index).
      !
      if (image_distinct) seed = seed + hidden
   end if

   if (repeatable) then
      call random_seed(put=seed);
   else
      call random_seed();
   end if

end subroutine _gfortran_random_init
