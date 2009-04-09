!   Copyright 2003, 2004, 2009 Free Software Foundation, Inc.
!   Contributed by Kejia Zhao <kejia_zh@yahoo.com.cn>
!
!This file is part of the GNU Fortran 95 runtime library (libgfortran).
!
!Libgfortran is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public
!License as published by the Free Software Foundation; either
!version 3 of the License, or (at your option) any later version.
!
!Libgfortran is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!Under Section 7 of GPL version 3, you are granted additional
!permissions described in the GCC Runtime Library Exception, version
!3.1, as published by the Free Software Foundation.
!
!You should have received a copy of the GNU General Public License and
!a copy of the GCC Runtime Library Exception along with this program;
!see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
!<http://www.gnu.org/licenses/>.

function _gfortran_selected_real_kind (p, r)
  implicit none
  integer, optional, intent (in) :: p, r
  integer :: _gfortran_selected_real_kind
  integer :: i, p2, r2
  logical :: found_p, found_r
  ! Real kind_precision_range table
  type :: real_info
    integer :: kind
    integer :: precision
    integer :: range
  end type real_info

  include "selected_real_kind.inc"

  _gfortran_selected_real_kind = 0
  p2 = 0
  r2 = 0
  found_p = .false.
  found_r = .false.

  if (present (p)) p2 = p
  if (present (r)) r2 = r

  ! Assumes each type has a greater precision and range than previous one.

  do i = 1, c
    if (p2 <= real_infos (i) % precision) found_p = .true.
    if (r2 <= real_infos (i) % range) found_r = .true.
    if (found_p .and. found_r) then
      _gfortran_selected_real_kind = real_infos (i) % kind
      return
    end if
  end do

  if (.not. (found_p)) _gfortran_selected_real_kind = _gfortran_selected_real_kind - 1
  if (.not. (found_r)) _gfortran_selected_real_kind = _gfortran_selected_real_kind - 2

  return
end function
