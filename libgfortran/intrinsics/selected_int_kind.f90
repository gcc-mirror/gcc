!   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

function _gfortran_selected_int_kind (r)
  implicit none
  integer, intent(in) :: r
  integer :: _gfortran_selected_int_kind
  integer :: i
  ! Integer kind_range table
  type :: int_info
    integer :: kind
    integer :: range
  end type int_info

  include "selected_int_kind.inc"

  do i = 1, c
    if (r <= int_infos(i)%range) then
      _gfortran_selected_int_kind = int_infos(i)%kind
      return
    end if
  end do
  _gfortran_selected_int_kind = -1
  return
end function


! At this time, our logical and integer kinds are the same

function _gfortran_selected_logical_kind (bits)
  implicit none
  integer, intent(in) :: bits
  integer :: _gfortran_selected_logical_kind
  integer :: i
  ! Integer kind_range table
  type :: int_info
    integer :: kind
    integer :: range
  end type int_info

  include "selected_int_kind.inc"

  do i = 1, c
    if (bits <= 8 * int_infos(i)%kind) then
      _gfortran_selected_logical_kind = int_infos(i)%kind
      return
    end if
  end do
  _gfortran_selected_logical_kind = -1
  return
end function
