!   Copyright 2003, 2004 Free Software Foundation, Inc.
!   Contributed by Kejia Zhao <kejia_zh@yahoo.com.cn>
!
!This file is part of the GNU Fortran 95 runtime library (libgfor).
!
!GNU libgfor is free software; you can redistribute it and/or
!modify it under the terms of the GNU Lesser General Public
!License as published by the Free Software Foundation; either
!version 2.1 of the License, or (at your option) any later version.
!
!GNU libgfor is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU Lesser General Public License for more details.
!
!You should have received a copy of the GNU Lesser General Public
!License along with libgfor; see the file COPYING.  If not,
!write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
!Boston, MA 02110-1301, USA.
!

function selected_int_kind (r)
  implicit none
  integer, intent (in) :: r
  integer :: selected_int_kind
  integer :: i
  ! Integer kind_range table
  type :: int_info
    integer :: kind
    integer :: range
  end type int_info

  include "selected_int_kind.inc"

  do i = 1, c
    if (r <= int_infos (i) % range) then
      selected_int_kind = int_infos (i) % kind
      return
    end if
  end do
  selected_int_kind = -1
  return
end function
