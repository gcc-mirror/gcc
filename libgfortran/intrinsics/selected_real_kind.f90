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
!write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
!Boston, MA 02111-1307, USA.
!

function selected_real_kind (p, r)
  implicit none
  integer, optional, intent (in) :: p, r
  integer :: selected_real_kind
  integer :: i, p2, r2
  logical :: found_p, found_r
  ! Real kind_precision_range table
  type :: real_info
    integer :: kind
    integer :: precision
    integer :: range
  end type real_info

  include "selected_real_kind.inc"

  selected_real_kind = 0
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
      selected_real_kind = real_infos (i) % kind
      return
    end if
  end do

  if (.not. (found_p)) selected_real_kind = selected_real_kind - 1
  if (.not. (found_r)) selected_real_kind = selected_real_kind - 2

  return
end function
