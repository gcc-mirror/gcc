!   Copyright 2002 Free Software Foundation, Inc.
!   Contributed by Paul Brook <paul@nowt.org>
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
!This file is machine generated.


elemental function specific__atan2_r4 (p1, p2)
   real (kind=4), intent (in) :: p1, p2
   real (kind=4) :: specific__atan2_r4

   specific__atan2_r4 = atan2 (p1, p2)
end function
