!   Copyright (C) 2002-2019 Free Software Foundation, Inc.
!   Contributed by Paul Brook <paul@nowt.org>
!
!This file is part of the GNU Fortran 95 runtime library (libgfortran).
!
!GNU libgfortran is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public
!License as published by the Free Software Foundation; either
!version 3 of the License, or (at your option) any later version.

!GNU libgfortran is distributed in the hope that it will be useful,
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
!
!This file is machine generated.





#include "config.h"
#include "kinds.inc"
#include "c99_protos.inc"

#if defined (HAVE_GFC_COMPLEX_8)
#ifdef HAVE_CCOS

elemental function _gfortran_specific__cos_c8 (parm)
   complex (kind=8), intent (in) :: parm
   complex (kind=8) :: _gfortran_specific__cos_c8

   _gfortran_specific__cos_c8 = cos (parm)
end function

#endif
#endif
