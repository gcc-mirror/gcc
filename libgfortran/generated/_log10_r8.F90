!   Copyright 2002, 2007 Free Software Foundation, Inc.
!   Contributed by Paul Brook <paul@nowt.org>
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
!This file is machine generated.





#include "config.h"
#include "kinds.inc"
#include "c99_protos.inc"

#if defined (HAVE_GFC_REAL_8)
#ifdef HAVE_LOG10

elemental function _gfortran_specific__log10_r8 (parm)
   real (kind=8), intent (in) :: parm
   real (kind=8) :: _gfortran_specific__log10_r8

   _gfortran_specific__log10_r8 = log10 (parm)
end function

#endif
#endif
