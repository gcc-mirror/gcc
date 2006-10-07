!   Copyright 2002 Free Software Foundation, Inc.
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




#if defined (HAVE_GFC_REAL_4) && defined (HAVE_GFC_INTEGER_4)
elemental function specific__nint_4_4 (parm)
   real (kind=4) , intent (in) :: parm
   integer (kind=4) :: specific__nint_4_4
   specific__nint_4_4 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_8) && defined (HAVE_GFC_INTEGER_4)
elemental function specific__nint_4_8 (parm)
   real (kind=8) , intent (in) :: parm
   integer (kind=4) :: specific__nint_4_8
   specific__nint_4_8 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_10) && defined (HAVE_GFC_INTEGER_4)
elemental function specific__nint_4_10 (parm)
   real (kind=10) , intent (in) :: parm
   integer (kind=4) :: specific__nint_4_10
   specific__nint_4_10 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_16) && defined (HAVE_GFC_INTEGER_4)
elemental function specific__nint_4_16 (parm)
   real (kind=16) , intent (in) :: parm
   integer (kind=4) :: specific__nint_4_16
   specific__nint_4_16 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_4) && defined (HAVE_GFC_INTEGER_8)
elemental function specific__nint_8_4 (parm)
   real (kind=4) , intent (in) :: parm
   integer (kind=8) :: specific__nint_8_4
   specific__nint_8_4 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_8) && defined (HAVE_GFC_INTEGER_8)
elemental function specific__nint_8_8 (parm)
   real (kind=8) , intent (in) :: parm
   integer (kind=8) :: specific__nint_8_8
   specific__nint_8_8 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_10) && defined (HAVE_GFC_INTEGER_8)
elemental function specific__nint_8_10 (parm)
   real (kind=10) , intent (in) :: parm
   integer (kind=8) :: specific__nint_8_10
   specific__nint_8_10 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_16) && defined (HAVE_GFC_INTEGER_8)
elemental function specific__nint_8_16 (parm)
   real (kind=16) , intent (in) :: parm
   integer (kind=8) :: specific__nint_8_16
   specific__nint_8_16 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_4) && defined (HAVE_GFC_INTEGER_16)
elemental function specific__nint_16_4 (parm)
   real (kind=4) , intent (in) :: parm
   integer (kind=16) :: specific__nint_16_4
   specific__nint_16_4 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_8) && defined (HAVE_GFC_INTEGER_16)
elemental function specific__nint_16_8 (parm)
   real (kind=8) , intent (in) :: parm
   integer (kind=16) :: specific__nint_16_8
   specific__nint_16_8 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_10) && defined (HAVE_GFC_INTEGER_16)
elemental function specific__nint_16_10 (parm)
   real (kind=10) , intent (in) :: parm
   integer (kind=16) :: specific__nint_16_10
   specific__nint_16_10 = nint (parm)
end function
#endif

#if defined (HAVE_GFC_REAL_16) && defined (HAVE_GFC_INTEGER_16)
elemental function specific__nint_16_16 (parm)
   real (kind=16) , intent (in) :: parm
   integer (kind=16) :: specific__nint_16_16
   specific__nint_16_16 = nint (parm)
end function
#endif



#if defined (HAVE_GFC_INTEGER_4)
elemental function specific__char_1_i4 (parm)
   integer (kind=4) , intent (in) :: parm
   character (kind=1,len=1) :: specific__char_1_i4
   specific__char_1_i4 = char (parm, kind=1)
end function
#endif

#if defined (HAVE_GFC_INTEGER_8)
elemental function specific__char_1_i8 (parm)
   integer (kind=8) , intent (in) :: parm
   character (kind=1,len=1) :: specific__char_1_i8
   specific__char_1_i8 = char (parm, kind=1)
end function
#endif

#if defined (HAVE_GFC_INTEGER_16)
elemental function specific__char_1_i16 (parm)
   integer (kind=16) , intent (in) :: parm
   character (kind=1,len=1) :: specific__char_1_i16
   specific__char_1_i16 = char (parm, kind=1)
end function
#endif



#if defined (HAVE_GFC_INTEGER_4)
elemental function specific__len_1_i4 (parm)
   character (kind=1,len=*) , intent (in) :: parm
   integer (kind=4) :: specific__len_1_i4
   specific__len_1_i4 = len (parm)
end function
#endif

#if defined (HAVE_GFC_INTEGER_8)
elemental function specific__len_1_i8 (parm)
   character (kind=1,len=*) , intent (in) :: parm
   integer (kind=8) :: specific__len_1_i8
   specific__len_1_i8 = len (parm)
end function
#endif

#if defined (HAVE_GFC_INTEGER_16)
elemental function specific__len_1_i16 (parm)
   character (kind=1,len=*) , intent (in) :: parm
   integer (kind=16) :: specific__len_1_i16
   specific__len_1_i16 = len (parm)
end function
#endif



#if defined (HAVE_GFC_INTEGER_4)
elemental function specific__index_1_i4 (parm1, parm2)
   character (kind=1,len=*) , intent (in) :: parm1, parm2
   integer (kind=4) :: specific__index_1_i4
   specific__index_1_i4 = index (parm1, parm2)
end function
#endif

#if defined (HAVE_GFC_INTEGER_8)
elemental function specific__index_1_i8 (parm1, parm2)
   character (kind=1,len=*) , intent (in) :: parm1, parm2
   integer (kind=8) :: specific__index_1_i8
   specific__index_1_i8 = index (parm1, parm2)
end function
#endif

#if defined (HAVE_GFC_INTEGER_16)
elemental function specific__index_1_i16 (parm1, parm2)
   character (kind=1,len=*) , intent (in) :: parm1, parm2
   integer (kind=16) :: specific__index_1_i16
   specific__index_1_i16 = index (parm1, parm2)
end function
#endif

