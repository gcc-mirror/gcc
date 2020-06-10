// Copyright (C) 2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do compile { target x86_64-*-linux* } }

// Names taken from coding_style.bad_identifiers in the libstdc++ manual.
// We can't test this on all targets, because these names are used in
// non-GCC system headers. Test on x86_64-linux where we know these particular
// names aren't used.

// For Solaris:
#define _B		_B is a BADNAME
#define _C		_C is a BADNAME
#define _L		_L is a BADNAME
#define _N		_N is a BADNAME
#define _P		_P is a BADNAME
#define _S		_S is a BADNAME
#define _U		_U is a BADNAME
#define _X		_X is a BADNAME
#define _E1		_E1 is a BADNAME
#define _E2		_E2 is a BADNAME
#define _E3		_E3 is a BADNAME
#define _E4		_E4 is a BADNAME
#define _E5		_E5 is a BADNAME
#define _E6		_E6 is a BADNAME
#define _E7		_E7 is a BADNAME
#define _E8		_E8 is a BADNAME
#define _E9		_E9 is a BADNAME
#define _E10		_E10 is a BADNAME
#define _E11		_E11 is a BADNAME
#define _E12		_E12 is a BADNAME
#define _E13		_E13 is a BADNAME
#define _E14		_E14 is a BADNAME
#define _E15		_E15 is a BADNAME
#define _E16		_E16 is a BADNAME
#define _E17		_E17 is a BADNAME
#define _E18		_E18 is a BADNAME
#define _E19		_E19 is a BADNAME
#define _E20		_E20 is a BADNAME
#define _E21		_E21 is a BADNAME
#define _E22		_E22 is a BADNAME
#define _E23		_E23 is a BADNAME
#define _E24		_E24 is a BADNAME

// Irix adds:
#define _A		_A is a BADNAME
#define _G		_G is a BADNAME

// MS adds:
#define _T		_T is a BADNAME

// BSD adds:
#define __used		__used is a BADNAME
#define __unused	__unused is a BADNAME
        // __inline	(glibc uses this so can't test here)
        // _Complex	(glibc uses this so can't test here)
#define __istype	__istype is a BADNAME
#define __maskrune	__maskrune is a BADNAME
#define __tolower	__tolower is a BADNAME
#define __toupper	__toupper is a BADNAME
#define __wchar_t	__wchar_t is a BADNAME
#define __wint_t	__wint_t is a BADNAME
        // _res
        // _res_ext
	// __tg_*

// VxWorks adds:
#define _C2		_C2 is a BADNAME


#include <bits/stdc++.h>
