// Methods for type_info for -*- C++ -*- Run Time Type Identification.

// Copyright (C) 1994-2020 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <cstddef>
#include "tinfo.h"

using std::type_info;

#if !__GXX_TYPEINFO_EQUALITY_INLINE

bool
type_info::before (const type_info &arg) const _GLIBCXX_NOEXCEPT
{
#if __GXX_MERGED_TYPEINFO_NAMES
  return name () < arg.name ();
#else
  return (name ()[0] == '*') ? name () < arg.name ()
    :  __builtin_strcmp (name (), arg.name ()) < 0;
#endif
}

#endif
