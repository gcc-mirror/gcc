// Methods for type_info for -*- C++ -*- Run Time Type Identification.
// Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
// 2003, 2004, 2005, 2006, 2007, 2009
// Free Software Foundation
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

#include <bits/c++config.h>
#include <cstddef>
#include "tinfo.h"

std::type_info::
~type_info ()
{ }

#if !__GXX_TYPEINFO_EQUALITY_INLINE

// We can't rely on common symbols being shared between shared objects.
bool std::type_info::
operator== (const std::type_info& arg) const
{
#if __GXX_MERGED_TYPEINFO_NAMES
  return name () == arg.name ();
#else
  return (&arg == this) || (__builtin_strcmp (name (), arg.name ()) == 0);
#endif
}

#endif

namespace std {

// return true if this is a type_info for a pointer type
bool type_info::
__is_pointer_p () const
{
  return false;
}

// return true if this is a type_info for a function type
bool type_info::
__is_function_p () const
{
  return false;
}

// try and catch a thrown object.
bool type_info::
__do_catch (const type_info *thr_type, void **, unsigned) const
{
  return *this == *thr_type;
}

// upcast from this type to the target. __class_type_info will override
bool type_info::
__do_upcast (const abi::__class_type_info *, void **) const
{
  return false;
}

}
