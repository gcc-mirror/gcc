// Copyright (C) 1994, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2007, 
// 2009, 2010
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

#include "tinfo.h"

namespace __cxxabiv1 {

__pbase_type_info::
~__pbase_type_info ()
{}

bool __pbase_type_info::
__do_catch (const type_info *thr_type,
            void **thr_obj,
            unsigned outer) const
{
  if (*this == *thr_type)
    return true;      // same type

#ifdef __GXX_RTTI
  if (typeid (*this) != typeid (*thr_type))
    return false;     // not both same kind of pointers
#endif
  
  if (!(outer & 1))
    // We're not the same and our outer pointers are not all const qualified
    // Therefore there must at least be a qualification conversion involved
    // But for that to be valid, our outer pointers must be const qualified.
    return false;
  
  const __pbase_type_info *thrown_type =
    static_cast <const __pbase_type_info *> (thr_type);
  
  if (thrown_type->__flags & ~__flags)
    // We're less qualified.
    return false;
  
  if (!(__flags & __const_mask))
    outer &= ~1;
  
  return __pointer_catch (thrown_type, thr_obj, outer);
}

}
