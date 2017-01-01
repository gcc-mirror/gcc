// Copyright (C) 1994-2017 Free Software Foundation, Inc.
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

__pointer_to_member_type_info::
~__pointer_to_member_type_info ()
{}

bool __pointer_to_member_type_info::
__pointer_catch (const __pbase_type_info *thr_type,
                 void **thr_obj,
                 unsigned outer) const
{
  // This static cast is always valid, as our caller will have determined that
  // thr_type is really a __pointer_to_member_type_info.
  const __pointer_to_member_type_info *thrown_type =
    static_cast <const __pointer_to_member_type_info *> (thr_type);
  
  if (*__context != *thrown_type->__context)
    return false;     // not pointers to member of same class
  
  return __pbase_type_info::__pointer_catch (thrown_type, thr_obj, outer);
}

}
