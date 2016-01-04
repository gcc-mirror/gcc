// Copyright (C) 1994-2016 Free Software Foundation, Inc.
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

#if __cpp_rtti
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

  unsigned tflags = thrown_type->__flags;

  bool throw_tx = (tflags & __transaction_safe_mask);
  bool catch_tx = (__flags & __transaction_safe_mask);
  if (throw_tx && !catch_tx)
    /* Catch can perform a transaction-safety conversion.  */
    tflags &= ~__transaction_safe_mask;
  if (catch_tx && !throw_tx)
    /* But not the reverse.  */
    return false;
  
  if (tflags & ~__flags)
    // We're less qualified.
    return false;
  
  if (!(__flags & __const_mask))
    outer &= ~1;
  
  return __pointer_catch (thrown_type, thr_obj, outer);
}

}
