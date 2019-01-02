// Copyright (C) 1994-2019 Free Software Foundation, Inc.
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

__class_type_info::
~__class_type_info ()
{}

bool __class_type_info::
__do_catch (const type_info *thr_type,
            void **thr_obj,
            unsigned outer) const
{
  if (*this == *thr_type)
    return true;
  if (outer >= 4)
    // Neither `A' nor `A *'.
    return false;
  return thr_type->__do_upcast (this, thr_obj);
}

bool __class_type_info::
__do_upcast (const __class_type_info *dst_type,
             void **obj_ptr) const
{
  __upcast_result result (__vmi_class_type_info::__flags_unknown_mask);
  
  __do_upcast (dst_type, *obj_ptr, result);
  if (!contained_public_p (result.part2dst))
    return false;
  *obj_ptr = const_cast <void *> (result.dst_ptr);
  return true;
}

__class_type_info::__sub_kind __class_type_info::
__do_find_public_src (ptrdiff_t,
                      const void *obj_ptr,
                      const __class_type_info *,
                      const void *src_ptr) const
{
  if (src_ptr == obj_ptr)
    // Must be our type, as the pointers match.
    return __contained_public;
  return __not_contained;
}

bool __class_type_info::
__do_dyncast (ptrdiff_t,
              __sub_kind access_path,
              const __class_type_info *dst_type,
              const void *obj_ptr,
              const __class_type_info *src_type,
              const void *src_ptr,
              __dyncast_result &__restrict result) const
{
  if (obj_ptr == src_ptr && *this == *src_type)
    {
      // The src object we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2src = access_path;
      return false;
    }
  if (*this == *dst_type)
    {
      result.dst_ptr = obj_ptr;
      result.whole2dst = access_path;
      result.dst2src = __not_contained;
      return false;
    }
  return false;
}

bool __class_type_info::
__do_upcast (const __class_type_info *dst, const void *obj,
             __upcast_result &__restrict result) const
{
  if (*this == *dst)
    {
      result.dst_ptr = obj;
      result.base_type = nonvirtual_base_type;
      result.part2dst = __contained_public;
      return true;
    }
  return false;
}

}
