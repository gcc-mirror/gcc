// Copyright (C) 1994, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2007
// Free Software Foundation
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA. 

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline enums from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include "tinfo.h"

namespace __cxxabiv1 {


// this is the external interface to the dynamic cast machinery
extern "C" void *
__dynamic_cast (const void *src_ptr,    // object started from
                const __class_type_info *src_type, // type of the starting object
                const __class_type_info *dst_type, // desired target type
                ptrdiff_t src2dst) // how src and dst are related
{
  const void *vtable = *static_cast <const void *const *> (src_ptr);
  const vtable_prefix *prefix =
      adjust_pointer <vtable_prefix> (vtable, 
				      -offsetof (vtable_prefix, origin));
  const void *whole_ptr =
      adjust_pointer <void> (src_ptr, prefix->whole_object);
  const __class_type_info *whole_type = prefix->whole_type;
  __class_type_info::__dyncast_result result;
  
  whole_type->__do_dyncast (src2dst, __class_type_info::__contained_public,
                            dst_type, whole_ptr, src_type, src_ptr, result);
  if (!result.dst_ptr)
    return NULL;
  if (contained_public_p (result.dst2src))
    // Src is known to be a public base of dst.
    return const_cast <void *> (result.dst_ptr);
  if (contained_public_p (__class_type_info::__sub_kind (result.whole2src & result.whole2dst)))
    // Both src and dst are known to be public bases of whole. Found a valid
    // cross cast.
    return const_cast <void *> (result.dst_ptr);
  if (contained_nonvirtual_p (result.whole2src))
    // Src is known to be a non-public nonvirtual base of whole, and not a
    // base of dst. Found an invalid cross cast, which cannot also be a down
    // cast
    return NULL;
  if (result.dst2src == __class_type_info::__unknown)
    result.dst2src = dst_type->__find_public_src (src2dst, result.dst_ptr,
                                                  src_type, src_ptr);
  if (contained_public_p (result.dst2src))
    // Found a valid down cast
    return const_cast <void *> (result.dst_ptr);
  // Must be an invalid down cast, or the cross cast wasn't bettered
  return NULL;
}

}
