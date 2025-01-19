// Copyright (C) 1994-2025 Free Software Foundation, Inc.
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

__vmi_class_type_info::
~__vmi_class_type_info ()
{}

__class_type_info::__sub_kind __vmi_class_type_info::
__do_find_public_src (ptrdiff_t src2dst,
                      const void *obj_ptr,
                      const __class_type_info *src_type,
                      const void *src_ptr) const
{
  if (obj_ptr == src_ptr && *this == *src_type)
    return __contained_public;

  for (std::size_t i = __base_count; i--;)
    {
      if (!__base_info[i].__is_public_p ())
        continue; // Not public, can't be here.

      const void *base = obj_ptr;
      ptrdiff_t offset = __base_info[i].__offset ();
      bool is_virtual = __base_info[i].__is_virtual_p ();

      if (is_virtual)
        {
          if (src2dst == -3)
            continue; // Not a virtual base, so can't be here.
        }
      base = convert_to_base (base, is_virtual, offset);

      __sub_kind base_kind = __base_info[i].__base_type->__do_find_public_src
                              (src2dst, base, src_type, src_ptr);
      if (contained_p (base_kind))
        {
          if (is_virtual)
            base_kind = __sub_kind (base_kind | __contained_virtual_mask);
          return base_kind;
        }
    }

  return __not_contained;
}

// This is a big hairy function. Although the run-time behaviour of
// dynamic_cast is simple to describe, it gives rise to some non-obvious
// behaviour. We also desire to determine as early as possible any definite
// answer we can get. Because it is unknown what the run-time ratio of
// succeeding to failing dynamic casts is, we do not know in which direction
// to bias any optimizations. To that end we make no particular effort towards
// early fail answers or early success answers. Instead we try to minimize
// work by filling in things lazily (when we know we need the information),
// and opportunisticly take early success or failure results.
bool __vmi_class_type_info::
__do_dyncast (ptrdiff_t src2dst,
              __sub_kind access_path,
              const __class_type_info *dst_type,
              const void *obj_ptr,
              const __class_type_info *src_type,
              const void *src_ptr,
              __dyncast_result &__restrict result) const
{
  if (result.whole_details & __flags_unknown_mask)
    result.whole_details = __flags;

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
      if (src2dst >= 0)
        result.dst2src = adjust_pointer <void> (obj_ptr, src2dst) == src_ptr
              ? __contained_public : __not_contained;
      else if (src2dst == -2)
        result.dst2src = __not_contained;
      return false;
    }

  // If src_type is a unique non-virtual base of dst_type, we have a good
  // guess at the address we want, so in the first pass try skipping any
  // bases which don't contain that address.
  const void *dst_cand = NULL;
  if (src2dst >= 0)
    dst_cand = adjust_pointer<void>(src_ptr, -src2dst);
  bool first_pass = true;
  bool skipped = false;

  bool result_ambig = false;
 again:
  for (std::size_t i = __base_count; i--;)
    {
      __dyncast_result result2 (result.whole_details);
      void const *base = obj_ptr;
      __sub_kind base_access = access_path;
      ptrdiff_t offset = __base_info[i].__offset ();
      bool is_virtual = __base_info[i].__is_virtual_p ();

      if (is_virtual)
        base_access = __sub_kind (base_access | __contained_virtual_mask);
      base = convert_to_base (base, is_virtual, offset);

      if (dst_cand)
	{
	  bool skip_on_first_pass = base > dst_cand;
	  if (skip_on_first_pass == first_pass)
	    {
	      // We aren't interested in this base on this pass: either
	      // we're on the first pass and this base doesn't contain the
	      // likely address, or we're on the second pass and we checked
	      // this base on the first pass.
	      skipped = true;
	      continue;
	    }
	}

      if (!__base_info[i].__is_public_p ())
        {
          if (src2dst == -2 &&
              !(result.whole_details
                & (__non_diamond_repeat_mask | __diamond_shaped_mask)))
            // The hierarchy has no duplicate bases (which might ambiguate
            // things) and where we started is not a public base of what we
            // want (so it cannot be a downcast). There is nothing of interest
            // hiding in a non-public base.
            continue;
          base_access = __sub_kind (base_access & ~__contained_public_mask);
        }

      bool result2_ambig
          = __base_info[i].__base_type->__do_dyncast (src2dst, base_access,
                                             dst_type, base,
                                             src_type, src_ptr, result2);
      result.whole2src = __sub_kind (result.whole2src | result2.whole2src);
      if (result2.dst2src == __contained_public
          || result2.dst2src == __contained_ambig)
        {
          result.dst_ptr = result2.dst_ptr;
          result.whole2dst = result2.whole2dst;
          result.dst2src = result2.dst2src;
          // Found a downcast which can't be bettered or an ambiguous downcast
          // which can't be disambiguated
          return result2_ambig;
        }

      if (!result_ambig && !result.dst_ptr)
        {
          // Not found anything yet.
          result.dst_ptr = result2.dst_ptr;
          result.whole2dst = result2.whole2dst;
          result_ambig = result2_ambig;
          if (result.dst_ptr && result.whole2src != __unknown
              && !(__flags & __non_diamond_repeat_mask))
            // Found dst and src and we don't have repeated bases.
            return result_ambig;
        }
      else if (result.dst_ptr && result.dst_ptr == result2.dst_ptr)
        {
          // Found at same address, must be via virtual.  Pick the most
          // accessible path.
          result.whole2dst =
              __sub_kind (result.whole2dst | result2.whole2dst);
        }
      else if ((result.dst_ptr != 0 && result2.dst_ptr != 0)
	       || (result.dst_ptr != 0 && result2_ambig)
	       || (result2.dst_ptr != 0 && result_ambig))
        {
          // Found two different DST_TYPE bases, or a valid one and a set of
          // ambiguous ones, must disambiguate. See whether SRC_PTR is
          // contained publicly within one of the non-ambiguous choices. If it
          // is in only one, then that's the choice. If it is in both, then
          // we're ambiguous and fail. If it is in neither, we're ambiguous,
          // but don't yet fail as we might later find a third base which does
          // contain SRC_PTR.

          __sub_kind new_sub_kind = result2.dst2src;
          __sub_kind old_sub_kind = result.dst2src;

          if (contained_p (result.whole2src)
              && (!virtual_p (result.whole2src)
                  || !(result.whole_details & __diamond_shaped_mask)))
            {
              // We already found SRC_PTR as a base of most derived, and
              // either it was non-virtual, or the whole hierarchy is
              // not-diamond shaped. Therefore if it is in either choice, it
              // can only be in one of them, and we will already know.
              if (old_sub_kind == __unknown)
                old_sub_kind = __not_contained;
              if (new_sub_kind == __unknown)
                new_sub_kind = __not_contained;
            }
          else
            {
              if (old_sub_kind >= __not_contained)
                ;// already calculated
              else if (contained_p (new_sub_kind)
                       && (!virtual_p (new_sub_kind)
                           || !(__flags & __diamond_shaped_mask)))
                // Already found inside the other choice, and it was
                // non-virtual or we are not diamond shaped.
                old_sub_kind = __not_contained;
              else
                old_sub_kind = dst_type->__find_public_src
                                (src2dst, result.dst_ptr, src_type, src_ptr);

              if (new_sub_kind >= __not_contained)
                ;// already calculated
              else if (contained_p (old_sub_kind)
                       && (!virtual_p (old_sub_kind)
                           || !(__flags & __diamond_shaped_mask)))
                // Already found inside the other choice, and it was
                // non-virtual or we are not diamond shaped.
                new_sub_kind = __not_contained;
              else
                new_sub_kind = dst_type->__find_public_src
                                (src2dst, result2.dst_ptr, src_type, src_ptr);
            }

          // Neither sub_kind can be contained_ambig -- we bail out early
          // when we find those.
          if (contained_p (__sub_kind (new_sub_kind ^ old_sub_kind)))
            {
              // Only on one choice, not ambiguous.
              if (contained_p (new_sub_kind))
                {
                  // Only in new.
                  result.dst_ptr = result2.dst_ptr;
                  result.whole2dst = result2.whole2dst;
                  result_ambig = false;
                  old_sub_kind = new_sub_kind;
                }
              result.dst2src = old_sub_kind;
              if (public_p (result.dst2src))
                return false; // Can't be an ambiguating downcast for later discovery.
              if (!virtual_p (result.dst2src))
                return false; // Found non-virtually can't be bettered
            }
          else if (contained_p (__sub_kind (new_sub_kind & old_sub_kind)))
            {
              // In both.
              result.dst_ptr = NULL;
              result.dst2src = __contained_ambig;
              return true;  // Fail.
            }
          else
            {
              // In neither publicly, ambiguous for the moment, but keep
              // looking. It is possible that it was private in one or
              // both and therefore we should fail, but that's just tough.
              result.dst_ptr = NULL;
              result.dst2src = __not_contained;
              result_ambig = true;
            }
        }

      if (result.whole2src == __contained_private)
        // We found SRC_PTR as a private non-virtual base, therefore all
        // cross casts will fail. We have already found a down cast, if
        // there is one.
        return result_ambig;
    }

  if (skipped && first_pass)
    {
      // We didn't find dst where we expected it, so let's go back and try
      // the bases we skipped (if any).
      first_pass = false;
      goto again;
    }

  return result_ambig;
}

bool __vmi_class_type_info::
__do_upcast (const __class_type_info *dst, const void *obj_ptr,
             __upcast_result &__restrict result) const
{
  if (__class_type_info::__do_upcast (dst, obj_ptr, result))
    return true;

  int src_details = result.src_details;
  if (src_details & __flags_unknown_mask)
    src_details = __flags;

  for (std::size_t i = __base_count; i--;)
    {
      __upcast_result result2 (src_details);
      const void *base = obj_ptr;
      ptrdiff_t offset = __base_info[i].__offset ();
      bool is_virtual = __base_info[i].__is_virtual_p ();
      bool is_public = __base_info[i].__is_public_p ();

      if (!is_public && !(src_details & __non_diamond_repeat_mask))
        // original cannot have an ambiguous base, so skip private bases
        continue;

      if (base)
        base = convert_to_base (base, is_virtual, offset);

      if (__base_info[i].__base_type->__do_upcast (dst, base, result2))
        {
          if (result2.base_type == nonvirtual_base_type && is_virtual)
            result2.base_type = __base_info[i].__base_type;
          if (contained_p (result2.part2dst) && !is_public)
            result2.part2dst = __sub_kind (result2.part2dst & ~__contained_public_mask);

          if (!result.base_type)
            {
              result = result2;
              if (!contained_p (result.part2dst))
                return true; // found ambiguously

              if (result.part2dst & __contained_public_mask)
                {
                  if (!(__flags & __non_diamond_repeat_mask))
                    return true;  // cannot have an ambiguous other base
                }
              else
                {
                  if (!virtual_p (result.part2dst))
                    return true; // cannot have another path
                  if (!(__flags & __diamond_shaped_mask))
                    return true; // cannot have a more accessible path
                }
            }
          else if (result.dst_ptr != result2.dst_ptr)
            {
              // Found an ambiguity.
	      result.dst_ptr = NULL;
	      result.part2dst = __contained_ambig;
	      return true;
            }
          else if (result.dst_ptr)
            {
              // Ok, found real object via a virtual path.
              result.part2dst
                  = __sub_kind (result.part2dst | result2.part2dst);
            }
          else
            {
              // Dealing with a null pointer, need to check vbase
              // containing each of the two choices.
              if (result2.base_type == nonvirtual_base_type
                  || result.base_type == nonvirtual_base_type
                  || !(*result2.base_type == *result.base_type))
                {
                  // Already ambiguous, not virtual or via different virtuals.
                  // Cannot match.
                  result.part2dst = __contained_ambig;
                  return true;
                }
              result.part2dst
                  = __sub_kind (result.part2dst | result2.part2dst);
            }
        }
    }
  return result.part2dst != __unknown;
}

}
