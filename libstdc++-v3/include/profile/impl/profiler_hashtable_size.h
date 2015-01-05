// -*- C++ -*-
//
// Copyright (C) 2009-2015 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

/** @file profile/impl/profiler_hashtable_size.h
 *  @brief Collection of hashtable size traces.
 */

// Written by Lixia Liu and Silvius Rus.

#ifndef _GLIBCXX_PROFILE_PROFILER_HASHTABLE_SIZE_H
#define _GLIBCXX_PROFILE_PROFILER_HASHTABLE_SIZE_H 1

#include "profile/impl/profiler.h"
#include "profile/impl/profiler_node.h"
#include "profile/impl/profiler_trace.h"
#include "profile/impl/profiler_state.h"
#include "profile/impl/profiler_container_size.h"

namespace __gnu_profile
{
  /** @brief Hashtable size instrumentation trace producer.  */
  class __trace_hashtable_size
  : public __trace_container_size
  {
  public:
    __trace_hashtable_size()
    : __trace_container_size()
    { __id = "hashtable-size"; }
  };

  inline void
  __trace_hashtable_size_init()
  { _GLIBCXX_PROFILE_DATA(_S_hashtable_size) = new __trace_hashtable_size(); }

  inline void
  __trace_hashtable_size_free()
  { delete _GLIBCXX_PROFILE_DATA(_S_hashtable_size); }

  inline void
  __trace_hashtable_size_report(FILE* __f, __warning_vector_t& __warnings)
  { __trace_report(_GLIBCXX_PROFILE_DATA(_S_hashtable_size), __f, __warnings); }

  inline __container_size_info*
  __trace_hashtable_size_construct(std::size_t __num)
  {
    if (!__profcxx_init())
      return 0;
  
    if (!__reentrance_guard::__get_in())
      return 0;

    __reentrance_guard __get_out;
    return _GLIBCXX_PROFILE_DATA(_S_hashtable_size)->
      __insert(__get_stack(), __num);
  }

  inline void
  __trace_hashtable_size_resize(__container_size_info* __obj_info,
				std::size_t __from, std::size_t __to)
  {
    if (!__obj_info)
      return;

    __obj_info->__resize(__from, __to);
  }

  inline void
  __trace_hashtable_size_destruct(__container_size_info* __obj_info,
				  std::size_t __num, std::size_t __inum)
  {
    if (!__obj_info)
      return;

    _GLIBCXX_PROFILE_DATA(_S_hashtable_size)->
      __destruct(__obj_info, __num, __inum);
  }

} // namespace __gnu_profile

#endif /* _GLIBCXX_PROFILE_PROFILER_HASHTABLE_SIZE_H */
