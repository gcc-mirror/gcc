// -*- C++ -*-
//
// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

/** @file profile/impl/profiler_list_to_vector.h
 *  @brief diagnostics for list to vector.
 */

// Written by Changhee Jung.

#ifndef _GLIBCXX_PROFILE_PROFILER_LIST_TO_VECTOR_H
#define _GLIBCXX_PROFILE_PROFILER_LIST_TO_VECTOR_H 1

#include <sstream>

#include "profile/impl/profiler.h"
#include "profile/impl/profiler_node.h"
#include "profile/impl/profiler_trace.h"

namespace __gnu_profile
{
  /** @brief A list-to-vector instrumentation line in the object table.  */
  class __list2vector_info
  : public __object_info_base
  {
  public:
    __list2vector_info(__stack_t __stack)
    : __object_info_base(__stack), _M_shift_count(0), _M_iterate(0),
      _M_resize(0), _M_list_cost(0), _M_vector_cost(0),
      _M_max_size(0) { }

    void
    __merge(const __list2vector_info& __o)
    {
      __object_info_base::__merge(__o);
      _M_shift_count  += __o._M_shift_count;
      _M_iterate      += __o._M_iterate;
      _M_vector_cost  += __o._M_vector_cost;
      _M_list_cost    += __o._M_list_cost;
      _M_resize       += __o._M_resize;
      _M_max_size     = std::max( _M_max_size, __o._M_max_size);
    }

    void
    __write(FILE* __f) const
    {
      std::fprintf(__f, "%Zu %Zu %Zu %.0f %.0f\n", _M_shift_count,
		   _M_resize, _M_iterate, _M_vector_cost, _M_list_cost);
    }

    float
    __magnitude() const
    { return _M_list_cost - _M_vector_cost; }
  
    std::string
    __advice() const
    {
      std::stringstream __sstream;
      __sstream 
	<< "change std::list to std::vector and its initial size from 0 to "
	<< _M_max_size;
      return __sstream.str();
    }

    std::size_t
    __shift_count()
    { return _M_shift_count; }
  
    std::size_t
    __iterate()
    { return _M_iterate; }
  
    float
    __list_cost()
    { return _M_list_cost; }
  
    std::size_t
    __resize()
    { return _M_resize; }
  
    void
    __set_list_cost(float __lc)
    { _M_list_cost = __lc; }
    
    void
    __set_vector_cost(float __vc)
    { _M_vector_cost = __vc; }
    
    void
    __opr_insert(std::size_t __shift, std::size_t __size)
    {
      _M_shift_count += __shift;
      _M_max_size = std::max(_M_max_size, __size);
    }

    void
    __opr_iterate(int __num)
    { __gnu_cxx::__atomic_add(&_M_iterate, __num); }

    void
    __resize(std::size_t __from, std::size_t)
    { _M_resize += __from; }

  private:
    std::size_t _M_shift_count;
    mutable _Atomic_word _M_iterate;
    std::size_t _M_resize;
    float _M_list_cost;
    float _M_vector_cost;
    std::size_t _M_max_size;
  };

  class __list2vector_stack_info 
  : public __list2vector_info
  {
  public:
    __list2vector_stack_info(const __list2vector_info& __o) 
    : __list2vector_info(__o) {}
  };

  class __trace_list_to_vector
  : public __trace_base<__list2vector_info, __list2vector_stack_info> 
  {
  public:
    __trace_list_to_vector()
    : __trace_base<__list2vector_info, __list2vector_stack_info>()
    { __id = "list-to-vector"; }

    ~__trace_list_to_vector() { }

    // Call at destruction/clean to set container final size.
    void
    __destruct(__list2vector_info* __obj_info)
    {
      float __vc = __vector_cost(__obj_info->__shift_count(),
				 __obj_info->__iterate());
      float __lc = __list_cost(__obj_info->__shift_count(),
			       __obj_info->__iterate());
      __obj_info->__set_vector_cost(__vc);
      __obj_info->__set_list_cost(__lc);
      __retire_object(__obj_info);
    }

    // Collect cost of operations.
    float
    __vector_cost(std::size_t __shift, std::size_t __iterate)
    {
      // The resulting vector will use a 'reserve' method.
      return (__shift
	      * _GLIBCXX_PROFILE_DATA(__vector_shift_cost_factor).__value
	      + __iterate
	      * _GLIBCXX_PROFILE_DATA(__vector_iterate_cost_factor).__value); 
    }

    float
    __list_cost(std::size_t __shift, std::size_t __iterate)
    {
      return (__shift
	      * _GLIBCXX_PROFILE_DATA(__list_shift_cost_factor).__value
	      + __iterate
	      * _GLIBCXX_PROFILE_DATA(__list_iterate_cost_factor).__value); 
    }
  };


  inline void
  __trace_list_to_vector_init()
  { _GLIBCXX_PROFILE_DATA(_S_list_to_vector) = new __trace_list_to_vector(); }

  inline void
  __trace_list_to_vector_free()
  { delete _GLIBCXX_PROFILE_DATA(_S_list_to_vector); }

  inline void
  __trace_list_to_vector_report(FILE* __f, __warning_vector_t& __warnings)
  { __trace_report(_GLIBCXX_PROFILE_DATA(_S_list_to_vector), __f, __warnings); }

  inline __list2vector_info*
  __trace_list_to_vector_construct()
  {
    if (!__profcxx_init())
      return 0;

    if (!__reentrance_guard::__get_in())
      return 0;

    __reentrance_guard __get_out;
    return _GLIBCXX_PROFILE_DATA(_S_list_to_vector)
      ->__add_object(__get_stack());
  }

  inline void
  __trace_list_to_vector_insert(__list2vector_info* __obj_info,
				std::size_t __shift, std::size_t __size)
  {
    if (!__obj_info)
      return;

    __obj_info->__opr_insert(__shift, __size);
  }

  inline void
  __trace_list_to_vector_iterate(__list2vector_info* __obj_info,
				 int)
  {
    if (!__obj_info)
      return;

    // We only collect if an iteration took place no matter in what side.
    __obj_info->__opr_iterate(1);
  }

  inline void
  __trace_list_to_vector_invalid_operator(__list2vector_info* __obj_info)
  {
    if (!__obj_info)
      return;

    __obj_info->__set_invalid();
  }

  inline void
  __trace_list_to_vector_resize(__list2vector_info* __obj_info, 
				std::size_t __from, std::size_t __to)
  {
    if (!__obj_info)
      return;

    __obj_info->__resize(__from, __to);
  }

  inline void
  __trace_list_to_vector_destruct(__list2vector_info* __obj_info)
  {
    if (!__obj_info)
      return;

    _GLIBCXX_PROFILE_DATA(_S_list_to_vector)->__destruct(__obj_info);
  }

} // namespace __gnu_profile
#endif /* _GLIBCXX_PROFILE_PROFILER_LIST_TO_VECTOR_H__ */
