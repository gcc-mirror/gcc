// -*- C++ -*-
//
// Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

/** @file profile/impl/profiler_map_to_unordered_map.h
 *  @brief Diagnostics for map to unordered_map.
 */

// Written by Silvius Rus.

#ifndef _GLIBCXX_PROFILE_PROFILER_MAP_TO_UNORDERED_MAP_H
#define _GLIBCXX_PROFILE_PROFILER_MAP_TO_UNORDERED_MAP_H 1

#include "profile/impl/profiler.h"
#include "profile/impl/profiler_node.h"
#include "profile/impl/profiler_trace.h"

namespace __gnu_profile
{
  inline int
  __log2(std::size_t __size)
  {
    for (int __bit_count = sizeof(std::size_t) - 1; __bit_count >= 0;
	 -- __bit_count)
      if ((2 << __bit_count) & __size)
	return __bit_count;
    return 0;
  }

  inline float
  __map_insert_cost(std::size_t __size)
  { return (_GLIBCXX_PROFILE_DATA(__map_insert_cost_factor).__value
	    * static_cast<float>(__log2(__size))); }

  inline float
  __map_erase_cost(std::size_t __size)
  { return (_GLIBCXX_PROFILE_DATA(__map_erase_cost_factor).__value
	    * static_cast<float>(__log2(__size))); }

  inline float
  __map_find_cost(std::size_t __size)
  { return (_GLIBCXX_PROFILE_DATA(__map_find_cost_factor).__value
	    * static_cast<float>(__log2(__size))); }

  /** @brief A map-to-unordered_map instrumentation line in the
      object table.  */
  class __map2umap_info
  : public __object_info_base
  {
  public:
    __map2umap_info(__stack_t __stack)
    : __object_info_base(__stack), _M_insert(0), _M_erase(0), _M_find(0),
      _M_iterate(0), _M_umap_cost(0.0), _M_map_cost(0.0)
    { }

    void
    __merge(const __map2umap_info& __o)
    {
      __object_info_base::__merge(__o);
      _M_insert		+= __o._M_insert;
      _M_erase		+= __o._M_erase;
      _M_find		+= __o._M_find;
      _M_iterate	+= __o._M_iterate;
      _M_umap_cost	+= __o._M_umap_cost;
      _M_map_cost	+= __o._M_map_cost;
    }

    void
    __write(FILE* __f) const
    {
      std::fprintf(__f, "%Zu %Zu %Zu %Zu %.0f %.0f\n",
		   _M_insert, _M_erase, _M_find, _M_iterate, _M_map_cost,
		   _M_umap_cost);
    }

    float
    __magnitude() const
    { return _M_map_cost - _M_umap_cost; }

    std::string
    __advice() const
    { return "prefer an unordered container"; }

    void
    __record_insert(std::size_t __size, std::size_t __count)
    {
      ++_M_insert;
      if (__count)
	{
	  _M_map_cost += __count * __map_insert_cost(__size);
	  _M_umap_cost
	    += (__count
		* _GLIBCXX_PROFILE_DATA(__umap_insert_cost_factor).__value);
	}
    }

    void
    __record_erase(std::size_t __size, std::size_t __count)
    {
      ++_M_erase;
      if (__count)
	{
	  _M_map_cost += __count * __map_erase_cost(__size);
	  _M_umap_cost
	    += (__count
		* _GLIBCXX_PROFILE_DATA(__umap_erase_cost_factor).__value);
	}
    }

    void
    __record_find(std::size_t __size)
    {
      ++_M_find;
      _M_map_cost += __map_find_cost(__size);
      _M_umap_cost += _GLIBCXX_PROFILE_DATA(__umap_find_cost_factor).__value;
    }

    void
    __record_iterate(int __count)
    { __gnu_cxx::__atomic_add(&_M_iterate, __count); }

    void
    __set_iterate_costs()
    {
      _M_umap_cost
	+= (_M_iterate
	    * _GLIBCXX_PROFILE_DATA(__umap_iterate_cost_factor).__value);
      _M_map_cost
	+= (_M_iterate
	    * _GLIBCXX_PROFILE_DATA(__map_iterate_cost_factor).__value);
    }

  private:
    std::size_t _M_insert;
    std::size_t _M_erase;
    std::size_t _M_find;
    mutable _Atomic_word _M_iterate;
    float _M_umap_cost;
    float _M_map_cost;
  };


  /** @brief A map-to-unordered_map instrumentation line in the
      stack table.  */
  class __map2umap_stack_info
  : public __map2umap_info
  {
  public:
    __map2umap_stack_info(const __map2umap_info& __o)
    : __map2umap_info(__o) { }
  };

  /** @brief Map-to-unordered_map instrumentation producer.  */
  class __trace_map2umap
  : public __trace_base<__map2umap_info, __map2umap_stack_info>
  {
  public:
    __trace_map2umap()
    : __trace_base<__map2umap_info, __map2umap_stack_info>()
    { __id = "ordered-to-unordered"; }

    // Call at destruction/clean to set container final size.
    void
    __destruct(__map2umap_info* __obj_info)
    {
      __obj_info->__set_iterate_costs();
      __retire_object(__obj_info);
    }
  };

  inline void
  __trace_map_to_unordered_map_init()
  { _GLIBCXX_PROFILE_DATA(_S_map2umap) = new __trace_map2umap(); }

  inline void
  __trace_map_to_unordered_map_free()
  { delete _GLIBCXX_PROFILE_DATA(_S_map2umap); }

  inline void
  __trace_map_to_unordered_map_report(FILE* __f,
				      __warning_vector_t& __warnings)
  { __trace_report(_GLIBCXX_PROFILE_DATA(_S_map2umap), __f, __warnings); }

  inline __map2umap_info*
  __trace_map_to_unordered_map_construct()
  {
    if (!__profcxx_init())
      return 0;

    if (!__reentrance_guard::__get_in())
      return 0;

    __reentrance_guard __get_out;
    return _GLIBCXX_PROFILE_DATA(_S_map2umap)->__add_object(__get_stack());
  }

  inline void
  __trace_map_to_unordered_map_insert(__map2umap_info* __info,
				      std::size_t __size, std::size_t __count)
  {
    if (!__info)
      return;

    __info->__record_insert(__size, __count);
  }

  inline void
  __trace_map_to_unordered_map_erase(__map2umap_info* __info,
				     std::size_t __size, std::size_t __count)
  {
    if (!__info)
      return;

    __info->__record_erase(__size, __count);
  }

  inline void
  __trace_map_to_unordered_map_find(__map2umap_info* __info,
				    std::size_t __size)
  {
    if (!__info)
      return;

    __info->__record_find(__size);
  }

  inline void
  __trace_map_to_unordered_map_iterate(__map2umap_info* __info,
				       int)
  {
    if (!__info)
      return;

    // We only collect if an iteration took place no matter in what side.
    __info->__record_iterate(1);
  }

  inline void
  __trace_map_to_unordered_map_invalidate(__map2umap_info* __info)
  {
    if (!__info)
      return;

    __info->__set_invalid();
  }

  inline void
  __trace_map_to_unordered_map_destruct(__map2umap_info* __info)
  {
    if (!__info)
      return;

    _GLIBCXX_PROFILE_DATA(_S_map2umap)->__destruct(__info);
  }
} // namespace __gnu_profile
#endif /* _GLIBCXX_PROFILE_PROFILER_MAP_TO_UNORDERED_MAP_H */
