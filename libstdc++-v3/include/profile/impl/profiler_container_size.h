// -*- C++ -*-
//
// Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

/** @file profile/impl/profiler_container_size.h
 *  @brief Diagnostics for container sizes.
 */

// Written by Lixia Liu and Silvius Rus.

#ifndef _GLIBCXX_PROFILE_PROFILER_CONTAINER_SIZE_H
#define _GLIBCXX_PROFILE_PROFILER_CONTAINER_SIZE_H 1

#include <sstream>

#include "profile/impl/profiler.h"
#include "profile/impl/profiler_node.h"
#include "profile/impl/profiler_trace.h"

namespace __gnu_profile
{
  /** @brief A container size instrumentation line in the object table.  */
  class __container_size_info
  : public __object_info_base 
  {
  public:
    __container_size_info()
    : _M_init(0), _M_max(0), _M_min(0), _M_total(0), _M_item_min(0),
      _M_item_max(0), _M_item_total(0), _M_count(0), _M_resize(0), _M_cost(0) 
    { }

    __container_size_info(const __container_size_info& __o)
    : __object_info_base(__o), _M_init(__o._M_init), _M_max(__o._M_max),
      _M_min(__o._M_min), _M_total(__o._M_total),
      _M_item_min(__o._M_item_min), _M_item_max(__o._M_item_max),
      _M_item_total(__o._M_item_total), _M_count(__o._M_count),
      _M_resize(__o._M_resize), _M_cost(__o._M_cost)
    { }

    __container_size_info(__stack_t __stack, std::size_t __num)
    : __object_info_base(__stack), _M_init(__num), _M_max(__num),
      _M_min(0), _M_total(0), _M_item_min(0), _M_item_max(0),
      _M_item_total(0), _M_count(0), _M_resize(0), _M_cost(0)
    { }

    virtual ~__container_size_info() { }

    void
    __write(FILE* __f) const
    {
      std::fprintf(__f, "%Zu %Zu %Zu %Zu %Zu %Zu %Zu %Zu %Zu %Zu\n", 
		   _M_init, _M_count, _M_cost, _M_resize, _M_min, _M_max,
		   _M_total, _M_item_min, _M_item_max, _M_item_total);
    }

    float
    __magnitude() const
    { return static_cast<float>(_M_cost); }

    std::string
    __advice() const
    {
      std::stringstream __message;
      if (_M_init < _M_item_max)
	__message << "change initial container size from " << _M_init
		  << " to " << _M_item_max;
      return __message.str();
    }

    void
    __merge(const __container_size_info& __o)
    {
      _M_init        = std::max(_M_init, __o._M_init);
      _M_max         = std::max(_M_max, __o._M_max);
      _M_item_max    = std::max(_M_item_max, __o._M_item_max);
      _M_min         = std::min(_M_min, __o._M_min);
      _M_item_min    = std::min(_M_item_min, __o._M_item_min);
      _M_total      += __o._M_total;
      _M_item_total += __o._M_item_total;
      _M_count      += __o._M_count;
      _M_cost       += __o._M_cost;
      _M_resize     += __o._M_resize;
    }

    // Call if a container is destructed or cleaned.
    void
    __destruct(std::size_t __num, std::size_t __inum)
    {
      _M_max = std::max(_M_max, __num);
      _M_item_max = std::max(_M_item_max, __inum);
      if (_M_min == 0)
	{
	  _M_min = __num; 
	  _M_item_min = __inum;
	}
      else
	{
	  _M_min = std::min(_M_min, __num);
	  _M_item_min = std::min(_M_item_min, __inum);
	}
      _M_total += __num;
      _M_item_total += __inum;
      _M_count += 1;
    }

    // Estimate the cost of resize/rehash. 
    float
    __resize_cost(std::size_t __from, std::size_t)
    { return __from; }

    // Call if container is resized.
    void
    __resize(std::size_t __from, std::size_t __to)
    {
      _M_cost += this->__resize_cost(__from, __to);
      _M_resize += 1;
      _M_max = std::max(_M_max, __to);
    }

  private:
    std::size_t _M_init;
    std::size_t _M_max;  // range of # buckets
    std::size_t _M_min;
    std::size_t _M_total;
    std::size_t _M_item_min;  // range of # items
    std::size_t _M_item_max;
    std::size_t _M_item_total;
    std::size_t _M_count;
    std::size_t _M_resize;
    std::size_t _M_cost;
  };


  /** @brief A container size instrumentation line in the stack table.  */
  class __container_size_stack_info
  : public __container_size_info
  {
  public:
    __container_size_stack_info(const __container_size_info& __o)
    : __container_size_info(__o) { }
  };

  
  /** @brief Container size instrumentation trace producer.  */
  class __trace_container_size
  : public __trace_base<__container_size_info, __container_size_stack_info> 
  {
  public:
    ~__trace_container_size() { }

    __trace_container_size()
    : __trace_base<__container_size_info, __container_size_stack_info>() { };

    // Insert a new node at construct with object, callstack and initial size. 
    void
    __insert(const __object_t __obj, __stack_t __stack, std::size_t __num)
    { __add_object(__obj, __container_size_info(__stack, __num)); }

    // XXX Undefined?
    void
    __construct(const void* __obj, std::size_t __inum);
  
    // Call at destruction/clean to set container final size.
    void
    __destruct(const void* __obj, std::size_t __num, std::size_t __inum)
    {
      if (!__is_on())
	return;

      __object_t __obj_handle = static_cast<__object_t>(__obj);

      __container_size_info* __object_info = __get_object_info(__obj_handle);
      if (!__object_info)
	return;

      __object_info->__destruct(__num, __inum);
      __retire_object(__obj_handle);
    }

    // Call at resize to set resize/cost information.
    void
    __resize(const void* __obj, int __from, int __to)
    {
      if (!__is_on())
	return;

      __container_size_info* __object_info = __get_object_info(__obj);
      if (!__object_info)
	return;

      __object_info->__resize(__from, __to);
    }
  };

} // namespace __gnu_profile
#endif /* _GLIBCXX_PROFILE_PROFILER_CONTAINER_SIZE_H */
