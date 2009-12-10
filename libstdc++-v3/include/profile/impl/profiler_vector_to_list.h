// -*- C++ -*-
//
// Copyright (C) 2009 Free Software Foundation, Inc.
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

/** @file profile/impl/profiler_trace.h
 *  @brief Data structures to represent profiling traces.
 */

// Written by Lixia Liu and Silvius Rus.

#ifndef _GLIBCXX_PROFILE_PROFILER_VECTOR_TO_LIST_H
#define _GLIBCXX_PROFILE_PROFILER_VECTOR_TO_LIST_H 1

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <cstdio>
#include <cstdlib>
#include <cstring>
#else
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#endif
#include "profile/impl/profiler.h"
#include "profile/impl/profiler_node.h"
#include "profile/impl/profiler_trace.h"

namespace __gnu_profile
{
  /** @brief A vector-to-list instrumentation line in the object table.  */
  class __vector2list_info: public __object_info_base
  {
  public:
    __vector2list_info()
    :_M_shift_count(0), _M_iterate(0), _M_resize(0), _M_list_cost(0),
     _M_vector_cost(0), _M_valid(true) { }

    __vector2list_info(__stack_t __stack)
    : __object_info_base(__stack), _M_shift_count(0), _M_iterate(0),
      _M_resize(0), _M_list_cost(0), _M_vector_cost(0), _M_valid(true) { } 

    virtual ~__vector2list_info() { }
    __vector2list_info(const __vector2list_info& __o);

    void __merge(const __vector2list_info& __o);
    void __write(FILE* __f) const;
    float __magnitude() const { return _M_vector_cost - _M_list_cost; }
    const char* __advice() const { return "change std::vector to std::list"; }

    size_t __shift_count() { return _M_shift_count; }
    size_t __iterate()   { return _M_iterate; }
    float __list_cost() { return _M_list_cost; }
    size_t __resize() { return _M_resize; }
    void __set_list_cost(float __lc) { _M_list_cost = __lc; }
    void __set_vector_cost(float __vc) { _M_vector_cost = __vc; }
    bool __is_valid() { return _M_valid; }
    void __set_invalid() { _M_valid = false; }

    void __opr_insert(size_t __pos, size_t __num);
    void __opr_iterate(size_t __num) { _M_iterate += __num; }
    void __resize(size_t __from, size_t __to);

  private:
    size_t _M_shift_count;
    size_t _M_iterate;
    size_t _M_resize;
    float _M_list_cost;
    float _M_vector_cost;
    bool  _M_valid;
  };

  inline __vector2list_info::__vector2list_info(const __vector2list_info& __o)
  : __object_info_base(__o)
  {
    _M_shift_count  = __o._M_shift_count;
    _M_iterate      = __o._M_iterate;
    _M_vector_cost  = __o._M_vector_cost;
    _M_list_cost    = __o._M_list_cost;
    _M_valid        = __o._M_valid;
    _M_resize       = __o._M_resize;
  }

  inline void 
  __vector2list_info::__merge(const __vector2list_info& __o)
  {
    _M_shift_count  += __o._M_shift_count;
    _M_iterate      += __o._M_iterate;
    _M_vector_cost  += __o._M_vector_cost;
    _M_list_cost    += __o._M_list_cost;
    _M_valid        &= __o._M_valid;
    _M_resize       += __o._M_resize;
  }

  inline void 
  __vector2list_info::__opr_insert(size_t __pos, size_t __num)
  { _M_shift_count += __num - __pos; }

  inline void 
  __vector2list_info::__resize(size_t __from, size_t __to)
  { _M_resize += __from; }

  /** @brief A vector-to-list instrumentation line in the stack table.  */
  class __vector2list_stack_info: public __vector2list_info 
  {
  public:
    __vector2list_stack_info(const __vector2list_info& __o) 
    : __vector2list_info(__o) { }
  };

  /** @brief Vector-to-list instrumentation producer.  */
  class __trace_vector_to_list
  : public __trace_base<__vector2list_info, __vector2list_stack_info> 
  {
  public:
    __trace_vector_to_list();
    ~__trace_vector_to_list() { }

    // Insert a new node at construct with object, callstack and initial size. 
    void __insert(__object_t __obj, __stack_t __stack);

    // Call at destruction/clean to set container final size.
    void __destruct(const void* __obj);

    // Find the node in the live map.
    __vector2list_info* __find(const void* __obj);

    // Collect cost of operations.
    void __opr_insert(const void* __obj, size_t __pos, size_t __num);
    void __opr_iterate(const void* __obj, size_t __num);
    void __invalid_operator(const void* __obj);
    void __resize(const void* __obj, size_t __from, size_t __to);
    float __vector_cost(size_t __shift, size_t __iterate, size_t __resize);
    float __list_cost(size_t __shift, size_t __iterate, size_t __resize);
  };

  inline __trace_vector_to_list::__trace_vector_to_list()
  : __trace_base<__vector2list_info, __vector2list_stack_info>()
  { __id = "vector-to-list"; }

  inline void 
  __trace_vector_to_list::__insert(__object_t __obj, __stack_t __stack)
  { __add_object(__obj, __vector2list_info(__stack)); }

  inline void 
  __vector2list_info::__write(FILE* __f) const
  {
    fprintf(__f, "%Zu %Zu %Zu %.0f %.0f\n", _M_shift_count, _M_resize, 
	    _M_iterate, _M_vector_cost, _M_list_cost);
  }

  // Cost model.  
  //  Vector operation cost:
  //   - Cost per shift: 1
  //   - Cost per access: 1
  //   - Cost per resize: 1
  //  List operation cost:
  //   - Cost per shift: 0
  //   - Cost per access: 10
  //   - Cost per resize: 0
  // XXX: get this from the cost model database instead.
  inline float 
  __trace_vector_to_list::__vector_cost(size_t __shift, size_t __iterate,
					size_t __resize)
  { return __shift * 1 + __iterate * 1 + __resize * 1; }

  inline float 
  __trace_vector_to_list::__list_cost(size_t __shift, size_t __iterate,
				      size_t __resize)
  { return __shift * 0 + __iterate * 10 + __resize * 0; }

  inline void 
  __trace_vector_to_list::__destruct(const void* __obj)
  {
    if (!__is_on())
      return;

    __vector2list_info* __res = __get_object_info(__obj);
    if (!__res)
      return;

    float __vc = __vector_cost(__res->__shift_count(), __res->__iterate(),
			       __res->__resize());
    float __lc = __list_cost(__res->__shift_count(), __res->__iterate(),
			     __res->__resize());
    __res->__set_vector_cost(__vc);
    __res->__set_list_cost(__lc);

    __retire_object(__obj);
  }

  inline void 
  __trace_vector_to_list::__opr_insert(const void* __obj, size_t __pos, 
				       size_t __num)
  {
    __vector2list_info* __res = __get_object_info(__obj);
    if (__res)
      __res->__opr_insert(__pos, __num);
  }

  inline void 
  __trace_vector_to_list::__opr_iterate(const void* __obj, size_t __num)
  {
    __vector2list_info* __res = __get_object_info(__obj);
    if (__res)
      __res->__opr_iterate(__num);
  }

  inline void 
  __trace_vector_to_list::__invalid_operator(const void* __obj)
  {
    __vector2list_info* __res = __get_object_info(__obj);
    if (__res)
      __res->__set_invalid();
  }

  inline void 
  __trace_vector_to_list::__resize(const void* __obj, size_t __from, 
				   size_t __to)
  {
    __vector2list_info* __res = __get_object_info(__obj);
    if (__res)
      __res->__resize(__from, __to);
  }


  // Initialization and report.
  inline void 
  __trace_vector_to_list_init()
  { __tables<0>::_S_vector_to_list = new __trace_vector_to_list(); }

  inline void 
  __trace_vector_to_list_report(FILE* __f, __warning_vector_t& __warnings)
  {
    if (__tables<0>::_S_vector_to_list) 
      {
	__tables<0>::_S_vector_to_list->__collect_warnings(__warnings);
	__tables<0>::_S_vector_to_list->__write(__f);
      }
  }

  // Implementations of instrumentation hooks.
  inline void 
  __trace_vector_to_list_construct(const void* __obj)
  {
    if (!__profcxx_init()) return;
    __tables<0>::_S_vector_to_list->__insert(__obj, __get_stack());
  }

  inline void 
  __trace_vector_to_list_destruct(const void* __obj)
  {
    if (!__profcxx_init()) return;
    __tables<0>::_S_vector_to_list->__destruct(__obj);
  }

  inline void 
  __trace_vector_to_list_insert(const void* __obj, size_t __pos, size_t __num)
  {
    if (!__profcxx_init()) return;
    __tables<0>::_S_vector_to_list->__opr_insert(__obj, __pos, __num);
  }

  inline void 
  __trace_vector_to_list_iterate(const void* __obj, size_t __num)
  {
    if (!__profcxx_init()) return;
    __tables<0>::_S_vector_to_list->__opr_iterate(__obj, __num);
  }

  inline void 
  __trace_vector_to_list_invalid_operator(const void* __obj)
  {
    if (!__profcxx_init()) return;
    __tables<0>::_S_vector_to_list->__invalid_operator(__obj);
  }

  inline void 
  __trace_vector_to_list_resize(const void* __obj, size_t __from, size_t __to)
  {
    if (!__profcxx_init()) return;
    __tables<0>::_S_vector_to_list->__resize(__obj, __from, __to);
  }
} // namespace __gnu_profile
#endif /* _GLIBCXX_PROFILE_PROFILER_VECTOR_TO_LIST_H */
