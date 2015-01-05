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

/** @file profile/impl/profiler.h
 *  @brief Interface of the profiling runtime library.
 */

// Written by Lixia Liu and Silvius Rus.

#ifndef _GLIBCXX_PROFILE_PROFILER_H
#define _GLIBCXX_PROFILE_PROFILER_H 1

#include <bits/c++config.h>

// Mechanism to define data with inline linkage.
#define _GLIBCXX_PROFILE_DEFINE_UNINIT_DATA(__type, __name)             \
  inline __type&                                                        \
  __get_##__name()                                                      \
  {                                                                     \
    static __type __name;                                               \
    return __name;                                                      \
  }
#define _GLIBCXX_PROFILE_DEFINE_DATA(__type, __name, __initial_value...) \
  inline __type& __get_##__name() {                                      \
    static __type __name(__initial_value);                               \
    return __name;                                                       \
  }
#define _GLIBCXX_PROFILE_DATA(__name) \
  __get_##__name()

namespace __gnu_profile
{
  /** @brief Reentrance guard.
   *
   * Mechanism to protect all __gnu_profile operations against recursion,
   * multithreaded and exception reentrance.
   */
  struct __reentrance_guard
  {
    static bool
    __get_in()
    {
      if (__inside() == true)
	return false;
      else
	{
	  __inside() = true;
	  return true;
	}
    }

    static bool&
    __inside()
    {
      static __thread bool _S_inside(false);
      return _S_inside;
    }

    __reentrance_guard() { }
    ~__reentrance_guard() { __inside() = false; }
  };

  // Forward declarations of implementation functions.
  // Don't use any __gnu_profile:: in user code.
  // Instead, use the __profcxx... macros, which offer guarded access.
  class __container_size_info;
  class __hashfunc_info;
  class __map2umap_info;
  class __vector2list_info;
  class __list2slist_info;
  class __list2vector_info;

  bool __turn_on();
  bool __turn_off();
  bool __is_invalid();
  bool __is_on();
  bool __is_off();
  void __report();

  __container_size_info*
  __trace_hashtable_size_construct(std::size_t);
  void __trace_hashtable_size_resize(__container_size_info*,
				     std::size_t, std::size_t);
  void __trace_hashtable_size_destruct(__container_size_info*,
				       std::size_t, std::size_t);

  __hashfunc_info*
  __trace_hash_func_construct();
  void __trace_hash_func_destruct(__hashfunc_info*,
				  std::size_t, std::size_t, std::size_t);

  __container_size_info*
  __trace_vector_size_construct(std::size_t);
  void __trace_vector_size_resize(__container_size_info*,
				  std::size_t, std::size_t);
  void __trace_vector_size_destruct(__container_size_info*,
				    std::size_t, std::size_t);

  __vector2list_info*
  __trace_vector_to_list_construct();
  void __trace_vector_to_list_insert(__vector2list_info*,
				     std::size_t, std::size_t);
  void __trace_vector_to_list_iterate(__vector2list_info*, int);
  void __trace_vector_to_list_invalid_operator(__vector2list_info*);
  void __trace_vector_to_list_resize(__vector2list_info*,
				     std::size_t, std::size_t);
  void __trace_vector_to_list_destruct(__vector2list_info*);

  __list2slist_info*
  __trace_list_to_slist_construct();
  void __trace_list_to_slist_rewind(__list2slist_info*);
  void __trace_list_to_slist_operation(__list2slist_info*);
  void __trace_list_to_slist_destruct(__list2slist_info*);

  __list2vector_info*
  __trace_list_to_vector_construct();
  void __trace_list_to_vector_insert(__list2vector_info*,
				     std::size_t, std::size_t);
  void __trace_list_to_vector_iterate(__list2vector_info*, int);
  void __trace_list_to_vector_invalid_operator(__list2vector_info*);
  void __trace_list_to_vector_resize(__list2vector_info*,
				     std::size_t, std::size_t);
  void __trace_list_to_vector_destruct(__list2vector_info*);

  __map2umap_info*
  __trace_map_to_unordered_map_construct();
  void __trace_map_to_unordered_map_invalidate(__map2umap_info*);
  void __trace_map_to_unordered_map_insert(__map2umap_info*, std::size_t,
					   std::size_t);
  void __trace_map_to_unordered_map_erase(__map2umap_info*, std::size_t,
					  std::size_t);
  void __trace_map_to_unordered_map_iterate(__map2umap_info*, std::size_t);
  void __trace_map_to_unordered_map_find(__map2umap_info*, std::size_t);
  void __trace_map_to_unordered_map_destruct(__map2umap_info*);
} // namespace __gnu_profile

// Master switch turns on all diagnostics that are not explicitly turned off.
#ifdef _GLIBCXX_PROFILE
#ifndef _GLIBCXX_PROFILE_NO_HASHTABLE_TOO_SMALL
#define _GLIBCXX_PROFILE_HASHTABLE_TOO_SMALL
#endif
#ifndef _GLIBCXX_PROFILE_NO_HASHTABLE_TOO_LARGE
#define _GLIBCXX_PROFILE_HASHTABLE_TOO_LARGE
#endif
#ifndef _GLIBCXX_PROFILE_NO_VECTOR_TOO_SMALL
#define _GLIBCXX_PROFILE_VECTOR_TOO_SMALL
#endif
#ifndef _GLIBCXX_PROFILE_NO_VECTOR_TOO_LARGE
#define _GLIBCXX_PROFILE_VECTOR_TOO_LARGE
#endif
#ifndef _GLIBCXX_PROFILE_NO_INEFFICIENT_HASH
#define _GLIBCXX_PROFILE_INEFFICIENT_HASH
#endif
#ifndef _GLIBCXX_PROFILE_NO_VECTOR_TO_LIST
#define _GLIBCXX_PROFILE_VECTOR_TO_LIST
#endif
#ifndef _GLIBCXX_PROFILE_NO_LIST_TO_SLIST
#define _GLIBCXX_PROFILE_LIST_TO_SLIST
#endif
#ifndef _GLIBCXX_PROFILE_NO_LIST_TO_VECTOR
#define _GLIBCXX_PROFILE_LIST_TO_VECTOR
#endif
#ifndef _GLIBCXX_PROFILE_NO_MAP_TO_UNORDERED_MAP
#define _GLIBCXX_PROFILE_MAP_TO_UNORDERED_MAP
#endif
#endif

// Expose global management routines to user code.
#ifdef _GLIBCXX_PROFILE
#define __profcxx_report() __gnu_profile::__report()
#define __profcxx_turn_on() __gnu_profile::__turn_on()
#define __profcxx_turn_off() __gnu_profile::__turn_off()
#define __profcxx_is_invalid() __gnu_profile::__is_invalid()
#define __profcxx_is_on() __gnu_profile::__is_on()
#define __profcxx_is_off() __gnu_profile::__is_off()
#else
#define __profcxx_report()
#define __profcxx_turn_on()
#define __profcxx_turn_off()
#define __profcxx_is_invalid()
#define __profcxx_is_on()
#define __profcxx_is_off()
#endif

// Turn on/off instrumentation for HASHTABLE_TOO_SMALL and HASHTABLE_TOO_LARGE.
#if (defined(_GLIBCXX_PROFILE_HASHTABLE_TOO_SMALL) \
     || defined(_GLIBCXX_PROFILE_HASHTABLE_TOO_LARGE))
#define __profcxx_hashtable_size_construct(__x...) \
  __gnu_profile::__trace_hashtable_size_construct(__x)
#define __profcxx_hashtable_size_resize(__x...) \
  __gnu_profile::__trace_hashtable_size_resize(__x)
#define __profcxx_hashtable_size_destruct(__x...) \
  __gnu_profile::__trace_hashtable_size_destruct(__x)
#else
#define __profcxx_hashtable_size_construct(__x...) 0
#define __profcxx_hashtable_size_resize(__x...)
#define __profcxx_hashtable_size_destruct(__x...)
#endif

// Turn on/off instrumentation for VECTOR_TOO_SMALL and VECTOR_TOO_LARGE.
#if (defined(_GLIBCXX_PROFILE_VECTOR_TOO_SMALL) \
     || defined(_GLIBCXX_PROFILE_VECTOR_TOO_LARGE))
#define __profcxx_vector_size_construct(__x...) \
  __gnu_profile::__trace_vector_size_construct(__x)
#define __profcxx_vector_size_resize(__x...) \
  __gnu_profile::__trace_vector_size_resize(__x)
#define __profcxx_vector_size_destruct(__x...) \
  __gnu_profile::__trace_vector_size_destruct(__x)
#else
#define __profcxx_vector_size_construct(__x...) 0
#define __profcxx_vector_size_resize(__x...)  
#define __profcxx_vector_size_destruct(__x...) 
#endif 

// Turn on/off instrumentation for INEFFICIENT_HASH.
#if defined(_GLIBCXX_PROFILE_INEFFICIENT_HASH)
#define __profcxx_hash_func_construct(__x...) \
  __gnu_profile::__trace_hash_func_construct(__x)
#define __profcxx_hash_func_destruct(__x...) \
  __gnu_profile::__trace_hash_func_destruct(__x)
#else
#define __profcxx_hash_func_construct(__x...) 0
#define __profcxx_hash_func_destruct(__x...)
#endif

// Turn on/off instrumentation for VECTOR_TO_LIST.
#if defined(_GLIBCXX_PROFILE_VECTOR_TO_LIST)
#define __profcxx_vector2list_construct(__x...) \
  __gnu_profile::__trace_vector_to_list_construct(__x)
#define __profcxx_vector2list_insert(__x...) \
  __gnu_profile::__trace_vector_to_list_insert(__x)
#define __profcxx_vector2list_iterate(__x...) \
  __gnu_profile::__trace_vector_to_list_iterate(__x)
#define __profcxx_vector2list_invalid_operator(__x...) \
  __gnu_profile::__trace_vector_to_list_invalid_operator(__x)
#define __profcxx_vector2list_resize(__x...) \
  __gnu_profile::__trace_vector_to_list_resize(__x)
#define __profcxx_vector2list_destruct(__x...) \
  __gnu_profile::__trace_vector_to_list_destruct(__x)
#else
#define __profcxx_vector2list_construct(__x...) 0
#define __profcxx_vector2list_insert(__x...)
#define __profcxx_vector2list_iterate(__x...)
#define __profcxx_vector2list_invalid_operator(__x...)
#define __profcxx_vector2list_resize(__x...)
#define __profcxx_vector2list_destruct(__x...)
#endif

// Turn on/off instrumentation for LIST_TO_VECTOR. 
#if defined(_GLIBCXX_PROFILE_LIST_TO_VECTOR)
#define __profcxx_list2vector_construct(__x...) \
  __gnu_profile::__trace_list_to_vector_construct(__x)
#define __profcxx_list2vector_insert(__x...) \
  __gnu_profile::__trace_list_to_vector_insert(__x)
#define __profcxx_list2vector_iterate(__x...) \
  __gnu_profile::__trace_list_to_vector_iterate(__x)
#define __profcxx_list2vector_invalid_operator(__x...) \
  __gnu_profile::__trace_list_to_vector_invalid_operator(__x)
#define __profcxx_list2vector_destruct(__x...) \
  __gnu_profile::__trace_list_to_vector_destruct(__x)
#else
#define __profcxx_list2vector_construct(__x...) 0
#define __profcxx_list2vector_insert(__x...)
#define __profcxx_list2vector_iterate(__x...)
#define __profcxx_list2vector_invalid_operator(__x...)
#define __profcxx_list2vector_destruct(__x...)
#endif

// Turn on/off instrumentation for LIST_TO_SLIST.  
#if defined(_GLIBCXX_PROFILE_LIST_TO_SLIST)
#define __profcxx_list2slist_construct(__x...) \
    __gnu_profile::__trace_list_to_slist_construct(__x)
#define __profcxx_list2slist_rewind(__x...) \
  __gnu_profile::__trace_list_to_slist_rewind(__x)
#define __profcxx_list2slist_operation(__x...) \
  __gnu_profile::__trace_list_to_slist_operation(__x)
#define __profcxx_list2slist_destruct(__x...) \
  __gnu_profile::__trace_list_to_slist_destruct(__x)
#else
#define __profcxx_list2slist_construct(__x...) 0
#define __profcxx_list2slist_rewind(__x...)
#define __profcxx_list2slist_operation(__x...)
#define __profcxx_list2slist_destruct(__x...)
#endif 

// Turn on/off instrumentation for MAP_TO_UNORDERED_MAP.
#if defined(_GLIBCXX_PROFILE_MAP_TO_UNORDERED_MAP)
#define __profcxx_map2umap_construct(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_construct(__x)
#define __profcxx_map2umap_insert(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_insert(__x)
#define __profcxx_map2umap_erase(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_erase(__x)
#define __profcxx_map2umap_iterate(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_iterate(__x)
#define __profcxx_map2umap_invalidate(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_invalidate(__x)
#define __profcxx_map2umap_find(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_find(__x)
#define __profcxx_map2umap_destruct(__x...) \
  __gnu_profile::__trace_map_to_unordered_map_destruct(__x)
#else
#define __profcxx_map2umap_construct(__x...) 0
#define __profcxx_map2umap_insert(__x...)
#define __profcxx_map2umap_erase(__x...)
#define __profcxx_map2umap_iterate(__x...)
#define __profcxx_map2umap_invalidate(__x...)
#define __profcxx_map2umap_find(__x...)
#define __profcxx_map2umap_destruct(__x...)
#endif

// Set default values for compile-time customizable variables.
#ifndef _GLIBCXX_PROFILE_TRACE_PATH_ROOT
#define _GLIBCXX_PROFILE_TRACE_PATH_ROOT "libstdcxx-profile"
#endif
#ifndef _GLIBCXX_PROFILE_TRACE_ENV_VAR
#define _GLIBCXX_PROFILE_TRACE_ENV_VAR "_GLIBCXX_PROFILE_TRACE_PATH_ROOT"
#endif
#ifndef _GLIBCXX_PROFILE_MAX_WARN_COUNT_ENV_VAR
#define _GLIBCXX_PROFILE_MAX_WARN_COUNT_ENV_VAR \
  "_GLIBCXX_PROFILE_MAX_WARN_COUNT"
#endif
#ifndef _GLIBCXX_PROFILE_MAX_WARN_COUNT
#define _GLIBCXX_PROFILE_MAX_WARN_COUNT 10
#endif
#ifndef _GLIBCXX_PROFILE_MAX_STACK_DEPTH
#define _GLIBCXX_PROFILE_MAX_STACK_DEPTH 32
#endif
#ifndef _GLIBCXX_PROFILE_MAX_STACK_DEPTH_ENV_VAR
#define _GLIBCXX_PROFILE_MAX_STACK_DEPTH_ENV_VAR \
  "_GLIBCXX_PROFILE_MAX_STACK_DEPTH"
#endif
#ifndef _GLIBCXX_PROFILE_MEM_PER_DIAGNOSTIC
#define _GLIBCXX_PROFILE_MEM_PER_DIAGNOSTIC (1 << 28)
#endif
#ifndef _GLIBCXX_PROFILE_MEM_PER_DIAGNOSTIC_ENV_VAR
#define _GLIBCXX_PROFILE_MEM_PER_DIAGNOSTIC_ENV_VAR \
  "_GLIBCXX_PROFILE_MEM_PER_DIAGNOSTIC"
#endif

// Instrumentation hook implementations.
#include "profile/impl/profiler_hash_func.h"
#include "profile/impl/profiler_hashtable_size.h"
#include "profile/impl/profiler_map_to_unordered_map.h"
#include "profile/impl/profiler_vector_size.h"
#include "profile/impl/profiler_vector_to_list.h"
#include "profile/impl/profiler_list_to_slist.h"
#include "profile/impl/profiler_list_to_vector.h"

#endif // _GLIBCXX_PROFILE_PROFILER_H
