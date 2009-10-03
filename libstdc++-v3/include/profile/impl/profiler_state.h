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

/** @file profile/impl/profiler_state.cc
 *  @brief Global profiler state.
 */

// Written by Lixia Liu and Silvius Rus.

#ifndef PROFCXX_PROFILER_STATE_H__
#define PROFCXX_PROFILER_STATE_H__ 1

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <cstdio>
#else
#include <stdio.h>
#endif

namespace __cxxprof_impl
{

/** @brief Profiling mode on/off state.  */
template <int _Unused=0>
class __state
{
 public:

  static __state<_Unused>* _S_diag_state;

  __state() : _M_state(__INVALID) {}
  ~__state() {}

  bool __is_on() { return _M_state == __ON; }
  bool __is_off() { return _M_state == __OFF; }
  bool __is_invalid() { return _M_state == __INVALID; }
  void __turn_on() { _M_state = __ON; }
  void __turn_off() { _M_state = __OFF; }

 private:
  enum __state_type { __ON, __OFF, __INVALID };
  __state_type _M_state;
};

template <int _Unused>
__state<_Unused>* __state<_Unused>::_S_diag_state = NULL;

inline bool __is_on()
{
  return __state<0>::_S_diag_state && __state<0>::_S_diag_state->__is_on();
}

inline bool __is_off()
{
  return __state<0>::_S_diag_state && __state<0>::_S_diag_state->__is_off();
}

inline bool __is_invalid()
{
  return (!__state<0>::_S_diag_state 
          || __state<0>::_S_diag_state->__is_invalid());
}

inline void __turn_on()
{
  if (!__state<0>::_S_diag_state) { 
    __state<0>::_S_diag_state = new __state<0>();
  }
  __state<0>::_S_diag_state->__turn_on();
}

inline void __turn_off()
{
  if (!__state<0>::_S_diag_state) { 
    __state<0>::_S_diag_state = new __state<0>();
  }
  __state<0>::_S_diag_state->__turn_off();
}

} // end namespace __cxxprof_impl
#endif /* PROFCXX_PROFILER_STATE_H__ */
