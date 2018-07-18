// future -*- C++ -*-

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
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

#include <future>
#include <bits/functexcept.h>

namespace
{
  struct future_error_category : public std::error_category
  {
    virtual const char*
    name() const noexcept
    { return "future"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    virtual std::string message(int __ec) const
    {
      std::string __msg;
      switch (std::future_errc(__ec))
      {
      case std::future_errc::broken_promise:
          __msg = "Broken promise";
          break;
      case std::future_errc::future_already_retrieved:
          __msg = "Future already retrieved";
          break;
      case std::future_errc::promise_already_satisfied:
          __msg = "Promise already satisfied";
          break;
      case std::future_errc::no_state:
          __msg = "No associated state";
          break;
      default:
          __msg = "Unknown error";
          break;
      }
      return __msg;
    }
  };

  const future_error_category&
  __future_category_instance() noexcept
  {
    static const future_error_category __fec{};
    return __fec;
  }
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_future_error(int __i __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(future_error(make_error_code(future_errc(__i)))); }

  const error_category& future_category() noexcept
  { return __future_category_instance(); }

  future_error::~future_error() noexcept { }

  const char*
  future_error::what() const noexcept { return logic_error::what(); }

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
  __future_base::_Result_base::_Result_base() = default;

  __future_base::_Result_base::~_Result_base() = default;

  void
  __future_base::_State_baseV2::_Make_ready::_S_run(void* p)
  {
    unique_ptr<_Make_ready> mr{static_cast<_Make_ready*>(p)};
    if (auto state = mr->_M_shared_state.lock())
      {
	// Use release MO to synchronize with observers of the ready state.
	state->_M_status._M_store_notify_all(_Status::__ready,
	    memory_order_release);
      }
  }

  // defined in src/c++11/condition_variable.cc
  extern void
  __at_thread_exit(__at_thread_exit_elt* elt);

  void
  __future_base::_State_baseV2::_Make_ready::_M_set()
  {
    _M_cb = &_Make_ready::_S_run;
    __at_thread_exit(this);
  }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
