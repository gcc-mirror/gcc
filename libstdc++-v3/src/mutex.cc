// mutex -*- C++ -*-

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

#include <mutex>

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
#ifndef _GLIBCXX_HAVE_TLS
namespace
{
  std::mutex&
  get_once_mutex()
  {
    static std::mutex once_mutex;
    return once_mutex;
  }
}
#endif

namespace std
{
  const defer_lock_t defer_lock = defer_lock_t();
  const try_to_lock_t try_to_lock = try_to_lock_t();
  const adopt_lock_t adopt_lock = adopt_lock_t();

  const char*
  lock_error::what() const throw()
  { return "std::lock_error"; }

#ifdef _GLIBCXX_HAVE_TLS
  __thread void* __once_callable;
  __thread void (*__once_call)();
#else
  // Explicit instantiation due to -fno-implicit-instantiation.
  template class function<void()>;
  function<void()> __once_functor;

  unique_lock<mutex>&
  __get_once_functor_lock()
  {
    static unique_lock<mutex> once_functor_lock(get_once_mutex(), defer_lock);
    return once_functor_lock;
  }
#endif

  extern "C"
  {
    void __once_proxy()
    {
#ifndef _GLIBCXX_HAVE_TLS
      function<void()> __once_call = std::move(__once_functor);
      __get_once_functor_lock().unlock();
#endif
      __once_call();
    }
  }
}

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
