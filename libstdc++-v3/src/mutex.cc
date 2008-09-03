// mutex -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <mutex>

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)

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
  // explicit instantiation due to -fno-implicit-instantiation
  template class function<void()>;
  function<void()> __once_functor;
  mutex __once_mutex;
  unique_lock<mutex> __once_functor_lock(__once_mutex, defer_lock);
#endif

  extern "C"
  {
    void __once_proxy()
    {
#ifndef _GLIBCXX_HAVE_TLS
      function<void()> __once_call = std::move(__once_functor);
      __once_functor_lock.unlock();
#endif
      __once_call();
    }
  }
}

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
