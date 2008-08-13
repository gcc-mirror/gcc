// Exception Handling support header (exception_ptr class) for -*- C++ -*-

// Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
// 2004, 2005, 2006, 2007, 2008
// Free Software Foundation
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
// 
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file exception_ptr.h
 *  This is an internal header file, included by other headers and the
 *  implementation. You should not attempt to use it directly.
 */

#ifndef __EXCEPTION_PTR_H__
#define __EXCEPTION_PTR_H__

#pragma GCC visibility push(default)

#include <bits/c++config.h>

extern "C++" {

namespace std 
{
  // Hide the free operators from other types
  namespace __exception_ptr
  {
    /**
     * @brief An opaque pointer to an arbitrary exception.
     */
    class exception_ptr;
  }

  using __exception_ptr::exception_ptr;

  /** Obtain an %exception_ptr to the currently handled exception. If there
   *  is none, or the currently handled exception is foreign, return the null
   *  value.
   */
  exception_ptr current_exception() throw();

  /// Throw the object pointed to by the %exception_ptr.
  void rethrow_exception(exception_ptr) __attribute__ ((__noreturn__));

  /// Obtain an %exception_ptr pointing to a copy of the supplied object.
  template <class _Ex>
  exception_ptr copy_exception(_Ex __ex) throw();


  namespace __exception_ptr
  {
    bool operator==(const exception_ptr&,
                    const exception_ptr&) throw();
    bool operator!=(const exception_ptr&,
                    const exception_ptr&) throw();

    class exception_ptr
    {
      void* _M_exception_object;

      explicit exception_ptr(void* __e) throw();

      void _M_addref() throw();
      void _M_release() throw();

      void *_M_get() const throw();

      void _M_safe_bool_dummy();

      friend exception_ptr std::current_exception() throw();
      friend void std::rethrow_exception(exception_ptr);

    public:
      exception_ptr() throw();

      typedef void (exception_ptr::*__safe_bool)();

      // For construction from nullptr or 0.
      exception_ptr(__safe_bool) throw();

      exception_ptr(const exception_ptr&) throw();

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      exception_ptr(exception_ptr&& __o) throw()
        : _M_exception_object(__o._M_exception_object)
      {
        __o._M_exception_object = 0;
      }
#endif

      exception_ptr& operator=(const exception_ptr&) throw();

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      exception_ptr& operator=(exception_ptr&& __o) throw()
      {
        exception_ptr(__o).swap(*this);
        return *this;
      }
#endif

      ~exception_ptr() throw();

      void swap(exception_ptr&) throw();

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      void swap(exception_ptr &&__o) throw()
      {
        void *__tmp = _M_exception_object;
        _M_exception_object = __o._M_exception_object;
        __o._M_exception_object = __tmp;
      }
#endif

      bool operator!() const throw();
      operator __safe_bool() const throw();

      friend bool operator==(const exception_ptr&,
                             const exception_ptr&) throw();

      const type_info *__cxa_exception_type() const throw();
    };

  } // namespace __exception_ptr


  template <class _Ex>
  exception_ptr copy_exception(_Ex __ex) throw()
  {
    try
      {
        throw __ex;
      }
    catch(...)
      {
        return current_exception ();
      }
  }

} // namespace std

} // extern "C++"

#pragma GCC visibility pop

#endif
