// -*- C++ -*-
// Utilities for testing threads for the C++ library testsuite.
//
// Copyright (C) 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _GLIBCXX_TESTSUITE_THREAD_H
#define _GLIBCXX_TESTSUITE_THREAD_H

#include <sstream>
#include <stdexcept>
#include <type_traits>

// C++0x only.
namespace __gnu_test
{  
  // Assume _Tp::native_handle_type.
  // Check C++ to native_handle_type characteristics: size and alignment.
  template<typename _Tp>
    void
    compare_type_to_native_type()
    {
      typedef _Tp test_type;

      // Remove possible pointer type.
      typedef typename test_type::native_handle_type native_handle;
      typedef typename std::remove_pointer<native_handle>::type native_type;

      int st = sizeof(test_type);
      int snt = sizeof(native_type);      
      int at = __alignof__(test_type);
      int ant = __alignof__(native_type);
      if (st != snt || at != ant)
	{
	  std::ostringstream s;
	  s << std::endl;
	  s << "size of _Tp: " << st << std::endl;
	  s << "alignment of _Tp: " << st << std::endl;
	  s << "size of *(_Tp::native_handle_type): " << snt << std::endl;
	  s << "alignment of *(_Tp::native_handle_type): " << snt << std::endl;
	  throw std::runtime_error(s.str());
	}
    }
} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_THREAD_H

