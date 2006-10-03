// -*- C++ -*-
// Testing allocator for the C++ library testsuite.
//
// Copyright (C) 2002, 2003 Free Software Foundation, Inc.
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

#include <iostream>
#include <testsuite_allocator.h>

namespace __gnu_test
{
  typedef tracker_allocator_counter counter_type;
  
  counter_type::size_type 
  counter_type::allocationCount_ = 0;
  
  counter_type::size_type 
  counter_type::deallocationCount_ = 0;

  int counter_type::constructCount_ = 0;
  int counter_type::destructCount_ = 0;

  bool
  check_construct_destroy(const char* tag, int expected_c, int expected_d)
  {
    bool ret = true;
    if (counter_type::get_construct_count() != expected_c 
	|| counter_type::get_destruct_count() != expected_d)
      {
	std::cerr << tag << ": "
		  << " construct = " << counter_type::get_construct_count()
		  << " (should be " << expected_c << "),"
		  << " destroy = " << counter_type::get_destruct_count()
		  << " (should be " << expected_d << ")"
		  << std::endl;
	ret = false;
      }
    return ret;
  }
}; // namespace __cxx_test

