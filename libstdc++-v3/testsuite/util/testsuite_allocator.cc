// -*- C++ -*-
// Testing allocator for the C++ library testsuite.
//
// Copyright (C) 2002-2025 Free Software Foundation, Inc.
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
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

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
} // namespace __cxx_test

