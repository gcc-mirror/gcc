// { dg-do compile }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <stdexcept>

namespace gnu
{
  typedef std::logic_error t1;
  typedef std::domain_error t2;
  typedef std::invalid_argument t3;
  typedef std::length_error t4;
  typedef std::out_of_range t5;
  typedef std::runtime_error t6;
  typedef std::range_error t7;
  typedef std::overflow_error t8;
  typedef std::underflow_error t9;
}
