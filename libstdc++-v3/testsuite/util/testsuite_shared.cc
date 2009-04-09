// Copyright (C) 2004, 2005, 2006, 2007, 2009 Free Software Foundation, Inc.
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

#include <string>
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <ext/mt_allocator.h>
#include <bits/functexcept.h>

// libstdc++/22309
extern "C" void
try_allocation()
{
  typedef char value_t;

  typedef __gnu_cxx::__mt_alloc<value_t> allocator_t;

  typedef std::char_traits<value_t> traits_t; 
  typedef std::basic_string<value_t, traits_t, allocator_t> string_t;

  string_t s;
  s += "west beach, indiana dunes";
}

// libstdc++/23591
extern "C" void 
try_throw_exception()
{
  try
    {
      std::__throw_bad_exception();
    }
  catch (const std::exception& e)
    { }
}

extern "C" void 
try_function_random_fail()
{
  long seed = lrand48();
  if (seed < 2000)
    seed = 2000;

  {
    std::ostringstream s;
    s << "random_throw, seed: " << seed << std::endl;
    std::cout << s.str();
  }

  while (--seed > 0)
    {
      try_throw_exception();
    }

  // Randomly throw. See if other threads cleanup.
  std::__throw_bad_exception();
}
