// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

// { dg-options "-Wno-deprecated" }

#include <ext/vstring.h>
#include <ext/array_allocator.h>
#include <testsuite_hooks.h>

typedef char char_type;
typedef std::char_traits<char_type> traits_type;
typedef std::tr1::array<char_type, 4> array_type;

array_type extern_array;

void test01() 
{
  using __gnu_cxx::__versa_string;
  typedef __gnu_cxx::array_allocator<char_type, array_type> allocator_type;
  typedef __versa_string<char_type, traits_type, allocator_type> string_type;

  // Construct array_allocator without underlying array.
  allocator_type a;
  string_type s(a);
    
  try
    {
      s.reserve(4); // Actually need 4 + 1 + sizeof(std::string::_Rep).
    }
  catch(std::bad_alloc& obj)
    {
      VERIFY( true );
    }
  catch(...)
    {
      VERIFY( false );
    }
}

int main()
{
  test01();
  return 0;
}
