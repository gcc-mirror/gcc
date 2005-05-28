// Expected execution error for PR19495.
// { dg-do run { xfail powerpc*-*-linux* } }

// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <string>
#include <iostream>
#include <ext/array_allocator.h>
#include <testsuite_hooks.h>

typedef char char_type;
typedef std::char_traits<char_type> traits_type;
typedef std::tr1::array<char_type, 32> array_type;

array_type extern_array;

void test01() 
{
  bool test __attribute__((unused)) = true;

  using std::basic_string;
  typedef __gnu_cxx::array_allocator<char_type, array_type> allocator_type;
  typedef basic_string<char_type, traits_type, allocator_type> string_type;

  size_t index = array_type::_S_index;
  allocator_type a(&extern_array);
  string_type s(a);
    
  try
    {
      s.reserve(4); // Actually need 4 + 1 + sizeof(std::string::_Rep).
    }
  catch(std::bad_alloc& obj)
    {
      VERIFY( false );
    }
  catch(...)
    {
      VERIFY( false );
    }

  s.append(1, 'c');
  s.append(2, 'b');

  std::cout << s.c_str() << std::endl;
}

int main()
{
  test01();
  return 0;
}
