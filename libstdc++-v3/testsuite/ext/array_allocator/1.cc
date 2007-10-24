// Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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

#include <ext/vstring.h>
#include <ext/array_allocator.h>
#include <testsuite_hooks.h>

typedef char char_type;
typedef std::char_traits<char_type> traits_type;
typedef std::tr1::array<char_type, 4> array_type;

array_type extern_array;

void test01() 
{
  bool test __attribute__((unused)) = true;

  using __gnu_cxx::__versa_string;
  typedef __gnu_cxx::array_allocator<char_type, array_type> allocator_type;
  typedef __versa_string<char_type, traits_type, allocator_type> string_type;

  allocator_type a(&extern_array);
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
