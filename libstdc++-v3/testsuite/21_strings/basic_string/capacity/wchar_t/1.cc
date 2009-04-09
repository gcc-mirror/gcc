// 1999-05-11 bkoz

// Copyright (C) 1999, 2002, 2003, 2004, 2009 Free Software Foundation, Inc.
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

// 21.3.3 string capacity

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  // POD types : resize, capacity, reserve
  bool test __attribute__((unused)) = true;
  std::wstring str01;
  typedef std::wstring::size_type size_type_s;

  size_type_s sz01 = str01.capacity();
  str01.reserve(100);
  size_type_s sz02 = str01.capacity();
  VERIFY( sz02 >= sz01 );
  VERIFY( sz02 >= 100 );
  str01.reserve();
  sz01 = str01.capacity();
  VERIFY( sz01 == 0 );

  sz01 = str01.size() + 5;
  str01.resize(sz01);
  sz02 = str01.size();
  VERIFY( sz01 == sz02 );

  sz01 = str01.size() - 5;
  str01.resize(sz01);
  sz02 = str01.size();
  VERIFY( sz01 == sz02 );

  std::wstring str05(30, L'q');
  std::wstring str06 = str05;
  str05 = str06 + str05;
  VERIFY( str05.capacity() >= str05.size() );
  VERIFY( str06.capacity() >= str06.size() );

  // POD types: size, length, max_size, clear(), empty()
  bool b01;
  std::wstring str011;
  b01 = str01.empty();  
  VERIFY( b01 == true );
  sz01 = str01.size();
  sz02 = str01.length();
  VERIFY( sz01 == sz02 );
  str01.c_str();
  sz01 = str01.size();
  sz02 = str01.length();
  VERIFY( sz01 == sz02 );

  sz01 = str01.length();
  str01.c_str();
  str011 = str01 +  L"_addendum_";
  str01.c_str();
  sz02 = str01.length();    
  VERIFY( sz01 == sz02 );
  sz02 = str011.length();
  VERIFY( sz02 > sz01 );
    
  // trickster allocator issues involved with these:
  std::wstring str3 = L"8-chars_8-chars_";
  std::wstring str4 = str3 + L"7-chars";
  
  sz01 = str01.size();
  sz02 = str01.max_size();  
  VERIFY( sz02 >= sz01 );

  sz01 = str01.size();
  str01.clear();  
  b01 = str01.empty(); 
  VERIFY( b01 == true );
  sz02 = str01.size();  
  VERIFY( sz01 >= sz02 );
}

int main()
{
  test01();
  return 0;
}
