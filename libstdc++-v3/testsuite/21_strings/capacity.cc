// 1999-05-11 bkoz

// Copyright (C) 1999 Free Software Foundation, Inc.
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

// 21.3.3 string capacity

#include <string>
#include <cstdio>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

template<typename T>
  struct A { };

template<typename T>
  bool
  operator==(const A<T>& a, const A<T>& b) { }

template<typename T>
  bool
  operator<(const A<T>& a, const A<T>& b) { }

struct B { };

bool test01()
{
  // 1 POD types : resize, capacity, reserve
  bool test = true;
  std::string str01;
  typedef std::string::size_type size_type_s;

  size_type_s sz01 = str01.capacity();
  str01.reserve(100);
  size_type_s sz02 = str01.capacity();
  test &= sz02 >= sz01;
  test &= sz02 >= 100;
  str01.reserve();
  sz01 = str01.capacity();
  test &= sz01 >= 0;

  sz01 = str01.size() + 5;
  str01.resize(sz01);
  sz02 = str01.size();
  test &= sz01 == sz02;

  sz01 = str01.size() - 5;
  str01.resize(sz01);
  sz02 = str01.size();
  test &= sz01 == sz02;

  std::string str05(30, 'q');
  std::string str06 = str05;
  str05 = str06 + str05;
  test &= str05.capacity() >= str05.size();
  test &= str06.capacity() >= str06.size();

  // 2 non POD types : resize, capacity, reserve
  std::basic_string< A<B> > str02;
  typedef std::basic_string< A<B> >::size_type size_type_o;
  size_type_o sz03;
  size_type_o sz04;

  sz03 = str02.capacity();
  str02.reserve(100);
  sz04 = str02.capacity();
  test &= sz04 >= sz03;
  test &= sz04 >= 100;
  str02.reserve();
  sz03 = str02.capacity();
  test &= sz03 >= 0;

  sz03 = str02.size() + 5;
  str02.resize(sz03);
  sz04 = str02.size();
  test &= sz03 == sz04;

  sz03 = str02.size() - 5;
  str02.resize(sz03);
  sz04 = str02.size();
  test &= sz03 == sz04;

  A<B> inst_obj;
  std::basic_string<A<B> > str07(30, inst_obj);
  std::basic_string<A<B> > str08 = str07;
  str07 = str08 + str07;
  test &= str07.capacity() >= str07.size();
  test &= str08.capacity() >= str08.size();

  // 3 POD types: size, length, max_size, clear(), empty()
  bool b01;
  std::string str011;
  b01 = str01.empty();  
  test &= b01 == true;
  sz01 = str01.size();
  sz02 = str01.length();
  test &= sz01 == sz02;
  str01.c_str();
  sz01 = str01.size();
  sz02 = str01.length();
  test &= sz01 == sz02;

  sz01 = str01.length();
  str01.c_str();
  str011 = str01 +  "_addendum_";
  str01.c_str();
  sz02 = str01.length();    
  test &= sz01 == sz02;
  sz02 = str011.length();
  test &= sz02 > sz01;
    
  // trickster allocator (__USE_MALLOC, luke) issues involved with these:
  std::string str3 = "8-chars_8-chars_";
  const char* p3 = str3.c_str();
  std::string str4 = str3 + "7-chars";
  const char* p4 = str3.c_str();
  
  sz01 = str01.size();
  sz02 = str01.max_size();  
  test &= sz02 >= sz01;

  sz01 = str01.size();
  str01.clear();  
  b01 = str01.empty(); 
  test &= b01 == true;
  sz02 = str01.size();  
  test &= sz01 >= sz02;


  // 4 non-POD types: size, length, max_size, clear(), empty()
  b01 = str02.empty();  
  test &= b01 == true;
  sz03 = str02.size();
  sz04 = str02.length();
  test &= sz03 == sz04;
  str02.c_str();
  sz03 = str02.size();
  sz04 = str02.length();
  test &= sz03 == sz04;

  sz03 = str02.max_size();  
  test &= sz03 >= sz04;

  sz03 = str02.size();
  str02.clear();  
  b01 = str02.empty(); 
  test &= b01 == true;
  sz04 = str02.size();  
  test &= sz03 >= sz04;

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  
  return test;
}

int main()
{
  test01();

  return 0;
}


