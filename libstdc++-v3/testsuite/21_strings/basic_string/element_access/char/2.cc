// 1999-06-08 bkoz

// Copyright (C) 1999-2017 Free Software Foundation, Inc.
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

// 21.3 template class basic_string

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

// Do a quick sanity check on known problems with element access and
// ref-counted strings. These should all pass, regardless of the
// underlying string implementation, of course.
void test01(void)
{
  typedef std::string::size_type csize_type;
  typedef std::string::iterator siterator;
  typedef std::string::reverse_iterator sriterator;
  csize_type csz01, csz02;
  siterator it1;
  sriterator rit1;  

  std::string str01("montara beach, half moon bay");
  const std::string str02("ocean beach, san francisco");
  std::string str03;

  // 21.3 p 5

  // References, pointers, and iterators referring to the elements of
  // a basic_string may be invalidated by the following uses of that
  // basic_string object:

  // ...

  // Susequent to any of the above uses except the forms of insert()
  // and erase() which return iterators, the first call to non-const
  // member functions operator[](), at(), begin(), rbegin(), end(), or
  // rend()

  str03 = str01;
  it1 = str01.begin();
  *it1 = 'x';
  VERIFY( str01[0] == 'x' );
  VERIFY( str03[0] == 'm' );

  str03 = str01; 
  csz01 = str01.size();
  rit1 = str01.rbegin(); // NB: Pointing at one-past the end, so ...
  *rit1 = 'z'; 		 // ... but it's taken care of here 
  VERIFY( str01[csz01 - 1] == 'z' );
  VERIFY( str03[csz01 - 1] == 'y' );

  str03 = str01;
  csz01 = str01.size();
  std::string::reference r1 = str01.at(csz01 - 2);
  VERIFY( str03 == str01 );
  r1 = 'd';
  VERIFY( str01[csz01 - 2] == 'd' );
  VERIFY( str03[csz01 - 2] == 'a' );

  str03 = str01; 
  csz01 = str01.size();
  std::string::reference r2 = str01[csz01 - 3];
  VERIFY( str03 == str01 );
  r2 = 'w'; 
  VERIFY( str01[csz01 - 3] == 'w' );
  VERIFY( str03[csz01 - 3] == 'b' );

  str03 = str01;
  csz02 = str01.size();
  it1 = str01.end();
  VERIFY( str03 == str01 );
  --it1;
  *it1 = 'q'; 
  VERIFY( str01[csz02 - 1] == 'q' );
  VERIFY( str03[csz02 - 1] == 'z' );

  str03 = str01;
  rit1 = str01.rend();
  VERIFY( str03 == str01 );
  --rit1; 	
  *rit1 = 'p'; 
  VERIFY( str01[0] == 'p' );
  VERIFY( str03[0] == 'x' );

  // need to also test for const begin/const end
}

int main()
{ 
  test01();
  return 0;
}
