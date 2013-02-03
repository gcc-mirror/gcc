// 1999-06-08 bkoz

// Copyright (C) 1999-2013 Free Software Foundation, Inc.
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
bool test01(void)
{
  bool test __attribute__((unused)) = true;
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::iterator siterator;
  typedef std::wstring::reverse_iterator sriterator;
  csize_type csz01, csz02;
  siterator it1;
  sriterator rit1;  

  std::wstring str01(L"montara beach, half moon bay");
  const std::wstring str02(L"ocean beach, san francisco");
  std::wstring str03;

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
  *it1 = L'x';
  VERIFY( str01[0] == L'x' );
  VERIFY( str03[0] == L'm' );

  str03 = str01; 
  csz01 = str01.size();
  rit1 = str01.rbegin(); // NB: Pointing at one-past the end, so ...
  *rit1 = L'z'; 	 // ... but it's taken care of here 
  VERIFY( str01[csz01 - 1] == L'z' );
  VERIFY( str03[csz01 - 1] == L'y' );

  str03 = str01;
  csz01 = str01.size();
  std::wstring::reference r1 = str01.at(csz01 - 2);
  VERIFY( str03 == str01 );
  r1 = L'd';
  VERIFY( str01[csz01 - 2] == L'd' );
  VERIFY( str03[csz01 - 2] == L'a' );

  str03 = str01; 
  csz01 = str01.size();
  std::wstring::reference r2 = str01[csz01 - 3];
  VERIFY( str03 == str01 );
  r2 = L'w'; 
  VERIFY( str01[csz01 - 3] == L'w' );
  VERIFY( str03[csz01 - 3] == L'b' );

  str03 = str01;
  csz02 = str01.size();
  it1 = str01.end();
  VERIFY( str03 == str01 );
  --it1;
  *it1 = L'q'; 
  VERIFY( str01[csz02 - 1] == L'q' );
  VERIFY( str03[csz02 - 1] == L'z' );

  str03 = str01;
  rit1 = str01.rend();
  VERIFY( str03 == str01 );
  --rit1; 	
  *rit1 = L'p'; 
  VERIFY( str01[0] == L'p' );
  VERIFY( str03[0] == L'x' );

  // need to also test for const begin/const end
  VERIFY(test);
  return test;
}

int main()
{ 
  test01();
  return 0;
}
