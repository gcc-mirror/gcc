// 1999-06-04 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

// 21.3.1 basic_string constructors.

#include <new>
#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01(void)
{
  bool test __attribute__((unused)) = true;
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::iterator citerator;
  csize_type npos = std::wstring::npos;
  csize_type csz01;

  const wchar_t str_lit01[] = L"rodeo beach, marin";
  const std::wstring str01(str_lit01);
  const std::wstring str02(L"baker beach, san francisco");

  // basic_string(const wstring&, size_type pos = 0, siz_type n = npos, alloc)
  csz01 = str01.size();
  try {
    std::wstring str03(str01, csz01 + 1);
    VERIFY( false );
  }		 
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::wstring str03(str01, csz01);
    VERIFY( str03.size() == 0 );
    VERIFY( str03.size() <= str03.capacity() );
  }		 
  catch(...) {
    VERIFY( false );
  }

  // basic_string(const wchar_t* s, size_type n, alloc)
  csz01 = str01.max_size();
  // NB: As strlen(str_lit01) != csz01, this test is undefined. It
  // should not crash, but what gets constructed is a bit arbitrary.
  try {
    std::wstring str03(str_lit01, csz01 + 1);
    VERIFY( true );
  }		 
  catch(std::length_error& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  // NB: As strlen(str_lit01) != csz01, this test is undefined. It
  // should not crash, but what gets constructed is a bit arbitrary.
  // The "maverick's" of all string objects.
  try {
    std::wstring str04(str_lit01, npos); 
    VERIFY( true );
  }		 
  catch(std::length_error& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  // Build a maxsize - 1 lengthed string consisting of all A's
  try {
    std::wstring str03(csz01 - 1, 'A');
    VERIFY( str03.size() == csz01 - 1 );
    VERIFY( str03.size() <= str03.capacity() );
  }		 
  // NB: bad_alloc is regrettable but entirely kosher for
  // out-of-memory situations.
  catch(std::bad_alloc& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  // basic_string(const wchar_t* s, const allocator& a = allocator())
  std::wstring str04(str_lit01);
  VERIFY( str01 == str04 );


  // basic_string(size_type n, char c, const allocator& a = allocator())
  csz01 = str01.max_size();
  try {
    std::wstring str03(csz01 + 1, L'z');
    VERIFY( false );
  }		 
  catch(std::length_error& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::wstring str04(npos, L'b'); // the "maverick's" of all string objects.
    VERIFY( false );
  }		 
  catch(std::length_error& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::wstring str03(csz01 - 1, L'z');
    VERIFY( str03.size() != 0 );
    VERIFY( str03.size() <= str03.capacity() );
  }		 
  // NB: bad_alloc is regrettable but entirely kosher for
  // out-of-memory situations.
  catch(std::bad_alloc& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }


  // template<typename _InputIter>
  //   basic_string(_InputIter begin, _InputIter end, const allocator& a)
  std::wstring str06(str01.begin(), str01.end());
  VERIFY( str06 == str01 );
}

int main()
{ 
  __gnu_test::set_memory_limits();
  test01();
  return 0;
}
