// 1999-06-08 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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

// Do another sanity check, this time for member functions that return
// iterators, namely insert and erase.
void test02(void)
{
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::iterator siterator;
  typedef std::wstring::reverse_iterator sriterator;
  siterator it1;
  sriterator rit1;  

  const std::wstring str01(L"its beach, santa cruz");

  std::wstring str02 = str01;
  std::wstring str05 = str02; // optional, so that begin below causes a mutate
  std::wstring::iterator p = str02.insert(str02.begin(), L' ');
  std::wstring str03 = str02;
  VERIFY( str03 == str02 );
  *p = L'!';
  VERIFY( *str03.c_str() == L' ' );
  str03[0] = L'@';
  VERIFY( str02[0] == L'!' );
  VERIFY( *p == L'!' );
  VERIFY( str02 != str05 );
  VERIFY( str02 != str03 );

  std::wstring str10 = str01;
  std::wstring::iterator p2 = str10.insert(str10.begin(), L'a');
  std::wstring str11 = str10;
  *p2 = L'e';
  VERIFY( str11 != str10 );

  std::wstring str06 = str01;
  std::wstring str07 = str06; // optional, so that begin below causes a mutate
  p = str06.erase(str06.begin());
  std::wstring str08 = str06;
  VERIFY( str08 == str06 );
  *p = L'!';
  VERIFY( *str08.c_str() == L't' );
  str08[0] = L'@';
  VERIFY( str06[0] == L'!' );
  VERIFY( *p == L'!' );
  VERIFY( str06 != str07 );
  VERIFY( str06 != str08 );

  std::wstring str12 = str01;
  p2 = str12.erase(str12.begin(), str12.begin() + str12.size() - 1);
  std::wstring str13 = str12;
  *p2 = L'e';
  VERIFY( str12 != str13 );
}

int main()
{ 
  test02();
  return 0;
}
