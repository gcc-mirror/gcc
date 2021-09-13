// 2004-09-29  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

// C++03 27.7.1.1  basic_stringbuf constructors  [lib.stringbuf.cons]

#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// http://gcc.gnu.org/ml/libstdc++/2004-09/msg00243.html
void test01()
{
  __gnu_test::constraint_wstringbuf sbuf;
  VERIFY( sbuf.check_pointers() );
}

void test02()
{
  std::wstringbuf sbuf;
  VERIFY( sbuf.str().empty() );

  std::wstringbuf sbuf1(std::wios::in);
  VERIFY( sbuf1.str().empty() );

  const std::wstring str = L"This is my boomstick!";

  std::wstringbuf sbuf2(str);
  VERIFY( sbuf2.str() == str );

  std::wstringbuf sbuf3(str, std::wios::in);
  VERIFY( sbuf3.str() == str );
  VERIFY( sbuf3.sgetc() == str[0] );
  VERIFY( sbuf3.sputc(L'X') == std::wstringbuf::traits_type::eof() );

  std::wstringbuf sbuf4(str, std::wios::out);
  VERIFY( sbuf4.str() == str );
  VERIFY( sbuf4.sputc(L'Y') == L'Y' );
  VERIFY( sbuf4.sgetc() == std::wstringbuf::traits_type::eof() );

#if __cplusplus >= 201103L
  static_assert( ! std::is_convertible<std::wios::openmode, std::wstringbuf>(),
		  "wstringbuf(wios::openmode) is explicit");

  static_assert( ! std::is_convertible<const std::wstring&, std::wstringbuf>(),
		  "wstringbuf(wstring, wios::openmode) is explicit");
#endif
}

int main()
{
  test01();
  test02();
  return 0;
}
