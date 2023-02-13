// 2004-09-29  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2023 Free Software Foundation, Inc.
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
  __gnu_test::constraint_stringbuf sbuf;
  VERIFY( sbuf.check_pointers() );
}

void test02()
{
  std::stringbuf sbuf;
  VERIFY( sbuf.str().empty() );

  std::stringbuf sbuf1(std::ios::in);
  VERIFY( sbuf1.str().empty() );

  const std::string str = "This is my boomstick!";

  std::stringbuf sbuf2(str);
  VERIFY( sbuf2.str() == str );

  std::stringbuf sbuf3(str, std::ios::in);
  VERIFY( sbuf3.str() == str );
  VERIFY( sbuf3.sgetc() == str[0] );
  VERIFY( sbuf3.sputc('X') == std::stringbuf::traits_type::eof() );

  std::stringbuf sbuf4(str, std::ios::out);
  VERIFY( sbuf4.str() == str );
  VERIFY( sbuf4.sputc('Y') == 'Y' );
  VERIFY( sbuf4.sgetc() == std::stringbuf::traits_type::eof() );

#if __cplusplus >= 201103L
  static_assert( ! std::is_convertible<std::ios::openmode, std::stringbuf>(),
		  "stringbuf(ios::openmode) is explicit");

  static_assert( ! std::is_convertible<const std::string&, std::stringbuf>(),
		  "stringbuf(string, ios::openmode) is explicit");
#endif
}

int main()
{
  test01();
  test02();
  return 0;
}
