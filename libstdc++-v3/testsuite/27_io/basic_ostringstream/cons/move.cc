// { dg-do run { target c++11 } }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// 27.8.4.1 basic_ostringstream constructors [ostringstream.cons]

#include <sstream>
#include <string>
#include <testsuite_hooks.h>

const std::string strings[] = {
  "one could carry out the description of a machine, ",
  "no matter how complicated, ",
  "in characters which would be merely the letters of the alphabet, and so ",
  "provide the mind with a method of knowing the machine and all its parts"
};

void
append(std::ostringstream& ss, std::string& s, const std::string& t)
{
  ss << t;
  s += t;
}

void
test01()
{
  std::string exp;
  std::ostringstream s1;
  append(s1, exp, strings[0]);

  std::ostringstream s2 = std::move(s1);
  VERIFY( s2.good() );
  VERIFY( s2.rdbuf() != nullptr );
  VERIFY( s2.str() == exp );
  append(s2, exp, strings[1]);
  VERIFY( s2.str() == exp );

  std::ostringstream s3 = std::move(s2);
  VERIFY( s3.good() );
  VERIFY( s3.rdbuf() != nullptr );
  VERIFY( s3.str() == exp );
  append(s3, exp, strings[2]);
  VERIFY( s3.str() == exp );

  s1.str("");
  s1.clear();
  exp.clear();
  append(s1, exp, strings[3]);
  VERIFY( s1.str() == exp );
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  std::wostringstream s1;
  std::wostringstream s2 = std::move(s1);
#endif
}

int
main()
{
  test01();
  test02();
}
