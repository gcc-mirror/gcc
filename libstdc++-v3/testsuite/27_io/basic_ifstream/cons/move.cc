// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-fileio "" }

// 27.9.1.7 basic_ifstream constructors [ifstream.cons]

#include <fstream>
#include <sstream>
#include <testsuite_hooks.h>

void
read(std::istream& in, std::ostream& out)
{
  std::string s;
  for (int i=0; i < 10; ++i)
  {
    getline(in, s);
    out << s << '\n';
  }
}

void
test01()
{
  std::string const name = "thirty_years_among_the_dead_preproc.txt";
  std::ostringstream ss0;
  {
    std::ifstream f0(name);
    read(f0, ss0);
  }
  std::ifstream f0(name);
  std::ifstream f1 = std::move(f0);
  VERIFY( !f0.is_open() );
  VERIFY( f1.is_open() );
  std::ostringstream ss1;
  read(f1, ss1);
  VERIFY( ss0.str() == ss1.str() );
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  std::wifstream f0("thirty_years_among_the_dead_preproc.txt");
  std::wifstream f1 = std::move(f0);
  VERIFY( f1.is_open() );
#endif
}

int
main()
{
  test01();
  test02();
}
