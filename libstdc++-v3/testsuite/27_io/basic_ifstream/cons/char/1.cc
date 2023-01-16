// Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

// 27.8.1.7 ifstream member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// { dg-require-fileio "" }

#include <istream>
#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "ifstream_members-1.tst";

// http://gcc.gnu.org/ml/libstdc++/2000-07/msg00004.html
void test02()
{
  const int more_than_max_open_files = 8200;
  
  for (int i = 0; ++i < more_than_max_open_files;)
    {
      std::ifstream ifs(name_01);
      VERIFY( static_cast<bool>(ifs) );
    }
}

int main()
{
  test02();
  return 0;
}



