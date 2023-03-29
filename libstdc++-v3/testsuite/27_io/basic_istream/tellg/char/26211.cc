// 2010-11-18  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/26211
void test01()
{
  using namespace std;

  typedef istringstream::pos_type pos_type;

  istringstream iss("Duos for Doris");
  ostringstream oss;

  VERIFY( iss.tellg() == pos_type(0) );

  iss >> oss.rdbuf();
  VERIFY( iss.rdstate() == iss.eofbit );
  VERIFY( iss.tellg() == pos_type(-1) );

  iss.clear();
  VERIFY( iss.tellg() == pos_type(14) );
}

int main()
{
  test01();
  return 0;
}
