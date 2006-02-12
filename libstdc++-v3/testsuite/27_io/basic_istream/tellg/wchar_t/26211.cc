// Copyright (C) 2006 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/26211
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  typedef wistringstream::pos_type pos_type;

  wistringstream iss(L"Duos for Doris");
  wostringstream oss;
  
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
