// 2000-02-16 bkoz

// Copyright (C) 2000-2013 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <testsuite_hooks.h>

// Dietmar Kühl via Peter Schmid 
class comma_ctype: public std::ctype<char>
{
public:
  comma_ctype(): std::ctype<char>() { }
  comma_ctype(const std::ctype_base::mask* m): std::ctype<char>(m) { }

  const mask* 
  get_classic_table()
  { return std::ctype<char>::classic_table(); }

  const mask* 
  get_table()
  { return this->table(); }
}; 

void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true; 

  comma_ctype obj;
  const ctype_base::mask* tmp = obj.get_classic_table();

  comma_ctype obj2(tmp);
  const ctype_base::mask* ctable = obj2.get_table();
  VERIFY ( tmp == ctable );
}

int main() 
{
  test01();
  return 0;
}
