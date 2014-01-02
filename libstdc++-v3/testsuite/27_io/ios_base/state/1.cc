// Copyright (C) 1997-2014 Free Software Foundation, Inc.
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


#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

void
test02()
{
  bool test __attribute__((unused)) = true;
  const std::string strue("true");
  const std::string sfalse("false");
  std::string str01;
  std::string str02;

  std::locale loc_c = std::locale::classic();
  std::ostringstream ostr01;
  ostr01.imbue(loc_c);
  ostr01.flags(std::ios_base::boolalpha);

  ostr01 << true;
  str02 = ostr01.str();
  VERIFY( str02 == strue );

  ostr01.str(str01);
  ostr01 << false;
  str02 = ostr01.str();
  VERIFY( str02 == sfalse );
}

int 
main() 
{
  test02();
  return 0;
}
