// 2004-01-26  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  std::string str01("Valle Del Salto");
  str01.replace(0, 5, str01.data() + 10, 5);
  VERIFY( str01 == "Salto Del Salto" );
  
  std::string str02("Colle di Val d'Elsa");
  str02.replace(0, 9, str02.data() + 10, 0);
  VERIFY( str02 == "Val d'Elsa" );

  std::string str03("Novi Ligure");
  str03.replace(11, 0, str03.data() + 4, 7);
  VERIFY( str03 == "Novi Ligure Ligure");

  std::string str04("Trebisacce");
  str04.replace(3, 4, str04.data(), 0);
  VERIFY( str04 == "Trecce" );

  std::string str05("Altopiano della Sila");
  str05.replace(1, 18, str05.data() + 19, 1);
  VERIFY( str05 == "Aaa" );
}

int main()
{
  test01();
  return 0;
}
