// 2004-02-09  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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


// 27.4.4.2 basic_ios member functions

#include <sstream>
#include <locale>
#include <typeinfo>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// libstdc++/14072
void test01()
{
  using namespace std;

  locale loc;
  loc = locale(loc, new ctype<__gnu_test::pod_uchar>());
  loc = locale(loc, new num_get<__gnu_test::pod_uchar>());
  loc = locale(loc, new num_put<__gnu_test::pod_uchar>());
	
  locale::global(loc);
  basic_stringstream<__gnu_test::pod_uchar> s;
  s << "10\n";
  s.seekg(0, ios_base::beg);
  s.imbue(locale::classic());
  locale::global(locale::classic());
  loc = locale::classic();
	
  try
    {
      s.widen('\0');
    }
  catch (bad_cast&)
    {
    }
  
  s.clear();
  
  try
    {
      int i = 0;
      s << i;
    }
  catch (bad_cast&)
    {
    }

  s.clear();

  try
    {
      int i = 0;
      s >> i;
    }
  catch (bad_cast&)
    {
    }
}

int main()
{
  test01();
  return 0;
}
