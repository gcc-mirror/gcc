// 1999-06-10 bkoz

// Copyright (C) 1999-2023 Free Software Foundation, Inc.
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

// 21.3.6.7 basic_string::substr

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01(void)
{
  typedef std::string::size_type csize_type;
  typedef std::string::const_reference cref;
  typedef std::string::reference ref;
  csize_type csz01;

  const char str_lit01[] = "rockaway, pacifica";
  const std::string str01(str_lit01);
  std::string str02;

  // basic_string<charT, _Traits, _Alloc>
  //  substr(size_type pos = 0, size_type n = npos) const;
  csz01 = str01.size();
  str02 = str01.substr(0, 1);
  VERIFY( str02 == "r" );
  str02 = str01.substr(10);
  VERIFY( str02 == "pacifica" );

  try {
    str02 = str01.substr(csz01 + 1);
    VERIFY( false ); 
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

 try {
    str02 = str01.substr(csz01);
    VERIFY( str02.size() == 0 );
  }
  catch(std::out_of_range& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }
}

int main()
{ 
  test01();
  return 0;
}
