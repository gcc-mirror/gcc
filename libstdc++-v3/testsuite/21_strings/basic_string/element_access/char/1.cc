// 1999-06-08 bkoz

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

// 21.3.4 basic_string element access

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01(void)
{
  typedef std::string::size_type csize_type;
  typedef std::string::const_reference cref;
  typedef std::string::reference ref;
  csize_type csz01, csz02;

  const std::string str01("tamarindo, costa rica");
  std::string str02("41st street beach, capitola, california");
  std::string str03;

  // const_reference operator[] (size_type pos) const;
  csz01 = str01.size();
  cref cref1 = str01[csz01 - 1];
  VERIFY( cref1 == 'a' );
  cref cref2 = str01[csz01];
  VERIFY( cref2 == char() );

  // reference operator[] (size_type pos);
  csz02 = str02.size();
  ref ref1 = str02[csz02 - 1];
  VERIFY( ref1 == 'a' );
  ref ref2 = str02[1];
  VERIFY( ref2 == '1' );

  // const_reference at(size_type pos) const;
  csz01 = str01.size();
  cref cref3 = str01.at(csz01 - 1);
  VERIFY( cref3 == 'a' );
  try {
    (void) str01.at(csz01);
    VERIFY( false ); // Should not get here, as exception thrown.
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  // reference at(size_type pos);
  csz01 = str02.size();
  ref ref3 = str02.at(csz02 - 1);
  VERIFY( ref3 == 'a' );
  try {
    (void) str02.at(csz02);
    VERIFY( false ); // Should not get here, as exception thrown.
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
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
