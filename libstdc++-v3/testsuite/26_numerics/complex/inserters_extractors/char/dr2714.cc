// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

#include <complex>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::istringstream in(" 1 (2) ( 2.0 , 0.5 ) ");
  std::complex<double> c1, c2, c3;
  in >> c1 >> c2 >> c3;
  VERIFY( in.good() );
  VERIFY( c1.real() == 1 && c1.imag() == 0 );
  VERIFY( c2.real() == 2 && c2.imag() == 0 );
  VERIFY( c3.real() == 2 && c3.imag() == 0.5 );
}

void
test02()
{
  std::istringstream in;
  std::complex<double> c(-1, -1);
  const std::complex<double> c0 = c;

  in.str("a");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'a' );

  in.str(" ( ) ");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == ')' );

  in.str("(,");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == ',' );

  in.str("(b)");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'b' );

  in.str("( c)");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'c' );

  in.str("(99d");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'd' );

  in.str("(99 e");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'e' );

  in.str("(99, f");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'f' );

  in.str("(99, 88g");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'g' );

  in.str("(99, 88 h");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == 'h' );

  in.str("(99, )");
  in >> c;
  VERIFY( in.fail() );
  in.clear();
  VERIFY( in.get() == ')' );

  VERIFY( c == c0 );
}

void
test03()
{
  // PR libstdc++/59568
  std::istringstream in;
  std::complex<double> c;

  in.str("");
  in >> c;
  VERIFY( in.fail() );
  VERIFY( in.eof() );
  in.clear();

  in.str(" ");
  in >> c;
  VERIFY( in.fail() );
  VERIFY( in.eof() );
  in.clear();

  in.str("(99");
  in >> c;
  VERIFY( in.fail() );
  VERIFY( in.eof() );
  in.clear();

  in.str("(99,");
  in >> c;
  VERIFY( in.fail() );
  VERIFY( in.eof() );
  in.clear();

  in.str("(99,99");
  in >> c;
  VERIFY( in.fail() );
  VERIFY( in.eof() );
  in.clear();
}

void
test04()
{
  // Test noskipws handling
  std::istringstream in;
  const char* bad_inputs[] = {
    " 1", " (2)", "( 2)", "(2 )", "(2 ,3)", "(2,3 )", 0
  };
  const std::complex<double> c0(-1, -1);
  std::complex<double> c;
  for (int i = 0; bad_inputs[i]; ++i)
  {
    c = c0;
    in.clear();
    in.str(bad_inputs[i]);
    in >> std::noskipws >> c;
    VERIFY( in.fail() );
    VERIFY( c == c0 );

    in.clear();
    in.str(bad_inputs[i]);
    in >> std::skipws >> c;
    VERIFY( !in.fail() );
    VERIFY( c != c0 );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
