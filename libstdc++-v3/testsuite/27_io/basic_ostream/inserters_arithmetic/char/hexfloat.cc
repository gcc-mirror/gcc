// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }
// { dg-xfail-run-if "PR libstdc++/64054" { *-*-solaris* } }

// 2014-03-27 Rüdiger Sonderfeld
// test the hexadecimal floating point inserters (facet num_put)

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <iomanip>
#include <sstream>
#include <string>
#include <testsuite_hooks.h>

#ifdef TEST_NUMPUT_VERBOSE
#  include <iostream>
#endif

using namespace std;

void
test01()
{
  ostringstream os;
  double d = 272.; // 0x1.1p+8;
#ifdef TEST_NUMPUT_VERBOSE
  cout << os.precision() << endl;
#endif
  os << hexfloat << setprecision(1);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stod(os.str()) == d );
  VERIFY( os.str().substr(0, 2) == "0x" );
  VERIFY( os.str().find('p') != std::string::npos );

  os.str("");
  os << uppercase << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stod(os.str()) == d );
  VERIFY( os.str().substr(0, 2) == "0X" );
  VERIFY( os.str().find('P') != std::string::npos );

  os << nouppercase;
  os.str("");
  os << defaultfloat << setprecision(6);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && os.str() == "272" );

  os.str("");
  d = 15.; //0x1.ep+3;
  os << hexfloat << setprecision(1);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stod(os.str()) == d );
  os.str("");
  os << uppercase << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stod(os.str()) == d );
  os << nouppercase;
  os.str("");
  os << defaultfloat << setprecision(6);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && os.str() == "15" );
}

void
test02()
{
  ostringstream os;
  long double d = 272.L; // 0x1.1p+8L;
  os << hexfloat << setprecision(1);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stold(os.str()) == d );
  os.str("");
  os << uppercase << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stold(os.str()) == d );
  os << nouppercase;
  os.str("");
  os << defaultfloat << setprecision(6);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && os.str() == "272" );

  os.str("");
  os << hexfloat << setprecision(1);
  d = 15.; //0x1.ep+3;
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stold(os.str()) == d );
  os.str("");
  os << uppercase << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && std::stold(os.str()) == d );
  os << nouppercase;
  os.str("");
  os << defaultfloat << setprecision(6);
  os << d;
#ifdef TEST_NUMPUT_VERBOSE
  cout << "got: " << os.str() << endl;
#endif
  VERIFY( os && os.str() == "15" );
}

int
main()
{
  test01();
  test02();
}
