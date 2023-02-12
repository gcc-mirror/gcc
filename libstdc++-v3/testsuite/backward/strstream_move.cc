// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-options "-Wno-deprecated" }
// { dg-do run { target c++11 } }

#include <strstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::istrstream is("15 16");
  std::istrstream is2 = std::move(is);
  int a;
  is >> a;
  VERIFY( !is );
  is2 >> a;
  VERIFY( is2 );
  VERIFY( a == 15 );
  std::istrstream is3 = std::move(is2);
  int b;
  is2 >> b;
  VERIFY( !is2 );
  is3 >> b;
  VERIFY( is3 );
  VERIFY( b == 16 );
}

void
test02()
{
  std::istrstream is("");
  int a;
  is >> a;
  VERIFY( !is );
  is = std::istrstream("17 18");
  is >> a;
  VERIFY( is );
  VERIFY( a == 17 );
  is = std::istrstream("");
  int b;
  is >> b;
  VERIFY( !is );
}

void
test03()
{
  std::ostrstream os;
  os << "a few chars";  // fits in initial allocation
  char* s = os.str(); // os now frozen
  std::ostrstream os2 = std::move(os);
  VERIFY( os2.str() == s );
  VERIFY( os.str() == nullptr );
  os2.freeze(false);

  os2 << "enough additional chars to force a reallocation";
  VERIFY( os2 );
  s = os2.str();  // os2 now frozen
  std::ostrstream os3 = std::move(os2);
  VERIFY( os3.str() == s );
  VERIFY( os2.str() == nullptr );
  delete[] s;
}

void
test04()
{
  char buf[16];
  std::ostrstream os(buf, sizeof(buf));
  os << "a few chars";
  char* s = os.str(); // os now frozen
  VERIFY( s == buf );
  std::ostrstream os2 = std::move(os);
  VERIFY( os2.str() == s );
  VERIFY( os.str() == nullptr );
  os2.freeze(false);

  os2 << "enough additional chars to force a reallocation";
  VERIFY( !os2 );
  s = os2.str();  // os2 now frozen
  VERIFY( s == buf );
  std::ostrstream os3 = std::move(os2);
  VERIFY( os3.str() == s );
  VERIFY( os2.str() == nullptr );
}

void
test05()
{
  char buf[] = "0123456789";
  std::ostrstream os(buf, 1);
  os << "aa";
  VERIFY( !os );
  os = std::ostrstream(buf, 10);
  os << "some chars";
  VERIFY( os );
  VERIFY( os.pcount() == 10 );
  os << "a";
  VERIFY( !os );
  os = std::ostrstream();
  os << "a";
  VERIFY( os );
  VERIFY( os.pcount() == 1 );
  char* s = os.str(); // os now frozen
  os = std::ostrstream();
  os.freeze(false);   // no effect
  delete[] s;
}

void
test06()
{
  char buf[] = "15 16";
  std::strstream ss(buf, 5, std::ios::in|std::ios::app);
  std::strstream ss2 = std::move(ss);
  int a;
  ss >> a;
  VERIFY( !ss );
  ss2 >> a;
  VERIFY( ss2 );
  VERIFY( a == 15 );
  std::strstream ss3 = std::move(ss2);
  int b;
  ss2 >> b;
  VERIFY( !ss2 );
  ss3 >> b;
  VERIFY( ss3 );
  VERIFY( b == 16 );
}

void
test07()
{
  std::strstream ss;
  int a;
  ss >> a;
  VERIFY( !ss );
  char buf[] = "17 18";
  ss = std::strstream(buf, 5, std::ios::in|std::ios::app);
  ss >> a;
  VERIFY( ss );
  VERIFY( a == 17 );
  ss = std::strstream();
  int b;
  ss >> b;
  VERIFY( !ss );
}

void
test08()
{
  std::strstream ss;
  ss << "a few chars";  // fits in initial allocation
  char* s = ss.str(); // ss now frozen
  std::strstream ss2 = std::move(ss);
  VERIFY( ss2.str() == s );
  VERIFY( ss.str() == nullptr );
  ss2.freeze(false);

  ss2 << "enough additional chars to force a reallocation";
  VERIFY( ss2 );
  s = ss2.str();  // ss2 now frozen
  std::strstream ss3 = std::move(ss2);
  VERIFY( ss3.str() == s );
  VERIFY( ss2.str() == nullptr );
  delete[] s;
}

void
test09()
{
  char buf[16];
  std::strstream ss(buf, sizeof(buf));
  ss << "a few chars";
  char* s = ss.str(); // ss now frozen
  VERIFY( s == buf );
  std::strstream ss2 = std::move(ss);
  VERIFY( ss2.str() == s );
  VERIFY( ss.str() == nullptr );
  ss2.freeze(false);

  ss2 << "enough additional chars to force a reallocation";
  VERIFY( !ss2 );
  s = ss2.str();  // ss2 now frozen
  VERIFY( s == buf );
  std::strstream ss3 = std::move(ss2);
  VERIFY( ss3.str() == s );
  VERIFY( ss2.str() == nullptr );
}

void
test10()
{
  char buf[] = "0123456789";
  std::strstream ss(buf, 1);
  ss << "aa";
  VERIFY( !ss );
  ss = std::strstream(buf, 10);
  ss << "some chars";
  VERIFY( ss );
  VERIFY( ss.pcount() == 10 );
  ss << "a";
  VERIFY( !ss );
  ss = std::strstream();
  ss << "a";
  VERIFY( ss );
  VERIFY( ss.pcount() == 1 );
  char* s = ss.str(); // ss now frozen
  ss = std::strstream();
  ss.freeze(false);   // no effect
  delete[] s;
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  test10();
}
