// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <streambuf>
#include <testsuite_hooks.h>

struct streambuf : std::basic_streambuf<char16_t>
{
  basic_streambuf* setbuf(char_type* s, std::streamsize n) override
  {
    setp(s, s + n);
    setg(s, s, s + n);
    return this;
  }
};

void
test01()
{
  using traits = streambuf::traits_type;

  char16_t buf[2] = { };
  streambuf sb;
  sb.pubsetbuf(buf, 2);

  streambuf::int_type res;

  res = sb.sputc(streambuf::char_type(-1));
  VERIFY( traits::eq_int_type(res, traits::eof()) == false );
  res = sb.sputc(streambuf::char_type(-2));
  VERIFY( traits::eq_int_type(res, traits::eof()) == false );
  res = sb.sputc(streambuf::char_type(1));
  VERIFY( traits::eq_int_type(res, traits::eof()) == true );

  VERIFY( buf[0] == streambuf::char_type(-1) );
  VERIFY( buf[1] == streambuf::char_type(-2) );
}

int
main()
{
  test01();
}
