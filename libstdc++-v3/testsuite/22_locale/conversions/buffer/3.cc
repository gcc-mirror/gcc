// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

#include <locale>
#include <streambuf>
#include <testsuite_hooks.h>

struct streambuf : std::streambuf
{
  int_type underflow() override
  {
    if (c != '\0')
    {
      this->setg(&c, &c, &c + 1);
      return *this->gptr();
    }
    c = '\0';
    return traits_type::eof();
  }

private:
  char c = 'a';
};

struct codecvt : std::codecvt<wchar_t, char, std::mbstate_t> { };

void
test01()
{
  // https://gcc.gnu.org/ml/libstdc++/2017-11/msg00022.html
  streambuf sb;
  std::wbuffer_convert<codecvt> conv(&sb);
  VERIFY( sb.in_avail() == 0 );
  wchar_t c = conv.sgetc();
  VERIFY( c == L'a' );
}

int
main()
{
  test01();
}
