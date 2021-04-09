// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do run { target { ! lp64 } } }
// { dg-timeout-factor 2 }

#include <istream>
#include <streambuf>
#include <limits>
#include <testsuite_hooks.h>

typedef wchar_t C;

struct buff : std::basic_streambuf<C>
{
  typedef std::streamsize		  streamsize;
  typedef std::numeric_limits<streamsize> limits;

  buff() : count(0), buf() { }

  int_type underflow()
  {
    // Number of characters left until we overflow the counter
    const streamsize headroom = limits::max() - count;

    if (headroom == 0)
      return traits_type::eof();

    if (bufsz < headroom)
      count += bufsz;
    else
      count = limits::max();

    this->setg(buf + 1, buf + 1, buf + bufsz);

    return buf[0];
  }

  int_type overflow(int_type c)
  {
    if (traits_type::eq_int_type(c , traits_type::eof()))
      return c;
    this->setp(buf, buf + bufsz - 1);
    return traits_type::not_eof(c);
  }

  streamsize count;

  static const streamsize bufsz = (2048 << limits::digits10) + 1;
  char_type buf[bufsz];
};

void
test01()
{
  // Not possible to overflow 64-bit streamsize in reasonable time.
  if (std::numeric_limits<std::streamsize>::digits > 32)
    return;

  std::basic_istream<C> in(new buff);
  buff out;
  in.get(out);
  VERIFY( in.gcount() == std::numeric_limits<std::streamsize>::max() );

  delete in.rdbuf(new buff);

  in.get();
  VERIFY( in.gcount() == 1 );

  in.get(out, 'a');
  VERIFY( in.gcount() == std::numeric_limits<std::streamsize>::max() );
}

int
main()
{
  test01();
}
