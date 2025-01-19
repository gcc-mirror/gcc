// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run }
// { dg-options "-DSIMULATOR_TEST" { target simulator } }
// { dg-timeout-factor 2 { target ilp32 } }

// PR libstdc++/94749
// basic_istream::ignore(n, c) discards n+1 if next character is equal to c.

#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

typedef wchar_t C;

void
test01()
{
  std::basic_istringstream<C> s(L" +   -");
  s.ignore(1, L'+');
  VERIFY( s.gcount() == 1 );
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.gcount() == 3 );
  VERIFY( s.get() == L'-' );
}

void
test02()
{
  std::basic_istringstream<C> s(L".+...-");
  s.ignore(1, L'+');
  VERIFY( s.gcount() == 1 );
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.gcount() == 3 );
  VERIFY( s.get() == L'-' );
}

void
test03()
{
  std::basic_istringstream<C, __gnu_cxx::char_traits<C> > s(L" +   -");
  s.ignore(1, L'+');
  VERIFY( s.gcount() == 1 );
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.gcount() == 3 );
  VERIFY( s.get() == L'-' );
}

void
test04()
{
  std::basic_istringstream<C, __gnu_cxx::char_traits<C> > s(L".+...-");
  s.ignore(1, L'+');
  VERIFY( s.gcount() == 1 );
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.gcount() == 3 );
  VERIFY( s.get() == L'-' );
}

// The original fix for PR libstdc++/94749 failed to discard the delimiter
// if it occurred after numeric_limits<streamsize>::max() had been seen.
// This streambuf will keep filling the get area with zero bytes until
// almost numeric_limits<streamsize>::max() characters have been read,
// and then return one more buffer that has L"123" at its end.
template<typename T>
struct buff : std::basic_streambuf<typename T::char_type, T>
{
  typedef typename T::char_type		  char_type;
  typedef typename T::int_type		  int_type;
  typedef std::streamsize		  streamsize;
  typedef std::numeric_limits<streamsize> limits;

  buff() : count(0), buf() { }

  int_type underflow()
  {
    // Number of characters left until we overflow the counter
    const streamsize headroom = limits::max() - count;

    if (headroom == 0)
      return T::eof();

    if (bufsz < headroom)
    {
      this->setg(buf, buf, buf + bufsz);
      count += bufsz;
    }
    else
    {
      // write L"123" across the 2GB boundary
      buf[headroom-1] = L'1';
      buf[headroom+0] = L'2';
      buf[headroom+1] = L'3';
      this->setg(buf, buf, buf + headroom + 2);
      count = limits::max();
    }

    return buf[0];
  }

  streamsize count;

  static const streamsize bufsz = 2048 << limits::digits10;
  char_type buf[bufsz + 2];
};

void
test05()
{
  // Not possible to overflow 64-bit streamsize in reasonable time.
  if (std::numeric_limits<std::streamsize>::digits > 32)
    return;

  typedef std::char_traits<C> T;

  std::basic_istream<C, T> in(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'1');
  VERIFY(in.good());
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == L'2');
  VERIFY(in.get() == L'3');
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'2');
  VERIFY(in.good());
  // The standard doesn't say what gcount() should return in this case:
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == L'3');
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'3');
  VERIFY(in.good());
  // The standard doesn't say what gcount() should return in this case:
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'4');
  VERIFY(in.eof());
  // The standard doesn't say what gcount() should return in this case:
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(0);
}

void
test06()
{
  if (std::numeric_limits<std::streamsize>::digits > 32)
    return;

  typedef __gnu_cxx::char_traits<C> T;

  std::basic_istream<C, T> in(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'1');
  VERIFY(in.good());
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == L'2');
  VERIFY(in.get() == L'3');
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'2');
  VERIFY(in.good());
  // The standard doesn't say what gcount() should return in this case:
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == L'3');
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'3');
  VERIFY(in.good());
  // The standard doesn't say what gcount() should return in this case:
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(new buff<T>);

  in.ignore(std::numeric_limits<std::streamsize>::max(), L'4');
  VERIFY(in.eof());
  // The standard doesn't say what gcount() should return in this case:
  VERIFY(in.gcount() == std::numeric_limits<std::streamsize>::max());
  VERIFY(in.get() == T::eof());

  delete in.rdbuf(0);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
#ifndef SIMULATOR_TEST
  test05();
  test06();
#endif
}
