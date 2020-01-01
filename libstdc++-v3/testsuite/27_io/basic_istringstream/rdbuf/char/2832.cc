// 2000-01-10 bkoz

// Copyright (C) 2000-2020 Free Software Foundation, Inc.
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

// 27.7.2.2 member functions (istringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

void 
redirect_buffer(std::ios& stream, std::streambuf* new_buf) 
{ stream.rdbuf(new_buf); }

std::streambuf*
active_buffer(std::ios& stream)
{ return stream.rdbuf(); }

// libstdc++/2832
void test02()
{
  const char* strlit01 = "fuck war";
  const std::string str00;
  const std::string str01(strlit01);
  std::string str02;
  std::stringbuf sbuf(str01);
  std::streambuf* pbasebuf0 = &sbuf;

  std::istringstream sstrm1;
  VERIFY( sstrm1.str() == str00 );
  // derived rdbuf() always returns original streambuf, even though
  // it's no longer associated with the stream.
  std::stringbuf* const buf1 = sstrm1.rdbuf();
  // base rdbuf() returns the currently associated streambuf
  std::streambuf* pbasebuf1 = active_buffer(sstrm1);
  redirect_buffer(sstrm1, &sbuf);
  std::stringbuf* const buf2 = sstrm1.rdbuf();
  std::streambuf* pbasebuf2 = active_buffer(sstrm1);
  VERIFY( buf1 == buf2 ); 
  VERIFY( pbasebuf1 != pbasebuf2 );
  VERIFY( pbasebuf2 == pbasebuf0 );

  // derived rdbuf() returns the original buf, so str() doesn't change.
  VERIFY( sstrm1.str() != str01 );
  VERIFY( sstrm1.str() == str00 );
  // however, casting the active streambuf to a stringbuf shows what's up:
  std::stringbuf* psbuf = dynamic_cast<std::stringbuf*>(pbasebuf2);
  str02 = psbuf->str();
  VERIFY( str02 == str01 );

  // How confusing and non-intuitive is this?
  // These semantics are a joke, a serious defect, and incredibly lame.
}

int main()
{
  test02();
  return 0;
}
