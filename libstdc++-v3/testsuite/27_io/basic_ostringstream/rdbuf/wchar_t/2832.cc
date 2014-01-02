// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

// 27.7.3.2 member functions (ostringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

void 
redirect_buffer(std::wios& stream, std::wstreambuf* new_buf) 
{ stream.rdbuf(new_buf); }

std::wstreambuf*
active_buffer(std::wios& stream)
{ return stream.rdbuf(); }

// libstdc++/2832
void test02()
{
  bool test __attribute__((unused)) = true;
  const wchar_t* strlit01 = L"fuck war";
  const std::wstring str00;
  const std::wstring str01(strlit01);
  std::wstring str02;
  std::wstringbuf sbuf(str01);
  std::wstreambuf* pbasebuf0 = &sbuf;

  std::wostringstream sstrm1;
  VERIFY( sstrm1.str() == str00 );
  // derived rdbuf() always returns original streambuf, even though
  // it's no longer associated with the stream.
  std::wstringbuf* const buf1 = sstrm1.rdbuf();
  // base rdbuf() returns the currently associated streambuf
  std::wstreambuf* pbasebuf1 = active_buffer(sstrm1);
  redirect_buffer(sstrm1, &sbuf);
  std::wstringbuf* const buf2 = sstrm1.rdbuf();
  std::wstreambuf* pbasebuf2 = active_buffer(sstrm1);
  VERIFY( buf1 == buf2 ); 
  VERIFY( pbasebuf1 != pbasebuf2 );
  VERIFY( pbasebuf2 == pbasebuf0 );

  // derived rdbuf() returns the original buf, so str() doesn't change.
  VERIFY( sstrm1.str() != str01 );
  VERIFY( sstrm1.str() == str00 );
  // however, casting the active streambuf to a stringbuf shows what's up:
  std::wstringbuf* psbuf = dynamic_cast<std::wstringbuf*>(pbasebuf2);
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
