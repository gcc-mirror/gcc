// 2003-05-01 Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

#include <ext/stdio_sync_filebuf.h>
#include <cstring>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef char_traits<wchar_t> traits_type;

  const char* c_lit = "black pearl jasmine tea";
  const wchar_t* w_lit = L"black pearl jasmine tea";
  unsigned size = strlen(c_lit);
  const char* name = "stdiobuf-1.txt";

  FILE* fout = fopen(name, "w");
  VERIFY( fwrite(c_lit, 1, size, fout) == size );
  fclose(fout);

  FILE* fin = fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<wchar_t> wsbuf(fin);

  VERIFY( traits_type::to_char_type(wsbuf.sgetc()) == w_lit[0] );
  VERIFY( traits_type::to_char_type(getwc(fin)) == w_lit[0] );
  VERIFY( traits_type::to_char_type(wsbuf.sgetc()) == w_lit[1] );
  VERIFY( traits_type::to_char_type(wsbuf.sbumpc()) == w_lit[1] );
  VERIFY( ungetwc(L'Z', fin) == L'Z' );
  VERIFY( wsbuf.sbumpc() == L'Z' );
  VERIFY( traits_type::to_char_type(getwc(fin)) == w_lit[2] );
  VERIFY( wsbuf.sputbackc(L'X') == L'X' );
  VERIFY( getwc(fin) == L'X' );

  wchar_t buf[5];
  wmemset(buf, static_cast<wchar_t>(0xdeadbeef), 5);
  VERIFY( wsbuf.sgetn(buf, 5) == 5 );
  VERIFY( !wmemcmp(buf, w_lit + 3, 5) );
  VERIFY( traits_type::to_char_type(getwc(fin)) == w_lit[8] );

  fclose(fin);
}

int main ()
{
  test01();
  return 0;
}
