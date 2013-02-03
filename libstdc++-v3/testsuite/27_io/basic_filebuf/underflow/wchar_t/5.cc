// { dg-require-namedlocale "se_NO.UTF-8" }

// 2003-09-04  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <locale>
#include <fstream>
#include <cstdio>
#include <testsuite_hooks.h>

// Test that unbuffered really means unbuffered for UTF-8
void test05()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  const char* name = "tmp_underflow-5";

  wfilebuf fb;
  fb.pubsetbuf(0, 0);
  fb.pubimbue(locale("se_NO.UTF-8"));

  FILE* file = fopen(name, "w");
  setvbuf(file, 0, _IONBF, 0);
  fputs("abcde", file);

  fb.open(name, ios_base::in);
  VERIFY( fb.sbumpc() == L'a' );
  
  fseek(file, 1, SEEK_SET);
  fputc('0', file);

  VERIFY( fb.sbumpc() == L'0' );
  VERIFY( fb.sbumpc() == L'c' );

  fputc('1', file);
  fputc('2', file);

  VERIFY( fb.sbumpc() == L'2' );
  VERIFY( fb.sbumpc() == L'e' );
  VERIFY( fb.sbumpc() == WEOF );

  fputc('3', file);
  fputc('4', file);

  VERIFY( fb.sbumpc() == L'4' );
  VERIFY( fb.sbumpc() == WEOF );

  fb.close();
  fclose(file);
}

int main()
{
  test05();
  return 0;
}
