// 2003-05-01 Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

#include <ext/stdio_sync_filebuf.h>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  bool test = true;
  const char* c_lit = "black pearl jasmine tea";
  int size = strlen(c_lit);
  const char* name = "stdiobuf-1.txt";

  FILE* fout = fopen(name, "w");
  fwrite(c_lit, 1, size, fout);
  fclose(fout);

  FILE* fin = fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<char> sbuf(fin);

  VERIFY( sbuf.sgetc() == c_lit[0] );
  VERIFY( getc(fin) == c_lit[0] );
  VERIFY( sbuf.sgetc() == c_lit[1] );
  VERIFY( sbuf.sbumpc() == c_lit[1] );
  VERIFY( ungetc('Z', fin) == 'Z' );
  VERIFY( sbuf.sbumpc() == 'Z' );
  VERIFY( getc(fin) == c_lit[2] );
  VERIFY( sbuf.sputbackc('X') == 'X' );
  VERIFY( getc(fin) == 'X' );

  char buf[5];
  memset(buf, 'x', 5);
  VERIFY( sbuf.sgetn(buf, 5) == 5 );
  VERIFY( !memcmp(buf, c_lit + 3, 5) );
  VERIFY( getc(fin) == c_lit[8] );

  fclose(fin);
}

// libstdc++/12048
void test02()
{
  bool test = true;
  const char* name = "cin_unget-1.txt";

  std::FILE* file = std::fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<char> sbuf(file);
  int c1 = sbuf.sbumpc();
  VERIFY( c1 != EOF );
  int c2 = sbuf.sungetc();
  VERIFY( c2 != EOF );
  int c3 = sbuf.sbumpc();
  VERIFY( c3 == c1 );

  std::fclose(file);
}

// libstdc++/12048
void test03()
{
  bool test = true;
  const char* name = "cin_unget-1.txt";

  std::FILE* file = std::fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<char> sbuf(file);
  int c1 = sbuf.sbumpc();
  VERIFY( c1 != EOF );
  int c2 = sbuf.sungetc();
  VERIFY( c2 != EOF );
  int c3 = std::fgetc(file);
  VERIFY( c3 == c1 );

  std::fclose(file);
}

// libstdc++/12048
void test04()
{
  bool test = true;
  const char* name = "cin_unget-1.txt";

  std::FILE* file = std::fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<char> sbuf(file);
  char buf[2];
  VERIFY( sbuf.sgetn(buf, 2) == 2 );
  int c2 = sbuf.sungetc();
  VERIFY( c2 != EOF );
  int c3 = sbuf.sbumpc();
  VERIFY( c3 == std::char_traits<char>::to_int_type(buf[1]) );

  std::fclose(file);
}

// libstdc++/12048
void test05()
{
  bool test = true;
  const char* name = "cin_unget-1.txt";

  std::FILE* file = std::fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<char> sbuf(file);
  char buf[2];
  VERIFY( sbuf.sgetn(buf, 2) == 2 );
  int c2 = sbuf.sungetc();
  VERIFY( c2 != EOF );
  int c3 = std::fgetc(file);
  VERIFY( c3 == std::char_traits<char>::to_int_type(buf[1]) );

  std::fclose(file);
}

int main ()
{
  test01();
  test02();
  test03();
  test04();
  test05();

  return 0;
}
