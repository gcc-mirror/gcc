// { dg-require-namedlocale "en_US" }
// { dg-require-namedlocale "fr_FR" }
// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// 2004-01-11  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

#include <fstream>
#include <locale>
#include <cstdlib>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <testsuite_hooks.h>

// libstdc++/13582
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std; 
  using namespace __gnu_test;

  locale loc_en(locale("en_US"));
  locale loc_fr(locale("fr_FR"));

  const char* name = "tmp_fifo_13582-2";
  unlink(name);
  mkfifo(name, S_IRWXU);

  int child = fork();
  if (child == 0)
    {
      filebuf fbout;
      fbout.open(name, ios_base::out);
      fbout.sputn("12345", 5);
      fbout.pubsync();
      fbout.close();
      exit(0);
    }

  filebuf fbin;
  fbin.open(name, ios_base::in);
  filebuf::int_type n = fbin.sbumpc();
  VERIFY( n == '1' );
  fbin.pubimbue(loc_en);
  n = fbin.sbumpc();
  VERIFY( n == '2' );
  fbin.pubimbue(loc_fr);
  n = fbin.sbumpc();
  VERIFY( n == '3' );
  n = fbin.sbumpc();
  VERIFY( n == '4' );
  n = fbin.sbumpc();
  VERIFY( n == '5' );
  n = fbin.sbumpc();
  VERIFY( n == filebuf::traits_type::eof() );
}

int main()
{
  test01();
  return 0;
}
