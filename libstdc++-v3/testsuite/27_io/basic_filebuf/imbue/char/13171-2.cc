// { dg-require-namedlocale "fr_FR.ISO8859-15" }
// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

#include <sys/types.h>
#include <sys/stat.h>

#include <testsuite_hooks.h>

// libstdc++/13171
bool test01()
{
  bool test = true;
  using namespace std;
  using namespace __gnu_test;

  locale loc_fr(locale(ISO_8859(15,fr_FR)));
  locale loc_en(locale(ISO_8859(1,en_US)));

  const char* name = "tmp_fifo_13171-2";
  unlink(name);
  mkfifo(name, S_IRWXU);
  semaphore s1, s2;
  
  int child = fork();
  if (child == 0)
    {
      filebuf fb;
      fb.open(name, ios_base::out);
      fb.sputc('S');
      fb.pubsync();
      s1.signal();
      s2.wait();
      fb.close();
      exit(0);
    }

  filebuf fb;
  fb.pubimbue(loc_fr);
  fb.open(name, ios_base::in);
  s1.wait();
  test &= bool( fb.is_open() );
  fb.pubimbue(loc_en);
  filebuf::int_type c = fb.sgetc();
  fb.close();
  test &= bool( c == 'S' );
  s2.signal();

  return test;
}

int main()
{
  return !test01();
}
