// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// 2003-04-30  Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

// Check that cin.rdbuf()->sputbackc() puts characters back to stdin.
// If cin.rdbuf() is a filebuf, this succeeds when stdin is a regular
// file, but fails otherwise, hence the named fifo.
bool test01()
{
  using namespace std;
  using namespace __gnu_test;

  bool test = true;

  const char* name = "tmp_fifo5";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  mkfifo(name, S_IRWXU);
  semaphore s1, s2;
  
  int child = fork();
  test &= bool( child != -1 );

  if (child == 0)
    {
      FILE* file = fopen(name, "r+");
      test &= bool( file != 0 );
      fputs("Whatever\n", file);
      fflush(file);
      s1.signal();
      s2.wait();
      fclose(file);
      s1.signal();
      exit(0);
    }

  test &= bool( freopen(name, "r", stdin) );
  s1.wait();

  int c1 = fgetc(stdin);
  test &= bool( c1 != EOF );
  int c2 = cin.rdbuf()->sputbackc('a');
  test &= bool( c2 != EOF );
  test &= bool( c2 == 'a' );
  
  int c3 = fgetc(stdin);
  test &= bool( c3 != EOF );
  test &= bool( c3 == c2 );
  int c4 = ungetc('b', stdin);
  test &= bool( c4 != EOF );
  test &= bool( c4 == 'b' );
  
  int c5 = cin.rdbuf()->sgetc();
  test &= bool( c5 != EOF );
  test &= bool( c5 == c4 );
  s2.signal();
  s1.wait();

  return test;
}

int main()
{
  return !test01();
}
