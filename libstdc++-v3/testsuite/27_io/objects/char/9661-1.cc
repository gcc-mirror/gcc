// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// 2003-04-30  Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// No asserts, avoid leaking the semaphores if a VERIFY fails.
#undef _GLIBCXX_ASSERT

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

  bool test __attribute__((unused)) = true;

  const char* name = "tmp_fifo5";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  mkfifo(name, S_IRWXU);
  semaphore s1, s2;
  
  int child = fork();
  VERIFY( child != -1 );

  if (child == 0)
    {
      FILE* file = fopen(name, "r+");
      VERIFY( file != NULL );
      fputs("Whatever\n", file);
      fflush(file);
      s1.signal();
      s2.wait();
      fclose(file);
      s1.signal();
      exit(0);
    }

  freopen(name, "r", stdin);
  s1.wait();

  int c1 = fgetc(stdin);
  VERIFY( c1 != EOF );
  int c2 = cin.rdbuf()->sputbackc('a');
  VERIFY( c2 != EOF );
  VERIFY( c2 == 'a' );
  
  int c3 = fgetc(stdin);
  VERIFY( c3 != EOF );
  VERIFY( c3 == c2 );
  int c4 = ungetc('b', stdin);
  VERIFY( c4 != EOF );
  VERIFY( c4 == 'b' );
  
  int c5 = cin.rdbuf()->sgetc();
  VERIFY( c5 != EOF );
  VERIFY( c5 == c4 );
  s2.signal();
  s1.wait();

  return test;
}

int main()
{
  return !test01();
}
