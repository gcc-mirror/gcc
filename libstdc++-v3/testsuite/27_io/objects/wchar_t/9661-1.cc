// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// 2003-04-30  Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009
// Free Software Foundation, Inc.
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

// Check that wcin.rdbuf()->sputbackc() puts characters back to stdin.
// If wcin.rdbuf() is a filebuf, this succeeds when stdin is a regular
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
      FILE* file = fopen(name, "w");
      fputs("Whatever\n", file);
      fflush(file);
      s1.signal();
      s2.wait();
      fclose(file);
      s1.signal();
      exit(0);
    }

  VERIFY( freopen(name, "r", stdin) );
  s1.wait();

  wint_t c1 = fgetwc(stdin);
  VERIFY( c1 != WEOF );
  wint_t c2 = wcin.rdbuf()->sputbackc(L'a');
  VERIFY( c2 != WEOF );
  VERIFY( c2 == L'a' );
  
  wint_t c3 = fgetwc(stdin);
  VERIFY( c3 != WEOF );
  VERIFY( c3 == c2 );
  wint_t c4 = ungetwc(L'b', stdin);
  VERIFY( c4 != WEOF );
  VERIFY( c4 == L'b' );
  
  wint_t c5 = wcin.rdbuf()->sgetc();
  VERIFY( c5 != WEOF );
  VERIFY( c5 == c4 );
  s2.signal();
  s1.wait();

  return test;
}

int main()
{
  return !test01();
}
