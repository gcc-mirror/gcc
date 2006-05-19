// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// 2006-03-22  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006 Free Software Foundation, Inc.
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
#include <fstream>
#include <sstream>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

// libstdc++/26777
bool test01()
{
  using namespace std;
  using namespace __gnu_test;

  bool test __attribute__((unused)) = true;

  const char* name = "tmp_fifo6";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  mkfifo(name, S_IRWXU);
  semaphore s1, s2;

  int child = fork();
  VERIFY( child != -1 );

  if (child == 0)
    {
      filebuf fbout;
      fbout.open(name, ios_base::in | ios_base::out);
      VERIFY( fbout.is_open() );
      fbout.sputn("Whatever", 8);
      fbout.pubsync();
      s1.signal();
      s2.wait();
      fbout.close();
      s1.signal();
      exit(0);
    }

  filebuf fbin;
  fbin.open(name, ios::in);
  s1.wait();

  fbin.sgetc();
  fbin.pubseekoff(0, ios::cur, ios::in);
  s2.signal();
  s1.wait();

  ostringstream oss;
  oss << &fbin;
  fbin.close();

  VERIFY( oss.str() == "Whatever" );

  return test;
}

int main()
{
  return !test01();
}
