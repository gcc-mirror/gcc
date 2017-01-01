// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// 2004-04-16  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

#include <fstream>
#include <locale>
#include <cstdlib>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <testsuite_hooks.h>

// libstdc++/14975
bool test01()
{
  using namespace std;
  using namespace __gnu_test;
  bool test = true;

  locale loc_us = locale(ISO_8859(1,en_US));

  const char* name = "tmp_14975-2";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  mkfifo(name, S_IRWXU);
  semaphore s1;

  int child = fork();
  test &= bool( child != -1 );

  if (child == 0)
    {
      {
	filebuf fbin;
	fbin.open(name, ios_base::in);
      }
      s1.signal();
      exit(0);
    }
  
  wfilebuf fb;
  fb.pubimbue(loc_us);
  wfilebuf* ret = fb.open(name, ios_base::out);
  test &= bool( ret != 0 );
  test &= bool( fb.is_open() );

  s1.wait();

  try
    {
      fb.sputc(L'a');
      fb.sputc(L'b');
      fb.pubimbue(locale::classic());
      fb.sputc(L'c');
      fb.close();
    }
  catch (std::exception&)
    {
    }

  return test;
}

int main()
{
  return !test01();
}
