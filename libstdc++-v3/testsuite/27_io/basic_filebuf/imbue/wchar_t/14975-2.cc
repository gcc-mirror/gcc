// 2004-04-16  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004, 2005 Free Software Foundation, Inc.
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

#include <fstream>
#include <locale>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <testsuite_hooks.h>

// libstdc++/14975
void test01()
{
  using namespace std;
  using namespace __gnu_test;
  bool test __attribute__((unused)) = true;

  locale loc_us = try_named_locale("en_US");

  const char* name = "tmp_14975-2";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  try_mkfifo(name, S_IRWXU);
  semaphore s1;

  int child = fork();
  VERIFY( child != -1 );

  if (child == 0)
    {
      {
	filebuf fbin;
	fbin.open(name, ios_base::in);
      }
      s1.signal ();
      exit(0);
    }
  
  wfilebuf fb;
  fb.pubimbue(loc_us);
  wfilebuf* ret = fb.open(name, ios_base::out);
  VERIFY( ret != NULL );
  VERIFY( fb.is_open() );

  s1.wait ();

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
}

int main()
{
  test01();
  return 0;
}
