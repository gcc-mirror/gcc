// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <testsuite_hooks.h>

class UnderBuf : public std::filebuf
{
public:
  int_type
  pub_underflow()
  { return underflow(); }

  std::streamsize
  pub_showmanyc()
  { return showmanyc(); }
};

// libstdc++/10097
// filebuf::underflow drops characters.
void test16()
{
  using namespace std;
  using namespace __gnu_test;
  bool test __attribute__((unused)) = true;

  const char* name = "tmp_fifo1";
  
  signal(SIGPIPE, SIG_IGN);
  unlink(name);
  
  if (0 != try_mkfifo(name, S_IRWXU))
    {
      VERIFY( false );
    }
  
  int fval = fork();
  if (fval == -1)
    {
      unlink(name);
      VERIFY( false );
    }
  else if (fval == 0)
    {
      filebuf fbout;
      fbout.open(name, ios_base::in|ios_base::out);
      VERIFY ( fbout.is_open() );
      fbout.sputn("0123456789", 10);
      fbout.pubsync();
      sleep(2);
      fbout.close();
      exit(0);
    }

  UnderBuf fb;
  fb.open(name, ios_base::in);
  sleep(1);
  
  fb.sgetc();
  streamsize n = fb.pub_showmanyc();

  while (n > 0)
    {
      --n;
      
      UnderBuf::int_type c = fb.pub_underflow();
      VERIFY( c != UnderBuf::traits_type::eof() );
      
      fb.sbumpc();
    }

  fb.close();
}

int main() 
{
  test16();
  return 0;
}
