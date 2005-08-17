// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  const char* name = "tmp_seekoff_3";

  wfilebuf fb;

  fb.open(name, ios_base::out);
  fb.sputc(0xf001);

  try
    {
      // seekoff should flush the output sequence, which will fail
      // if the output buffer contains illegal characters.
      fb.pubseekoff(0, ios_base::cur);
      VERIFY( false );
    }
  catch (std::exception&)
    {
    }
}

int main()
{
  test03();
  return 0;
}
