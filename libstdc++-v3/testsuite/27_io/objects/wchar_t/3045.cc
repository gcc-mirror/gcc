// 2003-05-01 Petur Runolfsson  <peturr02@ru.is>

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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <fstream>
#include <iostream>
#include <testsuite_hooks.h>

class gnu_filebuf: public std::wfilebuf
{
  int i;
public:
  gnu_filebuf(int j = 1): i(j) { }
  ~gnu_filebuf() { --i; }
  int get_i() { return i;}
};

const int initial = 4;
gnu_filebuf buf(initial);

// libstdc++/3045, in a vague way.
void test01()
{
  bool test __attribute__((unused)) = true;
  int k1;

  // 1 normal
  k1 = buf.get_i();
  VERIFY( k1 == initial );
  {
    std::wcout.rdbuf(&buf);
  }
  k1 = buf.get_i();
  VERIFY( k1 == initial );

  // 2 syncd off
  k1 = buf.get_i();
  VERIFY( k1 == initial );
  {
    std::wcout.rdbuf(&buf);
    std::ios_base::sync_with_stdio(false); // make sure doesn't clobber buf
  }
  k1 = buf.get_i();
  VERIFY( k1 == initial );

  // 3 callling init
  k1 = buf.get_i();
  VERIFY( k1 == initial );
  {
    std::wcout.rdbuf(&buf);
    std::ios_base::Init make_sure_initialized;
  }
  k1 = buf.get_i();
  VERIFY( k1 == initial );
}

int main()
{
  test01();
  return 0;
}
