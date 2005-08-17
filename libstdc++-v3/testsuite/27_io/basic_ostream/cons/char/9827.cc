// 2003-02-24 Petur Runolfsson <peturr02 at ru dot is>

// Copyright (C) 2003 Free Software Foundation, Inc.
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

#include <ostream>
#include <streambuf>
#include <testsuite_hooks.h>

// libstdc++/9827
class Buf : public std::streambuf
{
};

void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  Buf buf;
  ostream stream(&buf);

  stream << 1;
  VERIFY(!stream.good());
}

int main()
{
  test01();
  return 0;
}
