// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

// PR libstdc++/81395

#include <fstream>
#include <cstring>	// for std::memset
#include <cstdio>	// For BUFSIZ

using std::memset;

int main()
{
  {
    std::filebuf fb;
    fb.open("test.txt", std::ios::out);
    char data[BUFSIZ];
    memset(data, 'A', sizeof(data));
    fb.sputn(data, sizeof(data));
  }

  std::filebuf fb;
  fb.open("test.txt", std::ios::in|std::ios::out);
  char buf[BUFSIZ];
  memset(buf, 0, sizeof(buf));
  fb.sgetn(buf, sizeof(buf));
  // Switch from reading to writing without seeking first:
  fb.sputn("B", 1);
  fb.pubsync();
}
