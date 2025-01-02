// 2003-05-01  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

// 27.3 Standard iostream objects

// ios_base::Init::~Init() calls wcout.flush(), which may call
// wcout.setstate(badbit), which may throw an exception. Check that
// the exception doesn't escape from the destructor.

#include <iostream>
#include <streambuf>

class Badbuf : public std::wstreambuf
{
protected:
  virtual int sync()
  {
    return -1;
  }
};

void test06()
{
  std::ios_base::Init init;
  std::wcout.rdbuf(new Badbuf);
  std::wcout.exceptions(std::ios_base::badbit);
}

int main()
{
  test06();
  return 0;
}
