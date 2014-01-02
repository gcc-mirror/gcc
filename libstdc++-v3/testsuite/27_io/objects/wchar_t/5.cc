// 2003-05-01 Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// Check that standard streams can be used from constructors and
// destructors of static objects, provided that an instance of 
// ios_base::Init has been constructed.

void init_standard_streams();
int use_standard_streams();

struct Strange
{
  int i;

  Strange()
  {
    init_standard_streams();
    i = use_standard_streams();
  }

  ~Strange()
  {
    use_standard_streams();
    init_standard_streams();
  }
};

static Strange static_ob;

#include <testsuite_hooks.h>
#include <iostream>

void init_standard_streams()
{
  std::ios_base::Init init;
}

int use_standard_streams()
{
  std::wcout << L"Hello, world!" << std::endl;
  std::wcerr << L"World, hello!" << std::endl;

  int ret = std::ios_base::xalloc();
  std::wcin.iword(ret) = ret + 1;
  std::wcout.iword(ret) = ret + 2;
  std::wcerr.iword(ret) = ret + 3;
  std::wclog.iword(ret) = ret + 4;
  return ret;
}

void test05()
{
  bool test __attribute__((unused)) = true;
  int i = static_ob.i;

  VERIFY( std::wcin.iword(i) == i + 1 );
  VERIFY( std::wcout.iword(i) == i + 2 );
  VERIFY( std::wcerr.iword(i) == i + 3 );
  VERIFY( std::wclog.iword(i) == i + 4 );
}

int main()
{
  test05();
  return 0;
}
