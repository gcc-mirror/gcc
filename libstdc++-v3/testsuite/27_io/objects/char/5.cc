// 2003-04-26 Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003, 2004 Free Software Foundation
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
  std::cout << "Hello, world!" << std::endl;
  std::cerr << "World, hello!" << std::endl;

  int ret = std::ios_base::xalloc();
  std::cin.iword(ret) = ret + 1;
  std::cout.iword(ret) = ret + 2;
  std::cerr.iword(ret) = ret + 3;
  std::clog.iword(ret) = ret + 4;
  return ret;
}

void test05()
{
  bool test __attribute__((unused)) = true;
  int i = static_ob.i;

  VERIFY( std::cin.iword(i) == i + 1 );
  VERIFY( std::cout.iword(i) == i + 2 );
  VERIFY( std::cerr.iword(i) == i + 3 );
  VERIFY( std::clog.iword(i) == i + 4 );
}

int main()
{
  test05();
  return 0;
}
