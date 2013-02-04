// { dg-do run }
// { dg-options "-std=gnu++11 -g" }

// Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#include <memory>
#include <iostream>

template<class T>
void
placeholder(const T &s)
{
  std::cout << s;
}

template<class T>
void
use(const T &p)
{
  placeholder(&p);
}

struct deleter { void operator()(int*) {} };

std::shared_ptr<int> make(uintptr_t p)
{
  return std::shared_ptr<int>(reinterpret_cast<int*>(p), deleter());
}

int
main()
{
  typedef std::shared_ptr<int> shared;
  typedef std::weak_ptr<int> weak;

  shared esp;
// { dg-final { note-test esp "std::shared_ptr (empty) 0x0" } }
  weak ewp1;
// { dg-final { note-test ewp1 "std::weak_ptr (empty) 0x0" } }
  weak ewp2 = esp;
// { dg-final { note-test ewp2 "std::weak_ptr (empty) 0x0" } }

  shared sp1 = make(0x12345678);
  shared sp2 = sp1;
// { dg-final { note-test sp1 "std::shared_ptr (count 2, weak 0) 0x12345678" } }

  shared sp3 = make(0x12344321);
  weak sp4 = sp3;
  weak wp1 = sp3;
// { dg-final { note-test wp1 "std::weak_ptr (count 1, weak 2) 0x12344321" } }

  shared sp5 = make(0x56788765);
  weak wp2 = sp5;
  sp5.reset();
// { dg-final { note-test wp2 "std::weak_ptr (expired, weak 1) 0x56788765" } }

  placeholder(""); // Mark SPOT
  use(esp);
  use(ewp1);
  use(ewp2);
  use(sp1);
  use(wp1);
  use(wp2);

  return 0;
}

// { dg-final { gdb-test SPOT } }
