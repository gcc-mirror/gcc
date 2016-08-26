// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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


#include <mutex>
#include <testsuite_hooks.h>

struct Mutex
{
  Mutex() : locked(false) { }

  ~Mutex() throw(int)
  {
    if (locked)
      throw 0;
  }

  void lock()
  {
    if (locked)
      throw 0;
    locked = true;
  }

  void unlock()
  {
    if (!locked)
      throw 0;
    locked = false;
  }

  bool locked;
};

void test01()
{
  bool test __attribute__((unused)) = true;

  Mutex m;
  m.lock();

  try 
    {
      std::lock_guard<Mutex> l(m, std::adopt_lock);
    }
  catch (...)
    {
      VERIFY( false );
    }

  VERIFY( !m.locked );
}

int main()
{
  test01();
  return 0;
}
