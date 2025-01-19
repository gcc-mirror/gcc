// { dg-do run { target c++17 } }

// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

struct BasicLockable
{
  BasicLockable() : locked(false) { }

  ~BasicLockable() noexcept(false)
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

template<int>
struct Lockable
{
  BasicLockable m;
  void lock() { m.lock(); }
  void unlock() { m.unlock(); }
  bool try_lock() { if (m.locked) return false; m.lock(); return true; }
};

void test01()
{
  BasicLockable m;

  try
    {
      std::scoped_lock<BasicLockable> l(m);
      VERIFY( m.locked );
    }
  catch (...)
    {
      VERIFY( false );
    }

  VERIFY( !m.locked );

  m.lock();

  try
    {
      std::scoped_lock<BasicLockable> l(std::adopt_lock, m);
    }
  catch (...)
    {
      VERIFY( false );
    }

  VERIFY( !m.locked );
}

void test02()
{
  Lockable<1> m1;
  Lockable<2> m2;

  try
    {
      std::scoped_lock<Lockable<1>, Lockable<2>> l(m1, m2);
      VERIFY( m1.m.locked );
      VERIFY( m2.m.locked );
    }
  catch (...)
    {
      VERIFY( false );
    }

  VERIFY( !m1.m.locked );
  VERIFY( !m2.m.locked );

  m1.lock();
  m2.lock();

  try
    {
      std::scoped_lock<Lockable<1>, Lockable<2>> l(std::adopt_lock, m1, m2);
      VERIFY( m1.m.locked );
      VERIFY( m2.m.locked );
    }
  catch (...)
    {
      VERIFY( false );
    }

  VERIFY( !m1.m.locked );
  VERIFY( !m2.m.locked );
}

int main()
{
  test01();
  test02();
}
