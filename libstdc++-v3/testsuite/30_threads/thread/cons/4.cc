// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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


#include <functional> // std::ref, std::cref
#include <thread>
#include <system_error>
#include <testsuite_hooks.h>

struct noncopyable
{
  noncopyable() = default;
  ~noncopyable() = default;
  noncopyable(const noncopyable&) = delete;
  noncopyable& operator=(const noncopyable&) = delete;
  void operator()(std::thread::id& id) const
  { 
    id = std::this_thread::get_id();
  }
};

// same as 3, but function is noncopyable function object
// thread variadic cons not copied when std::ref
// thread variadic cons copied when not std::ref
// thread variadic cons not copied when std::cref
// thread variadic cons copied when not std::cref
// no errors
void test03()
{
  try
    {
      std::thread::id t1_id1;
      noncopyable nc1;
      std::thread t1(std::ref(nc1), std::ref(t1_id1));
      std::thread::id t1_id2 = t1.get_id();
      VERIFY( t1.joinable() );
      t1.join();
      VERIFY( !t1.joinable() );
      VERIFY( t1_id1 == t1_id2 );

      std::thread::id t2_id1;
      noncopyable nc2;
      std::thread t2(std::cref(nc2), std::ref(t2_id1));
      std::thread::id t2_id2 = t2.get_id();
      VERIFY( t2.joinable() );
      t2.join();
      VERIFY( !t2.joinable() );
      VERIFY( t2_id1 == t2_id2 );
    }
  catch(const std::system_error&)
    {
      VERIFY( false );
    }
  catch(...)
    {
      VERIFY( false );
    }
}

int main()
{
  test03();
  return 0;
}
