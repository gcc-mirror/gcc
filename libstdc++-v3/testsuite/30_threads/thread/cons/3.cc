// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2008 Free Software Foundation, Inc.
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

#include <functional> // std::unary_function
#include <utility> // std::ref
#include <thread>
#include <system_error>
#include <testsuite_hooks.h>

struct copyable : public std::unary_function<std::thread::id&, void>
{
  copyable() = default;
  ~copyable() = default;
  copyable(const copyable& c)
  { ++copy_count; }

  void operator()(std::thread::id& id) const
  {
    id = std::this_thread::get_id();
  }

  static int copy_count;
};

int copyable::copy_count = 0;

void test03()
{
  bool test __attribute__((unused)) = true;

  try
    {
      std::thread::id t1_id1;
      copyable c1;
      std::thread t1(std::ref(c1), std::ref(t1_id1));
      std::thread::id t1_id2 = t1.get_id();
      VERIFY( t1.joinable() );
      t1.join();
      VERIFY( !t1.joinable() );
      VERIFY( t1_id1 == t1_id2 );
      VERIFY( copyable::copy_count == 0 );

      std::thread::id t2_id1;
      copyable c2;
      std::thread t2(c2, std::ref(t2_id1));
      std::thread::id t2_id2 = t2.get_id();
      VERIFY( t2.joinable() );
      t2.join();
      VERIFY( !t2.joinable() );
      VERIFY( t2_id1 == t2_id2 );
      VERIFY( copyable::copy_count > 0 );
    }
  catch (const std::system_error&)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }
}

int main()
{
  test03();
  return 0;
}
