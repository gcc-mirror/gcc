// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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


#include <future>
#include <testsuite_hooks.h>

struct Chucky
{
  Chucky() : copied(false) { }

  Chucky(const Chucky& other) : copied(true)
  {
    if (other.copied)
      return;
    other.copied = true;
    using namespace std;
    // Throw on first DECAY_COPY to simulate inability to start a new thread.
    throw system_error(make_error_code(errc::resource_unavailable_try_again));
  }

  void operator()() const { }

  mutable bool copied;
};

void test01()
{
  using namespace std;

  future<void> f = async(Chucky{});
  VERIFY( f.wait_for(chrono::seconds(100)) == future_status::deferred );

  bool caught = false;
  try {
    f = async(launch::async, Chucky{});
  } catch (const system_error&) {
    caught = true;
  }
  VERIFY( caught );
}

int main()
{
  test01();
}
