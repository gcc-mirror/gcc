// { dg-options "-std=gnu++0x" }

// 2010-02-19  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

// XXX FIXME:  parallel-mode should deal correctly with moveable-only types
// per C++0x, at minimum smoothly fall back to serial.
#undef _GLIBCXX_PARALLEL

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct Function
{
  Function() : tot(0) { }
  Function(Function&& f) : tot(f.tot) { f.tot = 0; }

  Function(const Function&) = delete;

  void operator()(int num)
  { tot += num; }

  int get() { return tot; }

private:
  int tot;
};

void test01()
{
  bool test __attribute__((unused)) = true;
  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;

  typedef test_container<int, input_iterator_wrapper> Container;

  int array[5] = { 1, 2, 3, 4, 5 };
  Container con(array, array + 5);

  Function f;
  Function f_res = std::for_each(con.begin(), con.end(), std::move(f));

  VERIFY( f_res.get() == 15 );
}

int main()
{
  test01();
  return 0;
}
