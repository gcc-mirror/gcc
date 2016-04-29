// 2006-06-09  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 5.1.6 class random_device [tr.rand.device]
// 5.1.6, p3

#include <tr1/random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std::tr1;

#ifdef _GLIBCXX_USE_RANDOM_TR1
  random_device x("/dev/random");
#else
  random_device x("0");
#endif

  VERIFY( x.min() == std::numeric_limits<random_device::result_type>::min() );
  VERIFY( x.max() == std::numeric_limits<random_device::result_type>::max() );
}

int main()
{
  test01();
  return 0;
}
