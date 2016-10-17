// 2005-01-11  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

// 4.8 Other transformations

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using std::tr1::aligned_storage;
  using std::tr1::alignment_of;
  using namespace __gnu_test;

  const std::size_t align_c = alignment_of<char>::value;
  VERIFY( (sizeof(aligned_storage<4, align_c>::type) >= 4) );
  VERIFY( (__alignof__(aligned_storage<4, align_c>::type) == align_c) );

  const std::size_t align_s = alignment_of<short>::value;
  VERIFY( (sizeof(aligned_storage<1, align_s>::type) >= 1) );
  VERIFY( (__alignof__(aligned_storage<1, align_s>::type) == align_s) );

  const std::size_t align_i = alignment_of<int>::value;
  VERIFY( (sizeof(aligned_storage<7, align_i>::type) >= 7) );
  VERIFY( (__alignof__(aligned_storage<7, align_i>::type) == align_i) );

  const std::size_t align_d = alignment_of<double>::value;
  VERIFY( (sizeof(aligned_storage<2, align_d>::type) >= 2) );
  VERIFY( (__alignof__(aligned_storage<2, align_d>::type) == align_d) );

  const std::size_t align_ai = alignment_of<int[4]>::value;
  VERIFY( (sizeof(aligned_storage<20, align_ai>::type) >= 20) );
  VERIFY( (__alignof__(aligned_storage<20, align_ai>::type) == align_ai) );

  const std::size_t align_ct = alignment_of<ClassType>::value;
  VERIFY( (sizeof(aligned_storage<11, align_ct>::type) >= 11) );
  VERIFY( (__alignof__(aligned_storage<11, align_ct>::type) == align_ct) );
}

int main()
{
  test01();
  return 0;
}
