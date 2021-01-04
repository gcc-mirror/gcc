// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }
// { dg-additional-options "-pthread" { target pthread } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include "atomic/wait_notify_util.h"

void
test01()
{
  struct S{ int i; };
  std::atomic<S> s;

  s.wait(S{42});
}

int
main ()
{
  // check<bool> bb;
  check<char> ch;
  check<signed char> sch;
  check<unsigned char> uch;
  check<short> s;
  check<unsigned short> us;
  check<int> i;
  check<unsigned int> ui;
  check<long> l;
  check<unsigned long> ul;
  check<long long> ll;
  check<unsigned long long> ull;

  check<wchar_t> wch;
  check<char8_t> ch8;
  check<char16_t> ch16;
  check<char32_t> ch32;

  check<int8_t> i8;
  check<int16_t> i16;
  check<int32_t> i32;
  check<int64_t> i64;

  check<uint8_t> u8;
  check<uint16_t> u16;
  check<uint32_t> u32;
  check<uint64_t> u64;
  return 0;
}
