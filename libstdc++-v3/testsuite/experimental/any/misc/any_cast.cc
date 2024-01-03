// { dg-do run { target c++14 } }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

#include <experimental/any>
#include <string>
#include <cstring>
#include <testsuite_hooks.h>

using std::experimental::any;
using std::experimental::any_cast;
using std::experimental::bad_any_cast;

void test01()
{
  using std::string;
  using std::strcmp;

  // taken from example in N3804 proposal

  any x(5);                                   // x holds int
  VERIFY(any_cast<int>(x) == 5);              // cast to value
  any_cast<int&>(x) = 10;                     // cast to reference
  VERIFY(any_cast<int>(x) == 10);

  x = "Meow";                                 // x holds const char*
  VERIFY(strcmp(any_cast<const char*>(x), "Meow") == 0);
  any_cast<const char*&>(x) = "Harry";
  VERIFY(strcmp(any_cast<const char*>(x), "Harry") == 0);

  x = string("Meow");                         // x holds string
  string s, s2("Jane");
  s = move(any_cast<string&>(x));             // move from any
  VERIFY(s == "Meow");
  any_cast<string&>(x) = move(s2);            // move to any
  VERIFY(any_cast<const string&>(x) == "Jane");

  string cat("Meow");
  const any y(cat);                           // const y holds string
  VERIFY(any_cast<const string&>(y) == cat);
}

void test02()
{
  any x(1);
  auto p = any_cast<double>(&x);
  VERIFY(p == nullptr);

  x = 1.0;
  p = any_cast<double>(&x);
  VERIFY(p != nullptr);

  x = any();
  p = any_cast<double>(&x);
  VERIFY(p == nullptr);

  try {
    any_cast<double>(x);
    VERIFY(false);
  } catch (const bad_any_cast&) {
  }
}

static int move_count = 0;

void test03()
{
  struct MoveEnabled
  {
    MoveEnabled(MoveEnabled&&)
    {
      ++move_count;
    }
    MoveEnabled() = default;
    MoveEnabled(const MoveEnabled&) = default;
  };
  MoveEnabled m;
  MoveEnabled m2 = any_cast<MoveEnabled>(any(m));
  VERIFY(move_count == 1);
  MoveEnabled&& m3 = any_cast<MoveEnabled&&>(any(m));
  VERIFY(move_count == 1);
  struct MoveDeleted
  {
    MoveDeleted(MoveDeleted&&) = delete;
    MoveDeleted() = default;
    MoveDeleted(const MoveDeleted&) = default;
  };
  MoveDeleted md;
  MoveDeleted&& md2 = any_cast<MoveDeleted>(any(std::move(md)));
  MoveDeleted&& md3 = any_cast<MoveDeleted&&>(any(std::move(md)));
}

void test05()
{
  // PR libstdc++/69321
  struct noncopyable {
    noncopyable(noncopyable const&) = delete;
  };

  any a;
  auto p = any_cast<noncopyable>(&a);
  VERIFY( p == nullptr );
}

void test06()
{
  // The contained value of a std::any is always an object type,
  // but any_cast does not forbid checking for function types.

  any a(1);
  void (*p1)() = any_cast<void()>(&a);
  VERIFY( p1 == nullptr );
  int (*p2)(int) = any_cast<int(int)>(&a);
  VERIFY( p2 == nullptr );
  int (*p3)() = any_cast<int()>(&const_cast<const any&>(a));
  VERIFY( p3 == nullptr );

  try {
    any_cast<int(&)()>(a);
    VERIFY( false );
  } catch (const bad_any_cast&) {
  }

  try {
    any_cast<int(&)()>(std::move(a));
    VERIFY( false );
  } catch (const bad_any_cast&) {
  }

  try {
    any_cast<int(&)()>(const_cast<const any&>(a));
    VERIFY( false );
  } catch (const bad_any_cast&) {
  }
}

void test07()
{
  int arr[3];
  any a(arr);
#if __cpp_rtti
  VERIFY( a.type() == typeid(int*) );	// contained value is decayed
#endif

  int (*p1)[3] = any_cast<int[3]>(&a);
#if __cpp_rtti
  VERIFY( a.type() != typeid(int[3]) ); // so any_cast should return nullptr
#endif
  VERIFY( p1 == nullptr );
  int (*p2)[] = any_cast<int[]>(&a);
#if __cpp_rtti
  VERIFY( a.type() != typeid(int[]) );	// so any_cast should return nullptr
#endif
  VERIFY( p2 == nullptr );
  const int (*p3)[] = any_cast<int[]>(&const_cast<const any&>(a));
  VERIFY( p3 == nullptr );
}

int main()
{
  test01();
  test02();
  test03();
  test05();
  test06();
  test07();
}
