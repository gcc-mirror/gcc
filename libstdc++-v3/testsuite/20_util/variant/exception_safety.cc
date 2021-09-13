// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <variant>
#include <vector>
#include <string>
#include <memory_resource>
#include <memory>
#include <functional>
#include <any>
#include <optional>
#include <testsuite_hooks.h>

void
test01()
{
#if _GLIBCXX_USE_CXX11_ABI
  std::variant<int, std::pmr::string, std::pmr::vector<int>> v(1);
  VERIFY( v.index() == 0 );

  try
  {
    std::pmr::string s = "how long is a piece of SSO string?";
    v.emplace<1>(s, std::pmr::null_memory_resource());
    VERIFY( false );
  }
  catch(const std::bad_alloc&)
  {
    VERIFY( v.valueless_by_exception() );
  }

  v.emplace<0>(2);
  VERIFY( v.index() == 0 );

  try
  {
    v.emplace<2>({1, 2, 3}, std::pmr::null_memory_resource());
    VERIFY( false );
  }
  catch(const std::bad_alloc&)
  {
    VERIFY( v.valueless_by_exception() );
  }
#endif
}

void
test02()
{
  struct X
  {
    X(int i) : i(1) { if (i > 2) throw 3; }
    X(std::initializer_list<int> l) : i(2) { if (l.size() > 2) throw 3; }
    int i;
  };
  static_assert( std::is_trivially_copyable_v<X> );

  std::variant<std::monostate, int, X> v(111);
  VERIFY( v.index() == 1 );

  try
  {
    v.emplace<X>(3);
    VERIFY( false );
  }
  catch(int)
  {
    VERIFY( !v.valueless_by_exception() );
    VERIFY( v.index() == 1 );
    VERIFY( std::get<int>(v) == 111 );
  }

  v.emplace<X>(1);
  VERIFY( v.index() == 2 );
  VERIFY( std::get<X>(v).i == 1 );

  try
  {
    v.emplace<X>(3);
    VERIFY( false );
  }
  catch(int)
  {
    VERIFY( !v.valueless_by_exception() );
    VERIFY( v.index() == 2 );
    VERIFY( std::get<X>(v).i == 1 );
  }

  try
  {
    v.emplace<X>({1, 2, 3});
    VERIFY( false );
  }
  catch(int)
  {
    VERIFY( !v.valueless_by_exception() );
    VERIFY( v.index() == 2 );
    VERIFY( std::get<X>(v).i == 1 );
  }
}

template<typename T, typename V>
  bool bad_emplace(V& v)
  {
    struct X {
      operator T() const { throw 1; }
    };

    const auto index = v.index();

    try
    {
      if (std::is_same_v<T, std::any>)
      {
	// Need to test std::any differently, because emplace<std::any>(X{})
	// would create a std::any with a contained X, instead of using
	// X::operator any() to convert to std::any.
	struct ThrowOnCopy {
	  ThrowOnCopy() { }
	  ThrowOnCopy(const ThrowOnCopy&) { throw 1; }
	} t;
	v.template emplace<std::any>(t);
      }
      else
	v.template emplace<T>(X{});
    }
    catch (int)
    {
      return v.index() == index;
    }
    return false;
  }

void
test03()
{
  struct TriviallyCopyable { int i = 0; };

  std::variant<std::monostate, int, TriviallyCopyable, std::optional<int>,
    std::string, std::vector<int>, std::function<void()>, std::any,
    std::shared_ptr<int>, std::weak_ptr<int>, std::unique_ptr<int>> v(1);
  VERIFY( v.index() == 1 );

  VERIFY( bad_emplace<int>(v) );
  VERIFY( bad_emplace<TriviallyCopyable>(v) );
  VERIFY( bad_emplace<std::optional<int>>(v) );
  VERIFY( bad_emplace<std::string>(v) );
  VERIFY( bad_emplace<std::vector<int>>(v) );
  VERIFY( bad_emplace<std::function<void()>>(v) );
  VERIFY( bad_emplace<std::any>(v) );
  VERIFY( bad_emplace<std::shared_ptr<int>>(v) );
  VERIFY( bad_emplace<std::weak_ptr<int>>(v) );
  VERIFY( bad_emplace<std::unique_ptr<int>>(v) );
}

void
test04()
{
  // LWG 2904. Make variant move-assignment more exception safe

  struct ThrowOnCopy
  {
    ThrowOnCopy() { }
    ThrowOnCopy(const ThrowOnCopy&) { throw 1; }
    ThrowOnCopy& operator=(const ThrowOnCopy&) { throw "shouldn't happen"; }
    ThrowOnCopy(ThrowOnCopy&&) noexcept { }
  };

  std::variant<int, ThrowOnCopy> v1(std::in_place_type<ThrowOnCopy>), v2(2);
  try
  {
    v2 = v1; // uses variant<Types...>::operator=(const variant&)
    VERIFY( false );
  }
  catch (int)
  {
    VERIFY( !v2.valueless_by_exception() );
    VERIFY( v2.index() == 0 );
    VERIFY( std::get<0>(v2) == 2 );
  }

  try
  {
    ThrowOnCopy toc;
    v2 = toc; // uses variant<Types...>::operator=(T&&)
    VERIFY( false );
  }
  catch (int)
  {
    VERIFY( !v2.valueless_by_exception() );
    VERIFY( v2.index() == 0 );
    VERIFY( std::get<0>(v2) == 2 );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
