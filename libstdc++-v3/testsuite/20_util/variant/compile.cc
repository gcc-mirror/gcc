// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016 Free Software Foundation, Inc.
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

#include <variant>
#include <string>
#include <vector>

using namespace std;

struct AllDeleted
{
  AllDeleted() = delete;
  AllDeleted(const AllDeleted&) = delete;
  AllDeleted(AllDeleted&&) = delete;
  AllDeleted& operator=(const AllDeleted&) = delete;
  AllDeleted& operator=(AllDeleted&&) = delete;
};

struct Empty
{
  Empty() { };
  Empty(const Empty&) { };
  Empty(Empty&&) { };
  Empty& operator=(const Empty&) { return *this; };
  Empty& operator=(Empty&&) { return *this; };
};

struct DefaultNoexcept
{
  DefaultNoexcept() noexcept = default;
  DefaultNoexcept(const DefaultNoexcept&) noexcept = default;
  DefaultNoexcept(DefaultNoexcept&&) noexcept = default;
  DefaultNoexcept& operator=(const DefaultNoexcept&) noexcept = default;
  DefaultNoexcept& operator=(DefaultNoexcept&&) noexcept = default;
};

void default_ctor()
{
  static_assert(is_default_constructible_v<variant<int, string>>, "");
  static_assert(is_default_constructible_v<variant<string, string>>, "");
  static_assert(!is_default_constructible_v<variant<>>, "");
  static_assert(!is_default_constructible_v<variant<AllDeleted, string>>, "");
  static_assert(is_default_constructible_v<variant<string, AllDeleted>>, "");

  static_assert(noexcept(variant<int>()), "");
  static_assert(!noexcept(variant<Empty>()), "");
  static_assert(noexcept(variant<DefaultNoexcept>()), "");
}

void copy_ctor()
{
  static_assert(is_copy_constructible_v<variant<int, string>>, "");
  static_assert(!is_copy_constructible_v<variant<AllDeleted, string>>, "");

  {
    variant<int> a;
    static_assert(!noexcept(variant<int>(a)), "");
  }
  {
    variant<string> a;
    static_assert(!noexcept(variant<string>(a)), "");
  }
  {
    variant<int, string> a;
    static_assert(!noexcept(variant<int, string>(a)), "");
  }
  {
    variant<int, char> a;
    static_assert(!noexcept(variant<int, char>(a)), "");
  }
}

void move_ctor()
{
  static_assert(is_move_constructible_v<variant<int, string>>, "");
  static_assert(!is_move_constructible_v<variant<AllDeleted, string>>, "");
  static_assert(!noexcept(variant<int, Empty>(variant<int, Empty>())), "");
  static_assert(noexcept(variant<int, DefaultNoexcept>(variant<int, DefaultNoexcept>())), "");
}

void arbitrary_ctor()
{
  static_assert(!is_constructible_v<variant<string, string>, const char*>, "");
  static_assert(is_constructible_v<variant<int, string>, const char*>, "");
  static_assert(noexcept(variant<int, Empty>(int{})), "");
  static_assert(noexcept(variant<int, DefaultNoexcept>(int{})), "");
  static_assert(!noexcept(variant<int, Empty>(Empty{})), "");
  static_assert(noexcept(variant<int, DefaultNoexcept>(DefaultNoexcept{})), "");
}

void in_place_index_ctor()
{
  variant<string, string> a(in_place<0>, "a");
  variant<string, string> b(in_place<1>, {'a'});
}

void in_place_type_ctor()
{
  variant<int, string, int> a(in_place<string>, "a");
  variant<int, string, int> b(in_place<string>, {'a'});
  static_assert(!is_constructible_v<variant<string, string>, in_place_type_t<string>, const char*>, "");
}

void uses_alloc_ctors()
{
  std::allocator<char> alloc;
  variant<int> a(allocator_arg, alloc);
  static_assert(!is_constructible_v<variant<AllDeleted>, allocator_arg_t, std::allocator<char>>, "");
  {
    variant<int> b(allocator_arg, alloc, a);
    static_assert(!is_constructible_v<variant<void>, allocator_arg_t, std::allocator<char>, const variant<void>&>, "");
  }
  {
    variant<int> b(allocator_arg, alloc, std::move(a));
    static_assert(!is_constructible_v<variant<void>, allocator_arg_t, std::allocator<char>, variant<void>&&>, "");
  }
  {
    variant<string, int> b(allocator_arg, alloc, "a");
    static_assert(!is_constructible_v<variant<string, string>, allocator_arg_t, std::allocator<char>, const char*>, "");
  }
  {
    variant<string, int> b(allocator_arg, alloc, in_place<0>, "a");
    variant<string, string> c(allocator_arg, alloc, in_place<1>, "a");
  }
  {
    variant<string, int> b(allocator_arg, alloc, in_place<0>, {'a'});
    variant<string, string> c(allocator_arg, alloc, in_place<1>, {'a'});
  }
  {
    variant<int, string, int> b(allocator_arg, alloc, in_place<string>, "a");
  }
  {
    variant<int, string, int> b(allocator_arg, alloc, in_place<string>, {'a'});
  }
}

void dtor()
{
  static_assert(is_destructible_v<variant<int, string>>, "");
  static_assert(is_destructible_v<variant<AllDeleted, string>>, "");
}

void copy_assign()
{
  static_assert(is_copy_assignable_v<variant<int, string>>, "");
  static_assert(!is_copy_assignable_v<variant<AllDeleted, string>>, "");
  {
    variant<Empty> a;
    static_assert(!noexcept(a = a), "");
  }
  {
    variant<DefaultNoexcept> a;
    static_assert(!noexcept(a = a), "");
  }

  {
    float f1 = 1.0f, f2 = 2.0f;
    std::variant<float&> v1(f1);
    v1 = f2;
  }
}

void move_assign()
{
  static_assert(is_move_assignable_v<variant<int, string>>, "");
  static_assert(!is_move_assignable_v<variant<AllDeleted, string>>, "");
  {
    variant<Empty> a;
    static_assert(!noexcept(a = std::move(a)), "");
  }
  {
    variant<DefaultNoexcept> a;
    static_assert(noexcept(a = std::move(a)), "");
  }
}

void arbitrary_assign()
{
  static_assert(!is_assignable_v<variant<string, string>, const char*>, "");
  static_assert(is_assignable_v<variant<int, string>, const char*>, "");
  static_assert(noexcept(variant<int, Empty>() = int{}), "");
  static_assert(noexcept(variant<int, DefaultNoexcept>() = int{}), "");
  static_assert(!noexcept(variant<int, Empty>() = Empty{}), "");
  static_assert(noexcept(variant<int, DefaultNoexcept>() = DefaultNoexcept{}), "");
}

void test_get()
{
  {
    static_assert(is_same<decltype(get<0>(variant<int, string>())), int&&>::value, "");
    static_assert(is_same<decltype(get<1>(variant<int, string>())), string&&>::value, "");
    static_assert(is_same<decltype(get<1>(variant<int, string&>())), string&>::value, "");
    static_assert(is_same<decltype(get<1>(variant<int, string&&>())), string&&>::value, "");
    static_assert(is_same<decltype(get<1>(variant<int, const string>())), const string&&>::value, "");
    static_assert(is_same<decltype(get<1>(variant<int, const string&>())), const string&>::value, "");
    static_assert(is_same<decltype(get<1>(variant<int, const string&&>())), const string&&>::value, "");

    static_assert(is_same<decltype(get<int>(variant<int, string>())), int&&>::value, "");
    static_assert(is_same<decltype(get<string>(variant<int, string>())), string&&>::value, "");
    static_assert(is_same<decltype(get<string&>(variant<int, string&>())), string&>::value, "");
    static_assert(is_same<decltype(get<string&&>(variant<int, string&&>())), string&&>::value, "");
    static_assert(is_same<decltype(get<const string>(variant<int, const string>())), const string&&>::value, "");
    static_assert(is_same<decltype(get<const string&>(variant<int, const string&>())), const string&>::value, "");
    static_assert(is_same<decltype(get<const string&&>(variant<int, const string&&>())), const string&&>::value, "");
  }
  {
    variant<int, string> a;
    variant<int, string&> b;
    variant<int, string&&> c;
    variant<int, const string> d;
    variant<int, const string&> e;
    variant<int, const string&&> f;

    static_assert(is_same<decltype(get<0>(a)), int&>::value, "");
    static_assert(is_same<decltype(get<1>(a)), string&>::value, "");
    static_assert(is_same<decltype(get<1>(b)), string&>::value, "");
    static_assert(is_same<decltype(get<1>(c)), string&>::value, "");
    static_assert(is_same<decltype(get<1>(e)), const string&>::value, "");
    static_assert(is_same<decltype(get<1>(e)), const string&>::value, "");
    static_assert(is_same<decltype(get<1>(f)), const string&>::value, "");

    static_assert(is_same<decltype(get<int>(a)), int&>::value, "");
    static_assert(is_same<decltype(get<string>(a)), string&>::value, "");
    static_assert(is_same<decltype(get<string&>(b)), string&>::value, "");
    static_assert(is_same<decltype(get<string&&>(c)), string&>::value, "");
    static_assert(is_same<decltype(get<const string>(e)), const string&>::value, "");
    static_assert(is_same<decltype(get<const string&>(e)), const string&>::value, "");
    static_assert(is_same<decltype(get<const string&&>(f)), const string&>::value, "");

    static_assert(is_same<decltype(get_if<0>(&a)), int*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&a)), string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&b)), string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&c)), string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&e)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&e)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&f)), const string*>::value, "");

    static_assert(is_same<decltype(get_if<int>(&a)), int*>::value, "");
    static_assert(is_same<decltype(get_if<string>(&a)), string*>::value, "");
    static_assert(is_same<decltype(get_if<string&>(&b)), string*>::value, "");
    static_assert(is_same<decltype(get_if<string&&>(&c)), string*>::value, "");
    static_assert(is_same<decltype(get_if<const string>(&e)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<const string&>(&e)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<const string&&>(&f)), const string*>::value, "");
  }
  {
    const variant<int, string> a;
    const variant<int, string&> b;
    const variant<int, string&&> c;
    const variant<int, const string> d;
    const variant<int, const string&> e;
    const variant<int, const string&&> f;

    static_assert(is_same<decltype(get<0>(a)), const int&>::value, "");
    static_assert(is_same<decltype(get<1>(a)), const string&>::value, "");
    static_assert(is_same<decltype(get<1>(b)), string&>::value, "");
    static_assert(is_same<decltype(get<1>(c)), string&>::value, "");
    static_assert(is_same<decltype(get<1>(d)), const string&>::value, "");
    static_assert(is_same<decltype(get<1>(e)), const string&>::value, "");
    static_assert(is_same<decltype(get<1>(f)), const string&>::value, "");

    static_assert(is_same<decltype(get<int>(a)), const int&>::value, "");
    static_assert(is_same<decltype(get<string>(a)), const string&>::value, "");
    static_assert(is_same<decltype(get<string&>(b)), string&>::value, "");
    static_assert(is_same<decltype(get<string&&>(c)), string&>::value, "");
    static_assert(is_same<decltype(get<const string>(d)), const string&>::value, "");
    static_assert(is_same<decltype(get<const string&>(e)), const string&>::value, "");
    static_assert(is_same<decltype(get<const string&&>(f)), const string&>::value, "");

    static_assert(is_same<decltype(get_if<0>(&a)), const int*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&a)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&b)), string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&c)), string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&d)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&e)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<1>(&f)), const string*>::value, "");

    static_assert(is_same<decltype(get_if<int>(&a)), const int*>::value, "");
    static_assert(is_same<decltype(get_if<string>(&a)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<string&>(&b)), string*>::value, "");
    static_assert(is_same<decltype(get_if<string&&>(&c)), string*>::value, "");
    static_assert(is_same<decltype(get_if<const string>(&d)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<const string&>(&e)), const string*>::value, "");
    static_assert(is_same<decltype(get_if<const string&&>(&f)), const string*>::value, "");
  }
}

void test_relational()
{
  {
    const variant<int, string> a, b;
    (void)(a < b);
    (void)(a > b);
    (void)(a <= b);
    (void)(a == b);
    (void)(a != b);
    (void)(a >= b);
  }
  {
    const monostate a, b;
    (void)(a < b);
    (void)(a > b);
    (void)(a <= b);
    (void)(a == b);
    (void)(a != b);
    (void)(a >= b);
  }
}

void test_swap()
{
  variant<int, string> a, b;
  a.swap(b);
  swap(a, b);
}

void test_visit()
{
  {
    struct Visitor
    {
      void operator()(monostate) {}
      void operator()(const int&) {}
    };
    struct CVisitor
    {
      void operator()(monostate) const {}
      void operator()(const int&) const {}
    };
    variant<monostate, int&, const int&, int&&, const int&&> a;
    const variant<monostate, int&, const int&, int&&, const int&&> b;
    Visitor v;
    const CVisitor u;
    static_assert(is_same<void, decltype(visit(Visitor(), a))>::value, "");
    static_assert(is_same<void, decltype(visit(Visitor(), b))>::value, "");
    static_assert(is_same<void, decltype(visit(v, a))>::value, "");
    static_assert(is_same<void, decltype(visit(v, b))>::value, "");
    static_assert(is_same<void, decltype(visit(u, a))>::value, "");
    static_assert(is_same<void, decltype(visit(u, b))>::value, "");
  }
  {
    struct Visitor
    {
      bool operator()(int, float) { return false; }
      bool operator()(int, double) { return false; }
      bool operator()(char, float) { return false; }
      bool operator()(char, double) { return false; }
    };
    visit(Visitor(), variant<int, char>(), variant<float, double>());
  }
}

void test_constexpr()
{
  constexpr variant<int> a;
  static_assert(holds_alternative<int>(a), "");
  constexpr variant<int, char> b(in_place<0>, int{});
  static_assert(holds_alternative<int>(b), "");
  constexpr variant<int, char> c(in_place<int>, int{});
  static_assert(holds_alternative<int>(c), "");
  constexpr variant<int, char> d(in_place<1>, char{});
  static_assert(holds_alternative<char>(d), "");
  constexpr variant<int, char> e(in_place<char>, char{});
  static_assert(holds_alternative<char>(e), "");
  constexpr variant<int, char> f(char{});
  static_assert(holds_alternative<char>(f), "");

  {
    struct literal {
	constexpr literal() = default;
    };

    struct nonliteral {
	nonliteral() { }
    };

    constexpr variant<literal, nonliteral> v{};
    constexpr variant<literal, nonliteral> v1{in_place<literal>};
    constexpr variant<literal, nonliteral> v2{in_place<0>};
  }
}

void test_void()
{
  static_assert(is_same<int&&, decltype(get<int>(variant<int, void>()))>::value, "");
  static_assert(!is_default_constructible_v<variant<void, int>>, "");
  static_assert(!is_copy_constructible_v<variant<int, void>>, "");
  static_assert(!is_move_constructible_v<variant<int, void>>, "");
  static_assert(!is_copy_assignable_v<variant<int, void>>, "");
  static_assert(!is_move_assignable_v<variant<int, void>>, "");
  variant<int, void, string> v;
  v = 3;
  v = "asdf";
}

void test_pr77641()
{
  struct X {
    constexpr X() { }
  };

  constexpr std::variant<X> v1 = X{};
}
