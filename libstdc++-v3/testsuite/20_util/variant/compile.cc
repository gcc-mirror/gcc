// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

struct MoveCtorOnly
{
  MoveCtorOnly() noexcept = delete;
  MoveCtorOnly(const DefaultNoexcept&) noexcept = delete;
  MoveCtorOnly(DefaultNoexcept&&) noexcept { }
  MoveCtorOnly& operator=(const DefaultNoexcept&) noexcept = delete;
  MoveCtorOnly& operator=(DefaultNoexcept&&) noexcept = delete;
};

struct nonliteral
{
  nonliteral() { }

  bool operator<(const nonliteral&) const;
  bool operator<=(const nonliteral&) const;
  bool operator==(const nonliteral&) const;
  bool operator!=(const nonliteral&) const;
  bool operator>=(const nonliteral&) const;
  bool operator>(const nonliteral&) const;
};

void default_ctor()
{
  static_assert(is_default_constructible_v<variant<int, string>>, "");
  static_assert(is_default_constructible_v<variant<string, string>>, "");
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
  static_assert(is_trivially_copy_constructible_v<variant<int>>, "");
  static_assert(!is_trivially_copy_constructible_v<variant<std::string>>, "");

  {
    variant<int> a;
    static_assert(noexcept(variant<int>(a)), "");
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
    static_assert(noexcept(variant<int, char>(a)), "");
  }
}

void move_ctor()
{
  static_assert(is_move_constructible_v<variant<int, string>>, "");
  static_assert(!is_move_constructible_v<variant<AllDeleted, string>>, "");
  static_assert(is_trivially_move_constructible_v<variant<int>>, "");
  static_assert(!is_trivially_move_constructible_v<variant<std::string>>, "");
  static_assert(!noexcept(variant<int, Empty>(declval<variant<int, Empty>>())), "");
  static_assert(noexcept(variant<int, DefaultNoexcept>(declval<variant<int, DefaultNoexcept>>())), "");
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
  variant<string, string> a(in_place_index<0>, "a");
  variant<string, string> b(in_place_index<1>, {'a'});
}

void in_place_type_ctor()
{
  variant<int, string, int> a(in_place_type<string>, "a");
  variant<int, string, int> b(in_place_type<string>, {'a'});
  static_assert(!is_constructible_v<variant<string, string>, in_place_type_t<string>, const char*>, "");
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
  static_assert(is_trivially_copy_assignable_v<variant<int>>, "");
  static_assert(!is_trivially_copy_assignable_v<variant<string>>, "");
  {
    variant<Empty> a;
    static_assert(!noexcept(a = a), "");
  }
  {
    variant<DefaultNoexcept> a;
    static_assert(noexcept(a = a), "");
  }
}

void move_assign()
{
  static_assert(is_move_assignable_v<variant<int, string>>, "");
  static_assert(!is_move_assignable_v<variant<AllDeleted, string>>, "");
  static_assert(is_trivially_move_assignable_v<variant<int>>, "");
  static_assert(!is_trivially_move_assignable_v<variant<string>>, "");
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
  static_assert(is_same<decltype(get<0>(variant<int, string>())), int&&>::value, "");
  static_assert(is_same<decltype(get<1>(variant<int, string>())), string&&>::value, "");
  static_assert(is_same<decltype(get<1>(variant<int, const string>())), const string&&>::value, "");

  static_assert(is_same<decltype(get<int>(variant<int, string>())), int&&>::value, "");
  static_assert(is_same<decltype(get<string>(variant<int, string>())), string&&>::value, "");
  static_assert(is_same<decltype(get<const string>(variant<int, const string>())), const string&&>::value, "");
}

void test_relational()
{
  {
    constexpr variant<int, nonliteral> a(42), b(43);
    static_assert((a < b), "");
    static_assert(!(a > b), "");
    static_assert((a <= b), "");
    static_assert(!(a == b), "");
    static_assert((a != b), "");
    static_assert(!(a >= b), "");
  }
  {
    constexpr variant<int, nonliteral> a(42), b(42);
    static_assert(!(a < b), "");
    static_assert(!(a > b), "");
    static_assert((a <= b), "");
    static_assert((a == b), "");
    static_assert(!(a != b), "");
    static_assert((a >= b), "");
  }
  {
    constexpr variant<int, nonliteral> a(43), b(42);
    static_assert(!(a < b), "");
    static_assert((a > b), "");
    static_assert(!(a <= b), "");
    static_assert(!(a == b), "");
    static_assert((a != b), "");
    static_assert((a >= b), "");
  }
  {
    constexpr monostate a, b;
    static_assert(!(a < b), "");
    static_assert(!(a > b), "");
    static_assert((a <= b), "");
    static_assert((a == b), "");
    static_assert(!(a != b), "");
    static_assert((a >= b), "");
  }
}

// Not swappable, and variant<C> not swappable via the generic std::swap.
struct C { };
void swap(C&, C&) = delete;

static_assert( !std::is_swappable_v<variant<C>> );
static_assert( !std::is_swappable_v<variant<int, C>> );
static_assert( !std::is_swappable_v<variant<C, int>> );

// Not swappable, and variant<D> not swappable via the generic std::swap.
struct D { D(D&&) = delete; };

static_assert( !std::is_swappable_v<variant<D>> );
static_assert( !std::is_swappable_v<variant<int, D>> );
static_assert( !std::is_swappable_v<variant<D, int>> );

void test_swap()
{
  static_assert(is_swappable_v<variant<int, string>>, "");
  static_assert(is_swappable_v<variant<MoveCtorOnly>>, "");
  static_assert(!is_swappable_v<variant<AllDeleted>>, "");
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
  {
    struct Visitor
    {
      constexpr bool operator()(const int&) { return true; }
      constexpr bool operator()(const nonliteral&) { return false; }
    };
    static_assert(visit(Visitor(), variant<int, nonliteral>(0)), "");
  }
  {
    struct Visitor
    {
      constexpr bool operator()(const int&) { return true; }
      constexpr bool operator()(const nonliteral&) { return false; }
    };
    static_assert(visit(Visitor(), variant<int, nonliteral>(0)), "");
  }
  // PR libstdc++/79513
  {
    std::variant<int> v [[gnu::unused]] (5);
    std::visit([](int&){}, v);
    std::visit([](int&&){}, std::move(v));
  }
}

void test_constexpr()
{
  constexpr variant<int> a;
  static_assert(holds_alternative<int>(a), "");
  constexpr variant<int, char> b(in_place_index<0>, int{});
  static_assert(holds_alternative<int>(b), "");
  constexpr variant<int, char> c(in_place_type<int>, int{});
  static_assert(holds_alternative<int>(c), "");
  constexpr variant<int, char> d(in_place_index<1>, char{});
  static_assert(holds_alternative<char>(d), "");
  constexpr variant<int, char> e(in_place_type<char>, char{});
  static_assert(holds_alternative<char>(e), "");
  constexpr variant<int, char> f(char{});
  static_assert(holds_alternative<char>(f), "");

  {
    struct literal {
	constexpr literal() = default;
    };

    constexpr variant<literal, nonliteral> v{};
    constexpr variant<literal, nonliteral> v1{in_place_type<literal>};
    constexpr variant<literal, nonliteral> v2{in_place_index<0>};
  }

  {
    constexpr variant<int> a(42);
    static_assert(get<0>(a) == 42, "");
  }
  {
    constexpr variant<int, nonliteral> a(42);
    static_assert(get<0>(a) == 42, "");
  }
  {
    constexpr variant<nonliteral, int> a(42);
    static_assert(get<1>(a) == 42, "");
  }
  {
    constexpr variant<int> a(42);
    static_assert(get<int>(a) == 42, "");
  }
  {
    constexpr variant<int, nonliteral> a(42);
    static_assert(get<int>(a) == 42, "");
  }
  {
    constexpr variant<nonliteral, int> a(42);
    static_assert(get<int>(a) == 42, "");
  }
  {
    constexpr variant<int> a(42);
    static_assert(get<0>(std::move(a)) == 42, "");
  }
  {
    constexpr variant<int, nonliteral> a(42);
    static_assert(get<0>(std::move(a)) == 42, "");
  }
  {
    constexpr variant<nonliteral, int> a(42);
    static_assert(get<1>(std::move(a)) == 42, "");
  }
  {
    constexpr variant<int> a(42);
    static_assert(get<int>(std::move(a)) == 42, "");
  }
  {
    constexpr variant<int, nonliteral> a(42);
    static_assert(get<int>(std::move(a)) == 42, "");
  }
  {
    constexpr variant<nonliteral, int> a(42);
    static_assert(get<int>(std::move(a)) == 42, "");
  }
}

void test_pr77641()
{
  struct X {
    constexpr X() { }
  };

  constexpr std::variant<X> v1 = X{};
}

namespace adl_trap
{
  struct X {
    X() = default;
    X(int) { }
    X(std::initializer_list<int>, const X&) { }
  };
  template<typename T> void move(T&) { }
  template<typename T> void forward(T&) { }

  struct Visitor {
    template<typename T> void operator()(T&&) { }
  };
}

void test_adl()
{
   using adl_trap::X;
   X x;
   std::initializer_list<int> il;
   adl_trap::Visitor vis;

   std::variant<X> v0(x);
   v0 = x;
   v0.emplace<0>(x);
   v0.emplace<0>(il, x);
   visit(vis, v0);
   variant<X> v1{in_place_index<0>, x};
   variant<X> v2{in_place_type<X>, x};
   variant<X> v3{in_place_index<0>, il, x};
   variant<X> v4{in_place_type<X>, il, x};
}

void test_variant_alternative()
{
  static_assert(is_same_v<variant_alternative_t<0, variant<int, string>>, int>, "");
  static_assert(is_same_v<variant_alternative_t<1, variant<int, string>>, string>, "");

  static_assert(is_same_v<variant_alternative_t<0, const variant<int>>, const int>, "");
  static_assert(is_same_v<variant_alternative_t<0, volatile variant<int>>, volatile int>, "");
  static_assert(is_same_v<variant_alternative_t<0, const volatile variant<int>>, const volatile int>, "");
}

template<typename V, typename T>
  constexpr auto has_type_emplace(int) -> decltype((declval<V>().template emplace<T>(), true))
  { return true; };

template<typename V, typename T>
  constexpr bool has_type_emplace(...)
  { return false; };

template<typename V, size_t N>
  constexpr auto has_index_emplace(int) -> decltype((declval<V>().template emplace<N>(), true))
  { return true; };

template<typename V, size_t T>
  constexpr bool has_index_emplace(...)
  { return false; };

void test_emplace()
{
  static_assert(has_type_emplace<variant<int>, int>(0), "");
  static_assert(!has_type_emplace<variant<long>, int>(0), "");
  static_assert(has_index_emplace<variant<int>, 0>(0), "");
  static_assert(!has_type_emplace<variant<AllDeleted>, AllDeleted>(0), "");
  static_assert(!has_index_emplace<variant<AllDeleted>, 0>(0), "");
}

void test_triviality()
{
#define TEST_TEMPLATE(DT, CC, MC, CA, MA, CC_VAL, MC_VAL, CA_VAL, MA_VAL) \
  { \
    struct A \
    { \
      ~A() DT; \
      A(const A&) CC; \
      A(A&&) MC; \
      A& operator=(const A&) CA; \
      A& operator=(A&&) MA; \
    }; \
    static_assert(CC_VAL == is_trivially_copy_constructible_v<variant<A>>, ""); \
    static_assert(MC_VAL == is_trivially_move_constructible_v<variant<A>>, ""); \
    static_assert(CA_VAL == is_trivially_copy_assignable_v<variant<A>>, ""); \
    static_assert(MA_VAL == is_trivially_move_assignable_v<variant<A>>, ""); \
  }
  TEST_TEMPLATE(=default, =default, =default, =default, =default,  true,  true,  true,  true)
  TEST_TEMPLATE(=default, =default, =default, =default,         ,  true,  true,  true, false)
  TEST_TEMPLATE(=default, =default, =default,         , =default,  true,  true, false,  true)
  TEST_TEMPLATE(=default, =default, =default,         ,         ,  true,  true, false, false)
  TEST_TEMPLATE(=default, =default,         , =default, =default,  true, false,  true,  true)
  TEST_TEMPLATE(=default, =default,         , =default,         ,  true, false,  true, false)
  TEST_TEMPLATE(=default, =default,         ,         , =default,  true, false, false,  true)
  TEST_TEMPLATE(=default, =default,         ,         ,         ,  true, false, false, false)
  TEST_TEMPLATE(=default,         , =default, =default, =default, false,  true,  true,  true)
  TEST_TEMPLATE(=default,         , =default, =default,         , false,  true,  true, false)
  TEST_TEMPLATE(=default,         , =default,         , =default, false,  true, false,  true)
  TEST_TEMPLATE(=default,         , =default,         ,         , false,  true, false, false)
  TEST_TEMPLATE(=default,         ,         , =default, =default, false, false,  true,  true)
  TEST_TEMPLATE(=default,         ,         , =default,         , false, false,  true, false)
  TEST_TEMPLATE(=default,         ,         ,         , =default, false, false, false,  true)
  TEST_TEMPLATE(=default,         ,         ,         ,         , false, false, false, false)
  TEST_TEMPLATE(        , =default, =default, =default, =default, false, false, false, false)
  TEST_TEMPLATE(        , =default, =default, =default,         , false, false, false, false)
  TEST_TEMPLATE(        , =default, =default,         , =default, false, false, false, false)
  TEST_TEMPLATE(        , =default, =default,         ,         , false, false, false, false)
  TEST_TEMPLATE(        , =default,         , =default, =default, false, false, false, false)
  TEST_TEMPLATE(        , =default,         , =default,         , false, false, false, false)
  TEST_TEMPLATE(        , =default,         ,         , =default, false, false, false, false)
  TEST_TEMPLATE(        , =default,         ,         ,         , false, false, false, false)
  TEST_TEMPLATE(        ,         , =default, =default, =default, false, false, false, false)
  TEST_TEMPLATE(        ,         , =default, =default,         , false, false, false, false)
  TEST_TEMPLATE(        ,         , =default,         , =default, false, false, false, false)
  TEST_TEMPLATE(        ,         , =default,         ,         , false, false, false, false)
  TEST_TEMPLATE(        ,         ,         , =default, =default, false, false, false, false)
  TEST_TEMPLATE(        ,         ,         , =default,         , false, false, false, false)
  TEST_TEMPLATE(        ,         ,         ,         , =default, false, false, false, false)
  TEST_TEMPLATE(        ,         ,         ,         ,         , false, false, false, false)
#undef TEST_TEMPLATE

#define TEST_TEMPLATE(CC, MC, CA, MA) \
  { \
    struct A \
    { \
      A(const A&) CC; \
      A(A&&) MC; \
      A& operator=(const A&) CA; \
      A& operator=(A&&) MA; \
    }; \
    static_assert(!is_trivially_copy_constructible_v<variant<AllDeleted, A>>, ""); \
    static_assert(!is_trivially_move_constructible_v<variant<AllDeleted, A>>, ""); \
    static_assert(!is_trivially_copy_assignable_v<variant<AllDeleted, A>>, ""); \
    static_assert(!is_trivially_move_assignable_v<variant<AllDeleted, A>>, ""); \
  }
  TEST_TEMPLATE(=default, =default, =default, =default)
  TEST_TEMPLATE(=default, =default, =default,         )
  TEST_TEMPLATE(=default, =default,         , =default)
  TEST_TEMPLATE(=default, =default,         ,         )
  TEST_TEMPLATE(=default,         , =default, =default)
  TEST_TEMPLATE(=default,         , =default,         )
  TEST_TEMPLATE(=default,         ,         , =default)
  TEST_TEMPLATE(=default,         ,         ,         )
  TEST_TEMPLATE(        , =default, =default, =default)
  TEST_TEMPLATE(        , =default, =default,         )
  TEST_TEMPLATE(        , =default,         , =default)
  TEST_TEMPLATE(        , =default,         ,         )
  TEST_TEMPLATE(        ,         , =default, =default)
  TEST_TEMPLATE(        ,         , =default,         )
  TEST_TEMPLATE(        ,         ,         , =default)
  TEST_TEMPLATE(        ,         ,         ,         )
#undef TEST_TEMPLATE

  static_assert(is_trivially_copy_constructible_v<variant<DefaultNoexcept, int, char, float, double>>, "");
  static_assert(is_trivially_move_constructible_v<variant<DefaultNoexcept, int, char, float, double>>, "");
  static_assert(is_trivially_copy_assignable_v<variant<DefaultNoexcept, int, char, float, double>>, "");
  static_assert(is_trivially_move_assignable_v<variant<DefaultNoexcept, int, char, float, double>>, "");
}
