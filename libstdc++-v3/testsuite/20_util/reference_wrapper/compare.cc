// { dg-do compile { target c++26 } }


#include <functional>

#ifndef __cpp_lib_reference_wrapper
# error "Feature-test macro for reference_wrapper missing"
#elif __cpp_lib_reference_wrapper != 202403
# error "Feature-test macro for reference_wrapper has wrong value"
#endif

// P2944R3 Comparisons for reference_wrapper

auto check(int i, std::reference_wrapper<int> r) -> bool {
  return i == r;
}

template <class T> using Ref = std::reference_wrapper<T>;

template <class T>
concept ref_equality_comparable
= requires (T a, T const ca, Ref<T> r, Ref<T const> cr) {
    // the usual T is equality-comparable with itself
    a == a;
    a == ca;
    ca == ca;

    // Ref<T> is equality-comparable with itself
    r == r;
    r == cr;
    cr == cr;

    // T and Ref<T> are equality-comparable
    a == r;
    a == cr;
    ca == r;
    ca == cr;
};

static_assert( ref_equality_comparable<int> );

struct A {
    auto operator==(A const&) const -> bool { return true; }
};

struct B {
    friend auto operator==(B const&, B const&) -> bool { return true; }
};

template <class T>
struct C {
    friend auto operator==(C const&, C const&) -> bool { return true; }
};

template <class T>
struct D { };
template <class T>
auto operator==(D<T> const&, D<T> const&) -> bool { return true; }

static_assert(ref_equality_comparable<int>);
static_assert(ref_equality_comparable<A>);
static_assert(ref_equality_comparable<B>);
static_assert(ref_equality_comparable<C<int>>);
static_assert(ref_equality_comparable<D<int>>);
#include <string_view>
static_assert(ref_equality_comparable<std::string_view>);

template <typename T>
struct ValArray {
  friend auto operator==(ValArray const&, ValArray const&) -> ValArray<bool> {
    return {};
  }
};

void f(ValArray<int> v) {
  // this is valid and has type ValArray<bool>
  v == v;

  // this is also valid today and has the same type
  std::ref(v) == std::ref(v);
}

struct ComparesAsInt {
  friend auto operator==(ComparesAsInt, ComparesAsInt) -> int;
};

auto f(std::reference_wrapper<ComparesAsInt> a,
    std::reference_wrapper<ComparesAsInt> b) {
  // today: compiles and returns int
  // proposed: compiles and returns bool
  return a == b;
}

ComparesAsInt& c();
static_assert( std::is_same_v<decltype(f(c(), c())), bool> );
