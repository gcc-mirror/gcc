// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

// P2538R1 ADL-proof std::projected
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2538r1.html

#include <iterator>

template<typename T>
  concept has_diff_type = requires { typename T::difference_type; };

static_assert( has_diff_type<std::projected<int*, void(*)(int)>> );

struct Indy {
  using value_type = int;
  int operator*() const { return 0; }
};
static_assert( ! std::weakly_incrementable<Indy> );
static_assert( ! has_diff_type<std::projected<Indy, void(*)(int)>> );


// Examples from the paper:

template<class T> struct Holder { T t; };
struct Incomplete;

void test_concepts()
{
  using T = Holder<Incomplete>*;
  static_assert(std::equality_comparable<T>);
  (void) std::indirectly_comparable<T*, T*, std::equal_to<>>;
  (void) std::sortable<T*>;
}

#include <algorithm>

void test_count()
{
  Holder<Incomplete>* a = nullptr;
  (void) std::count(&a, &a, nullptr);
  (void) std::ranges::count(&a, &a, nullptr); // { dg-bogus "." }
}
