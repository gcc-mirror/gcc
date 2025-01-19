// { dg-do run { target { c++20 && c++23_down } } }
// { dg-do compile { target c++26 } }

#include <span>
#include <utility>

#include <testsuite_hooks.h>

struct Any {
  constexpr Any() { }
  template<typename T> constexpr Any(T) { }
};

// Examples from P2447R4
void one(std::pair<int, int>) {}
void one(std::span<const int>) {}
void two(std::span<const int, 2>) {}
constexpr std::size_t three(std::span<void * const> v) { return v.size(); }
constexpr std::size_t four(std::span<const Any> v) { return v.size(); }

void *array3[10];
Any array4[10];

int main()
{
  one({1, 2}); // { dg-error "call of overloaded" "should be ambiguous with the one(std::pair) overload" { target c++26 } }
  two({{1, 2}}); // { dg-error "would use explicit constructor" "should prefer the initializer_list constructor, which is explicit" { target c++26 } }

#if __cpp_lib_span_initializer_list
  static_assert(three({array3, 0}) == 2);
  static_assert(four({array4, array4 + 10}) == 2);
#else
  static_assert(three({array3, 0}) == 0);
  static_assert(four({array4, array4 + 10}) == 10);
#endif
}
