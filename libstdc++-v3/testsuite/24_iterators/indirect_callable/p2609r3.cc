// Verify the P2609R3 "Relaxing Ranges Just A Smidge" changes.
// { dg-do compile { target c++20 } }

#include <iterator>
#include <algorithm>
#include <memory>
#include <ranges>

int
main()
{
  auto v = std::views::iota(0, 5);
  auto proj = [](int v) { return std::make_unique<int>(v); };
  using it = std::projected<std::ranges::iterator_t<decltype(v)>, decltype(proj)>;

  auto f = [](auto) { return false; };
  static_assert(std::indirectly_unary_invocable<decltype(f), it>);
  static_assert(std::indirectly_regular_unary_invocable<decltype(f), it>);
  static_assert(std::indirect_unary_predicate<decltype(f), it>);

  auto g = [](auto, auto) { return false; };
  static_assert(std::indirect_binary_predicate<decltype(g), it, it>);
  static_assert(std::indirect_equivalence_relation<decltype(g), it, it>);
  static_assert(std::indirect_strict_weak_order<decltype(g), it, it>);

  std::ranges::for_each(v, f, proj);
}
