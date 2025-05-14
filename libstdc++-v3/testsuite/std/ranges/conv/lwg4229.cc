// { dg-do compile { target c++23 } }

// LWG 4229 std::ranges::to with union return type

#include <ranges>

union U
{
  template<std::ranges::input_range R> U(std::from_range_t, R&&) { }

  int i;
};

void
test_lwg4229(std::ranges::subrange<int*> r)
{
  U u = std::ranges::to<U>(r);
}
