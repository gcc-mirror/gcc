// { dg-do run { target c++20 } }
// Bug libstdc++/108487
// ~20-30x slowdown in populating std::vector from std::ranges::iota_view

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test_pr108487()
{
  using __gnu_test::tracker_allocator;
  using __gnu_test::tracker_allocator_counter;
  auto r = std::ranges::iota_view{0, 20};
  tracker_allocator_counter::reset();
  std::vector<int, tracker_allocator<int>> v{r.begin(), r.end()};
  const std::size_t bytes = v.capacity() * sizeof(v.front());
  VERIFY( tracker_allocator_counter::get_allocation_count() == bytes );
}

int main()
{
  test_pr108487();
}
