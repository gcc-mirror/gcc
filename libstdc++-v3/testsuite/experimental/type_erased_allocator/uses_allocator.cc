#include <bits/uses_allocator.h>
#include <vector>
#include <experimental/utility>
#include <memory>

using std::vector;
using std::allocator;
using std::uses_allocator;

struct A {
  using allocator_type = std::experimental::erased_type;
};

void test01() {
    static_assert(uses_allocator<vector<int>, allocator<int>>());
    static_assert(uses_allocator<A, allocator<A>>());
}

int main() {
  test01();
  return 0;
}
