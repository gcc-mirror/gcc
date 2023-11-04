// { dg-do compile { target c++20 } }

#include <span>

void
test_observers(std::span<int> s)
{
  s.size(); // { dg-warning "ignoring return value" }
  s.size_bytes(); // { dg-warning "ignoring return value" }
  s.empty(); // { dg-warning "ignoring return value" }
}

void
test_element_access(std::span<float> s)
{
  s.front(); // { dg-warning "ignoring return value" }
  s.back(); // { dg-warning "ignoring return value" }
  s[1]; // { dg-warning "ignoring return value" }
  s.data(); // { dg-warning "ignoring return value" }
}

struct S { };

void
test_iterators(std::span<S> s)
{
  s.begin(); // { dg-warning "ignoring return value" }
  s.end(); // { dg-warning "ignoring return value" }
  s.rbegin(); // { dg-warning "ignoring return value" }
  s.rend(); // { dg-warning "ignoring return value" }

#if __cplusplus > 202002L
  s.cbegin(); // { dg-warning "ignoring return value" "" { target c++23 } }
  s.cend(); // { dg-warning "ignoring return value" "" { target c++23 } }
  s.crbegin(); // { dg-warning "ignoring return value" "" { target c++23 } }
  s.crend(); // { dg-warning "ignoring return value" "" { target c++23 } }
#endif
}

void
test_subviews(std::span<long, 20> s)
{
  s.first<5>(); // { dg-warning "ignoring return value" }
  s.first(6); // { dg-warning "ignoring return value" }
  s.last<7>(); // { dg-warning "ignoring return value" }
  s.last(8); // { dg-warning "ignoring return value" }
  s.subspan<1>(); // { dg-warning "ignoring return value" }
  s.subspan<2, 3>(); // { dg-warning "ignoring return value" }
  s.subspan(4); // { dg-warning "ignoring return value" }
  s.subspan(5, 6); // { dg-warning "ignoring return value" }
}

void
test_non_members(std::span<S, 20> s)
{
  std::as_bytes(s); // { dg-warning "ignoring return value" }
  std::as_writable_bytes(s); // { dg-warning "ignoring return value" }
}
