// { dg-do compile { target c++20 } }
// { dg-require-effective-target hosted }

// PR libstdc++/105957

#include <memory>

consteval bool test_pr105957()
{
  std::allocator<long long> a;
  auto n = std::size_t(-1) / (sizeof(long long) - 1);
  auto p = a.allocate(n); // { dg-error "constexpr" "" { target c++23_down } }
  a.deallocate(p, n);
  return true;
}
static_assert( test_pr105957() ); // { dg-error "non-constant" }
// { dg-error "uncaught exception of type 'std::bad_array_new_length'" "" { target c++26 } 16 }

// { dg-error "throw_bad_array_new_length" "" { target c++23_down } 0 }
