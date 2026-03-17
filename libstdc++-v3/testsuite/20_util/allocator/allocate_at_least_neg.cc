// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

template <typename T>
  struct A : std::allocator<T>
  {
    using Base = std::allocator<T>;
    std::allocation_result<T*> allocate_at_least(size_t) = delete;
    T* allocate(std::size_t n) { return this->Base::allocate(n); }
  };

struct incomplete;

int main()
{
  A<incomplete> a;
  using traits = std::allocator_traits<A<incomplete>>;
  (void) traits::allocate_at_least(a, 1); // { dg-error "from here" }
  // { dg-error "incomplete type" "" { target { *-*-* } } 0 }
}
