// { dg-do compile { target c++11 } }
#include <vector>

// Custom allocator defined with std::allocator, but doesn't provide rebind.
template<typename T> struct Alloc : std::allocator<T> { };

std::vector<int, Alloc<int>> v; // { dg-error "here" "" { target c++17_down } }

// Custom allocator that does provide rebind, but incorrectly.
template<typename T> struct Alloc2
{
  using value_type = T;
  template<typename U> struct rebind { using other = Alloc<U>; }; // not Alloc2
  T* allocate(std::size_t n) { return std::allocator<T>().allocate(n); }
  void deallocate(T* p, std::size_t n) { std::allocator<T>().deallocate(p, n); }
};

std::vector<int, Alloc2<int>> v2; // { dg-error "here" }

// { dg-error "static assertion failed: .*rebind_alloc" "" { target *-*-* } 0 }
