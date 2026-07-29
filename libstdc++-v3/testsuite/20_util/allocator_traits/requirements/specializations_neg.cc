// { dg-do compile { target c++11 } }

#include <memory>

template<typename T> struct Alloc : std::allocator<T>
{
  template<typename U>
  struct rebind { using other = Alloc<U>; };
};

template<typename T>
struct std::allocator_traits<Alloc<T>> // { dg-error "cannot be specialized" "" { target c++23 } }
{};

template<>
struct std::allocator_traits<Alloc<void>> // { dg-error "cannot be specialized" "" { target c++23 } }
{};

// { dg-bogus "cannot be specialized" "" { target c++20_down } 0 }
