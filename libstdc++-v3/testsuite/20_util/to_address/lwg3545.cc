// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <memory>

template<typename T, bool> struct nttp_ptr
{
  T* operator->() const { return nullptr; }
};

// This gives an error in C++20, which the LWG 3545 resolution should fix:
auto x = std::to_address( nttp_ptr<int, true>() );

template<typename T>
struct clever_ptr
{
  static T obj;
  constexpr T* operator->() const { return &obj; }
};

// pointer_traits specialization is valid, but to_address uses operator->
static_assert( std::to_address(clever_ptr<char>{}) == &clever_ptr<char>::obj );

int the_int;

template<>
struct std::pointer_traits<clever_ptr<int>>
{
  using element_type = int;
  using difference_type = std::ptrdiff_t;
  using pointer = clever_ptr<int>;

  static constexpr int* to_address(pointer p) { return &the_int; }
};

// Uses pointer_traits<clever_ptr<int>>::to_address
static_assert( std::to_address(clever_ptr<int>{}) == &the_int );
