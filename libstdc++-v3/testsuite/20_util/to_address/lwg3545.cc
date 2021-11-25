// { dg-do compile { target c++20 } }
// { dg-options "-std=gnu++20" }

#include <memory>

template<typename T, bool> struct nttp_ptr
{
  T* operator->() const { return nullptr; }
};

// This gives an error in C++20, which the LWG 3545 resolution should fix:
auto x = std::to_address( nttp_ptr<int, true>() );
