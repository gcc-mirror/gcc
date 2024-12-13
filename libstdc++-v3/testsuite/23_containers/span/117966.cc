// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do compile { target c++20 } }

// Bug 117966
// constexpr std::span construction fails to compile with D_GLIBCXX_DEBUG

#include <array>
#include <span>

struct A {
  constexpr A(std::span<const unsigned char>) {}
};
constexpr A val{std::array<unsigned char, 2>{0x11, 0x22}};
