// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <string>

// PR libstdc++/105995
// Not required by the standard, but supported for QoI.
constexpr std::string pr105995_empty;
constexpr std::string pr105995_partial = "0";
constexpr std::string pr105995_full = "0123456789abcde";
