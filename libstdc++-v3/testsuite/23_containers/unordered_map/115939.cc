// { dg-options "-pedantic" }
// { dg-do compile { target c++11 } }

// Bug 115939 - Cannot unambiguously compare iterators in std::unordered_map

#include <unordered_map>

struct any_conv
{
  template<typename T>
    any_conv(T&&) { }

  template<typename T>
    friend bool operator==(const any_conv&, const T&) { return true; }

  template<typename T>
    friend bool operator!=(const any_conv&, const T&) { return false; }
};

std::unordered_map<int, any_conv>::iterator i{};
std::unordered_map<int, any_conv>::const_iterator j{};
bool b1 = i == i; // { dg-bogus "ambiguous" }
bool b2 = j == j; // { dg-bogus "ambiguous" }
bool b3 = i == j; // { dg-bogus "ambiguous" }
bool b4 = j == i; // { dg-bogus "ambiguous" }
bool b5 = i != i; // { dg-bogus "ambiguous" }
bool b6 = j != j; // { dg-bogus "ambiguous" }
bool b7 = i != j; // { dg-bogus "ambiguous" }
bool b8 = j != i; // { dg-bogus "ambiguous" }
