// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <format>

struct Str
{
  consteval operator std::string_view() const { return ""; }
  operator std::string_view() = delete;
};

// PR libstdc++/108024
static_assert( std::is_constructible_v<std::format_string<>, const Str&> );
static_assert( std::is_convertible_v<const Str&, std::format_string<>> );

constinit std::format_string<> s = Str();
