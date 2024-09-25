// { dg-do compile { target c++20 } }

// LWG 3746. optional's spaceship with U with a type derived from optional
// causes infinite constraint meta-recursion

#include <optional>

struct S : std::optional<char>
{
    bool operator==(const S&) const;
    bool operator<(const S&) const;
    bool operator>(const S&) const;
    bool operator<=(const S&) const;
    bool operator>=(const S&) const;
};

auto cmp(const S& s, const std::optional<char>& o)
{
  return s <=> o;
}
