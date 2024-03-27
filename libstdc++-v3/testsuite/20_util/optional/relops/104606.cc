// { dg-do compile { target c++17 } }

// Bug 104606 comparison operator resolution with std::optional and -std=c++20

#include <optional>
#include <variant>
#include <vector>

struct Value : std::variant<std::vector<Value>> { };

struct Comparator {
  template <typename T> bool operator<=(const T &) { return true; }
};

std::optional<Value> o;
Comparator c;

auto x = c <= o;
