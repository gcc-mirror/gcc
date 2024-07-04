// { dg-do compile { target c++17 } }
// PR libstdc++/115145
// lambda in rewritten std::variant comparisons does not specify return type

#include <variant>

struct Bool {
  operator bool() & { return val; }
  const bool val;
};

Bool t{true}, f{false};

struct A {
  Bool& operator==(const A&) const { return t; }
  Bool& operator!=(const A&) const { return f; }
  Bool& operator<(const A&) const { return f; }
  Bool& operator>(const A&) const { return f; }
  Bool& operator<=(const A&) const { return t; }
  Bool& operator>=(const A&) const { return t; }
};

bool check_bool(bool);
template<typename T> void check_bool(T) = delete;

void
test_pr115145()
{
  std::variant<A> v;
  check_bool( v == v );
  check_bool( !(v != v) );
  check_bool( !(v < v) );
  check_bool( !(v > v) );
  check_bool( v <= v );
  check_bool( v >= v );
}
