// { dg-do compile { target c++11 } }
// Bug libstdc++/119545
// tuple::operator==()'s help lambda does not specify return type as bool

#include <tuple>

void
test_pr119545()
{
  struct Bool {
    Bool() = default;
    Bool(const Bool&) = delete;
    operator bool() const { return true; }
  };

  static Bool b;

  struct Object {
    const Bool& operator==(const Object&) const { return b; }
  };

  std::tuple<Object> t;
  (void) (t == t);
}
