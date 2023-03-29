// { dg-do compile { target c++11 } }

#include <type_traits>

class S {
  operator int();
  friend void g(); // #1
};

void
g()
{
  int i = 0;
  S s;
  i = s; // this works, because we're inside a friend.

  // But the traits are evaluated in "a context unrelated to either type".
  static_assert( ! std::is_assignable<int&, S>::value, "unfriendly");
#if __cplusplus >= 201703L
  static_assert( ! std::is_assignable_v<int&, S>, "unfriendly");
#endif
}
