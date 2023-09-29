// { dg-do compile { target c++14 } }

// PR c++/111357 - __integer_pack fails to work with values of dependent type
// convertible to integers in noexcept context

#include <utility>

using std::integer_sequence;
using std::make_integer_sequence;

template<int... V>
void g(integer_sequence<int,V...>)
{}

template<typename ...T>
struct c1
{
  static constexpr int value = 1;
  constexpr operator int() { return value; }
};

template<typename T>
struct R
{
  using S = make_integer_sequence<int,c1<T>{}>;

  R() noexcept(noexcept(g(S()))) // { dg-bogus "argument to .__integer_pack." }
  {}
};

int main()
{
  R<int>();
}
