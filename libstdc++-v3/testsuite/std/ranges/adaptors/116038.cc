// PR libstdc++/116038
// { dg-do compile { target c++20 } }

#include <ranges>
#include <utility>

struct A { };
struct B { };

template<class... Ts>
struct overloaded : private Ts... {
  overloaded(Ts...);
  using Ts::operator()...;
};

int x[5];
struct integralish { operator int() const; } i;

int main() {
  overloaded o1 = { std::views::drop(i) };
  o1(x);
  std::move(o1)(x);
  std::as_const(o1)(x);

  overloaded o2 = { std::views::drop(i) | std::views::take(i) };
  o2(x);
  std::move(o2)(x);
  std::as_const(o1)(x);
}
