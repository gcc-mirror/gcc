// PR libstdc++/116038
// { dg-do compile { target c++23 } }

#include <functional>
#include <utility>

struct A { };
struct B { };

template<class... Ts>
struct overloaded : private Ts... {
  overloaded(Ts...);
  using Ts::operator()...;
};

int apply_a(A, int);
int apply_b(B, int);

int main() {
  overloaded o = { std::bind_back(apply_a, 1),
		   std::bind_back(apply_b, 2) };
  A a;
  o(a);
  std::as_const(o)(a);
  std::move(o)(a);
  std::move(std::as_const(o))(a);
}
