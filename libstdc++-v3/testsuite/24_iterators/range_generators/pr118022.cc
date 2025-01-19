// { dg-do compile { target c++23 } }
#include <generator>

struct O {
  O() = default;
  explicit O(const O&) = default;
};

std::generator<O&&> gen() {
  const O o;
  co_yield o;
}

int
main()
{}
