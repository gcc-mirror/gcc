// PR libstdc++/111327 - std::bind_front (and std::not_fn) doesn't always
// perfectly forward according to value category of the call wrapper object
// { dg-do compile { target c++17 } }

#include <functional>
#include <utility>

struct F {
  void operator()(...) & = delete;
  bool operator()(...) const &;
};

struct G {
  void operator()(...) && = delete;
  bool operator()(...) const &&;
};

int main() {
  auto f = std::not_fn(F{});
  f(); // { dg-error "deleted" }
  std::move(f)();
  std::as_const(f)();
  std::move(std::as_const(f))();

  auto g = std::not_fn(G{});
  g(); // { dg-error "deleted" }
  std::move(g)(); // { dg-error "deleted" }
  std::move(std::as_const(g))();
}
