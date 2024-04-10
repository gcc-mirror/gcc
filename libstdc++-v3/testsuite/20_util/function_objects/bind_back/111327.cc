// PR libstdc++/111327 - std::bind_front (and std::not_fn) doesn't always
// perfectly forward according to value category of the call wrapper object
// { dg-do compile { target c++23 } }

#include <functional>
#include <utility>

struct F {
  void operator()(...) & = delete;
  void operator()(...) const &;
};

struct G {
  void operator()(...) && = delete;
  void operator()(...) const &&;
};

int main() {
  auto f0 = std::bind_back(F{});
  f0(); // { dg-error "deleted|no match" }
  std::move(f0)();
  std::as_const(f0)();
  std::move(std::as_const(f0))();

  auto g0 = std::bind_back(G{});
  g0(); // { dg-error "deleted|no match" }
  std::move(g0)(); // { dg-error "deleted|no match" }
  std::move(std::as_const(g0))();

  auto f1 = std::bind_back(F{}, 42);
  f1(); // { dg-error "deleted|no match" }
  std::move(f1)();
  std::as_const(f1)();
  std::move(std::as_const(f1))();

  auto g1 = std::bind_back(G{}, 42);
  g1(); // { dg-error "deleted|no match" }
  std::move(g1)(); // { dg-error "deleted|no match" }
  std::move(std::as_const(g1))();
}

// { dg-error "no type named 'type' in 'struct std::invoke_result" "" { target c++23 } 0 }
