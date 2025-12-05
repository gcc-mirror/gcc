// PR libstdc++/111327 - std::bind_front (and std::not_fn) doesn't always
// perfectly forward according to value category of the call wrapper object
// { dg-do compile { target c++20 } }

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
  auto f0 = std::bind_front(F{});
  f0(); // { dg-error "deleted|no match" }
  std::move(f0)();
  std::as_const(f0)();
  std::move(std::as_const(f0))();

  auto g0 = std::bind_front(G{});
  g0(); // { dg-error "deleted|no match" }
  std::move(g0)(); // { dg-error "deleted|no match" }
  std::move(std::as_const(g0))();

  auto f1 = std::bind_front(F{}, 42);
  f1(); // { dg-error "deleted|no match" }
  std::move(f1)();
  std::as_const(f1)();
  std::move(std::as_const(f1))();

  auto g1 = std::bind_front(G{}, 42);
  g1(); // { dg-error "deleted|no match" }
  std::move(g1)(); // { dg-error "deleted|no match" }
  std::move(std::as_const(g1))();

  auto f2 = std::bind_front(F{}, 42, 10);
  f2(); // { dg-error "deleted|no match" }
  std::move(f2)();
  std::as_const(f2)();
  std::move(std::as_const(f2))();

  auto g2 = std::bind_front(G{}, 42, 10);
  g2(); // { dg-error "deleted|no match" }
  std::move(g2)(); // { dg-error "deleted|no match" }
  std::move(std::as_const(g2))();
}

// { dg-error "no type named 'type' in 'std::__conditional_t<false, std::invoke_result<" "" { target *-*-* } 0 }
