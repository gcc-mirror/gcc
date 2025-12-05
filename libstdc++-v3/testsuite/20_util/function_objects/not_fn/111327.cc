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

struct Weird {
  void operator()();
  bool operator()() const { return true; }
};

int main() {
  auto f = std::not_fn(F{});
  f(); // { dg-error "no match" }
  std::move(f)();
  std::as_const(f)();
  std::move(std::as_const(f))();

  auto g = std::not_fn(G{});
  g(); // { dg-error "no match" }
  std::move(g)(); // { dg-error "no match" }
  std::move(std::as_const(g))();

  auto h = std::not_fn(Weird{});
  h(); // { dg-error "no match" }
}

// { dg-error "no type named 'type' in 'struct std::__invoke_result<" "" { target *-*-* } 0 }
// { dg-error "no matching function for call to 'std::_Not_fn<Weird>" "" { target *-*-* } 0 }
// { dg-error "could not convert 'std::declval<void>\\(\\)' from 'void' to 'bool'" "" { target *-*-* } 0 }
// { dg-error "in argument to unary !" "" { target *-*-* } 0 }
