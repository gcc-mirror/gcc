// { dg-do compile { target c++11 } }

// LWG 4154. The Mandates for std::packaged_task's constructor from
// a callable entity should consider decaying

#include <future>

struct F {
  void operator()() & = delete;
  void operator()() const & { }
};

// Mandates: is_invocable_r_v<R, decay_t<F>&, ArgTypes...> is true.
const F f;
std::packaged_task<void()> p(f); // { dg-error "here" "" { target c++17 } }
// { dg-error "static assertion failed" "" { target c++17 } 0 }
// { dg-error "invoke_r" "" { target *-*-* } 0 }
// { dg-prune-output "enable_if<false" }

// Only callable as rvalue
struct Frv {
  int* operator()() && { return 0; }
};
std::packaged_task<int*()> p2(Frv{}); // { dg-error "here" "" { target c++17 } }

// Only callable as non-const lvalue
struct Fnc {
  void operator()() const & = delete;
  void operator()() & { }
};

// In C++11/14/17/20 std::packaged_task::packaged_task<F>(F&&) incorrectly
// required that the callable passed to the constructor can be invoked.
// If the type of the parameter is const F& we might not be able to invoke
// the parameter, but that's OK because we store and invoke a non-const F.
// So this should work without errors:
const Fnc fnc;
std::packaged_task<void()> p3(fnc);
