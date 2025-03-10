// { dg-do compile { target c++11 } }
#include <future>

// C++20 [futures.task.members]
// Mandates: is_invocable_r_v<R, F&, ArgTypes...> is true.

int f();
std::packaged_task<const int&()> task(f);
// { dg-error "dangling reference" "" { target { c++14_down } } 0 }
// { dg-error "reference to temporary" "" { target { c++14_down } } 0 }
// { dg-error "no matching function" "" { target c++17 } 0 }
// { dg-error "enable_if" "" { target c++17 } 0 }
// { dg-error "static assertion failed" "" { target c++17 } 0 }
