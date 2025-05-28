// { dg-do compile { target c++26 } }

#include <functional>

struct IncompleteClass;

using T1 = std::copyable_function<int(IncompleteClass)>::result_type; // { dg-error "here" }
using T2 = std::copyable_function<int(int, IncompleteClass)>::result_type; // { dg-error "here" }

enum Enum {
  x = [] {
    // Enum enumeration is incomplete here
    using T3 = std::copyable_function<int(Enum)>::result_type; // { dg-error "here" }
    return T3(1);
  }()
};

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
