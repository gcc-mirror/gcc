// { dg-do compile { target c++26 } }

#include <functional>

struct IncompleteClass;

int a1 = alignof(std::function_ref<int(IncompleteClass)>); // { dg-error "here" }
int a2 = alignof(std::function_ref<int(int, IncompleteClass)>); // { dg-error "here" }

enum Enum {
  x = [] {
    // Enum enumeration is incomplete here
    int a3 = alignof(std::function_ref<int(Enum)>); // { dg-error "here" }
    return 1;
  }()
};

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
