// { dg-do compile { target c++20 } }

// P2905R2 Runtime format strings

#include <format>

std::string rval() { return "path/etic/experience"; }

void f()
{
  (void)std::make_format_args(rval()); // { dg-error "cannot bind non-const lvalue reference" }
}
