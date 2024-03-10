// { dg-do compile { target c++17 } }

#include <variant>

struct A{ A(int); };
struct B{ B(){}; };

void f(std::variant<A>);
int f(B);

int unambiguous = f({}); // { dg-bogus "ambiguous" }
