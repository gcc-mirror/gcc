// { dg-options "-fcontracts" }
// { dg-do compile { target c++26 } }

// We should not get errors from including this before <contracts>:
#include <source_location>
#include <contracts>

// There should be no private std::source_location ctor that participates in
// the calls to f below.
struct S { S(char const *); };
void f(S);
void f(std::source_location);

void
test01()
{
  f("");   // { dg-bogus "ambiguous" }
  f({""}); // { dg-bogus "ambiguous" }
}
