// { dg-do compile { target c++11 } }
#include <memory>

struct Incomplete;
struct pr101236
{
  // The standard says "T shall be a complete type" for unique_ptr<T[], D>
  // so this is a GCC extension.
  std::unique_ptr<Incomplete[]> p;

  Incomplete& f() { return p[0]; }
};
struct Incomplete { };
