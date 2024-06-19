// { dg-do run }

// Test the problematic cases identified in PR libstdc++/109150
// where the previous std::fill was non-conforming.

#include <algorithm>
#include <testsuite_hooks.h>

const int global = 0;

struct X {
  void operator=(const int& i) { VERIFY(&i == &global); }
};

void
test_identity_matters()
{
  X x;
  // Assigning int to X has non-trivial side effects, so we cannot
  // hoist the load outside the loop, we have to do exactly what the
  // standard says to do.
  std::fill(&x, &x+1, global);
}

struct Y {
  int i;
  void operator=(int ii) { i = ii + 1; }
};

void
test_self_aliasing()
{
  Y y[2] = { };
  // Assigning int to X has non-trivial side effects, altering the value
  // used to fill the later elements. Must not load it outside the loop.
  std::fill(y, y+2, y[0].i);
  VERIFY(y[1].i == 2);
}

struct Z
{
  Z() { }
#if __cplusplus >= 201103L
  explicit Z(const Z&) = default;
#endif
};

void
test_explicit_copy_ctor()
{
  Z z;
  // The optimization that copies the fill value must use direct-initialization
  // otherwise this case would be ill-formed due to the explicit constructor.
  std::fill(&z, &z, z);
}

int main()
{
  test_identity_matters();
  test_self_aliasing();
  test_explicit_copy_ctor();
}
