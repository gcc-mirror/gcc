#include <testsuite_hooks.h>

// Test related to PR libstdc++/105387
// Check that pointer-to-member type exceptions can still be caught with -frtti.
// { dg-require-effective-target rtti }

void test_catch_ptr_to_member()
{
  bool exception_thrown = false;
  struct X { int i; };
  try {
    throw &X::i;
  }
  catch (const int X::*) {
    exception_thrown = true;
  }

  VERIFY(exception_thrown);
}

int main()
{
  test_catch_ptr_to_member();
  return 0;
}
