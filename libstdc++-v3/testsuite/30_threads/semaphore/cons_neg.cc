// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run { target c++20 xfail *-*-* } }
// { dg-require-effective-target hosted }
// { dg-add-options libatomic }

#include <semaphore>

int main()
{
  // Preconditions: desired >= 0 is true, and desired <= max() is true.
  std::binary_semaphore b(2);
}
