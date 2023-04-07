// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run { xfail *-*-* } }

#include <valarray>

int main()
{
  using std::valarray;

  // This is adapted from an example in C++11 [valarray.sub].
  // mask_array<T> operator[](const valarray<bool>& boolarr);

  valarray<char> v0("ab", 2);
  const bool vb[] = {false, false, true, true, false, true};
  (void) v0[valarray<bool>(vb, 6)]; // aborts, mask has more elements than v0
}
