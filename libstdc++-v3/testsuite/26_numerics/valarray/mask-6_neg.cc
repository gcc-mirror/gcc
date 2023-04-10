// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run { xfail *-*-* } }

#include <valarray>

int main()
{
  using std::valarray;
  using std::mask_array;

  // This is adapted from an example in C++11 [valarray.sub].

  valarray<char> v0("abcdef", 6);
  valarray<char> v1("ABCDEF", 6);
  const bool vb[] = {false, false, true, true, false, true};
  const mask_array<char> m0 = v0[valarray<bool>(vb, 5)];
  const mask_array<char> m1 = v1[valarray<bool>(vb, 6)];
  m0 = m1; // aborts, m0 has fewer elements than m1
}
