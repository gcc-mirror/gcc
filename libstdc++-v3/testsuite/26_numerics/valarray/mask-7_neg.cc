// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run { xfail *-*-* } }

#include <valarray>

int main()
{
  using std::valarray;
  using std::mask_array;

  // This is adapted from an example in C++11 [valarray.sub].

  valarray<char> v0("abcdefghijklmnop", 16);
  valarray<char> v1("ABCD", 4);
  const bool vb[] = {false, false, true, true, false, true};
  const mask_array<char> m = v0[valarray<bool>(vb, 6)];
  m += v1; // aborts, v1 has more elements than m
}
