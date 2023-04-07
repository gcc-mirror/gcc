// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run }

#include <valarray>
#include <testsuite_hooks.h>

using std::valarray;

template<typename T>
bool equal(const valarray<T>& lhs, const valarray<T>& rhs)
{
  if (lhs.size() != rhs.size())
    return false;
  for (unsigned i = 0; i < lhs.size(); ++i)
    if (lhs[i] != rhs[i])
      return false;
  return true;
}

// Taken from examples in C++11 [valarray.sub].

void
test01() // valarray<T> operator[](const valarray<bool>& boolarr) const;
{
  const valarray<char> v0("abcdefghijklmnop", 16);
  const bool vb[] = {false, false, true, true, false, true};
  valarray<char> v1 = v0[valarray<bool>(vb, 6)];

  VERIFY( equal(v1, valarray<char>("cdf", 3)) );
}

void
test02() // mask_array<T> operator[](const valarray<bool>& boolarr);
{
  valarray<char> v0("abcdefghijklmnop", 16);
  valarray<char> v1("ABC", 3);
  const bool vb[] = {false, false, true, true, false, true};
  v0[valarray<bool>(vb, 6)] = v1;

  VERIFY( equal(v0, valarray<char>("abABeCghijklmnop", 16)) );
}

int main()
{
  test01();
  test02();
}
