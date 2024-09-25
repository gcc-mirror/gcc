// { dg-do run }
// { dg-options "-Wno-deprecated-declarations" }

// Bug 88935 std::random_shuffle does not work if the sequence
// is longer than RAND_MAX elements

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

int main()
{
  const std::size_t N = 30000;
  std::vector<unsigned char> v(N, (unsigned char)0);
  std::fill(v.begin() + (N / 5 * 4), v.end(), (unsigned char)1);
  std::random_shuffle(v.begin(), v.end());
  double sum = 0;
  for (std::size_t i = 0; i < v.size(); ++i)
  {
    sum += v[i];
    if (i > 0 && i % (N / 100) == 0)
      VERIFY( (sum / i) < 0.3 );
  }
}
