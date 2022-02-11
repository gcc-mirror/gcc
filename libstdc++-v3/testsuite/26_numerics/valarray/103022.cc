// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do compile { target c++11 } }

#include <valarray>

int main()
{
  // PR libstdc++/103022
  std::valarray<double> va;
  (void) std::begin(va);
  (void) std::end(va);
  const auto& cva = va;
  (void) std::begin(cva);
  (void) std::end(cva);
}
