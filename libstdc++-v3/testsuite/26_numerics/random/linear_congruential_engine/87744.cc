// { dg-do run { target c++11 } }
// PR libstdc++/87744 Some valid instantiations of linear_congruential_engine
// produce compiler errors when __int128 isn't available

#include <random>
#include <testsuite_hooks.h>

int main()
{
  std::linear_congruential_engine<unsigned long long int,
				  864691128455135232ULL, 12347ULL,
				  4052555153018976267ULL> gen;
  gen();
  VERIFY( gen() == 20120904253298372 );
  VERIFY( gen() == 499698276788149646 );

  std::linear_congruential_engine<unsigned long long, 6364136223846793005ULL,
				  1ULL, (-1ULL >> 1)> gen2;
  for (int i = 0; i < 99; ++i)
    gen2();
  VERIFY( gen2() == 5913590678204212798 );
}
