/* std::initializer_list in target region.  */

#include <initializer_list>

#include "target-flex-common.h"

struct S0
{
  int _v;
  S0(std::initializer_list<int> il)
    : _v(0)
  {
    for (auto const& e : il)
      _v += e;
  }
};

struct S1
{
  int _v;
  template<typename T>
  S1(std::initializer_list<T> il)
    : _v(0)
  {
    for (auto const& e : il)
      _v += e;
  }
};

template<typename T>
struct S2
{
  T _v;
  S2(std::initializer_list<T> il)
    : _v(0)
  {
    for (auto const& e : il)
      _v += e;
  }
};

#if __cplusplus >= 201703L
template<typename T>
S2(std::initializer_list<T>) -> S2<T>;
#endif

bool test_initializer_list(int arg)
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arg)
    {
      bool inner_ok = true;
      {
	static constexpr int partial_sum = 0 + 1 + 2 + 3 + 4 + 5;

	S0 s0{0, 1, 2, 3, 4, 5, arg};
	VERIFY (s0._v == partial_sum + arg);

	S1 s1{0, 1, 2, 3, 4, 5, arg};
	VERIFY (s1._v == partial_sum + arg);

	S2<int> s2{0, 1, 2, 3, 4, 5, arg};
	VERIFY (s2._v == partial_sum + arg);

	#if __cplusplus >= 201703L
	  S2 s2_ctad{0, 1, 2, 3, 4, 5, arg};
	  VERIFY (s2_ctad._v == partial_sum + arg);
	#endif
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

int main()
{
  volatile int arg = 42;
  return test_initializer_list(arg) ? 0 : 1;
}
