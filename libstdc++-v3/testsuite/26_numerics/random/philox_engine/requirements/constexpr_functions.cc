// { dg-do run { target c++26 } }
// { dg-require-cstdint "" }

// 29.5.4 Random Number Engine Class Templates
// 29.5.4.5 Class Template philox_engine

#include <random>
#include <testsuite_common_types.h>

namespace __gnu_test
{
  struct constexpr_member_functions
  {
    template<typename _Ttesttype>
      void
      operator()()
      {
	struct _Concept
	{
	  void __constraint()
	  {
	    constexpr auto v1 __attribute__((unused))
		= _Ttesttype::min();
	    constexpr auto v2 __attribute__((unused))
		= _Ttesttype::max();
	  }
	};
	_Concept c;
	c.__constraint();
      }
  };
}

int
main()
{
  __gnu_test::constexpr_member_functions test;
  typedef std::philox4x32 type;
  test.operator()<type>();
  return 0;
}
