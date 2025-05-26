/* structured bindings  */

#include <array>
#include <tuple>

#include "target-flex-common.h"

template<typename Array, typename Tuple, typename Struct>
bool test(Array array, Tuple tuple, Struct s)
{
  bool ok;
  auto array_2nd_in = std::get<2>(array);
  auto tuple_2nd_in = std::get<2>(tuple);
  auto s_2nd_in = s._2;
  decltype(array_2nd_in) array_2nd_out_0;
  decltype(tuple_2nd_in) tuple_2nd_out_0;
  decltype(s_2nd_in) s_2nd_out_0;
  decltype(array_2nd_in) array_2nd_out_1;
  decltype(tuple_2nd_in) tuple_2nd_out_1;
  decltype(s_2nd_in) s_2nd_out_1;
  decltype(array_2nd_in) array_2nd_out_2;
  decltype(tuple_2nd_in) tuple_2nd_out_2;
  decltype(s_2nd_in) s_2nd_out_2;
  #pragma omp target map(from: ok, \
			       array_2nd_out_0, tuple_2nd_out_0, s_2nd_out_0, \
			       array_2nd_out_1, tuple_2nd_out_1, s_2nd_out_1, \
			       array_2nd_out_2, tuple_2nd_out_2, s_2nd_out_2) \
		     map(to: array_2nd_in, tuple_2nd_in, s_2nd_in, array, tuple, s)
    {
      bool inner_ok = true;
      {
	{
	  auto [array_0th, array_1st, array_2nd] = array;
	  VERIFY (array_2nd_in == array_2nd);
	  VERIFY (std::get<2>(array) == array_2nd);
	  array_2nd_out_0 = array_2nd;
	  auto [tuple_0th, tuple_1st, tuple_2nd] = tuple;
	  VERIFY (tuple_2nd_in == tuple_2nd);
	  VERIFY (std::get<2>(tuple) == tuple_2nd);
	  tuple_2nd_out_0 = tuple_2nd;
	  auto [s_0th, s_1st, s_2nd] = s;
	  VERIFY (s_2nd_in == s_2nd);
	  VERIFY (s._2 == s_2nd);
	  s_2nd_out_0 = s_2nd;
	}
	{
	  auto& [array_0th, array_1st, array_2nd] = array;
	  VERIFY (array_2nd_in == array_2nd);
	  VERIFY (std::get<2>(array) == array_2nd);
	  array_2nd_out_1 = array_2nd;
	  auto& [tuple_0th, tuple_1st, tuple_2nd] = tuple;
	  VERIFY (tuple_2nd_in == tuple_2nd);
	  VERIFY (std::get<2>(tuple) == tuple_2nd);
	  tuple_2nd_out_1 = tuple_2nd;
	  auto& [s_0th, s_1st, s_2nd] = s;
	  VERIFY (s_2nd_in == s_2nd);
	  VERIFY (s._2 == s_2nd);
	  s_2nd_out_1 = s_2nd;
	}
	{
	  const auto& [array_0th, array_1st, array_2nd] = array;
	  VERIFY (array_2nd_in == array_2nd);
	  VERIFY (std::get<2>(array) == array_2nd);
	  array_2nd_out_2 = array_2nd;
	  const auto& [tuple_0th, tuple_1st, tuple_2nd] = tuple;
	  VERIFY (tuple_2nd_in == tuple_2nd);
	  VERIFY (std::get<2>(tuple) == tuple_2nd);
	  tuple_2nd_out_2 = tuple_2nd;
	  const auto& [s_0th, s_1st, s_2nd] = s;
	  VERIFY (s_2nd_in == s_2nd);
	  VERIFY (s._2 == s_2nd);
	  s_2nd_out_2 = s_2nd;
	}
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (array_2nd_out_0 == array_2nd_in);
  VERIFY_NON_TARGET (tuple_2nd_out_0 == tuple_2nd_in);
  VERIFY_NON_TARGET (s_2nd_out_0 == s_2nd_in);
  VERIFY_NON_TARGET (array_2nd_out_1 == array_2nd_in);
  VERIFY_NON_TARGET (tuple_2nd_out_1 == tuple_2nd_in);
  VERIFY_NON_TARGET (s_2nd_out_1 == s_2nd_in);
  VERIFY_NON_TARGET (array_2nd_out_2 == array_2nd_in);
  VERIFY_NON_TARGET (tuple_2nd_out_2 == tuple_2nd_in);
  VERIFY_NON_TARGET (s_2nd_out_2 == s_2nd_in);

  return true;
}

struct S
{
  char _0;
  float _1;
  int _2;
};

int main()
{
  const bool test_res
    = test(std::array{0, 1, 2},
	   std::tuple{'a', 3.14f, 42},
	   S{'a', 3.14f, 42});
  return test_res ? 0 : 1;
}
