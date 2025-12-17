// { dg-do compile { target c++26 } }

// N5014 29.5.4.5 Class Template philox_engine

#include <random>

void
test01()
{
  typedef std::philox_engine<std::uint_fast32_t,
		     32, 4, 10, 0xCD9E8D57,
		     0x9E3779B9, 0xD2511F53,
		     0xBB67AE85> testType;

  typedef testType::result_type result_type;
  static_assert( std::is_same_v<result_type, std::uint_fast32_t> );
}

void
test02()
{
  typedef std::philox_engine<std::uint_fast64_t,
		     64, 4, 10, 0xCA5A826395121157,
		     0x9E3779B97F4A7C15, 0xD2E7470EE14C6C93,
		      0xBB67AE8584CAA73B> testType;

  typedef testType::result_type result_type;
  static_assert( std::is_same_v<result_type, std::uint_fast64_t> );
}
