// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>

using namespace std::chrono;

struct local_tz : time_zone { local_tz(); };

template<> struct std::chrono::zoned_traits<const local_tz*>
{
  static auto default_zone() { return current_zone(); }

  static auto locate_zone(std::string_view name)
  { return std::chrono::locate_zone(name); }
};

void
test_ctad()
{
  zoned_time z1;
  static_assert( std::is_same_v<decltype(z1), zoned_time<seconds>> );
  zoned_time z2 = z1;
  static_assert( std::is_same_v<decltype(z2), decltype(z1)> );

  zoned_time z3 = sys_time<milliseconds>();
  static_assert( std::is_same_v<decltype(z3), zoned_time<milliseconds>> );

  const local_tz ltz;
  zoned_time z4(&ltz);
  static_assert( std::is_same_v<decltype(z4),
				zoned_time<seconds, const local_tz*>> );

  zoned_time z5("GMT");
  static_assert( std::is_same_v<decltype(z5), zoned_time<seconds>> );

  zoned_time z6(&ltz, sys_time<minutes>());
  static_assert( std::is_same_v<decltype(z6),
				zoned_time<seconds, const local_tz*>> );

  zoned_time z7(&ltz, sys_time<milliseconds>());
  static_assert( std::is_same_v<decltype(z7),
				zoned_time<milliseconds, const local_tz*>> );

  zoned_time z8("GMT", sys_time<minutes>());
  static_assert( std::is_same_v<decltype(z8), zoned_time<seconds>> );

  zoned_time z9("GMT", sys_time<microseconds>());
  static_assert( std::is_same_v<decltype(z9), zoned_time<microseconds>> );

  zoned_time z10(&ltz, local_time<minutes>());
  static_assert( std::is_same_v<decltype(z10),
				zoned_time<seconds, const local_tz*>> );

  zoned_time z11(&ltz, local_time<nanoseconds>(), choose::earliest);
  static_assert( std::is_same_v<decltype(z11),
				zoned_time<nanoseconds, const local_tz*>> );

  zoned_time z12("GMT", local_time<minutes>());
  static_assert( std::is_same_v<decltype(z12), zoned_time<seconds>> );

  zoned_time z13("GMT", local_time<nanoseconds>(), choose::earliest);
  static_assert( std::is_same_v<decltype(z13), zoned_time<nanoseconds>> );

  zoned_time z14(&ltz, z13);
  static_assert( std::is_same_v<decltype(z14),
				zoned_time<nanoseconds, const local_tz*>> );

  zoned_time z15(&ltz, z12, choose::earliest);
  static_assert( std::is_same_v<decltype(z15),
				zoned_time<seconds, const local_tz*>> );

  zoned_time z16("GMT", z14);
  static_assert( std::is_same_v<decltype(z16), zoned_time<nanoseconds>> );

  zoned_time z17("GMT", z12, choose::earliest);
  static_assert( std::is_same_v<decltype(z17), zoned_time<seconds>> );
}
