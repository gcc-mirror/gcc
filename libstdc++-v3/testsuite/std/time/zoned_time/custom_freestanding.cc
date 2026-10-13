// { dg-do run { target c++20 } }

#include <chrono>
#include <type_traits>
#include <testsuite_hooks.h>

struct zone_offset
{
  std::chrono::seconds offset;

  template<typename Dur>
    std::chrono::local_time<std::common_type_t<Dur, std::chrono::seconds>>
    to_local(const std::chrono::sys_time<Dur>& tp) const noexcept
    { return std::chrono::local_time<std::common_type_t<Dur, std::chrono::seconds>>(tp.time_since_epoch() + offset); }

  template<typename Dur>
    std::chrono::sys_time<std::common_type_t<Dur, std::chrono::seconds>>
    to_sys(const std::chrono::local_time<Dur>& tp) const noexcept
    { return std::chrono::sys_time<std::common_type_t<Dur, std::chrono::seconds>>(tp.time_since_epoch() - offset); }

  // Allows zone_offset to be stored directly in zoned_time
  const zone_offset* operator->() const noexcept
  { return this; }
};

void
test_custom_tzptr()
{
  using namespace std::chrono;

  static_assert( !std::is_default_constructible_v<zoned_time<seconds, zone_offset>> );
  static_assert( !std::is_constructible_v<zoned_time<seconds, zone_offset>, std::string_view> );

  zoned_time z1(zone_offset{1h}, sys_days(2012y/December/12) + 12h);
  static_assert( std::is_same_v<decltype(z1), zoned_time<seconds, zone_offset>> );
  VERIFY( z1.get_sys_time() == sys_days(2012y/December/12) + 12h );
  VERIFY( z1.get_local_time() == local_days(2012y/December/12) + 13h );

  zoned_time z2(zone_offset{1h}, local_days(2012y/December/12) + 12h);
  static_assert( std::is_same_v<decltype(z2), zoned_time<seconds, zone_offset>> );
  VERIFY( z2.get_sys_time() == sys_days(2012y/December/12) + 11h );
  VERIFY( z2.get_local_time() == local_days(2012y/December/12) + 12h );
}

int main()
{
  test_custom_tzptr();
}
