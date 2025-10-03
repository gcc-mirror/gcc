// { dg-do compile { target c++23 } }

#include <format>
#include <chrono>
#include "custom_rep.h"

static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::day>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::month>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::year>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::weekday>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::weekday_indexed>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::weekday_last>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::month_day>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::month_day_last>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::month_weekday>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::month_weekday_last>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::year_month>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::year_month_day>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::year_month_day_last>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::year_month_weekday>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::year_month_weekday_last>);

#if _GLIBCXX_USE_CXX11_ABI || !_GLIBCXX_USE_DUAL_ABI
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::local_info>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::sys_info>);
#endif

template<typename Duration>
using local_time_fmt
  = decltype(std::chrono::local_time_format(std::chrono::local_time<Duration>{}));

static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::seconds>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::duration<float>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::duration<long long, std::mega>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::local_time<std::chrono::seconds>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::sys_time<std::chrono::seconds>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::utc_time<std::chrono::seconds>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::gps_time<std::chrono::seconds>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::tai_time<std::chrono::seconds>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::file_time<std::chrono::seconds>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		local_time_fmt<std::chrono::seconds>>);

using BufferedDuration = std::chrono::duration<Rep<void, int>>;

static_assert(!std::enable_nonlocking_formatter_optimization<
		BufferedDuration>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::local_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::sys_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::utc_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::gps_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::tai_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::file_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		local_time_fmt<BufferedDuration>>);

template<>
inline constexpr bool
  std::enable_nonlocking_formatter_optimization<Rep<void, long>> = true;

using NonBufferedRep = std::chrono::duration<Rep<void, long>>;

static_assert(!std::enable_nonlocking_formatter_optimization<
		NonBufferedRep>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::local_time<NonBufferedRep>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::sys_time<NonBufferedRep>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::utc_time<NonBufferedRep>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::gps_time<NonBufferedRep>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::tai_time<NonBufferedRep>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::file_time<NonBufferedRep>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		local_time_fmt<NonBufferedRep>>);

using NonBufferedDuration = std::chrono::duration<Rep<void, short>>;

template<>
inline constexpr bool
  std::enable_nonlocking_formatter_optimization<NonBufferedDuration> = true;

static_assert(std::enable_nonlocking_formatter_optimization<
		NonBufferedDuration>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::local_time<NonBufferedDuration>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::sys_time<NonBufferedDuration>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::utc_time<NonBufferedDuration>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::gps_time<NonBufferedDuration>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::tai_time<NonBufferedDuration>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::file_time<NonBufferedDuration>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		local_time_fmt<NonBufferedDuration>>);

#if _GLIBCXX_USE_CXX11_ABI || !_GLIBCXX_USE_DUAL_ABI
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<std::chrono::seconds>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<BufferedDuration>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<NonBufferedRep>>);
static_assert(std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<NonBufferedDuration>>);

struct MyTimeZone : std::chrono::time_zone
{};

template<>
struct std::chrono::zoned_traits<MyTimeZone>
{
  static const MyTimeZone* default_zone();
  static const MyTimeZone* locate_zone(std::string_view name);
};

static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<std::chrono::seconds, MyTimeZone>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<BufferedDuration, MyTimeZone>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<NonBufferedRep, MyTimeZone>>);
static_assert(!std::enable_nonlocking_formatter_optimization<
		std::chrono::zoned_time<NonBufferedDuration, MyTimeZone>>);
#endif

