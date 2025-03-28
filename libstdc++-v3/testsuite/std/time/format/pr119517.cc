// { dg-do compile { target c++23 } }

#include <chrono>
#include <format>

static_assert( std::formattable<std::chrono::weekday, char> );
static_assert( std::formattable<std::chrono::weekday, wchar_t> );
static_assert( !std::formattable<std::chrono::weekday, char16_t> );

static_assert( std::formattable<std::chrono::sys_days, char> );
static_assert( std::formattable<std::chrono::sys_days, wchar_t> );
static_assert( !std::formattable<std::chrono::sys_days, char16_t> );

static_assert( !std::formattable<std::chrono::seconds, int> );

static_assert( !std::formattable<std::chrono::day, int> );
static_assert( !std::formattable<std::chrono::month, int> );
static_assert( !std::formattable<std::chrono::year, int> );
static_assert( !std::formattable<std::chrono::weekday, int> );
static_assert( !std::formattable<std::chrono::weekday_indexed, int> );
static_assert( !std::formattable<std::chrono::weekday_last, int> );
static_assert( !std::formattable<std::chrono::month_day, int> );
static_assert( !std::formattable<std::chrono::month_day_last, int> );
static_assert( !std::formattable<std::chrono::month_weekday, int> );
static_assert( !std::formattable<std::chrono::month_weekday_last, int> );
static_assert( !std::formattable<std::chrono::year_month_day, int> );
static_assert( !std::formattable<std::chrono::year_month_day_last, int> );
static_assert( !std::formattable<std::chrono::year_month_weekday, int> );
static_assert( !std::formattable<std::chrono::year_month_weekday_last, int> );
static_assert( !std::formattable<std::chrono::hh_mm_ss<std::chrono::seconds>, int> );

static_assert( !std::formattable<std::chrono::sys_seconds, int> );
static_assert( !std::formattable<std::chrono::utc_seconds, int> );
static_assert( !std::formattable<std::chrono::tai_seconds, int> );
static_assert( !std::formattable<std::chrono::gps_seconds, int> );
static_assert( !std::formattable<std::chrono::local_seconds, int> );
static_assert( !std::formattable<std::chrono::file_time<std::chrono::seconds>, int> );
#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
static_assert( !std::formattable<std::chrono::zoned_seconds, int> );

static_assert( !std::formattable<std::chrono::sys_info, int> );
static_assert( !std::formattable<std::chrono::local_info, int> );
#endif
