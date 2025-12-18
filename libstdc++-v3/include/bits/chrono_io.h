// <chrono> Formatting -*- C++ -*-

// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/bits/chrono_io.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{chrono}
 */

#ifndef _GLIBCXX_CHRONO_IO_H
#define _GLIBCXX_CHRONO_IO_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#if __cplusplus >= 202002L

#include <sstream> // ostringstream
#include <format>
#include <charconv> // from_chars
#include <stdexcept> // __sso_string

#include <bits/streambuf_iterator.h>
#include <bits/unique_ptr.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace chrono
{
/// @addtogroup chrono
/// @{

/// @cond undocumented
namespace __detail
{
#define _GLIBCXX_WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define _GLIBCXX_WIDEN(S) _GLIBCXX_WIDEN_(_CharT, S)

  template<typename _Period, typename _CharT>
    constexpr basic_string_view<_CharT>
    __units_suffix() noexcept
    {
      // The standard say these are all narrow strings, which would need to
      // be widened at run-time when inserted into a wide stream. We use
      // STATICALLY-WIDEN to widen at compile-time.
#define _GLIBCXX_UNITS_SUFFIX(period, suffix) \
    if constexpr (is_same_v<_Period, period>) \
      return _GLIBCXX_WIDEN(suffix);	      \
    else

      _GLIBCXX_UNITS_SUFFIX(atto,  "as")
      _GLIBCXX_UNITS_SUFFIX(femto, "fs")
      _GLIBCXX_UNITS_SUFFIX(pico,  "ps")
      _GLIBCXX_UNITS_SUFFIX(nano,  "ns")
      _GLIBCXX_UNITS_SUFFIX(milli, "ms")
#if _GLIBCXX_USE_ALT_MICROSECONDS_SUFFIX
      // Deciding this at compile-time is wrong, maybe use nl_langinfo(CODESET)
      // to check runtime environment and return u8"\u00b5s", "\xb5s", or "us".
      _GLIBCXX_UNITS_SUFFIX(micro, "\u00b5s")
#else
      _GLIBCXX_UNITS_SUFFIX(micro, "us")
#endif
      _GLIBCXX_UNITS_SUFFIX(centi, "cs")
      _GLIBCXX_UNITS_SUFFIX(deci,  "ds")
      _GLIBCXX_UNITS_SUFFIX(ratio<1>, "s")
      _GLIBCXX_UNITS_SUFFIX(deca,  "das")
      _GLIBCXX_UNITS_SUFFIX(hecto, "hs")
      _GLIBCXX_UNITS_SUFFIX(kilo,  "ks")
      _GLIBCXX_UNITS_SUFFIX(mega,  "Ms")
      _GLIBCXX_UNITS_SUFFIX(giga,  "Gs")
      _GLIBCXX_UNITS_SUFFIX(tera,  "Ts")
      _GLIBCXX_UNITS_SUFFIX(tera,  "Ts")
      _GLIBCXX_UNITS_SUFFIX(peta,  "Ps")
      _GLIBCXX_UNITS_SUFFIX(exa,   "Es")
      _GLIBCXX_UNITS_SUFFIX(ratio<60>,    "min")
      _GLIBCXX_UNITS_SUFFIX(ratio<3600>,  "h")
      _GLIBCXX_UNITS_SUFFIX(ratio<86400>, "d")
#undef _GLIBCXX_UNITS_SUFFIX
	return {};
    }

  template<typename _Period, typename _CharT, typename _Out>
    inline _Out
    __fmt_units_suffix(_Out __out) noexcept
    {
      if (auto __s = __detail::__units_suffix<_Period, _CharT>(); __s.size())
	return __format::__write(std::move(__out), __s);
      else if constexpr (_Period::den == 1)
	return std::format_to(std::move(__out), _GLIBCXX_WIDEN("[{}]s"),
			      (uintmax_t)_Period::num);
      else
	return std::format_to(std::move(__out), _GLIBCXX_WIDEN("[{}/{}]s"),
			      (uintmax_t)_Period::num,
			      (uintmax_t)_Period::den);
    }
} // namespace __detail
/// @endcond

  /** Write a `chrono::duration` to an ostream.
   *
   * @since C++20
   */
  template<typename _CharT, typename _Traits,
	   typename _Rep, typename _Period>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(std::basic_ostream<_CharT, _Traits>& __os,
	       const duration<_Rep, _Period>& __d)
    {
      using _Out = ostreambuf_iterator<_CharT, _Traits>;
      using period = typename _Period::type;
      std::basic_ostringstream<_CharT, _Traits> __s;
      __s.flags(__os.flags());
      __s.imbue(__os.getloc());
      __s.precision(__os.precision());
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 4118. How should duration formatters format custom rep types?
      __s << +__d.count();
      __detail::__fmt_units_suffix<period, _CharT>(_Out(__s));
      __os << std::move(__s).str();
      return __os;
    }

/// @cond undocumented
namespace __detail
{
  // An unspecified type returned by `chrono::local_time_format`.
  // This is called `local-time-format-t` in the standard.
  template<typename _Duration>
    struct __local_time_fmt
    {
      local_time<_Duration> _M_time;
      const string* _M_abbrev;
      const seconds* _M_offset_sec;
    };

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4124. Cannot format zoned_time with resolution coarser than seconds
  template<typename _Duration>
    using __local_time_fmt_for
      = __local_time_fmt<common_type_t<_Duration, seconds>>;
}
/// @endcond

  /** Return an object that asssociates timezone info with a local time.
   *
   * A `chrono::local_time` object has no timezone associated with it. This
   * function creates an object that allows formatting a `local_time` as
   * though it refers to a timezone with the given abbreviated name and
   * offset from UTC.
   *
   * @since C++20
   */
  template<typename _Duration>
    inline __detail::__local_time_fmt<_Duration>
    local_time_format(local_time<_Duration> __time,
		      const string* __abbrev = nullptr,
		      const seconds* __offset_sec = nullptr)
    { return {__time, __abbrev, __offset_sec}; }

  /// @}
} // namespace chrono

/// @cond undocumented
namespace __format
{
  [[noreturn,__gnu__::__always_inline__]]
  inline void
  __not_valid_for_duration()
  { __throw_format_error("format error: chrono-format-spec not valid for "
			 "chrono::duration"); }

  [[noreturn,__gnu__::__always_inline__]]
  inline void
  __invalid_chrono_spec()
  { __throw_format_error("format error: chrono-format-spec not valid for "
			 "argument type"); }

  // Represents the information provided by a chrono type.
  // e.g. month_weekday has month and weekday but no year or time of day,
  // hh_mm_ss has time of day but no date, sys_time is time_point+timezone.
  enum class _ChronoParts : unsigned short {
    _None = 0, _TotalSeconds = 1u, _Subseconds = 1u << 2,

    // time since epoch
    _EpochUnits = 1u << 3, _UnitSuffix = 1u << 4,
    _EpochSeconds = _EpochUnits | _TotalSeconds,

    // local (wall) time
    _LocalDays = 1u << 5,
    _LocalSeconds = _LocalDays | _TotalSeconds,

    _Year = 1u << 6, _Month = 1u << 7, _Day = 1u << 8,
    _Weekday = 1u << 9, _WeekdayIndex = 1u << 10,  _DayOfYear = 1u << 11,
    _IndexedWeekday = _Weekday | _WeekdayIndex,
    _YearMonthDay = _Year | _Month | _Day,
    _Date = _LocalDays | _YearMonthDay | _IndexedWeekday | _DayOfYear,

    _HoursMinutesSeconds = 1u << 12,
    _TimeOfDay = _HoursMinutesSeconds | _Subseconds,
    _Time = _TimeOfDay | _TotalSeconds,
    _EpochTime = _Time | _EpochUnits | _UnitSuffix,
    _DateTime = _Date | _Time,

    _ZoneAbbrev = 1u << 13, _ZoneOffset = 1u << 14,
    _TimeZone = _ZoneAbbrev | _ZoneOffset,
    _ZonedDateTime = _DateTime | _TimeZone,
  };

  [[__gnu__::__always_inline__]]
  constexpr _ChronoParts
  operator&(_ChronoParts __x, _ChronoParts __y) noexcept
  { return static_cast<_ChronoParts>((unsigned)__x & (unsigned)__y); }

  [[__gnu__::__always_inline__]]
  constexpr _ChronoParts&
  operator&=(_ChronoParts& __x, _ChronoParts __y) noexcept
  { return __x = __x & __y; }

  [[__gnu__::__always_inline__]]
  constexpr _ChronoParts
  operator|(_ChronoParts __x, _ChronoParts __y) noexcept
  { return static_cast<_ChronoParts>((unsigned short)__x | (unsigned short)__y); }

  [[__gnu__::__always_inline__]]
  constexpr _ChronoParts&
  operator|=(_ChronoParts& __x, _ChronoParts __y) noexcept
  { return __x = __x | __y; }

  // returns copy of x with all bits from y unset.
  [[__gnu__::__always_inline__]]
  constexpr _ChronoParts
  operator-(_ChronoParts __x, _ChronoParts __y) noexcept
  { return static_cast<_ChronoParts>((unsigned short)__x & ~(unsigned short)__y); }

  // unsets all bits of x that are set in y
  [[__gnu__::__always_inline__]]
  constexpr _ChronoParts&
  operator-=(_ChronoParts& __x, _ChronoParts __y) noexcept
  { return __x = __x - __y; }

  [[__gnu__::__always_inline__]]
  constexpr bool
  operator==(_ChronoParts __x, decltype(nullptr)) noexcept
  { return (unsigned short)__x == 0; }

  template<typename _CharT>
    struct _ChronoSpec : _Spec<_CharT>
    {
      // When _M_prec_kind is _WP_none, the _M_prec contains the default
      // value of fraction digits to be used for time '%S'.

      // Placed in tail-padding of __format::_Spec<C>.
      // This indicates that a locale-dependent conversion specifier such as
      // %a is used in the chrono-specs. This is not the same as the
      // _Spec<C>::_M_localized member which indicates that "L" was present
      // in the format-spec, e.g. "{:L%a}" is localized and locale-specific,
      // but "{:L}" is only localized and "{:%a}" is only locale-specific.
      unsigned _M_locale_specific : 1;
      // Indicates if parts that are checked for ok come directly from the
      // input, instead of being computed.
      unsigned _M_needs_ok_check : 1;
      // Indicates that duration should be treated as floating point.
      unsigned _M_floating_point_rep : 1;
      // Indicate that duration uses user-defined representation.
      unsigned _M_custom_rep : 1;
      unsigned _M_unused : 4;

      // Chrono parts required by format specs
      _ChronoParts _M_needed;
      basic_string_view<_CharT> _M_chrono_specs;

      [[__gnu__::__always_inline__]]
      constexpr bool
      _M_needs(_ChronoParts __parts) const
      { return (_M_needed & __parts) != 0; }
    };

  template<typename _CharT>
  struct _ChronoFormats
  {
    using _String_view = basic_string_view<_CharT>;

    static consteval
    _String_view
    _S_ftz() noexcept
    { return _GLIBCXX_WIDEN("%F %T %Z"); }

    static consteval
    _String_view
    _S_ft() noexcept
    { return _S_ftz().substr(0, 5); }

    static consteval
    _String_view
    _S_f() noexcept
    { return _S_ftz().substr(0, 2); }

    static consteval
    _String_view
    _S_t() noexcept
    { return _S_ftz().substr(3, 2); }

    static consteval
    _String_view
    _S_ymd() noexcept
    { return _GLIBCXX_WIDEN("%Y/%b/%d"); }

    static consteval
    _String_view
    _S_ym() noexcept
    { return _S_ymd().substr(0, 5); }

    static consteval
    _String_view
    _S_md() noexcept
    { return _S_ymd().substr(3); }

    static consteval
    _String_view
    _S_y() noexcept
    { return _S_ymd().substr(0, 2); }

    static consteval
    _String_view
    _S_m() noexcept
    { return _S_ymd().substr(3, 2); }

    static consteval
    _String_view
    _S_d() noexcept
    { return _S_ymd().substr(6, 2); }

    static consteval
    _String_view
    _S_ymwi() noexcept
    // %\0 is extension for handling weekday index
    { return _String_view(_GLIBCXX_WIDEN("%Y/%b/%a[%\0]"), 12); }

    static consteval
    _String_view
    _S_mwi() noexcept
    { return _S_ymwi().substr(3); }

    static consteval
    _String_view
    _S_wi() noexcept
    { return _S_ymwi().substr(6); }

    static consteval
    _String_view
    _S_w() noexcept
    { return _S_ymwi().substr(6, 2); }

    static consteval
    _String_view
    _S_ymwl() noexcept
    { return _GLIBCXX_WIDEN("%Y/%b/%a[last]"); }

    static consteval
    _String_view
    _S_mwl() noexcept
    { return _S_ymwl().substr(3); }

    static consteval
    _String_view
    _S_wl() noexcept
    { return _S_ymwl().substr(6); }

    static consteval
    _String_view
    _S_yml() noexcept
    { return _GLIBCXX_WIDEN("%Y/%b/last"); }

    static consteval
    _String_view
    _S_ml() noexcept
    { return _S_yml().substr(3); }
  };

  template<typename _CharT>
    struct _ChronoData
    {
      static constexpr unsigned _S_max_prec = 18;
      using _Attoseconds = chrono::duration<__UINT_LEAST64_TYPE__, atto>;

      using _FormatContext
	= basic_format_context<_Sink_iter<_CharT>, _CharT>;
      using _FormatArgs = basic_format_args<_FormatContext>;
      static inline auto _S_args = std::make_format_args<_FormatContext>();

      _ChronoData() = default;
      _ChronoData(_ChronoData&&) = delete;

      // time since epoch
      chrono::seconds           _M_eseconds;
      // n.b. due offset being seconds or coarser, local and epoch subseconds
      // has the same value
      _Attoseconds              _M_subseconds;
      // _M_ereps.get(0) stores duration units
      // _M_ereps.get(1) stores subseconds units
      // _M_ereps.get(2) stores precision
      _FormatArgs               _M_ereps = _S_args;
      basic_string_view<_CharT> _M_unit_suffix;

      // local (wall) time
      chrono::local_seconds _M_lseconds;
      chrono::local_days    _M_ldays;

      chrono::year    _M_year;
      chrono::month   _M_month;
      chrono::day     _M_day;
      chrono::weekday _M_weekday;
      unsigned char   _M_weekday_index;
      chrono::days    _M_day_of_year;

      bool            _M_is_neg;
      chrono::hours   _M_hours;
      chrono::minutes _M_minutes;
      chrono::seconds _M_seconds;

      chrono::seconds           _M_zone_offset;
      basic_string_view<_CharT> _M_zone_abbrev;
      const char*               _M_zone_cstr = "";

      template<typename _YearMonth>
	[[__gnu__::__always_inline__]]
	_ChronoParts
	_M_fill_year_month(const _YearMonth& __ym, _ChronoParts __parts)
	{
	  _M_year = __ym.year();
	  __parts -= _ChronoParts::_Year;
	  _M_month = __ym.month();
	  __parts -= _ChronoParts::_Month;
	  return __parts;
	}

      [[__gnu__::__always_inline__]]
      _ChronoParts
      _M_fill_day(chrono::day __d, _ChronoParts __parts)
      {
	_M_day = __d;
	__parts -= _ChronoParts::_Day;
	_M_weekday_index = ((unsigned)__d + 6u) / 7u;
	__parts -= _ChronoParts::_WeekdayIndex;
	return __parts;
      }

      [[__gnu__::__always_inline__]]
      _ChronoParts
      _M_fill_weekday(chrono::weekday_indexed __wi, _ChronoParts __parts)
      {
	_M_weekday = __wi.weekday();
	__parts -= _ChronoParts::_Weekday;
	_M_weekday_index = __wi.index();
	__parts -= _ChronoParts::_WeekdayIndex;
	return __parts;
      }

      // pre: _M_year is set
      [[__gnu__::__always_inline__]]
      _ChronoParts
      _M_fill_aux(chrono::local_days __ld, _ChronoParts __parts)
      {
	using namespace chrono;
	if ((__parts & _ChronoParts::_Weekday) != 0)
	  _M_weekday = weekday(__ld);
	__parts -= _ChronoParts::_Weekday;
	if ((__parts & _ChronoParts::_DayOfYear) != 0)
	  // See "Calculating Ordinal Dates" at
	  // https://github.com/HowardHinnant/date/wiki/Examples-and-Recipes
	  _M_day_of_year = __ld - local_days(_M_year/January/0);
	__parts -= _ChronoParts::_DayOfYear;
	return __parts;
      }

      // pre: _M_year is set
      [[__gnu__::__always_inline__]]
      _ChronoParts
      _M_fill_ldays(chrono::local_days __ld, _ChronoParts __parts)
      {
	_M_ldays = __ld;
	__parts -= _ChronoParts::_LocalDays;
	return _M_fill_aux(__ld, __parts);
      }

      void
      _M_fill_time(chrono::seconds __d)
      {
	chrono::hh_mm_ss<chrono::seconds> __hms(__d);
	_M_hours = __hms.hours();
	_M_minutes = __hms.minutes();
	_M_seconds = __hms.seconds();
      }

      void
      _M_fill_date_time(chrono::local_seconds __ls, _ChronoParts __parts)
      {
	_M_ldays = chrono::floor<chrono::days>(__ls);
	__parts -= _ChronoParts::_LocalDays;
	if ((__parts & _ChronoParts::_HoursMinutesSeconds) != 0)
	  _M_fill_time(_M_lseconds - _M_ldays);

	if ((__parts & _ChronoParts::_Date) != 0)
	  {
	    const chrono::year_month_day __ymd(_M_ldays);
	    _M_fill_year_month(__ymd, __parts);
	    _M_fill_day(__ymd.day(), __parts);
	    _M_fill_aux(_M_ldays, __parts);
	  }
      }

      void
      _M_fill_zone(const char* __abbrev, const wchar_t* __wabbrev)
      {
	if constexpr (is_same_v<_CharT, char>)
	  _M_zone_abbrev = __abbrev;
	else
	  _M_zone_abbrev = __wabbrev;
	_M_zone_cstr = __abbrev;
      }

      [[__gnu__::__always_inline__]]
      void
      _M_fill_utc_zone()
      { _M_fill_zone("UTC", L"UTC"); }
    };

  // TODO rename this to chrono::__formatter? or chrono::__detail::__formatter?
  template<typename _CharT>
    struct __formatter_chrono
    {
      using __string_view = basic_string_view<_CharT>;
      using __string = basic_string<_CharT>;

      __formatter_chrono() = default;

      constexpr explicit
      __formatter_chrono(_ChronoSpec<_CharT> __spec) noexcept
      : _M_spec(__spec)
      { }

      constexpr typename basic_format_parse_context<_CharT>::iterator
      _M_parse(basic_format_parse_context<_CharT>& __pc, _ChronoParts __parts,
	       const _ChronoSpec<_CharT>& __def)
      {
	auto __first = __pc.begin();
	auto __last = __pc.end();

	_ChronoSpec<_CharT> __spec = __def;

	auto __finalize = [this, &__spec, &__def] {
	  using enum _ChronoParts;
	  _ChronoParts __checked
	    = __spec._M_debug ? _YearMonthDay|_IndexedWeekday
			      : _Month|_Weekday;
	  // n.b. for calendar types __def._M_needed contains only parts
	  // copied from the input, remaining ones are computed, and thus ok
	  __spec._M_needs_ok_check
	     = __spec._M_needs(__def._M_needed & __checked);
	  _M_spec = __spec;
	};

	auto __finished = [&] {
	  if (__first == __last || *__first == '}')
	    {
	      __finalize();
	      return true;
	    }
	  return false;
	};

	if (__finished())
	  return __first;

	__first = __spec._M_parse_fill_and_align(__first, __last);
	if (__finished())
	  return __first;

	__first = __spec._M_parse_width(__first, __last, __pc);
	if (__finished())
	  return __first;

	if (*__first == '.')
	  {
	    if ((__parts & _ChronoParts::_EpochUnits) == 0
		  || !__spec._M_floating_point_rep)
	      __throw_format_error("format error: invalid precision for duration");

	    // Precision is allowed, but value is ignored.
	    __first = _Spec<_CharT>()._M_parse_precision(__first, __last, __pc);
	    // Still inditate that there was user supplied precision.
	    __spec._M_prec_kind = _WP_value;
	    if (__finished())
	      return __first;
	  }

	__spec._M_localized = false;
	__first = __spec._M_parse_locale(__first, __last);
	if (__finished())
	  return __first;

	// Everything up to the end of the string or the first '}' is a
	// chrono-specs string. Check it is valid.
	{
	  __string_view __str(__first, __last - __first);
	  auto __end = __str.find('}');
	  if (__end != __str.npos)
	    {
	      __str.remove_suffix(__str.length() - __end);
	      __last = __first + __end;
	    }
	  if (__str.find('{') != __str.npos)
	    __throw_format_error("chrono format error: '{' in chrono-specs");
	}

	// Parse chrono-specs in [first,last), checking each conversion-spec
	// against __parts (so fail for %Y if no year in parts).
	// Save range in __spec._M_chrono_specs.
	__spec._M_debug = false;
	__spec._M_locale_specific = false;
	__spec._M_needed = _ChronoParts::_None;
	__spec._M_chrono_specs = __string_view();

	const auto __chrono_specs = __first++; // Skip leading '%'
	if (*__chrono_specs != '%')
	  __throw_format_error("chrono format error: no '%' at start of "
			       "chrono-specs");

	_CharT __mod{};
	bool __conv = true;
	while (__first != __last)
	  {
	    enum _Mods { _Mod_none, _Mod_E, _Mod_O, _Mod_E_O };
	    _Mods __allowed_mods = _Mod_none;

	    _ChronoParts __needed = _ChronoParts::_None;
	    bool __locale_specific = false;

	    _CharT __c = *__first++;
	    switch (__c)
	      {
	      using enum _ChronoParts;
	      case 'a':
	      case 'A':
		__needed = _Weekday;
		__locale_specific = true;
		break;
	      case 'b':
	      case 'h':
	      case 'B':
		__needed = _Month;
		__locale_specific = true;
		break;
	      case 'c':
		__needed = _Date|_HoursMinutesSeconds;
		__allowed_mods = _Mod_E;
		__locale_specific = true;
		break;
	      case 'C':
		__needed = _Year;
		__allowed_mods = _Mod_E;
		break;
	      case 'd':
	      case 'e':
		__needed = _Day;
		__allowed_mods = _Mod_O;
		break;
	      case 'D':
	      case 'F':
		__needed = _YearMonthDay;
		break;
	      case 'g':
	      case 'G':
	      case 'V':
		__needed = _LocalDays|_Year|_DayOfYear|_Weekday;
		break;
	      case 'H':
	      case 'I':
		__needed = _HoursMinutesSeconds;
		__allowed_mods = _Mod_O;
		break;
	      case 'j':
		__needed = __parts & _DayOfYear;
		// If we do not know day-of-year then we must have a duration,
		// which is to be formatted as decimal number of days.
		if (__needed == _None)
		  __needed = _HoursMinutesSeconds;
		break;
	      case 'm':
		__needed = _Month;
		__allowed_mods = _Mod_O;
		break;
	      case 'M':
		__needed = _HoursMinutesSeconds;
		__allowed_mods = _Mod_O;
		break;
	      case 'p':
	      case 'r':
		__locale_specific = true;
		[[fallthrough]];
	      case 'R':
		__needed = _HoursMinutesSeconds;
		break;
	      case 'T':
		__needed = _TimeOfDay;
		break;
	      case 'q':
		__needed = _UnitSuffix;
		break;
	      case 'Q':
		__needed = _EpochUnits;
		break;
	      case 'S':
		__needed = _TimeOfDay;
		__allowed_mods = _Mod_O;
		break;
	      case 'u':
	      case 'w':
		__needed = _Weekday;
		__allowed_mods = _Mod_O;
		break;
	      case 'U':
	      case 'W':
		__needed = _DayOfYear|_Weekday;
		__allowed_mods = _Mod_O;
		break;
	      case 'x':
		__needed = _Date;
		__locale_specific = true;
		__allowed_mods = _Mod_E;
		break;
	      case 'X':
		__needed = _HoursMinutesSeconds;
		__locale_specific = true;
		__allowed_mods = _Mod_E;
		break;
	      case 'y':
		__needed = _Year;
		__allowed_mods = _Mod_E_O;
		break;
	      case 'Y':
		__needed = _Year;
		__allowed_mods = _Mod_E;
		break;
	      case 'z':
		__needed = _ZoneOffset;
		__allowed_mods = _Mod_E_O;
		break;
	      case 'Z':
		__needed = _ZoneAbbrev;
		break;
	      case 'n':
	      case 't':
	      case '%':
		break;
	      case 'O':
	      case 'E':
		if (__mod) [[unlikely]]
		  {
		    __allowed_mods = _Mod_none;
		    break;
		  }
		__mod = __c;
		continue;
	      default:
		__throw_format_error("chrono format error: invalid specifier "
				     "in chrono-specs");
	    }

	    if ((__mod == 'E' && !(__allowed_mods & _Mod_E))
		   || (__mod == 'O' && !(__allowed_mods & _Mod_O)))
	      __throw_format_error("chrono format error: invalid  modifier "
				   "in chrono-specs");
	    if (__mod && __c != 'z')
	      __locale_specific = true;
	    __mod = _CharT();

	    // localized formats do not include subseconds
	    if (__locale_specific)
	      __needed -= _ChronoParts::_Subseconds;

	    if ((__parts & __needed) != __needed)
	      __throw_format_error("chrono format error: format argument does "
				   "not contain the information required by the "
				   "chrono-specs");
	    __spec._M_needed |= __needed;
	    __spec._M_locale_specific |= __locale_specific;

	    // Scan for next '%', ignoring literal-chars before it.
	    size_t __pos = __string_view(__first, __last - __first).find('%');
	    if (__pos == 0)
	      ++__first;
	    else
	      {
		if (__pos == __string_view::npos)
		  {
		    __first = __last;
		    __conv = false;
		  }
		else
		  __first += __pos + 1;
	      }
	  }

	// Check for a '%' conversion-spec without a type.
	if (__conv || __mod != _CharT())
	  __throw_format_error("chrono format error: unescaped '%' in "
			       "chrono-specs");

	__spec._M_chrono_specs
	  = __string_view(__chrono_specs, __first - __chrono_specs);

	__finalize();
	return __first;
      }

      // pre: !_M_spec._M_chrono_specs.empty()
      template<typename _FormatContext>
	typename _FormatContext::iterator
	_M_format(const _ChronoData<_CharT>& __t, _FormatContext& __fc) const
	{
#if defined _GLIBCXX_USE_NL_LANGINFO_L && __CHAR_BIT__ == 8
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 3565. Handling of encodings in localized formatting
	  //       of chrono types is underspecified
	  if constexpr (is_same_v<_CharT, char>)
	    if constexpr (__unicode::__literal_encoding_is_utf8())
	      if (_M_spec._M_localized && _M_spec._M_locale_specific)
		{
		  extern locale __with_encoding_conversion(const locale&);

		  // Allocate and cache the necessary state to convert strings
		  // in the locale's encoding to UTF-8.
		  locale __loc = __fc.locale();
		  if (__loc != locale::classic())
		    __fc._M_loc = __with_encoding_conversion(__loc);
		}
#endif

	  const size_t __padwidth = _M_spec._M_get_width(__fc);
	  if (__padwidth == 0)
	    return _M_format_to(__t, __fc.out(), __fc);

	  using _Out = typename _FormatContext::iterator;
	  _Padding_sink<_Out, _CharT> __sink(__fc.out(), __padwidth);
	  _M_format_to(__t, __sink.out(), __fc);
	  return __sink._M_finish(_M_spec._M_align, _M_spec._M_fill);
	}

      _ChronoSpec<_CharT> _M_spec;

    protected:
      static constexpr const _CharT* _S_chars
	= _GLIBCXX_WIDEN("0123456789.Lf:/ +-{}");
      static constexpr _CharT _S_dot = _S_chars[10];
      static constexpr _CharT _S_colon = _S_chars[13];
      static constexpr _CharT _S_slash = _S_chars[14];
      static constexpr _CharT _S_space = _S_chars[15];
      static constexpr const _CharT* _S_fp_fmt = _S_chars + 11;
      static constexpr const _CharT* _S_plus_minus = _S_chars + 16;
      static constexpr const _CharT* _S_minus_empty_spec = _S_chars + 17;
      static constexpr const _CharT* _S_empty_spec = _S_chars + 18;

      [[__gnu__::__always_inline__]]
      static _Runtime_format_string<_CharT>
      _S_empty_fs()
      { return _Runtime_format_string<_CharT>(_S_empty_spec); }

      static constexpr const _CharT* _S_weekdays[]
      {
	_GLIBCXX_WIDEN("Sunday"),
	_GLIBCXX_WIDEN("Monday"),
	_GLIBCXX_WIDEN("Tuesday"),
	_GLIBCXX_WIDEN("Wednesday"),
	_GLIBCXX_WIDEN("Thursday"),
	_GLIBCXX_WIDEN("Friday"),
	_GLIBCXX_WIDEN("Saturday"),
      };

      static constexpr const _CharT* _S_months[]
      {
	_GLIBCXX_WIDEN("January"),
	_GLIBCXX_WIDEN("February"),
	_GLIBCXX_WIDEN("March"),
	_GLIBCXX_WIDEN("April"),
	_GLIBCXX_WIDEN("May"),
	_GLIBCXX_WIDEN("June"),
	_GLIBCXX_WIDEN("July"),
	_GLIBCXX_WIDEN("August"),
	_GLIBCXX_WIDEN("September"),
	_GLIBCXX_WIDEN("October"),
	_GLIBCXX_WIDEN("November"),
	_GLIBCXX_WIDEN("December"),
      };

    private:
      template<typename _OutIter>
	_OutIter
	_M_write(_OutIter __out, [[maybe_unused]] const locale& __loc,
		 __string_view __s) const
	{
#if defined _GLIBCXX_USE_NL_LANGINFO_L && __CHAR_BIT__ == 8
	  __sso_string __buf;
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 3565. Handling of encodings in localized formatting
	  //       of chrono types is underspecified
	  if constexpr (is_same_v<_CharT, char>)
	    if constexpr (__unicode::__literal_encoding_is_utf8())
	      if (_M_spec._M_localized && _M_spec._M_locale_specific
		    && __loc != locale::classic())
		{
		  extern string_view
		  __locale_encoding_to_utf8(const locale&, string_view, void*);

		  __s = __locale_encoding_to_utf8(__loc, __s, &__buf);
		}
#endif
	  return __format::__write(std::move(__out), __s);
	}

      [[__gnu__::__always_inline__]]
      static bool
      _S_localized_spec(_CharT __conv, _CharT __mod)
      {
	switch (__conv)
	  {
	  case 'a':
	  case 'A':
	  case 'b':
	  case 'B':
	  case 'c':
	  case 'h':
	  case 'p':
	  case 'r':
	  case 'x':
	  case 'X':
	    return true;
	  case 'z':
	    return false;
	  default:
	    return (bool)__mod;
	  };
      }

      // Use the formatting locale's std::time_put facet to produce
      // a locale-specific representation.
      template<typename _Iter>
	_Iter
	_M_locale_fmt(_Iter __out, const locale& __loc, const struct tm& __tm,
		      char __fmt, char __mod) const
	{
	  basic_ostringstream<_CharT> __os;
	  __os.imbue(__loc);
	  const auto& __tp = use_facet<time_put<_CharT>>(__loc);
	  __tp.put(__os, __os, _S_space, &__tm, __fmt, __mod);
	  if (__os)
	    __out = _M_write(std::move(__out), __loc, __os.view());
	  return __out;
	}

      __string_view
      _M_check_ok(const _ChronoData<_CharT>& __t, _CharT& __conv) const
      {
	if (!_M_spec._M_debug)
	  {
	    switch (__conv)
	    {
	    case 'a':
	    case 'A':
	      if (!__t._M_weekday.ok()) [[unlikely]]
		__throw_format_error("format error: invalid weekday");
	      break;
	    case 'b':
	    case 'h':
	    case 'B':
	      if (!__t._M_month.ok()) [[unlikely]]
		__throw_format_error("format error: invalid month");
	      break;
	    default:
	      break;
	    }
	    return __string_view();
	  }

	switch (__conv)
	{
	// %\0 is extension for handling weekday index
	case '\0':
	  if (__t._M_weekday_index < 1 || __t._M_weekday_index > 5) [[unlikely]]
	    return _GLIBCXX_WIDEN("index");
	  break;
	case 'a':
	case 'A':
	  if (!__t._M_weekday.ok()) [[unlikely]]
	    {
	      __conv = 'w'; // print as decimal number
	      return _GLIBCXX_WIDEN("weekday");
	    }
	  break;
	case 'b':
	case 'h':
	case 'B':
	  if (!__t._M_month.ok()) [[unlikely]]
	    {
	      __conv = 'm'; // print as decimal number
	      return _GLIBCXX_WIDEN("month");
	    }
	  break;
	case 'd':
	case 'e':
	  if (!__t._M_day.ok()) [[unlikely]]
	    return _GLIBCXX_WIDEN("day");
	  break;
	case 'F':
	  if (!(__t._M_year/__t._M_month/__t._M_day).ok()) [[unlikely]]
	    return _GLIBCXX_WIDEN("date");
	  break;
	case 'Y':
	  if (!__t._M_year.ok()) [[unlikely]]
	    return _GLIBCXX_WIDEN("year");
	  break;
	default:
	  break;
	}
	return __string_view();
      }

      template<typename _OutIter, typename _FormatContext>
	_OutIter
	_M_format_to(const _ChronoData<_CharT>& __t, _OutIter __out,
		     _FormatContext& __fc) const
	{
	  auto __first = _M_spec._M_chrono_specs.begin();
	  const auto __last = _M_spec._M_chrono_specs.end();

	  auto __print_sign = [__is_neg = __t._M_is_neg, &__out] () mutable {
	    if (__is_neg)
		{
		  *__out++ = _S_plus_minus[1];
		  __is_neg = false;
		}
	    return std::move(__out);
	  };

	  struct tm __tm{};
	  bool __use_locale_fmt = false;
	  if (_M_spec._M_localized && _M_spec._M_locale_specific)
	    if (__fc.locale() != locale::classic())
	      {
		__use_locale_fmt = true;

		__tm.tm_year = (int)__t._M_year - 1900;
		__tm.tm_yday = __t._M_day_of_year.count();
		__tm.tm_mon = (unsigned)__t._M_month - 1;
		__tm.tm_mday = (unsigned)__t._M_day;
		__tm.tm_wday = __t._M_weekday.c_encoding();
		__tm.tm_hour = __t._M_hours.count();
		__tm.tm_min = __t._M_minutes.count();
		__tm.tm_sec = __t._M_seconds.count();

		// Some locales use %Z in their %c format but we don't want strftime
		// to use the system's local time zone (from /etc/localtime or $TZ)
		// as the output for %Z. Setting tm_isdst to -1 says there is no
		// time zone info available for the time in __tm.
		__tm.tm_isdst = -1;

#ifdef _GLIBCXX_USE_STRUCT_TM_TM_ZONE
		// POSIX.1-2024 adds tm.tm_zone which will be used for %Z.
		// BSD has had tm_zone since 1987 but as char* so cast away const.
		if (__t._M_zone_cstr)
		  __tm.tm_zone = const_cast<char*>(__t._M_zone_cstr);
#endif
	      }

	  // Characters to output for "%n", "%t" and "%%" specifiers.
	  constexpr const _CharT* __literals = _GLIBCXX_WIDEN("\n\t%");

	  ++__first; // Skip leading '%' at start of chrono-specs.

	  _CharT __mod{};
	  do
	    {
	      _CharT __c = *__first++;
	      __string_view __invalid;
	      if (_M_spec._M_needs_ok_check)
		__invalid = _M_check_ok(__t, __c);

	      if (__invalid.empty() &&__use_locale_fmt
		    && _S_localized_spec(__c, __mod)) [[unlikely]]
		__out = _M_locale_fmt(std::move(__out), __fc.locale(),
				      __tm, __c, __mod);
	      else switch (__c)
		{
		// %\0 is extension for handling weekday index
		case '\0':
		  __out = _M_wi(__t._M_weekday_index, std::move(__out));
		  break;
		case 'a':
		case 'A':
		  __out = _M_a_A(__t._M_weekday, std::move(__out), __c == 'A');
		  break;
		case 'b':
		case 'h':
		case 'B':
		  __out = _M_b_B(__t._M_month, std::move(__out), __c == 'B');
		  break;
		case 'c':
		  __out = _M_c(__t, std::move(__out));
		  break;
		case 'C':
		case 'y':
		case 'Y':
		  __out = _M_C_y_Y(__t._M_year, std::move(__out), __c);
		  break;
		case 'd':
		case 'e':
		  __out = _M_d_e(__t._M_day, std::move(__out), __c);
		  break;
		case 'D':
		case 'x':
		  __out = _M_D_x(__t, std::move(__out));
		  break;
		case 'F':
		  __out = _M_F(__t, std::move(__out));
		  break;
		case 'g':
		case 'G':
		case 'V':
		  __out = _M_g_G_V(__t, std::move(__out), __c);
		  break;
		case 'H':
		case 'I':
		  __out = _M_H_I(__t._M_hours, __print_sign(), __c);
		  break;
		case 'j':
		  __out = _M_j(__t, __print_sign());
		  break;
		case 'm':
		  __out = _M_m(__t._M_month, std::move(__out));
		  break;
		case 'M':
		  __out = _M_M(__t._M_minutes, __print_sign());
		  break;
		case 'p':
		  __out = _M_p(__t._M_hours, std::move(__out));
		  break;
		case 'q':
		  __out = _M_q(__t._M_unit_suffix, std::move(__out));
		  break;
		case 'Q':
		  __out = _M_Q(__t, __print_sign(), __fc);
		  break;
		case 'r':
		  __out = _M_r(__t, __print_sign());
		  break;
		case 'R':
		case 'X':
		  __out = _M_R_X(__t, __print_sign(), __c != 'R');
		  break;
		case 'T':
		  __out = _M_T(__t, __print_sign(), __fc);
		  break;
		case 'S':
		  __out = _M_S(__t, __print_sign(), __fc, __mod != 'O');
		  break;
		case 'u':
		case 'w':
		  __out = _M_u_w(__t._M_weekday, std::move(__out), __c);
		  break;
		case 'U':
		case 'W':
		  __out = _M_U_W(__t, std::move(__out), __c);
		  break;
		case 'z':
		  __out = _M_z(__t._M_zone_offset, std::move(__out), (bool)__mod);
		  break;
		case 'Z':
		  __out = _M_Z(__t._M_zone_abbrev, std::move(__out));
		  break;
		case 'n':
		  *__out++ = __literals[0];
		  break;
		case 't':
		  *__out++ = __literals[1];
		  break;
		case '%':
		  *__out++ = __literals[2];
		  break;
		case 'O':
		case 'E':
		  __mod = __c;
		  continue;
		case '}':
		  __first = __last;
		  break;
		}

	      if (!__invalid.empty())
		{
		  constexpr __string_view __pref = _GLIBCXX_WIDEN(" is not a valid ");
		  __out = __format::__write(std::move(__out), __pref);
		  __out = __format::__write(std::move(__out), __invalid);
		}

	      __mod = _CharT();
	      // Scan for next '%' and write out everything before it.
	      __string_view __str(__first, __last - __first);
	      size_t __pos = __str.find('%');
	      if (__pos == 0)
		++__first;
	      else
		{
		  if (__pos == __str.npos)
		    __first = __last;
		  else
		    {
		      __str.remove_suffix(__str.length() - __pos);
		      __first += __pos + 1;
		    }
		  __out = __format::__write(std::move(__out), __str);
		}
	    }
	  while (__first != __last);
	  return std::move(__out);
	}

      template<typename _OutIter>
	_OutIter
	_M_wi(unsigned __wi, _OutIter __out) const
	{
	  // %\0 Extension to format weekday index, used only by empty format spec
	  _CharT __buf[3];
	  __out = __format::__write(std::move(__out), _S_str_d1(__buf, __wi));
	  return std::move(__out);
	}

      template<typename _OutIter>
	_OutIter
	_M_a_A(chrono::weekday __wd, _OutIter __out, bool __full) const
	{
	  // %a Locale's abbreviated weekday name.
	  // %A Locale's full weekday name.
	  __string_view __str = _S_weekdays[__wd.c_encoding()];
	  if (!__full)
	    __str = __str.substr(0, 3);
	  return __format::__write(std::move(__out), __str);
	}

      template<typename _OutIter>
	_OutIter
	_M_b_B(chrono::month __m, _OutIter __out, bool __full) const
	{
	  // %b Locale's abbreviated month name.
	  // %B Locale's full month name.
	  __string_view __str = _S_months[(unsigned)__m - 1];
	  if (!__full)
	    __str = __str.substr(0, 3);
	  return __format::__write(std::move(__out), __str);
	}

      template<typename _OutIter>
	_OutIter
	_M_c(const _ChronoData<_CharT>& __t, _OutIter __out) const
	{
	  // %c  Locale's date and time representation, for C-locale: %a %b %e %T %Y
	  // %Ec Locale's alternate date and time representation, for C-locale same as above

	  __out = _M_a_A(__t._M_weekday, std::move(__out), false);
	  *__out = _S_space;
	  __out = _M_b_B(__t._M_month, std::move(++__out), false);
	  *__out = _S_space;
	  __out = _M_d_e(__t._M_day, std::move(++__out), 'e');
	  *__out = _S_space;
	  __out = _M_R_X(__t, std::move(++__out), true);
	  *__out = _S_space;
	  return _M_C_y_Y(__t._M_year, std::move(++__out), 'Y');
	}

      template<typename _OutIter>
	_OutIter
	_M_C_y_Y(chrono::year __y, _OutIter __out, _CharT __conv) const
	{
	  // %C  Year divided by 100 using floored division.
	  // %EC Locale's alternative preresentation of the century (era name).
	  // %y  Last two decimal digits of the year.
	  // %Oy Locale's alternative representation.
	  // %Ey Locale's alternative representation of offset from %EC.
	  // %Y  Year as a decimal number.
	  // %EY Locale's alternative full year representation.

	  int __yi = (int)__y;
	  const bool __is_neg = __yi < 0;
	  __yi = __builtin_abs(__yi);
	  int __ci = __yi / 100;
	  // For floored division -123//100 is -2 and -100//100 is -1
	  if (__conv == 'C' && __is_neg && (__ci * 100) != __yi) [[unlikely]]
	    ++__ci;

	  if (__conv != 'y' && __ci >= 100) [[unlikely]]
	    {
	      using _FmtStr = _Runtime_format_string<_CharT>;
	      __string_view __fs = _S_minus_empty_spec + !__is_neg;
	      __out = std::format_to(std::move(__out), _FmtStr(__fs),
				     __conv == 'C' ? __ci : __yi);
	    }
	  else
	    {
	      _CharT __buf[5];
	      __buf[0] = _S_plus_minus[1];
	      __string_view __sv(__buf + 3, __buf + 3);
	      if (__conv != 'y')
		{
		  _S_fill_two_digits(__buf + 1, __ci);
		  __sv = __string_view(__buf + !__is_neg, __buf + 3);
		}
	      if (__conv != 'C')
		{
		  _S_fill_two_digits(__buf + 3, __yi % 100);
		  __sv = __string_view(__sv.data(), __buf + 5);
		}
	      __out = __format::__write(std::move(__out), __sv);
	    }
	  return __out;
	}

      template<typename _OutIter>
	_OutIter
	_M_D_x(const _ChronoData<_CharT>& __t, _OutIter __out) const
	{
	  // %D  Equivalent to %m/%d/%y
	  // %x  Locale's date rep, for C-locale: %m/%d/%y
	  // %Ex Locale's alternative date representation, for C-locale same as above

	  auto __di = (unsigned)__t._M_day;
	  auto __mi = (unsigned)__t._M_month;
	  auto __yi = __builtin_abs((int)__t._M_year) % 100;

	  if (__mi >= 100 || __di >= 100) [[unlikely]]
	    {
	      using _FmtStr = _Runtime_format_string<_CharT>;
	      __string_view __fs = _GLIBCXX_WIDEN("{:02d}/{:02d}/{:02d}");
	      __out = std::format_to(std::move(__out), _FmtStr(__fs),
				     __mi, __di, __yi);
	    }
	  else
	    {
	      _CharT __buf[8];
	      __buf[2] = _S_slash;
	      __buf[5] = _S_slash;
	      __string_view __sv(__buf, __buf + 8);

	      _S_fill_two_digits(__buf, __mi);
	      _S_fill_two_digits(__buf + 3, __di);
	      _S_fill_two_digits(__buf + 6, __yi);
	      __out = __format::__write(std::move(__out), __sv);
	    }
	  return std::move(__out);
	}

      template<typename _OutIter>
	_OutIter
	_M_d_e(chrono::day __d, _OutIter __out, _CharT __conv) const
	{
	  // %d  The day of month as a decimal number.
	  // %Od Locale's alternative representation.
	  // %e  Day of month as decimal number, padded with space.
	  // %Oe Locale's alternative digits.

	  unsigned __i = (unsigned)__d;

	  _CharT __buf[3];
	  auto __sv = _S_str_d2(__buf, __i);
	  if (__conv == _CharT('e') && __i < 10)
	    {
	      __buf[1] = __sv[1];
	      __buf[0] = _S_space;
	      __sv = {__buf, 2};
	    }

	  __out = __format::__write(std::move(__out), __sv);
	  return std::move(__out);
	}

      template<typename _OutIter>
	_OutIter
	_M_F(const _ChronoData<_CharT>& __t, _OutIter __out) const
	{
	  auto __di = (unsigned)__t._M_day;
	  auto __mi = (unsigned)__t._M_month;
	  auto __yi = (int)__t._M_year;
	  const bool __is_neg = __yi < 0;
	  __yi = __builtin_abs(__yi);

	  if (__yi >= 10000 || __mi >= 100 || __di >= 100) [[unlikely]]
	    {
	      using _FmtStr = _Runtime_format_string<_CharT>;
	      __string_view __fs
		= _GLIBCXX_WIDEN("-{:04d}-{:02d}-{:02d}") + !__is_neg;
	      __out = std::format_to(std::move(__out), _FmtStr(__fs),
				     __yi, __mi, __di);
	    }
	  else
	    {
	      _CharT __buf[11];
	      __buf[0] = _S_plus_minus[1];
	      __buf[5] = _S_plus_minus[1];
	      __buf[8] = _S_plus_minus[1];
	      __string_view __sv(__buf + !__is_neg, __buf + 11);

	      _S_fill_two_digits(__buf + 1, __yi / 100);
	      _S_fill_two_digits(__buf + 3, __yi % 100);
	      _S_fill_two_digits(__buf + 6, __mi);
	      _S_fill_two_digits(__buf + 9, __di);
	      __out = __format::__write(std::move(__out), __sv);
	    }

	  return std::move(__out);
	}

      template<typename _OutIter>
	_OutIter
	_M_g_G_V(const _ChronoData<_CharT>& __t, _OutIter __out,
		_CharT __conv) const
	{
	  // %g  last two decimal digits of the ISO week-based year.
	  // %G  ISO week-based year.
	  // %V  ISO week-based week number as a decimal number.
	  // %OV Locale's alternative numeric rep.

	  // ISO week-based year of __t is the year that contains the nearest
	  // Thursday. The ISO week of __t is the number of weeks since
	  // January 1 of that year.

	  using namespace chrono;
	  // Offset of the nearest Thursday:
	  const days __offset = (__t._M_weekday - Monday) - days(3);
	  // Nearest Thursday as local days:
	  const local_days __ild = __t._M_ldays - __offset;
	  // Day of year of nearest Thursday:
	  days __idoy = __t._M_day_of_year - __offset;

	  // Year of nearest Thursday:
	  year __iyear;
	  if (__idoy <= days(0))
	    __iyear = __t._M_year - years(1);
	  else if (__idoy <= days(365))
	    __iyear = __t._M_year;
	  else if (__idoy == days(366) && __t._M_year.is_leap())
	    __iyear = __t._M_year;
	  else if (__idoy <= days(730))
	    __iyear = __t._M_year + years(1);
	  else [[unlikely]]
	    __iyear = year_month_day(__ild).year();

	  if (__conv != 'V')
	    return _M_C_y_Y(__iyear, std::move(__out), "yY"[__conv == 'G']);

	  if (__iyear != __t._M_year)
	    __idoy = __ild - local_days(__iyear/January/0);

	  const auto __wi = chrono::floor<weeks>(__idoy - days(1)).count() + 1;
	  return __format::__write(std::move(__out), _S_two_digits(__wi));
	}

      template<typename _OutIter>
	_OutIter
	_M_H_I(chrono::hours __h, _OutIter __out, _CharT __conv) const
	{
	  // %H  The hour (24-hour clock) as a decimal number.
	  // %OH Locale's alternative representation.
	  // %I  The hour (12-hour clock) as a decimal number.
	  // %OI Locale's alternative representation.

	  int __i = __h.count();

	  if (__conv == _CharT('I'))
	    {
	      __i %= 12;
	      if (__i == 0)
		__i = 12;
	    }
	  else if (__i >= 100) [[unlikely]]
	    return std::format_to(std::move(__out), _S_empty_fs(), __i);

	  return __format::__write(std::move(__out), _S_two_digits(__i));
	}

      template<typename _OutIter>
	_OutIter
	_M_j(const _ChronoData<_CharT>& __t, _OutIter __out) const
	{
	  if (!_M_spec._M_needs(_ChronoParts::_DayOfYear))
	  {
	    // Decimal number of days, without padding.
	    auto __d = chrono::floor<chrono::days>(__t._M_hours).count();
	    return std::format_to(std::move(__out), _S_empty_fs(), __d);
	  }

	  auto __d = __t._M_day_of_year.count();
	  if (__d >= 1000) [[unlikely]]
	    return std::format_to(std::move(__out), _S_empty_fs(), __d);

	  _CharT __buf[3];
	  return __format::__write(std::move(__out), _S_str_d3(__buf, __d));
	}

      template<typename _OutIter>
	_OutIter
	_M_m(chrono::month __m, _OutIter __out) const
	{
	  // %m  month as a decimal number.
	  // %Om Locale's alternative representation.
	  auto __i = (unsigned)__m;
	  if (__i == 0 && _M_spec._M_debug) [[unlikely]]
	    // 0 should not be padded to two digits
	    return __format::__write(std::move(__out), _S_digit(0));

	  _CharT __buf[3];
	  return __format::__write(std::move(__out), _S_str_d2(__buf, __i));
	}

      template<typename _OutIter>
	_OutIter
	_M_M(chrono::minutes __m, _OutIter __out) const
	{
	  // %M  The minute as a decimal number.
	  // %OM Locale's alternative representation.

	  auto __i = __m.count();
	  return __format::__write(std::move(__out), _S_two_digits(__i));
	}

      template<typename _OutIter>
	_OutIter
	_M_p(chrono::hours __h, _OutIter __out) const
	{
	  // %p The locale's equivalent of the AM/PM designations.

	  _CharT __buf[2];
	  _S_fill_ampm(__buf, __h);
	  return __format::__write(std::move(__out), __string_view(__buf, 2));
	}

      template<typename _OutIter>
	_OutIter
	_M_q(__string_view __us, _OutIter __out) const
	{
	  // %q The duration's unit suffix
	  return __format::__write(std::move(__out), __us);
	}

      template<typename _OutIter, typename _FormatContext>
	_OutIter
	_M_Q(const _ChronoData<_CharT>& __t, _OutIter __out,
	     _FormatContext&) const
	{
	  // %Q The duration's numeric value.
	  return std::vformat_to(std::move(__out), _S_empty_spec, __t._M_ereps);
	}

      template<typename _OutIter>
	_OutIter
	_M_r(const _ChronoData<_CharT>& __t, _OutIter __out) const
	{
	  // %r Locale's 12-hour clock time, for C-locale: %I:%M:%S %p
	  auto __hi = __t._M_hours.count() % 12;
	  if (__hi == 0)
	    __hi = 12;

	  _CharT __buf[11];
	  __buf[2] = _S_colon;
	  __buf[5] = _S_colon;
	  __buf[8] = _S_space;
	  _S_fill_two_digits(__buf, __hi);
	  _S_fill_two_digits(__buf + 3, __t._M_minutes.count());
	  _S_fill_two_digits(__buf + 6, __t._M_seconds.count());
	  _S_fill_ampm(__buf + 9, __t._M_hours);

	  return __format::__write(std::move(__out), __string_view(__buf, 11));
	}

      template<typename _OutIter>
	_OutIter
	_M_R_X(const _ChronoData<_CharT>& __t, _OutIter __out,
	       bool __secs) const
	{
	  // %R  Equivalent to %H:%M
	  // %X  Locale's time rep, for C-locale: %H:%M:%S (without subseconds)
	  // %EX Locale's alternative time representation, for C-locale same as above

	  auto __hi = __t._M_hours.count();

	  _CharT __buf[8];
	  __buf[2] = _S_colon;
	  __buf[5] = _S_colon;
	  __string_view __sv(__buf, 8);

	  if (__hi >= 100) [[unlikely]]
	    {
	      __out = std::format_to(std::move(__out), _S_empty_fs(), __hi);
	      __sv.remove_prefix(2);
	    }
	  else
	    _S_fill_two_digits(__buf, __hi);

	  _S_fill_two_digits(__buf + 3, __t._M_minutes.count());
	  if (__secs)
	    _S_fill_two_digits(__buf + 6, __t._M_seconds.count());
	  else
	    __sv.remove_suffix(3);

	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _OutIter, typename _FormatContext>
	_OutIter
	_M_S(const _ChronoData<_CharT>& __t, _OutIter __out,
	     _FormatContext& __ctx, bool __subs = true) const
	{
	  // %S  Seconds as a decimal number.
	  // %OS The locale's alternative representation.
	  auto __s = __t._M_seconds;

	  __out = __format::__write(std::move(__out),
				    _S_two_digits(__s.count()));
	  if (__subs)
	    __out = _M_subsecs(__t, std::move(__out), __ctx);
	  return __out;
	}

      template<typename _OutIter, typename _FormatContext>
	_OutIter
	_M_subsecs(const _ChronoData<_CharT>& __t, _OutIter __out,
		   _FormatContext& __ctx) const
	{
	  unsigned __prec = _M_spec._M_prec_kind != _WP_none
			  ? _M_spec._M_get_precision(__ctx)
			  : _M_spec._M_prec;
	  if (__prec == 0)
	    return __out;

	  _CharT __dot = _S_dot;
	  if (_M_spec._M_localized) [[unlikely]]
	    {
	      auto __loc = __ctx.locale();
	      const auto& __np = use_facet<numpunct<_CharT>>(__loc);
	      __dot = __np.decimal_point();
	    }
	  *__out = __dot;
	  ++__out;

	  if (_M_spec._M_floating_point_rep)
	    {
	      _Str_sink<_CharT> __sink;
	      if (_M_spec._M_localized && _M_spec._M_custom_rep) [[unlikely]]
		 std::vformat_to(__sink.out(), __ctx.locale(),
				 _GLIBCXX_WIDEN("{1:0.{2}Lf}"), __t._M_ereps);
	      else
		 std::vformat_to(__sink.out(),
				 _GLIBCXX_WIDEN("{1:0.{2}f}"), __t._M_ereps);

	      auto __sv = __sink.view();
	      // Skip leading zero and dot
	      __sv.remove_prefix(2);
	      return __format::__write(std::move(__out), __sv);
	    }

	  constexpr unsigned __max_prec = _ChronoData<_CharT>::_S_max_prec;
	  constexpr typename _ChronoData<_CharT>::_Attoseconds::rep __pow10t[]
	  {
	    1u,
	    10u,                     100u,                     1000u,
	    10'000u,                 100'000u,                 1000'000u,
	    10'000'000u,             100'000'000u,             1000'000'000u,
	    10'000'000'000u,         100'000'000'000u,         1000'000'000'000u,
	    10'000'000'000'000u,     100'000'000'000'000u,     1000'000'000'000'000u,
	    10'000'000'000'000'000u, 100'000'000'000'000'000u, 1000'000'000'000'000'000u,
	  };

	  auto __subs = __t._M_subseconds.count();
	  if (__prec < __max_prec)
	    __subs /= __pow10t[__max_prec - __prec];
	  else if (__prec > __max_prec)
	    __prec = __max_prec;

	  using _FmtStr = _Runtime_format_string<_CharT>;
	  return std::format_to(__out, _FmtStr(_GLIBCXX_WIDEN("{0:0{1}}")),
				__subs, __prec);
	}

      // %t handled in _M_format

      template<typename _OutIter, typename _FormatContext>
	_OutIter
	_M_T(const _ChronoData<_CharT>& __t, _OutIter __out,
	     _FormatContext& __ctx) const
	{
	  // %T Equivalent to %H:%M:%S, with subseconds
	  __out = _M_R_X(__t, std::move(__out), true);
	  return _M_subsecs(__t, std::move(__out), __ctx);
	}

      template<typename _OutIter>
	_OutIter
	_M_u_w(chrono::weekday __wd, _OutIter __out, _CharT __conv) const
	{
	  // %u  ISO weekday as a decimal number (1-7), where Monday is 1.
	  // %Ou Locale's alternative numeric rep.
	  // %w  Weekday as a decimal number (0-6), where Sunday is 0.
	  // %Ow Locale's alternative numeric rep.
	  unsigned __wdi = __conv == 'u' ? __wd.iso_encoding()
					 : __wd.c_encoding();
	  _CharT __buf[3];
	  return __format::__write(std::move(__out), _S_str_d1(__buf, __wdi));
	}

      template<typename _OutIter>
	_OutIter
	_M_U_W(const _ChronoData<_CharT>& __t, _OutIter __out,
	       _CharT __conv) const
	{
	  // %U  Week number of the year as a decimal number, from first Sunday.
	  // %OU Locale's alternative numeric rep.
	  // %W  Week number of the year as a decimal number, from first Monday.
	  // %OW Locale's alternative numeric rep.

	  using namespace chrono;
	  const weekday __weekstart = __conv == 'U' ? Sunday : Monday;
	  const days __offset = __t._M_weekday - __weekstart;
	  auto __weeks = chrono::floor<weeks>(__t._M_day_of_year - __offset - days(1));
	  return __format::__write(std::move(__out), _S_two_digits(__weeks.count() + 1));
	}

      template<typename _OutIter>
	_OutIter
	_M_z(chrono::seconds __ts, _OutIter __out, bool __mod = false) const
	{
	  if (__ts == 0s)
	    {
	      __string_view __zero
		= __mod ? _GLIBCXX_WIDEN("+00:00") : _GLIBCXX_WIDEN("+0000");
	      return __format::__write(std::move(__out), __zero);
	    }

	  chrono::hh_mm_ss<chrono::seconds> __hms(__ts);
	  unsigned __mo = 3 + __mod;

	  _CharT __buf[6];
	  __buf[0] = _S_plus_minus[__hms.is_negative()];
	  __buf[3] = _S_colon;
	  _S_fill_two_digits(__buf + 1, __hms.hours().count());
	  _S_fill_two_digits(__buf + __mo, __hms.minutes().count());

	  __string_view __sv(__buf, __mo + 2);
	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _OutIter>
	_OutIter
	_M_Z(__string_view __abbrev, _OutIter __out) const
	{ return __format::__write(std::move(__out), __abbrev);	}

      // %% handled in _M_format

      // A string view of single digit character, "0".."9".
      static basic_string_view<_CharT>
      _S_digit(int __n) noexcept
      {
	// Extra 9s avoid past-the-end read on bad input.
	return { _GLIBCXX_WIDEN("0123456789999999") + (__n & 0xf), 1 };
      }

      // A string view of two digit characters, "00".."99".
      static basic_string_view<_CharT>
      _S_two_digits(int __n) noexcept
      {
	return {
	  _GLIBCXX_WIDEN("0001020304050607080910111213141516171819"
			 "2021222324252627282930313233343536373839"
			 "4041424344454647484950515253545556575859"
			 "6061626364656667686970717273747576777879"
			 "8081828384858687888990919293949596979899"
			 "9999999999999999999999999999999999999999"
			 "9999999999999999") + 2 * (__n & 0x7f),
	  2
	};
      }

      // Fills __buf[0] and __buf[1] with 2 digit value of __n.
      [[__gnu__::__always_inline__]]
      static void
      _S_fill_two_digits(_CharT* __buf, unsigned __n)
      {
	auto __sv = _S_two_digits(__n);
	__buf[0] = __sv[0];
	__buf[1] = __sv[1];
      }

      // Fills __buf[0] and __buf[1] with "AM", "PM" depending on __h.
      [[__gnu__::__always_inline__]]
      static void
      _S_fill_ampm(_CharT* __buf, chrono::hours __h)
      {
	auto __hi =  __h.count();
	if (__hi >= 24) [[unlikely]]
	  __hi %= 24;

	constexpr const _CharT* __apm = _GLIBCXX_WIDEN("APM");
	__buf[0] = __apm[__hi >= 12];
	__buf[1] = __apm[2];
      }

      // Returns decimal representation of __n.
      // Returned string_view may point to __buf.
      [[__gnu__::__always_inline__]]
      static basic_string_view<_CharT>
      _S_str_d1(span<_CharT, 3> __buf, unsigned __n)
      {
	if (__n < 10) [[likely]]
	  return _S_digit(__n);
	return _S_str_d2(__buf, __n);
      }

      // Returns decimal representation of __n, padded to 2 digits.
      // Returned string_view may point to __buf.
      [[__gnu__::__always_inline__]]
      static basic_string_view<_CharT>
      _S_str_d2(span<_CharT, 3> __buf, unsigned __n)
      {
	if (__n < 100) [[likely]]
	  return _S_two_digits(__n);
	return _S_str_d3(__buf, __n);
      }

      // Returns decimal representation of __n, padded to 3 digits.
      // Returned string_view points to __buf.
      [[__gnu__::__always_inline__]]
      static basic_string_view<_CharT>
      _S_str_d3(span<_CharT, 3> __buf, unsigned __n)
      {
	_S_fill_two_digits(__buf.data(), __n / 10);
	__buf[2] = _S_chars[__n % 10];
	return __string_view(__buf.data(), 3);
      }
    };

  template<typename _CharT>
    struct __formatter_duration : private __formatter_chrono<_CharT>
    {
      template<typename _Rep, typename _Period>
	constexpr static auto
	_S_subseconds(const chrono::duration<_Rep, _Period>& __d)
	{
	  if constexpr (chrono::treat_as_floating_point_v<_Rep>)
	    return chrono::duration<_Rep>(__d);
	  else if constexpr (_Period::den == 1)
	    return chrono::seconds(0);
	  else
	   {
	     using _Attoseconds = _ChronoData<_CharT>::_Attoseconds;
	     using _CRep = common_type_t<_Rep, typename _Attoseconds::rep>;
	     chrono::duration<_CRep, _Period> subs(__d.count());
	     return chrono::duration_cast<_Attoseconds>(subs);
	   }
       }

    public:
      template<typename _Duration>
	static consteval
	_ChronoSpec<_CharT>
	_S_spec_for(_ChronoParts __parts)
	{
	  using _Rep = typename _Duration::rep;
	  using enum _ChronoParts;

	  _ChronoSpec<_CharT> __res{};
	  __res._M_floating_point_rep = chrono::treat_as_floating_point_v<_Rep>;
	  __res._M_custom_rep = !is_arithmetic_v<_Rep>;
	  __res._M_prec = chrono::hh_mm_ss<_Duration>::fractional_width;
	  if ((__parts & _TimeOfDay) != 0)
	     __res._M_localized = __res._M_prec > 0 || __res._M_floating_point_rep;

	  if ((__parts & _TimeOfDay) != 0)
	    __res._M_needed |= _TimeOfDay;
	  if ((__parts & _Date) != 0)
	    __res._M_needed |= _YearMonthDay;
	  if ((__parts & _ZoneAbbrev) != 0)
	    __res._M_needed |= _ZoneAbbrev;

	  switch (__parts)
	    {
	    case _ZonedDateTime:
	      __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_ftz();
	      break;
	    case _DateTime:
	      __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_ft();
	      break;
	    case _Date:
	      __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_f();
	      break;
	    case _Time:
	      __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_t();
	      break;
	    case _None:
	      break;
	    default:
	      __builtin_unreachable();
	    }
	  return __res;
	};

      template<typename _Duration>
	static consteval
	_ChronoSpec<_CharT>
	_S_spec_for_tp()
	{
	  using enum _ChronoParts;
	  // streaming of local_time is defined in terms of sys_time
	  constexpr bool __stream_insertable =
	    requires (basic_ostream<_CharT>& __os, chrono::sys_time<_Duration> __t)
	      { __os << __t; };
	  if constexpr (!__stream_insertable)
	    return _S_spec_for<_Duration>(_None);
	  else if constexpr (is_convertible_v<_Duration, chrono::days>)
	    return _S_spec_for<_Duration>(_Date);
	  else
	    return _S_spec_for<_Duration>(_DateTime);
	}

      using __formatter_chrono<_CharT>::__formatter_chrono;
      using __formatter_chrono<_CharT>::_M_spec;

      template<typename _Duration>
	constexpr typename basic_format_parse_context<_CharT>::iterator
	_M_parse(basic_format_parse_context<_CharT>& __pc, _ChronoParts __parts,
		 const _ChronoSpec<_CharT>& __def)
	{
	  using _Rep = typename _Duration::rep;
	  using enum _ChronoParts;

	  auto __res
	    = __formatter_chrono<_CharT>::_M_parse(__pc, __parts, __def);
	  // n.b. durations do not contain date parts, and for time point all
	  // date parts are computed, and they are always ok.
	  _M_spec._M_needs_ok_check = false;

	  // check for custom floating point durations, if digits of output
	  // will contain subseconds, then formatters must support specifying
	  // precision.
	  if constexpr (!is_floating_point_v<_Rep>)
	    if constexpr (chrono::treat_as_floating_point_v<_Rep>)
	      if (_M_spec._M_needs(_Subseconds|_EpochUnits)
		    || _M_spec._M_prec_kind != _WP_none
		    || _M_spec._M_prec_value > 0)
		{
		  constexpr const _CharT* __fs = _GLIBCXX_WIDEN("#02.5Lf");
		  basic_format_parse_context<_CharT> __npc(__fs);
		  formatter<_Rep, _CharT> __fmtter;
		  __fmtter.parse(__npc);
		}
	  return __res;
	}

      // Return the formatting locale.
      template<typename _FormatContext>
	std::locale
	_M_locale(_FormatContext& __fc) const
	{
	  if (!_M_spec._M_localized)
	    return std::locale::classic();
	  else
	    return __fc.locale();
	}

      // Format duration for empty chrono-specs, e.g. "{}" (C++20 [time.format] p6).
      template<typename _Rep, typename _Period, typename _FormatContext>
	typename _FormatContext::iterator
	_M_format_to_ostream(const chrono::duration<_Rep, _Period>& __d,
			     bool __is_neg,
			     _FormatContext& __fc) const
	{
	   basic_ostringstream<_CharT> __os;
	   __os.imbue(this->_M_locale(__fc));

	   if (__is_neg) [[unlikely]]
	     __os << this->_S_plus_minus[1];
	   __os << __d;

	  auto __str = std::move(__os).str();
	  return __format::__write_padded_as_spec(__str, __str.size(),
						  __fc, _M_spec);
	}

      template<typename _Rep1, typename _Period1,
	       typename _Rep2, typename _Period2,
	       typename _FormatContext>
	typename _FormatContext::iterator
	_M_format_units(_ChronoData<_CharT>& __cd,
			const chrono::duration<_Rep1, _Period1>& __ed,
			const chrono::duration<_Rep2, _Period2>& __ss,
			_FormatContext& __fc) const
	{
	  __format::_Str_sink<_CharT> __suffix_store;
	  constexpr auto _S_unit_suffix
	    = chrono::__detail::__units_suffix<_Period1, _CharT>();
	  if constexpr (!_S_unit_suffix.empty())
	    __cd._M_unit_suffix = _S_unit_suffix;
	  else if (_M_spec._M_needs(_ChronoParts::_UnitSuffix))
	    {
	      chrono::__detail::
		__fmt_units_suffix<_Period1, _CharT>(__suffix_store.out());
	      __cd._M_unit_suffix = __suffix_store.view();
	    }

	  const auto __prec = _M_spec._M_prec_kind != _WP_none
			    ? _M_spec._M_get_precision(__fc)
			    : _M_spec._M_prec;

	  using _ErasedContext = typename _ChronoData<_CharT>::_FormatContext;
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 4118. How should duration formatters format custom rep?
	  auto __ereps = +__ed.count();
	  if (!_M_spec._M_needs(_ChronoParts::_Subseconds))
	  {
	    auto __ssreps = 0u;
	    auto __args_store
	      = std::make_format_args<_ErasedContext>(__ereps, __ssreps, __prec);
	    __cd._M_ereps = __args_store;
	    return this->_M_format(__cd, __fc);
	  }

	  using _Attoseconds = _ChronoData<_CharT>::_Attoseconds;
	  auto __nss = _S_subseconds(__ss);
	  __cd._M_subseconds = chrono::duration_cast<_Attoseconds>(__nss);

	  auto __ssreps = __nss.count();
	  auto __args_store
	    = std::make_format_args<_ErasedContext>(__ereps, __ssreps, __prec);
	  __cd._M_ereps = __args_store;

	  return this->_M_format(__cd, __fc);
	}

      // pre: __cd._M_lseconds and __cd._M_eseconds are set.
      template<typename _Rep1, typename _Period1, typename _FormatContext>
	typename _FormatContext::iterator
	_M_format_time_point(_ChronoData<_CharT>& __cd,
			     const chrono::duration<_Rep1, _Period1>& __ed,
			     _FormatContext& __fc) const
	{
	  auto __parts = _M_spec._M_needed - _ChronoParts::_TotalSeconds;
	  if ((__parts & _ChronoParts::_DateTime) != 0)
	    __cd._M_fill_date_time(__cd._M_lseconds, __parts);
	  return _M_format_units(__cd, __ed, __ed - __cd._M_eseconds, __fc);
	}
    };

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  template<typename _CharT>
    struct __formatter_chrono_info
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      { return _M_f._M_parse(__pc, _ChronoParts(), {}); }

      template<typename _Info, typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const _Info& __i,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  // n.b. only acceptable chrono-spec for info is one containing
	  // only whitespaces and %%, that do not depend on formatted object.
	  if (!_M_f._M_spec._M_chrono_specs.empty()) [[unlikely]]
	    return _M_f._M_format(_ChronoData<_CharT>{}, __fc);

	  const size_t __padwidth = _M_f._M_spec._M_get_width(__fc);
	  if (__padwidth == 0)
	    return _M_format_to(__fc.out(), __i);

	  _Padding_sink<_Out, _CharT> __sink(__fc.out(), __padwidth);
	  _M_format_to(__sink.out(), __i);
	  return __sink._M_finish(_M_f._M_spec._M_align, _M_f._M_spec._M_fill);
	}

    private:
       template<typename _Out>
	 _Out
	 _M_format_to(_Out __out, const chrono::sys_info& __si) const
	 {
	   using _FmtStr = _Runtime_format_string<_CharT>;
	   // n.b. only decimal separator is locale dependent for specifiers
	   // used below, as sys_info uses seconds and minutes duration, the
	   // output is locale-independent.
	   constexpr auto* __fs
	     = _GLIBCXX_WIDEN("[{0:%F %T},{1:%F %T},{2:%T},{3:%Q%q},{0:%Z}]");
	   const chrono::local_seconds __lb(__si.begin.time_since_epoch());
	   return std::format_to(std::move(__out), _FmtStr(__fs),
				 chrono::local_time_format(__lb, &__si.abbrev),
				 __si.end, __si.offset, __si.save);
	 }

       template<typename _Out>
	 _Out
	 _M_format_to(_Out __out, const chrono::local_info& __li) const
	 {
	   *__out = _Separators<_CharT>::_S_squares()[0];
	   ++__out;
	   if (__li.result == chrono::local_info::unique)
	     __out = _M_format_to(std::move(__out), __li.first);
	   else
	    {
	      basic_string_view<_CharT> __sv;
	      if (__li.result == chrono::local_info::nonexistent)
		__sv =_GLIBCXX_WIDEN("nonexistent");
	      else
		__sv = _GLIBCXX_WIDEN("ambiguous");
	      __out = __format::__write(std::move(__out), __sv);

	      __sv = _GLIBCXX_WIDEN(" local time between ");
	      __out = __format::__write(std::move(__out), __sv);
	      __out = _M_format_to(std::move(__out), __li.first);

	      __sv = _GLIBCXX_WIDEN(" and ");
	      __out = __format::__write(std::move(__out), __sv);
	      __out = _M_format_to(std::move(__out), __li.second);
	    }
	  *__out = _Separators<_CharT>::_S_squares()[1];
	  ++__out;
	  return std::move(__out);
	}

      __formatter_chrono<_CharT> _M_f;
    };
#endif

} // namespace __format
/// @endcond

  template<typename _Rep, typename _Period, typename _CharT>
    requires __format::__formattable_impl<_Rep, _CharT>
    struct formatter<chrono::duration<_Rep, _Period>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Duration>(__pc, _EpochTime, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::duration<_Rep, _Period>& __d,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  if constexpr (numeric_limits<_Rep>::is_signed)
	    if (__d < __d.zero()) [[unlikely]]
	      {
		if constexpr (is_integral_v<_Rep>)
		  {
		    // -d is undefined for the most negative integer.
		    // Convert duration to corresponding unsigned rep.
		    using _URep = make_unsigned_t<_Rep>;
		    auto __ucnt = -static_cast<_URep>(__d.count());
		    auto __ud = chrono::duration<_URep, _Period>(__ucnt);
		    return _M_format(__ud, true, __fc);
		  }
		else
		  return _M_format(-__d, true, __fc);
	      }
	  return _M_format(__d, false, __fc);
	}

    private:
      using _Duration = chrono::duration<_Rep, _Period>;

      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using enum __format::_ChronoParts;
	  auto __res = __format::__formatter_duration<_CharT>::
			 template _S_spec_for<_Duration>(_None);
	  __res._M_localized = !is_integral_v<_Rep>;
	  // n.b. for integral format output is the same as ostream output
	  if constexpr (is_integral_v<_Rep>)
	    {
	      __res._M_needed = _EpochUnits|_UnitSuffix;
	      __res._M_chrono_specs = _GLIBCXX_WIDEN("%Q%q");
	    }
	  return __res;
	}();

      template<typename _Rep2, typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	_M_format(const chrono::duration<_Rep2, _Period>& __d,
		  bool __is_neg,
		  basic_format_context<_Out, _CharT>& __fc) const
	{
	  using namespace chrono;
	  using enum __format::_ChronoParts;
	  if constexpr (!is_integral_v<_Rep>)
	    if (_M_f._M_spec._M_chrono_specs.empty())
	      return _M_f._M_format_to_ostream(__d, __is_neg, __fc);

	  __format::_ChronoData<_CharT> __cd;
	  __cd._M_is_neg = __is_neg;
	  auto __ts = chrono::floor<chrono::seconds>(__d);
	  __cd._M_eseconds = __ts;
	  if (_M_f._M_spec._M_needs(_HoursMinutesSeconds))
	    __cd._M_fill_time(__ts);
	  return _M_f._M_format_units(__cd, __d, __d - __ts, __fc);
	}

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Rep, typename _Period>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::duration<_Rep, _Period>>
      = is_arithmetic_v<_Rep>;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::day, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Day|_WeekdayIndex, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::day& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_day(__t, __defSpec._M_needed);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_needed = _Day;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_d();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::day> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::month, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Month, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::month& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_month = __t;
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Month;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_m();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::month> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::year, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Year, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::year& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_year = __t;
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_needed = _Year;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_y();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::year> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::weekday, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Weekday, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::weekday& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_weekday = __t;
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Weekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_w();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::weekday> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::weekday_indexed, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _IndexedWeekday, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::weekday_indexed& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_weekday(__t, __defSpec._M_needed);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _IndexedWeekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_wi();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::weekday_indexed> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::weekday_last, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Weekday, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::weekday_last& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_weekday = __t.weekday();
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Weekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_wl();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::weekday_last> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::month_day, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Month|_Day|_WeekdayIndex, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::month_day& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_month = __t.month();
	  __cd._M_fill_day(__t.day(), __defSpec._M_needed);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Month|_Day;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_md();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::month_day> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::month_day_last, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Month, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::month_day_last& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_month = __t.month();
	  return _M_f._M_format(__cd, __fc);
	}

    private:
       static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Month;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_ml();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::month_day_last> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::month_weekday, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Month|_IndexedWeekday, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::month_weekday& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_month = __t.month();
	  __cd._M_fill_weekday(__t.weekday_indexed(), __defSpec._M_needed);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Month|_IndexedWeekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_mwi();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::month_weekday> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::month_weekday_last, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Month|_Weekday, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::month_weekday_last& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_month = __t.month();
	  __cd._M_weekday = __t.weekday_last().weekday();
	  return _M_f._M_format(__cd, __fc);
	}

    private:
       static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Month|_Weekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_mwl();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::month_weekday_last> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::year_month, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Year|_Month, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::year_month& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_year_month(__t, __defSpec._M_needed);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
       static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Year|_Month;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_ym();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::year_month> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::year_month_day, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Date, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::year_month_day& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  auto __parts = _M_f._M_spec._M_needed;
	  __parts = __cd._M_fill_year_month(__t, __parts);
	  __parts = __cd._M_fill_day(__t.day(), __parts);
	  if (__parts == 0)
	    return _M_f._M_format(__cd, __fc);

	  __cd._M_fill_ldays(chrono::local_days(__t), __parts);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_needed = _YearMonthDay;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_f();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::year_month_day> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::year_month_day_last, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Date, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::year_month_day_last& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using enum __format::_ChronoParts;

	  __format::_ChronoData<_CharT> __cd{};
	  auto __parts = _M_f._M_spec._M_needed;
	  __parts = __cd._M_fill_year_month(__t, __parts);
	  if (_M_f._M_spec._M_needs(_Day|_WeekdayIndex))
	    __parts = __cd._M_fill_day(__t.day(), __parts);
	  if (__parts == 0)
	    return _M_f._M_format(__cd, __fc);

	  __cd._M_fill_ldays(chrono::local_days(__t), __parts);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
	static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Year|_Month;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_yml();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::year_month_day_last> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::year_month_weekday, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Date, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::year_month_weekday& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  auto __parts = _M_f._M_spec._M_needed;
	  __parts = __cd._M_fill_year_month(__t, __parts);
	  __parts = __cd._M_fill_weekday(__t.weekday_indexed(), __parts);
	  if (__t.index() == 0) [[unlikely]]
            // n.b. day cannot be negative, so any 0th weekday uses
	    // value-initialized (0) day of month
            __parts -= __format::_ChronoParts::_Day;
	  if (__parts == 0)
	    return _M_f._M_format(__cd, __fc);

	  chrono::local_days __ld(__t);
	  __parts = __cd._M_fill_ldays(__ld, __parts);
	  if (__parts == 0)
	    return _M_f._M_format(__cd, __fc);

	  auto __dom = __ld - chrono::local_days(__t.year()/__t.month()/0);
	  // n.b. weekday index is supplied by input, do not override it
	  __cd._M_day = chrono::day(__dom.count());
	  return _M_f._M_format(__cd, __fc);
	}

    private:
	static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Year|_Month|_IndexedWeekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_ymwi();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::year_month_weekday> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::year_month_weekday_last, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f._M_parse(__pc, _Date, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::year_month_weekday_last& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  auto __parts = _M_f._M_spec._M_needed;
	  __parts = __cd._M_fill_year_month(__t, __parts);
	  __cd._M_weekday = __t.weekday_last().weekday();
	  __parts -= __format::_ChronoParts::_Weekday;
	  if (__parts == 0)
	    return _M_f._M_format(__cd, __fc);

	  chrono::local_days __ld(__t);
	  __parts = __cd._M_fill_ldays(__ld, __parts);
	  if (__parts == 0)
	    return _M_f._M_format(__cd, __fc);

	  auto __dom = __ld - chrono::local_days(__t.year()/__t.month()/0);
	  __cd._M_fill_day(chrono::day(__dom.count()), __parts);
	  return _M_f._M_format(__cd, __fc);
	}

    private:
	static constexpr __format::_ChronoSpec<_CharT> __defSpec = []
	{
	  using __format::_ChronoFormats;
	  using enum __format::_ChronoParts;

	  __format::_ChronoSpec<_CharT> __res{};
	  __res._M_debug = true;
	  __res._M_localized = true;
	  __res._M_locale_specific = true;
	  __res._M_needed = _Year|_Month|_Weekday;
	  __res._M_chrono_specs = _ChronoFormats<_CharT>::_S_ymwl();
	  return __res;
	}();

      __format::__formatter_chrono<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::year_month_weekday_last> = true;
#endif

  template<typename _Rep, typename _Period, __format::__char _CharT>
    struct formatter<chrono::hh_mm_ss<chrono::duration<_Rep, _Period>>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Precision>(__pc, _Time, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::hh_mm_ss<chrono::duration<_Rep, _Period>>& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using enum __format::_ChronoParts;

	  __format::_ChronoData<_CharT> __cd;
	  __cd._M_is_neg = __t.is_negative();
	  __cd._M_hours = __t.hours();
	  __cd._M_minutes = __t.minutes();
	  __cd._M_seconds = __t.seconds();

	  _Precision __d(0);
	  // n.b. computing total duration or total seconds may overflow,
	  // do not compute them if not requested.
	  if (_M_f._M_spec._M_needs(_EpochUnits))
	    __d = __t.to_duration();
	  if (_M_f._M_spec._M_needs(_TotalSeconds))
	    __cd._M_eseconds
	      = __cd._M_hours + __cd._M_minutes + __cd._M_seconds;
	  return _M_f._M_format_units(__cd, __d, __t.subseconds(), __fc);
	}

    private:
      using _Precision
	= typename chrono::hh_mm_ss<chrono::duration<_Rep, _Period>>::precision;
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::
	   template _S_spec_for<_Precision>(__format::_ChronoParts::_Time);

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::hh_mm_ss<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  template<__format::__char _CharT>
    struct formatter<chrono::sys_info, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      { return _M_f.parse(__pc); }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::sys_info& __i,
	       basic_format_context<_Out, _CharT>& __fc) const
	{ return _M_f.format(__i, __fc); }

    private:
      __format::__formatter_chrono_info<_CharT> _M_f;
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::sys_info> = true;
#endif

  template<__format::__char _CharT>
    struct formatter<chrono::local_info, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      { return _M_f.parse(__pc); }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::local_info& __i,
	       basic_format_context<_Out, _CharT>& __fc) const
	{ return _M_f.format(__i, __fc); }

    private:
      __format::__formatter_chrono_info<_CharT> _M_f;
    };

#if __glibcxx_print >= 202406L
  template<>
    inline constexpr bool
    enable_nonlocking_formatter_optimization<chrono::local_info> = true;
#endif
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::sys_time<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	auto __res
	  = _M_f.template _M_parse<_Duration>(__pc, _ZonedDateTime, __defSpec);
	if constexpr (__defSpec._M_chrono_specs.empty())
	  if (_M_f._M_spec._M_chrono_specs.empty())
	    __format::__invalid_chrono_spec(); // chrono-specs can't be empty
	return __res;
     }

     template<typename _Out>
       typename basic_format_context<_Out, _CharT>::iterator
       format(const chrono::sys_time<_Duration>& __t,
	      basic_format_context<_Out, _CharT>& __fc) const
       {
	 __format::_ChronoData<_CharT> __cd{};
	 __cd._M_fill_utc_zone();

	 _Duration __ed = __t.time_since_epoch();
	 __cd._M_eseconds = chrono::floor<chrono::seconds>(__ed);
	 __cd._M_lseconds = chrono::local_seconds(__cd._M_eseconds);
	 return _M_f._M_format_time_point(__cd, __ed, __fc);
       }

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::template _S_spec_for_tp<_Duration>();

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::sys_time<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::utc_time<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Duration>(__pc, _ZonedDateTime, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::utc_time<_Duration>& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using __format::_ChronoParts;
	  using namespace chrono;
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_utc_zone();

	  _Duration __ed = __t.time_since_epoch();
	  __cd._M_eseconds = chrono::floor<seconds>(__ed);
	  // Adjust by removing leap seconds to get equivalent sys_time.
	  // We can't just use clock_cast because we want to know if the time
	  // falls within a leap second insertion, and format seconds as "60".
	  const auto __li = chrono::get_leap_second_info(__t);
	  __cd._M_lseconds = local_seconds(__cd._M_eseconds - __li.elapsed);
	  auto __parts = _M_f._M_spec._M_needed - _ChronoParts::_TotalSeconds;
	  if ((__parts & _ChronoParts::_DateTime) != 0)
	  {
	    __cd._M_fill_date_time(__cd._M_lseconds, __parts);
	    __cd._M_seconds += seconds(__li.is_leap_second);
	  }
	  return _M_f._M_format_units(__cd, __ed, __ed - __cd._M_eseconds, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::
	  template _S_spec_for<_Duration>(__format::_ChronoParts::_DateTime);

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::utc_time<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::tai_time<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Duration>(__pc, _ZonedDateTime, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::tai_time<_Duration>& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using namespace chrono;
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_zone("TAI", L"TAI");

	  _Duration __ed = __t.time_since_epoch();
	  __cd._M_eseconds = chrono::floor<seconds>(__ed);
	  // Offset is 1970y/January/1 - 1958y/January/1
	  constexpr chrono::days __tai_offset = chrono::days(4383);
	  __cd._M_lseconds = local_seconds(__cd._M_eseconds - __tai_offset);
	  return _M_f._M_format_time_point(__cd, __ed, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::
	  template _S_spec_for<_Duration>(__format::_ChronoParts::_DateTime);

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::tai_time<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::gps_time<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Duration>(__pc, _ZonedDateTime, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::gps_time<_Duration>& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using namespace chrono;
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_zone("GPS", L"GPS");

	  _Duration __ed = __t.time_since_epoch();
	  __cd._M_eseconds = chrono::floor<seconds>(__ed);
	  // Offset is 1980y/January/Sunday[1] - 1970y/January/1
	  constexpr chrono::days __gps_offset = chrono::days(3657);
	  __cd._M_lseconds = local_seconds(__cd._M_eseconds + __gps_offset);
	  return _M_f._M_format_time_point(__cd, __ed, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::
	  template _S_spec_for<_Duration>(__format::_ChronoParts::_DateTime);

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::gps_time<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::file_time<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Duration>(__pc, _ZonedDateTime, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::file_time<_Duration>& __t,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using namespace chrono;
	  __format::_ChronoData<_CharT> __cd{};
	  __cd._M_fill_utc_zone();

	  _Duration __ed = __t.time_since_epoch();
	  __cd._M_eseconds = chrono::floor<seconds>(__ed);
	  auto __st = chrono::clock_cast<system_clock>(__t);
	  __cd._M_lseconds
	    = local_seconds(chrono::floor<seconds>(__st.time_since_epoch()));
	  return _M_f._M_format_time_point(__cd, __ed, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::
	  template _S_spec_for<_Duration>(__format::_ChronoParts::_DateTime);

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
     };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::file_time<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::local_time<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	auto __res
	  = _M_f.template _M_parse<_Duration>(__pc, _DateTime, __defSpec);
	if constexpr (__defSpec._M_chrono_specs.empty())
	  if (_M_f._M_spec._M_chrono_specs.empty())
	    __format::__invalid_chrono_spec(); // chrono-specs can't be empty
	return __res;
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::local_time<_Duration>& __lt,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  __format::_ChronoData<_CharT> __cd{};
	  _Duration __ed = __lt.time_since_epoch();
	  __cd._M_lseconds = chrono::floor<chrono::seconds>(__lt);
	  __cd._M_eseconds = __cd._M_lseconds.time_since_epoch();
	  return _M_f._M_format_time_point(__cd, __ed, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::template _S_spec_for_tp<_Duration>();

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<chrono::local_time<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

  template<typename _Duration, __format::__char _CharT>
    struct formatter<chrono::__detail::__local_time_fmt<_Duration>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using enum __format::_ChronoParts;
	return _M_f.template _M_parse<_Duration>(__pc, _ZonedDateTime, __defSpec);
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::__detail::__local_time_fmt<_Duration>& __zt,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using enum __format::_ChronoParts;
	  __format::_ChronoData<_CharT> __cd{};

	  if (_M_f._M_spec._M_needs(_ZoneOffset))
	    {
	      if (!__zt._M_offset_sec)
		std::__throw_format_error("format error: no timezone available for %z");
	      __cd._M_zone_offset = *__zt._M_offset_sec;
	    }

	  basic_string<_CharT> __zone_store;
	  if (_M_f._M_spec._M_needs(_ZoneAbbrev))
	    {
	      if (!__zt._M_abbrev)
		std::__throw_format_error("format error: no timezone available for %Z");

	      __cd._M_zone_cstr = __zt._M_abbrev->data();
	      if constexpr (is_same_v<_CharT, char>)
		__cd._M_zone_abbrev = *__zt._M_abbrev;
	      else
		{
		  // TODO: use resize_for_override
		  __zone_store.resize(__zt._M_abbrev->size());
		  auto& __ct = use_facet<ctype<_CharT>>(_M_f._M_locale(__fc));
		  __ct.widen(__zt._M_abbrev->data(),
			     __zt._M_abbrev->data() + __zt._M_abbrev->size(),
			     __zone_store.data());
		  __cd._M_zone_abbrev = __zone_store;
		}
	    }

	  _Duration __ed = __zt._M_time.time_since_epoch();
	  __cd._M_lseconds = chrono::floor<chrono::seconds>(__zt._M_time);
	  __cd._M_eseconds = __cd._M_lseconds.time_since_epoch();
	  return _M_f._M_format_time_point(__cd, __ed, __fc);
	}

    private:
      static constexpr __format::_ChronoSpec<_CharT> __defSpec =
	__format::__formatter_duration<_CharT>::
	  template _S_spec_for<_Duration>(__format::_ChronoParts::_ZonedDateTime);

      __format::__formatter_duration<_CharT> _M_f{__defSpec};
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<
      chrono::__detail::__local_time_fmt<_Duration>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  template<typename _Duration, typename _TimeZonePtr, __format::__char _CharT>
    struct formatter<chrono::zoned_time<_Duration, _TimeZonePtr>, _CharT>
    : formatter<chrono::__detail::__local_time_fmt_for<_Duration>, _CharT>
    {
      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::zoned_time<_Duration, _TimeZonePtr>& __tp,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  using _Ltf = chrono::__detail::__local_time_fmt_for<_Duration>;
	  using _Base = formatter<_Ltf, _CharT>;
	  const chrono::sys_info __info = __tp.get_info();
	  const auto __lf = chrono::local_time_format(__tp.get_local_time(),
						      &__info.abbrev,
						      &__info.offset);
	  return _Base::format(__lf, __fc);
	}
    };

#if __glibcxx_print >= 202406L
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4400. enable_nonlocking_formatter_optimization for durations with custom rep
  template<typename _Duration>
    constexpr bool
    enable_nonlocking_formatter_optimization<
      chrono::zoned_time<_Duration, const chrono::time_zone*>>
      = enable_nonlocking_formatter_optimization<_Duration>;
#endif
#endif

namespace chrono
{
/// @addtogroup chrono
/// @{

/// @cond undocumented
namespace __detail
{
  template<typename _Duration = seconds>
    struct _Parser
    {
      static_assert(is_same_v<common_type_t<_Duration, seconds>, _Duration>);

      explicit
      _Parser(__format::_ChronoParts __need) : _M_need(__need) { }

      _Parser(_Parser&&) = delete;
      void operator=(_Parser&&) = delete;

      _Duration _M_time{}; // since midnight
      sys_days _M_sys_days{};
      year_month_day _M_ymd{};
      weekday _M_wd{};
      __format::_ChronoParts _M_need;
      unsigned _M_is_leap_second : 1 {};
      unsigned _M_reserved : 15 {};

      template<typename _CharT, typename _Traits, typename _Alloc>
	basic_istream<_CharT, _Traits>&
	operator()(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		   basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		   minutes* __offset = nullptr);

    private:
      // Read an unsigned integer from the stream and return it.
      // Extract no more than __n digits. Set failbit if an integer isn't read.
      template<typename _CharT, typename _Traits>
	static int_least32_t
	_S_read_unsigned(basic_istream<_CharT, _Traits>& __is,
			 ios_base::iostate& __err, int __n)
	{
	  int_least32_t __val = _S_try_read_digit(__is, __err);
	  if (__val == -1) [[unlikely]]
	    __err |= ios_base::failbit;
	  else
	    {
	      int __n1 = (std::min)(__n, 9);
	      // Cannot overflow __val unless we read more than 9 digits
	      for (int __i = 1; __i < __n1; ++__i)
		if (auto __dig = _S_try_read_digit(__is, __err); __dig != -1)
		  {
		    __val *= 10;
		    __val += __dig;
		  }

	      while (__n1++ < __n) [[unlikely]]
		if (auto __dig = _S_try_read_digit(__is, __err); __dig != -1)
		  {
		    if (__builtin_mul_overflow(__val, 10, &__val)
			  || __builtin_add_overflow(__val, __dig, &__val))
		      {
			__err |= ios_base::failbit;
			return -1;
		      }
		  }
	    }
	  return __val;
	}

      // Read an unsigned integer from the stream and return it.
      // Extract no more than __n digits. Set failbit if an integer isn't read.
      template<typename _CharT, typename _Traits>
	static int_least32_t
	_S_read_signed(basic_istream<_CharT, _Traits>& __is,
			 ios_base::iostate& __err, int __n)
	{
	  auto __sign = __is.peek();
	  if (__sign == '-' || __sign == '+')
	    (void) __is.get();
	  int_least32_t __val = _S_read_unsigned(__is, __err, __n);
	  if (__err & ios_base::failbit)
	    {
	      if (__sign == '-') [[unlikely]]
		__val *= -1;
	    }
	  return __val;
	}

      // Read a digit from the stream and return it, or return -1.
      // If no digit is read eofbit will be set (but not failbit).
      template<typename _CharT, typename _Traits>
	static int_least32_t
	_S_try_read_digit(basic_istream<_CharT, _Traits>& __is,
			  ios_base::iostate& __err)
	{
	  int_least32_t __val = -1;
	  auto __i = __is.peek();
	  if (!_Traits::eq_int_type(__i, _Traits::eof())) [[likely]]
	    {
	      _CharT __c = _Traits::to_char_type(__i);
	      if (_CharT('0') <= __c && __c <= _CharT('9')) [[likely]]
		{
		  (void) __is.get();
		  __val = __c - _CharT('0');
		}
	    }
	  else
	    __err |= ios_base::eofbit;
	  return __val;
	}

      // Read the specified character and return true.
      // If the character is not found, set failbit and return false.
      template<typename _CharT, typename _Traits>
	static bool
	_S_read_chr(basic_istream<_CharT, _Traits>& __is,
		    ios_base::iostate& __err, _CharT __c)
	{
	  auto __i = __is.peek();
	  if (_Traits::eq_int_type(__i, _Traits::eof()))
	    __err |= ios_base::eofbit;
	  else if (_Traits::to_char_type(__i) == __c) [[likely]]
	    {
	      (void) __is.get();
	      return true;
	    }
	  __err |= ios_base::failbit;
	  return false;
	}
    };

  template<typename _Duration>
    using _Parser_t = _Parser<common_type_t<_Duration, seconds>>;

  template<typename _Duration>
    consteval bool
    __use_floor()
    {
      if constexpr (_Duration::period::den == 1)
	{
	  switch (_Duration::period::num)
	  {
	    case minutes::period::num:
	    case hours::period::num:
	    case days::period::num:
	    case weeks::period::num:
	    case years::period::num:
	      return true;
	  }
	}
      return false;
    }

  // A "do the right thing" rounding function for duration and time_point
  // values extracted by from_stream. When treat_as_floating_point is true
  // we don't want to do anything, just a straightforward conversion.
  // When the destination type has a period of minutes, hours, days, weeks,
  // or years, we use chrono::floor to truncate towards negative infinity.
  // This ensures that an extracted timestamp such as 2024-09-05 13:00:00
  // will produce 2024-09-05 when rounded to days, rather than rounding up
  // to 2024-09-06 (a different day).
  // Otherwise, use chrono::round to get the nearest value representable
  // in the destination type.
  template<typename _ToDur, typename _Tp>
    constexpr auto
    __round(const _Tp& __t)
    {
      if constexpr (__is_duration_v<_Tp>)
	{
	  if constexpr (treat_as_floating_point_v<typename _Tp::rep>)
	    return chrono::duration_cast<_ToDur>(__t);
	  else if constexpr (__detail::__use_floor<_ToDur>())
	    return chrono::floor<_ToDur>(__t);
	  else
	    return chrono::round<_ToDur>(__t);
	}
      else
	{
	  static_assert(__is_time_point_v<_Tp>);
	  using _Tpt = time_point<typename _Tp::clock, _ToDur>;
	  return _Tpt(__detail::__round<_ToDur>(__t.time_since_epoch()));
	}
    }

} // namespace __detail
/// @endcond

  template<typename _CharT, typename _Traits, typename _Rep, typename _Period,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		duration<_Rep, _Period>& __d,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      auto __need = __format::_ChronoParts::_TimeOfDay;
      __detail::_Parser_t<duration<_Rep, _Period>> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	__d = __detail::__round<duration<_Rep, _Period>>(__p._M_time);
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const day& __d)
    {
      using _Ctx = __format::__format_context<_CharT>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:02d} is not a valid day");
      if (__d.ok())
	__s = __s.substr(0, 6);
      auto __u = (unsigned)__d;
      __os << std::vformat(__s, make_format_args<_Ctx>(__u));
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		day& __d,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      __detail::_Parser<> __p(__format::_ChronoParts::_Day);
      if (__p(__is, __fmt, __abbrev, __offset))
	__d = __p._M_ymd.day();
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const month& __m)
    {
      using _Ctx = __format::__format_context<_CharT>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:L%b}{} is not a valid month");
      if (__m.ok())
	__os << std::vformat(__os.getloc(), __s.substr(0, 6),
			     make_format_args<_Ctx>(__m));
      else
	{
	  auto __u = (unsigned)__m;
	  __os << std::vformat(__s.substr(6), make_format_args<_Ctx>(__u));
	}
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		month& __m,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      __detail::_Parser<> __p(__format::_ChronoParts::_Month);
      if (__p(__is, __fmt, __abbrev, __offset))
	__m = __p._M_ymd.month();
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const year& __y)
    {
      using _Ctx = __format::__format_context<_CharT>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("-{:04d} is not a valid year");
      if (__y.ok())
	__s = __s.substr(0, 7);
      int __i = (int)__y;
      if (__i >= 0) [[likely]]
	__s.remove_prefix(1);
      else
	__i = -__i;
      __os << std::vformat(__s, make_format_args<_Ctx>(__i));
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		year& __y,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      __detail::_Parser<> __p(__format::_ChronoParts::_Year);
      if (__p(__is, __fmt, __abbrev, __offset))
	__y = __p._M_ymd.year();
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const weekday& __wd)
    {
      using _Ctx = __format::__format_context<_CharT>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:L%a}{} is not a valid weekday");
      if (__wd.ok())
	__os << std::vformat(__os.getloc(), __s.substr(0, 6),
			     make_format_args<_Ctx>(__wd));
      else
	{
	  auto __c = __wd.c_encoding();
	  __os << std::vformat(__s.substr(6), make_format_args<_Ctx>(__c));
	}
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		weekday& __wd,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      __detail::_Parser<> __p(__format::_ChronoParts::_Weekday);
      if (__p(__is, __fmt, __abbrev, __offset))
	__wd = __p._M_wd;
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const weekday_indexed& __wdi)
    {
      // The standard says to format wdi.weekday() and wdi.index() using
      // either "{:L}[{}]" or "{:L}[{} is not a valid index]". The {:L} spec
      // means to format the weekday using ostringstream, so just do that.
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __wdi.weekday();
      const auto __i = __wdi.index();
      basic_string_view<_CharT> __s
	= _GLIBCXX_WIDEN("[ is not a valid index]");
      __os2 << __s[0];
      __os2 << std::format(_GLIBCXX_WIDEN("{}"), __i);
      if (__i >= 1 && __i <= 5)
	__os2 << __s.back();
      else
	__os2 << __s.substr(1);
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const weekday_last& __wdl)
    {
      // As above, just write straight to a stringstream, as if by "{:L}[last]"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __wdl.weekday() << _GLIBCXX_WIDEN("[last]");
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const month_day& __md)
    {
      // As above, just write straight to a stringstream, as if by "{:L}/{}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __md.month();
      if constexpr (is_same_v<_CharT, char>)
	__os2 << '/';
      else
	__os2 << L'/';
      __os2 << __md.day();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		month_day& __md,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_Month | _ChronoParts::_Day;
      __detail::_Parser<> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	__md = month_day(__p._M_ymd.month(), __p._M_ymd.day());
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const month_day_last& __mdl)
    {
      // As above, just write straight to a stringstream, as if by "{:L}/last"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __mdl.month() << _GLIBCXX_WIDEN("/last");
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const month_weekday& __mwd)
    {
      // As above, just write straight to a stringstream, as if by "{:L}/{:L}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __mwd.month();
      if constexpr (is_same_v<_CharT, char>)
	__os2 << '/';
      else
	__os2 << L'/';
      __os2 << __mwd.weekday_indexed();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const month_weekday_last& __mwdl)
    {
      // As above, just write straight to a stringstream, as if by "{:L}/{:L}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __mwdl.month();
      if constexpr (is_same_v<_CharT, char>)
	__os2 << '/';
      else
	__os2 << L'/';
      __os2 << __mwdl.weekday_last();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const year_month& __ym)
    {
      // As above, just write straight to a stringstream, as if by "{}/{:L}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __ym.year();
      if constexpr (is_same_v<_CharT, char>)
	__os2 << '/';
      else
	__os2 << L'/';
      __os2 << __ym.month();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		year_month& __ym,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month;
      __detail::_Parser<> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	__ym = year_month(__p._M_ymd.year(), __p._M_ymd.month());
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const year_month_day& __ymd)
    {
      using _Ctx = __format::__format_context<_CharT>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:%F} is not a valid date");
      __os << std::vformat(__ymd.ok() ? __s.substr(0, 5) : __s,
			   make_format_args<_Ctx>(__ymd));
      return __os;
    }

  template<typename _CharT, typename _Traits,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		year_month_day& __ymd,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month
		    | _ChronoParts::_Day;
      __detail::_Parser<> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	__ymd = __p._M_ymd;
      return __is;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const year_month_day_last& __ymdl)
    {
      // As above, just write straight to a stringstream, as if by "{}/{:L}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __ymdl.year();
      if constexpr (is_same_v<_CharT, char>)
	__os2 << '/';
      else
	__os2 << L'/';
      __os2 << __ymdl.month_day_last();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const year_month_weekday& __ymwd)
    {
      // As above, just write straight to a stringstream, as if by
      // "{}/{:L}/{:L}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      _CharT __slash;
      if constexpr (is_same_v<_CharT, char>)
	__slash = '/';
      else
	__slash = L'/';
      __os2 << __ymwd.year() << __slash << __ymwd.month() << __slash
	    << __ymwd.weekday_indexed();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const year_month_weekday_last& __ymwdl)
    {
      // As above, just write straight to a stringstream, as if by
      // "{}/{:L}/{:L}"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      _CharT __slash;
      if constexpr (is_same_v<_CharT, char>)
	__slash = '/';
      else
	__slash = L'/';
      __os2 << __ymwdl.year() << __slash << __ymwdl.month() << __slash
	    << __ymwdl.weekday_last();
      __os << __os2.view();
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const hh_mm_ss<_Duration>& __hms)
    {
      return __os << format(__os.getloc(), _GLIBCXX_WIDEN("{:L%T}"), __hms);
    }

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  /// Writes a sys_info object to an ostream in an unspecified format.
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const sys_info& __i)
    {
      return __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{}"), __i);
    }

  /// Writes a local_info object to an ostream in an unspecified format.
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const local_info& __li)
    {
      __os << __format::_Separators<_CharT>::_S_squares()[0];
      if (__li.result == local_info::unique)
	__os << __li.first;
      else
	{
	  if (__li.result == local_info::nonexistent)
	    __os << _GLIBCXX_WIDEN("nonexistent");
	  else
	    __os << _GLIBCXX_WIDEN("ambiguous");
	  __os << _GLIBCXX_WIDEN(" local time between ") << __li.first;
	  __os << _GLIBCXX_WIDEN(" and ") << __li.second;
	}
      __os << __format::_Separators<_CharT>::_S_squares()[1];
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _TimeZonePtr>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const zoned_time<_Duration, _TimeZonePtr>& __t)
    {
      __os << format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T %Z}"), __t);
      return __os;
    }
#endif

  template<typename _CharT, typename _Traits, typename _Duration>
    requires (!treat_as_floating_point_v<typename _Duration::rep>)
      && ratio_less_v<typename _Duration::period, days::period>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const sys_time<_Duration>& __tp)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __tp);
      return __os;
    }

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const sys_days& __dp)
    {
      __os << year_month_day{__dp};
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _Alloc = allocator<_CharT>>
    basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		sys_time<_Duration>& __tp,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      minutes __off{};
      if (!__offset)
	__offset = &__off;
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month
		    | _ChronoParts::_Day | _ChronoParts::_TimeOfDay;
      __detail::_Parser_t<_Duration> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	{
	  if (__p._M_is_leap_second)
	    __is.setstate(ios_base::failbit);
	  else
	    {
	      auto __st = __p._M_sys_days + __p._M_time - *__offset;
	      __tp = __detail::__round<_Duration>(__st);
	    }
	}
      return __is;
    }

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const utc_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		utc_time<_Duration>& __tp,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      minutes __off{};
      if (!__offset)
	__offset = &__off;
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month
		    | _ChronoParts::_Day | _ChronoParts::_TimeOfDay;
      __detail::_Parser_t<_Duration> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	{
	  // Converting to utc_time before adding _M_time is necessary for
	  // "23:59:60" to correctly produce a time within a leap second.
	  auto __ut = utc_clock::from_sys(__p._M_sys_days) + __p._M_time
			- *__offset;
	  __tp = __detail::__round<_Duration>(__ut);
	}
      return __is;
    }

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const tai_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		tai_time<_Duration>& __tp,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      minutes __off{};
      if (!__offset)
	__offset = &__off;
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month
		    | _ChronoParts::_Day | _ChronoParts::_TimeOfDay;
      __detail::_Parser_t<_Duration> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	{
	  if (__p._M_is_leap_second)
	    __is.setstate(ios_base::failbit);
	  else
	    {
	      constexpr sys_days __epoch(-days(4383)); // 1958y/1/1
	      auto __d = __p._M_sys_days - __epoch + __p._M_time - *__offset;
	      tai_time<common_type_t<_Duration, seconds>> __tt(__d);
	      __tp = __detail::__round<_Duration>(__tt);
	    }
	}
      return __is;
    }

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const gps_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		gps_time<_Duration>& __tp,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      minutes __off{};
      if (!__offset)
	__offset = &__off;
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_YearMonthDay | _ChronoParts::_TimeOfDay;
      __detail::_Parser_t<_Duration> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	{
	  if (__p._M_is_leap_second)
	    __is.setstate(ios_base::failbit);
	  else
	    {
	      constexpr sys_days __epoch(days(3657)); // 1980y/1/Sunday[1]
	      auto __d = __p._M_sys_days - __epoch + __p._M_time - *__offset;
	      gps_time<common_type_t<_Duration, seconds>> __gt(__d);
	      __tp = __detail::__round<_Duration>(__gt);
	    }
	}
      return __is;
    }

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const file_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _Alloc = allocator<_CharT>>
    inline basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		file_time<_Duration>& __tp,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      sys_time<_Duration> __st;
      if (chrono::from_stream(__is, __fmt, __st, __abbrev, __offset))
	__tp = __detail::__round<_Duration>(file_clock::from_sys(__st));
      return __is;
    }

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const local_time<_Duration>& __lt)
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 4257. Stream insertion for chrono::local_time should be constrained
    requires requires(const sys_time<_Duration>& __st) { __os << __st; }
    {
      __os << sys_time<_Duration>{__lt.time_since_epoch()};
      return __os;
    }

  template<typename _CharT, typename _Traits, typename _Duration,
	   typename _Alloc = allocator<_CharT>>
    basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		local_time<_Duration>& __tp,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
      using __format::_ChronoParts;
      auto __need = _ChronoParts::_YearMonthDay | _ChronoParts::_TimeOfDay;
      __detail::_Parser_t<_Duration> __p(__need);
      if (__p(__is, __fmt, __abbrev, __offset))
	{
	  days __d = __p._M_sys_days.time_since_epoch();
	  auto __t = local_days(__d) + __p._M_time; // ignore offset
	  __tp = __detail::__round<_Duration>(__t);
	}
      return __is;
    }

  // [time.parse] parsing

namespace __detail
{
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 3956. chrono::parse uses from_stream as a customization point
  void from_stream() = delete;

  template<typename _Parsable, typename _CharT,
	   typename _Traits = std::char_traits<_CharT>,
	   typename... _OptArgs>
    concept __parsable = requires (basic_istream<_CharT, _Traits>& __is,
				   const _CharT* __fmt, _Parsable& __tp,
				   _OptArgs*... __args)
    { from_stream(__is, __fmt, __tp, __args...); };

  template<typename _Parsable, typename _CharT,
	   typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT>>
    struct _Parse
    {
    private:
      using __string_type = basic_string<_CharT, _Traits, _Alloc>;

    public:
      _Parse(const _CharT* __fmt, _Parsable& __tp,
	     basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
	     minutes* __offset = nullptr)
      : _M_fmt(__fmt), _M_tp(std::__addressof(__tp)),
	_M_abbrev(__abbrev), _M_offset(__offset)
      { }

      _Parse(_Parse&&) = delete;
      _Parse& operator=(_Parse&&) = delete;

    private:
      using __stream_type = basic_istream<_CharT, _Traits>;

      const _CharT* const  _M_fmt;
      _Parsable* const     _M_tp;
      __string_type* const _M_abbrev;
      minutes* const       _M_offset;

      friend __stream_type&
      operator>>(__stream_type& __is, _Parse&& __p)
      {
	if (__p._M_offset)
	  from_stream(__is, __p._M_fmt, *__p._M_tp, __p._M_abbrev,
		      __p._M_offset);
	else if (__p._M_abbrev)
	  from_stream(__is, __p._M_fmt, *__p._M_tp, __p._M_abbrev);
	else
	  from_stream(__is, __p._M_fmt, *__p._M_tp);
	return __is;
      }

      friend void operator>>(__stream_type&, _Parse&) = delete;
      friend void operator>>(__stream_type&, const _Parse&) = delete;
    };
} // namespace __detail

  template<typename _CharT, __detail::__parsable<_CharT> _Parsable>
    [[nodiscard, __gnu__::__access__(__read_only__, 1)]]
    inline auto
    parse(const _CharT* __fmt, _Parsable& __tp)
    { return __detail::_Parse<_Parsable, _CharT>(__fmt, __tp); }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   __detail::__parsable<_CharT, _Traits> _Parsable>
    [[nodiscard]]
    inline auto
    parse(const basic_string<_CharT, _Traits, _Alloc>& __fmt, _Parsable& __tp)
    {
      return __detail::_Parse<_Parsable, _CharT, _Traits>(__fmt.c_str(), __tp);
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   typename _StrT = basic_string<_CharT, _Traits, _Alloc>,
	   __detail::__parsable<_CharT, _Traits, _StrT> _Parsable>
    [[nodiscard, __gnu__::__access__(__read_only__, 1)]]
    inline auto
    parse(const _CharT* __fmt, _Parsable& __tp,
	  basic_string<_CharT, _Traits, _Alloc>& __abbrev)
    {
      auto __pa = std::__addressof(__abbrev);
      return __detail::_Parse<_Parsable, _CharT, _Traits, _Alloc>(__fmt, __tp,
								  __pa);
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   typename _StrT = basic_string<_CharT, _Traits, _Alloc>,
	   __detail::__parsable<_CharT, _Traits, _StrT> _Parsable>
    [[nodiscard]]
    inline auto
    parse(const basic_string<_CharT, _Traits, _Alloc>& __fmt, _Parsable& __tp,
	  basic_string<_CharT, _Traits, _Alloc>& __abbrev)
    {
      auto __pa = std::__addressof(__abbrev);
      return __detail::_Parse<_Parsable, _CharT, _Traits, _Alloc>(__fmt.c_str(),
								  __tp, __pa);
    }

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _StrT = basic_string<_CharT, _Traits>,
	   __detail::__parsable<_CharT, _Traits, _StrT, minutes> _Parsable>
    [[nodiscard, __gnu__::__access__(__read_only__, 1)]]
    inline auto
    parse(const _CharT* __fmt, _Parsable& __tp, minutes& __offset)
    {
      return __detail::_Parse<_Parsable, _CharT>(__fmt, __tp, nullptr,
						 &__offset);
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   typename _StrT = basic_string<_CharT, _Traits>,
	   __detail::__parsable<_CharT, _Traits, _StrT, minutes> _Parsable>
    [[nodiscard]]
    inline auto
    parse(const basic_string<_CharT, _Traits, _Alloc>& __fmt, _Parsable& __tp,
	  minutes& __offset)
    {
      return __detail::_Parse<_Parsable, _CharT, _Traits, _Alloc>(__fmt.c_str(),
								  __tp, nullptr,
								  &__offset);
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   typename _StrT = basic_string<_CharT, _Traits, _Alloc>,
	   __detail::__parsable<_CharT, _Traits, _StrT, minutes> _Parsable>
    [[nodiscard, __gnu__::__access__(__read_only__, 1)]]
    inline auto
    parse(const _CharT* __fmt, _Parsable& __tp,
	  basic_string<_CharT, _Traits, _Alloc>& __abbrev, minutes& __offset)
    {
      auto __pa = std::__addressof(__abbrev);
      return __detail::_Parse<_Parsable, _CharT, _Traits, _Alloc>(__fmt, __tp,
								  __pa,
								  &__offset);
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   typename _StrT = basic_string<_CharT, _Traits, _Alloc>,
	   __detail::__parsable<_CharT, _Traits, _StrT, minutes> _Parsable>
    [[nodiscard]]
    inline auto
    parse(const basic_string<_CharT, _Traits, _Alloc>& __fmt, _Parsable& __tp,
	  basic_string<_CharT, _Traits, _Alloc>& __abbrev, minutes& __offset)
    {
      auto __pa = std::__addressof(__abbrev);
      return __detail::_Parse<_Parsable, _CharT, _Traits, _Alloc>(__fmt.c_str(),
								  __tp, __pa,
								  &__offset);
    }

  /// @cond undocumented
  template<typename _Duration>
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_istream<_CharT, _Traits>&
    __detail::_Parser<_Duration>::
    operator()(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
	       basic_string<_CharT, _Traits, _Alloc>* __abbrev,
	       minutes* __offset)
    {
      using sentry = typename basic_istream<_CharT, _Traits>::sentry;
      ios_base::iostate __err = ios_base::goodbit;
      if (sentry __cerb(__is, true); __cerb)
	{
	  locale __loc = __is.getloc();
	  auto& __tmget = std::use_facet<std::time_get<_CharT>>(__loc);
	  auto& __tmpunct = std::use_facet<std::__timepunct<_CharT>>(__loc);

	  // RAII type to save and restore stream state.
	  struct _Stream_state
	  {
	    explicit
	    _Stream_state(basic_istream<_CharT, _Traits>& __i)
	    : _M_is(__i),
	      _M_flags(__i.flags(ios_base::skipws | ios_base::dec)),
	      _M_w(__i.width(0))
	    { }

	    ~_Stream_state()
	    {
	      _M_is.flags(_M_flags);
	      _M_is.width(_M_w);
	    }

	    _Stream_state(_Stream_state&&) = delete;

	    basic_istream<_CharT, _Traits>& _M_is;
	    ios_base::fmtflags _M_flags;
	    streamsize _M_w;
	  };

	  auto __is_failed = [](ios_base::iostate __e) {
	    return static_cast<bool>(__e & ios_base::failbit);
	  };

	  // Read an unsigned integer from the stream and return it.
	  // Extract no more than __n digits. Set __err on error.
	  auto __read_unsigned = [&] (int __n) {
	    return _S_read_unsigned(__is, __err, __n);
	  };

	  // Read a signed integer from the stream and return it.
	  // Extract no more than __n digits. Set __err on error.
	  auto __read_signed = [&] (int __n) {
	    return _S_read_signed(__is, __err, __n);
	  };

	  // Read an expected character from the stream.
	  auto __read_chr = [&__is, &__err] (_CharT __c) {
	    return _S_read_chr(__is, __err, __c);
	  };

	  using __format::_ChronoParts;
	  _ChronoParts __parts{};

	  const year __bad_y = --year::min(); // SHRT_MIN
	  const month __bad_mon(255);
	  const day __bad_day(255);
	  const weekday __bad_wday(255);
	  const hours __bad_h(-1);
	  const minutes __bad_min(-9999);
	  const seconds __bad_sec(-1);

	  year __y = __bad_y, __yy = __bad_y;         // %Y, %yy
	  year __iso_y = __bad_y, __iso_yy = __bad_y; // %G, %g
	  month __m = __bad_mon;                      // %m
	  day __d = __bad_day;                        // %d
	  weekday __wday = __bad_wday;                // %a %A %u %w
	  hours __h = __bad_h, __h12 = __bad_h;       // %H, %I
	  minutes __min = __bad_min;                  // %M
	  _Duration __s = __bad_sec;                  // %S
	  int __ampm = 0;                             // %p
	  int __iso_wk = -1, __sunday_wk = -1, __monday_wk = -1; // %V, %U, %W
	  int __century = -1;                         // %C
	  int __dayofyear = -1;                       // %j (for non-duration)

	  minutes __tz_offset = __bad_min;
	  basic_string<_CharT, _Traits> __tz_abbr;

	  if ((_M_need & _ChronoParts::_TimeOfDay) != 0
		&& (_M_need & _ChronoParts::_Year) != 0)
	    {
	      // For time_points assume "00:00:00" is implicitly present,
	      // so we don't fail to parse if it's not (PR libstdc++/114240).
	      // We will still fail to parse if there's no year+month+day.
	      __h = hours(0);
	      __parts = _ChronoParts::_TimeOfDay;
	    }

	  // bool __is_neg = false; // TODO: how is this handled for parsing?

	  _CharT __mod{}; // One of 'E' or 'O' or nul.
	  unsigned __num = 0; // Non-zero for N modifier.
	  bool __is_flag = false; // True if we're processing a % flag.

	  constexpr bool __is_floating
	    = treat_as_floating_point_v<typename _Duration::rep>;

	  // If an out-of-range value is extracted (e.g. 61min for %M),
	  // do not set failbit immediately because we might not need it
	  // (e.g. parsing chrono::year doesn't care about invalid %M values).
	  // Instead set the variable back to its initial 'bad' state,
	  // and also set related variables corresponding to the same field
	  // (e.g. a bad %M value for __min should also reset __h and __s).
	  // If a valid value is needed later the bad value will cause failure.

	  // For some fields we don't know the correct range when parsing and
	  // we have to be liberal in what we accept, e.g. we allow 366 for
	  // day-of-year because that's valid in leap years, and we allow 31
	  // for day-of-month. If those values are needed to determine the
	  // result then we can do a correct range check at the end when we
	  // know the how many days the relevant year or month actually has.

	  while (*__fmt)
	    {
	      _CharT __c = *__fmt++;
	      if (!__is_flag)
		{
		  if (__c == '%')
		    __is_flag = true; // This is the start of a flag.
		  else if (std::isspace(__c, __loc))
		    std::ws(__is); // Match zero or more whitespace characters.
		  else if (!__read_chr(__c)) [[unlikely]]
		    break; // Failed to match the expected character.

		  continue; // Process next character in the format string.
		}

	      // Now processing a flag.
	      switch (__c)
	      {
		case 'a': // Locale's weekday name
		case 'A': // (full or abbreviated, matched case-insensitively).
		  if (__mod || __num) [[unlikely]]
		    __err = ios_base::failbit;
		  else
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2, __fmt);
		      if (!__is_failed(__err))
			__wday = weekday(__tm.tm_wday);
		    }
		  __parts |= _ChronoParts::_Weekday;
		  break;

		case 'b': // Locale's month name
		case 'h': // (full or abbreviated, matched case-insensitively).
		case 'B':
		  if (__mod || __num) [[unlikely]]
		    __err = ios_base::failbit;
		  else
		    {
		      // strptime behaves differently for %b and %B,
		      // but chrono::parse says they're equivalent.
		      // Luckily libstdc++ std::time_get works as needed.
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2, __fmt);
		      if (!__is_failed(__err))
			__m = month(__tm.tm_mon + 1);
		    }
		  __parts |= _ChronoParts::_Month;
		  break;

		case 'c': // Locale's date and time representation.
		  if (__mod == 'O' || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2 - (__mod == 'E'), __fmt);
		      if (!__is_failed(__err))
			{
			  __y = year(__tm.tm_year + 1900);
			  __m = month(__tm.tm_mon + 1);
			  __d = day(__tm.tm_mday);
			  __h = hours(__tm.tm_hour);
			  __min = minutes(__tm.tm_min);
			  __s = seconds(__tm.tm_sec);
			}
		    }
		  __parts |= _ChronoParts::_DateTime;
		  break;

		case 'C': // Century
		  if (!__mod) [[likely]]
		    {
		      auto __v = __read_signed(__num ? __num : 2);
		      if (!__is_failed(__err))
			{
			  int __cmin = (int)year::min() / 100;
			  int __cmax = (int)year::max() / 100;
			  if (__cmin <= __v && __v <= __cmax)
			    __century = __v * 100;
			  else
			    __century = -2; // This prevents guessing century.
			}
		    }
		  else if (__mod == 'E')
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 3, __fmt);
		      if (!__is_failed(__err))
			__century = __tm.tm_year;
		    }
		  else [[unlikely]]
		    __err |= ios_base::failbit;
		  // N.B. don't set this here: __parts |= _ChronoParts::_Year;
		  break;

		case 'd': // Day of month (1-31)
		case 'e':
		  if (!__mod) [[likely]]
		    {
		      auto __v = __read_unsigned(__num ? __num : 2);
		      if (!__is_failed(__err))
			__d = day(__v);
		    }
		  else if (__mod == 'O')
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 3, __fmt);
		      if (!__is_failed(__err))
			__d = day(__tm.tm_mday);
		    }
		  else [[unlikely]]
		    __err |= ios_base::failbit;
		  __parts |= _ChronoParts::_Day;
		  break;

		case 'D': // %m/%d/%y
		  if (__mod || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      auto __month = __read_unsigned(2); // %m
		      __read_chr('/');
		      auto __day = __read_unsigned(2); // %d
		      __read_chr('/');
		      auto __year = __read_unsigned(2); // %y
		      if (__is_failed(__err))
			break;
		      __y = year(__year + 1900 + 100 * int(__year < 69));
		      __m = month(__month);
		      __d = day(__day);
		      if (!year_month_day(__y, __m, __d).ok())
			{
			  __y = __yy = __iso_y = __iso_yy = __bad_y;
			  __m = __bad_mon;
			  __d = __bad_day;
			  break;
			}
		    }
		  __parts |= _ChronoParts::_Date;
		  break;

		case 'F': // %Y-%m-%d - any N modifier only applies to %Y.
		  if (__mod) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      auto __year = __read_signed(__num ? __num : 4); // %Y
		      __read_chr('-');
		      auto __month = __read_unsigned(2); // %m
		      __read_chr('-');
		      auto __day = __read_unsigned(2); // %d
		      if (__is_failed(__err))
			break;
		      __y = year(__year);
		      __m = month(__month);
		      __d = day(__day);
		      if (!year_month_day(__y, __m, __d).ok())
			{
			  __y = __yy = __iso_y = __iso_yy = __bad_y;
			  __m = __bad_mon;
			  __d = __bad_day;
			  break;
			}
		    }
		  __parts |= _ChronoParts::_Date;
		  break;

		case 'g': // Last two digits of ISO week-based year.
		  if (__mod) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (__val >= 0 && __val <= 99)
			{
			  __iso_yy = year(__val);
			  if (__century == -1) // No %C has been parsed yet.
			    __century = 2000;
			}
		      else
			__iso_yy = __iso_y = __y = __yy = __bad_y;
		    }
		  __parts |= _ChronoParts::_Year;
		  break;

		case 'G': // ISO week-based year.
		  if (__mod) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    __iso_y = year(__read_unsigned(__num ? __num : 4));
		  __parts |= _ChronoParts::_Year;
		  break;

		case 'H': // 24-hour (00-23)
		case 'I': // 12-hour (1-12)
		  if (__mod == 'E') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'O')
		    {
#if 0
		      struct tm __tm{};
		      __tm.tm_ampm = 1;
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 3, __fmt);
		      if (!__is_failed(__err))
			{
			  if (__c == 'I')
			    {
			      __h12 = hours(__tm.tm_hour);
			      __h = __bad_h;
			    }
			  else
			    __h = hours(__tm.tm_hour);
			}
#else
		      // XXX %OI seems to be unimplementable.
		      __err |= ios_base::failbit;
#endif
		    }
		  else
		    {
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (__c == 'I' && __val >= 1 && __val <= 12)
			{
			  __h12 = hours(__val);
			  __h = __bad_h;
			}
		      else if (__c == 'H' && __val >= 0 && __val <= 23)
			{
			  __h = hours(__val);
			  __h12 = __bad_h;
			}
		      else
			{
			  if ((_M_need & _ChronoParts::_TimeOfDay) != 0)
			    __err |= ios_base::failbit;
			  break;
			}
		    }
		  __parts |= _ChronoParts::_TimeOfDay;
		  break;

		case 'j': // For duration, count of days, otherwise day of year
		  if (__mod) [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (_M_need == _ChronoParts::_TimeOfDay) // duration
		    {
		      auto __val = __read_signed(__num ? __num : 3);
		      if (!__is_failed(__err))
			{
			  __h = days(__val); // __h will get added to _M_time
			  __parts |= _ChronoParts::_TimeOfDay;
			}
		    }
		  else
		    {
		      __dayofyear = __read_unsigned(__num ? __num : 3);
		      // N.B. do not alter __parts here, done after loop.
		      // No need for range checking here either.
		    }
		  break;

		case 'm': // Month (1-12)
		  if (__mod == 'E') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'O')
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2, __fmt);
		      if (!__is_failed(__err))
			__m = month(__tm.tm_mon + 1);
		    }
		  else
		    {
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (__val >= 1 && __val <= 12)
			__m = month(__val);
		      else
			__m = __bad_mon;
		    }
		  __parts |= _ChronoParts::_Month;
		  break;

		case 'M': // Minutes
		  if (__mod == 'E') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'O')
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2, __fmt);
		      if (!__is_failed(__err))
			__min = minutes(__tm.tm_min);
		    }
		  else
		    {
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (0 <= __val && __val < 60)
			__min = minutes(__val);
		      else
			{
			  if ((_M_need & _ChronoParts::_TimeOfDay) != 0)
			    __err |= ios_base::failbit;
			  break;
			}
		    }
		  __parts |= _ChronoParts::_TimeOfDay;
		  break;

		case 'p': // Locale's AM/PM designation for 12-hour clock.
		  if (__mod || __num)
		    __err |= ios_base::failbit;
		  else
		    {
		      // Can't use std::time_get here as it can't parse %p
		      // in isolation without %I. This might be faster anyway.
		      const _CharT* __ampms[2];
		      __tmpunct._M_am_pm(__ampms);
		      int __n = 0, __which = 3;
		      while (__which != 0)
			{
			  auto __i = __is.peek();
			  if (_Traits::eq_int_type(__i, _Traits::eof()))
			    {
			      __err |= ios_base::eofbit | ios_base::failbit;
			      break;
			    }
			  __i = std::toupper(_Traits::to_char_type(__i), __loc);
			  if (__which & 1)
			    {
			      if (__i != std::toupper(__ampms[0][__n], __loc))
				__which ^= 1;
			      else if (__ampms[0][__n + 1] == _CharT())
				{
				  __which = 1;
				  (void) __is.get();
				  break;
				}
			    }
			  if (__which & 2)
			    {
			      if (__i != std::toupper(__ampms[1][__n], __loc))
				__which ^= 2;
			      else if (__ampms[1][__n + 1] == _CharT())
				{
				  __which = 2;
				  (void) __is.get();
				  break;
				}
			    }
			  if (__which)
			    (void) __is.get();
			  ++__n;
			}
		      if (__which == 0 || __which == 3)
			__err |= ios_base::failbit;
		      else
			__ampm = __which;
		    }
		  break;

		case 'r': // Locale's 12-hour time.
		  if (__mod || __num)
		    __err |= ios_base::failbit;
		  else
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2, __fmt);
		      if (!__is_failed(__err))
			{
			  __h = hours(__tm.tm_hour);
			  __min = minutes(__tm.tm_min);
			  __s = seconds(__tm.tm_sec);
			}
		    }
		  __parts |= _ChronoParts::_TimeOfDay;
		  break;

		case 'R': // %H:%M
		case 'T': // %H:%M:%S
		  if (__mod || __num) [[unlikely]]
		    {
		      __err |= ios_base::failbit;
		      break;
		    }
		  else
		    {
		      auto __val = __read_unsigned(2);
		      if (__val == -1 || __val > 23) [[unlikely]]
			{
			  if ((_M_need & _ChronoParts::_TimeOfDay) != 0)
			    __err |= ios_base::failbit;
			  break;
			}
		      if (!__read_chr(':')) [[unlikely]]
			break;
		      __h = hours(__val);

		      __val = __read_unsigned(2);
		      if (__val == -1 || __val > 60) [[unlikely]]
			{
			  if ((_M_need & _ChronoParts::_TimeOfDay) != 0)
			    __err |= ios_base::failbit;
			  break;
			}
		      __min = minutes(__val);

		      if (__c == 'R')
			{
			  __parts |= _ChronoParts::_TimeOfDay;
			  break;
			}
		      else if (!__read_chr(':')) [[unlikely]]
			break;
		    }
		  [[fallthrough]];

		case 'S': // Seconds
		  if (__mod == 'E') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'O')
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 3, __fmt);
		      if (!__is_failed(__err))
			__s = seconds(__tm.tm_sec);
		    }
		  else if constexpr (_Duration::period::den == 1
				       && !__is_floating)
		    {
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (0 <= __val && __val <= 59) [[likely]]
			__s = seconds(__val);
		      else
			{
			  if ((_M_need & _ChronoParts::_TimeOfDay) != 0)
			    __err |= ios_base::failbit;
			  break;
			}
		    }
		  else // Read fractional seconds
		    {
		      stringstream __buf;
		      auto __digit = _S_try_read_digit(__is, __err);
		      if (__digit != -1)
			{
			  __buf.put('0' + __digit);
			  __digit = _S_try_read_digit(__is, __err);
			  if (__digit != -1)
			    __buf.put('0' + __digit);
			}

		      auto __i = __is.peek();
		      if (_Traits::eq_int_type(__i, _Traits::eof()))
			__err |= ios_base::eofbit;
		      else
			{
			  _CharT __dp = '.';
			  if (__loc != locale::classic())
			    {
			      auto& __np = use_facet<numpunct<_CharT>>(__loc);
			      __dp = __np.decimal_point();
			    }
			  _CharT __c = _Traits::to_char_type(__i);
			  if (__c == __dp)
			    {
			      (void) __is.get();
			      __buf.put('.');
			      int __prec
				= hh_mm_ss<_Duration>::fractional_width;
			      do
				{
				  __digit = _S_try_read_digit(__is, __err);
				  if (__digit != -1)
				    __buf.put('0' + __digit);
				  else
				    break;
				}
			      while (--__prec);
			    }
			}

		      if (!__is_failed(__err)) [[likely]]
			{
			  long double __val{};
#if __cpp_lib_to_chars
			  string __str = std::move(__buf).str();
			  auto __first = __str.data();
			  auto __last = __first + __str.size();
			  using enum chars_format;
			  auto [ptr, ec] = std::from_chars(__first, __last,
							   __val, fixed);
			  if ((bool)ec || ptr != __last) [[unlikely]]
			    __err |= ios_base::failbit;
			  else
#else
			  if (__buf >> __val)
#endif
			    {
			      duration<long double> __fs(__val);
			      if constexpr (__is_floating)
				__s = __fs;
			      else
				__s = chrono::round<_Duration>(__fs);
			    }
			}
		    }
		  __parts |= _ChronoParts::_TimeOfDay;
		  break;

		case 'u': // ISO weekday (1-7)
		case 'w': // Weekday (0-6)
		  if (__mod == 'E') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'O')
		    {
		      if (__c == 'w')
			{
			  struct tm __tm{};
			  __tmget.get(__is, {}, __is, __err, &__tm,
				      __fmt - 3, __fmt);
			  if (!__is_failed(__err))
			    __wday = weekday(__tm.tm_wday);
			}
		      else
			__err |= ios_base::failbit;
		    }
		  else
		    {
		      const int __lo = __c == 'u' ? 1 : 0;
		      const int __hi = __lo + 6;
		      auto __val = __read_unsigned(__num ? __num : 1);
		      if (__lo <= __val && __val <= __hi)
			__wday = weekday(__val);
		      else
			{
			  __wday = __bad_wday;
			  break;
			}
		    }
		  __parts |= _ChronoParts::_Weekday;
		  break;

		case 'U': // Week number of the year (from first Sunday).
		case 'V': // ISO week-based week number.
		case 'W': // Week number of the year (from first Monday).
		  if (__mod == 'E') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'O')
		    {
		      if (__c == 'V') [[unlikely]]
			__err |= ios_base::failbit;
		      else
			{
			  // TODO nl_langinfo_l(ALT_DIGITS) ?
			  // Not implementable using std::time_get.
			}
		    }
		  else
		    {
		      const int __lo = __c == 'V' ? 1 : 0;
		      const int __hi = 53;
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (__lo <= __val && __val <= __hi)
			{
			  switch (__c)
			  {
			    case 'U':
			      __sunday_wk = __val;
			      break;
			    case 'V':
			      __iso_wk = __val;
			      break;
			    case 'W':
			      __monday_wk = __val;
			      break;
			  }
			}
		      else
			__iso_wk = __sunday_wk = __monday_wk = -1;
		    }
		  // N.B. do not alter __parts here, done after loop.
		  break;

		case 'x': // Locale's date representation.
		  if (__mod == 'O' || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2 - (__mod == 'E'), __fmt);
		      if (!__is_failed(__err))
			{
			  __y = year(__tm.tm_year + 1900);
			  __m = month(__tm.tm_mon + 1);
			  __d = day(__tm.tm_mday);
			}
		    }
		  __parts |= _ChronoParts::_Date;
		  break;

		case 'X': // Locale's time representation.
		  if (__mod == 'O' || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 2 - (__mod == 'E'), __fmt);
		      if (!__is_failed(__err))
			{
			  __h = hours(__tm.tm_hour);
			  __min = minutes(__tm.tm_min);
			  __s = seconds(__tm.tm_sec);
			}
		    }
		  __parts |= _ChronoParts::_TimeOfDay;
		  break;

		case 'y': // Last two digits of year.
		  if (__mod) [[unlikely]]
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 3, __fmt);
		      if (!__is_failed(__err))
			{
			  int __cent = __tm.tm_year < 2000 ? 1900 : 2000;
			  __yy = year(__tm.tm_year - __cent);
			  if (__century == -1) // No %C has been parsed yet.
			    __century = __cent;
			}
		    }
		  else
		    {
		      auto __val = __read_unsigned(__num ? __num : 2);
		      if (__val >= 0 && __val <= 99)
			{
			  __yy = year(__val);
			  if (__century == -1) // No %C has been parsed yet.
			    __century = __val < 69 ? 2000 : 1900;
			}
		      else
			__y = __yy = __iso_yy = __iso_y = __bad_y;
		    }
		  __parts |= _ChronoParts::_Year;
		  break;

		case 'Y': // Year
		  if (__mod == 'O') [[unlikely]]
		    __err |= ios_base::failbit;
		  else if (__mod == 'E')
		    {
		      struct tm __tm{};
		      __tmget.get(__is, {}, __is, __err, &__tm,
				  __fmt - 3, __fmt);
		      if (!__is_failed(__err))
			__y = year(__tm.tm_year);
		    }
		  else
		    {
		      auto __val = __read_unsigned(__num ? __num : 4);
		      if (!__is_failed(__err))
			__y = year(__val);
		    }
		  __parts |= _ChronoParts::_Year;
		  break;

		case 'z':
		  if (__num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      // For %Ez and %Oz read [+|-][h]h[:mm].
		      // For %z read [+|-]hh[mm].

		      auto __i = __is.peek();
		      if (_Traits::eq_int_type(__i, _Traits::eof()))
			{
			  __err |= ios_base::eofbit | ios_base::failbit;
			  break;
			}
		      _CharT __ic = _Traits::to_char_type(__i);
		      const bool __neg = __ic == _CharT('-');
		      if (__ic == _CharT('-') || __ic == _CharT('+'))
			(void) __is.get();

		      int_least32_t __hh;
		      if (__mod)
			{
			  // Read h[h]
			  __hh = __read_unsigned(2);
			}
		      else
			{
			  // Read hh
			  __hh = 10 * _S_try_read_digit(__is, __err);
			  __hh += _S_try_read_digit(__is, __err);
			}

		      if (__is_failed(__err))
			break;

		      __i = __is.peek();
		      if (_Traits::eq_int_type(__i, _Traits::eof()))
			{
			  __err |= ios_base::eofbit;
			  __tz_offset = minutes(__hh * (__neg ? -60 : 60));
			  break;
			}
		      __ic = _Traits::to_char_type(__i);

		      bool __read_mm = false;
		      if (__mod)
			{
			  if (__ic == _GLIBCXX_WIDEN(":")[0])
			    {
			      // Read [:mm] part.
			      (void) __is.get();
			      __read_mm = true;
			    }
			}
		      else if (_CharT('0') <= __ic && __ic <= _CharT('9'))
			{
			  // Read [mm] part.
			  __read_mm = true;
			}

		      int_least32_t __mm = 0;
		      if (__read_mm)
			{
			  __mm = 10 * _S_try_read_digit(__is, __err);
			  __mm += _S_try_read_digit(__is, __err);
			}

		      if (!__is_failed(__err))
			{
			  auto __z = __hh * 60 + __mm;
			  __tz_offset = minutes(__neg ? -__z : __z);
			}
		    }
		  break;

		case 'Z':
		  if (__mod || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      basic_string_view<_CharT> __x = _GLIBCXX_WIDEN("_/-+");
		      __tz_abbr.clear();
		      while (true)
			{
			  auto __i = __is.peek();
			  if (!_Traits::eq_int_type(__i, _Traits::eof()))
			    {
			      _CharT __a = _Traits::to_char_type(__i);
			      if (std::isalnum(__a, __loc)
				    || __x.find(__a) != __x.npos)
				{
				  __tz_abbr.push_back(__a);
				  (void) __is.get();
				  continue;
				}
			    }
			  else
			    __err |= ios_base::eofbit;
			  break;
			}
		      if (__tz_abbr.empty())
			__err |= ios_base::failbit;
		    }
		  break;

		case 'n': // Exactly one whitespace character.
		  if (__mod || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      _CharT __i = __is.peek();
		      if (_Traits::eq_int_type(__i, _Traits::eof()))
			__err |= ios_base::eofbit | ios_base::failbit;
		      else if (std::isspace(_Traits::to_char_type(__i), __loc))
			(void) __is.get();
		      else
			__err |= ios_base::failbit;
		    }
		  break;

		case 't': // Zero or one whitespace characters.
		  if (__mod || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    {
		      _CharT __i = __is.peek();
		      if (_Traits::eq_int_type(__i, _Traits::eof()))
			__err |= ios_base::eofbit;
		      else if (std::isspace(_Traits::to_char_type(__i), __loc))
			(void) __is.get();
		    }
		  break;

		case '%': // A % character.
		  if (__mod || __num) [[unlikely]]
		    __err |= ios_base::failbit;
		  else
		    __read_chr('%');
		  break;

		case 'O': // Modifiers
		case 'E':
		  if (__mod || __num) [[unlikely]]
		    {
		      __err |= ios_base::failbit;
		      break;
		    }
		  __mod = __c;
		  continue;

		default:
		  if (_CharT('1') <= __c && __c <= _CharT('9'))
		    {
		      if (!__mod) [[likely]]
			{
			  // %Nx - extract positive decimal integer N
			  auto __end = __fmt + _Traits::length(__fmt);
			  auto [__v, __ptr]
			    = __format::__parse_integer(__fmt - 1, __end);
			  if (__ptr) [[likely]]
			    {
			      __num = __v;
			      __fmt = __ptr;
			      continue;
			    }
			}
		    }
		  __err |= ios_base::failbit;
		}

	      if (__is_failed(__err)) [[unlikely]]
		break;

	      __is_flag = false;
	      __num = 0;
	      __mod = _CharT();
	    }

	  if (__century >= 0)
	    {
	      if (__yy != __bad_y && __y == __bad_y)
		__y = years(__century) + __yy; // Use %y instead of %Y
	      if (__iso_yy != __bad_y && __iso_y == __bad_y)
		__iso_y = years(__century) + __iso_yy; // Use %g instead of %G
	    }

	  bool __can_use_doy = false;
	  bool __can_use_iso_wk = false;
	  bool __can_use_sun_wk = false;
	  bool __can_use_mon_wk = false;

	  // A year + day-of-year can be converted to a full date.
	  if (__y != __bad_y && __dayofyear >= 0)
	    {
	      __can_use_doy = true;
	      __parts |= _ChronoParts::_Date;
	    }
	  else if (__y != __bad_y && __wday != __bad_wday && __sunday_wk >= 0)
	    {
	      __can_use_sun_wk = true;
	      __parts |= _ChronoParts::_Date;
	    }
	  else if (__y != __bad_y && __wday != __bad_wday && __monday_wk >= 0)
	    {
	      __can_use_mon_wk = true;
	      __parts |= _ChronoParts::_Date;
	    }
	  else if (__iso_y != __bad_y && __wday != __bad_wday && __iso_wk > 0)
	    {
	      // An ISO week date can be converted to a full date.
	      __can_use_iso_wk = true;
	      __parts |= _ChronoParts::_Date;
	    }

	  if (__is_failed(__err)) [[unlikely]]
	    ; // Don't bother doing any more work.
	  else if (__is_flag) [[unlikely]] // incomplete format flag
	    __err |= ios_base::failbit;
	  else if ((_M_need & __parts) == _M_need) [[likely]]
	    {
	      // We try to avoid calculating _M_sys_days and _M_ymd unless
	      // necessary, because converting sys_days to year_month_day
	      // (or vice versa) requires non-trivial calculations.
	      // If we have y/m/d values then use them to populate _M_ymd
	      // and only convert it to _M_sys_days if the caller needs that.
	      // But if we don't have y/m/d and need to calculate the date
	      // from the day-of-year or a week+weekday then we set _M_sys_days
	      // and only convert it to _M_ymd if the caller needs that.

	      // We do more error checking here, but only for the fields that
	      // we actually need to use. For example, we will not diagnose
	      // an invalid dayofyear==366 for non-leap years unless actually
	      // using __dayofyear. This should mean we never produce invalid
	      // results, but it means not all invalid inputs are diagnosed,
	      // e.g. "2023-01-01 366" >> "%F %j" ignores the invalid 366.
	      // We also do not diagnose inconsistent values for the same
	      // field, e.g. "2021 2022 2023" >> "%C%y %Y %Y" just uses 2023.

	      // Whether the caller wants _M_wd.
	      // The _Weekday bit is only set for chrono::weekday.
	      const bool __need_wday = (_M_need & _ChronoParts::_Weekday) != 0;

	      // Whether the caller wants _M_sys_days and _M_time.
	      // Only true for durations and time_points.
	      const bool __need_time = (_M_need & _ChronoParts::_TimeOfDay) != 0;

	      if (__need_wday && __wday != __bad_wday)
		_M_wd = __wday; // Caller only wants a weekday and we have one.
	      else if ((_M_need & _ChronoParts::_Date) != 0) // subsumes __need_wday
		{
		  // Whether the caller wants _M_ymd.
		  // True for chrono::year etc., false for time_points.
		  const bool __need_ymd = !__need_wday && !__need_time;

		  if (((_M_need & _ChronoParts::_Year) != 0 && __y == __bad_y)
		     || ((_M_need & _ChronoParts::_Month) != 0 && __m == __bad_mon)
		     || ((_M_need & _ChronoParts::_Day) != 0 && __d == __bad_day))
		    {
		      // Missing at least one of y/m/d so calculate sys_days
		      // from the other data we have available.

		      if (__can_use_doy)
			{
			  if ((0 < __dayofyear && __dayofyear <= 365)
				|| (__dayofyear == 366 && __y.is_leap()))
			    [[likely]]
			    {
			      _M_sys_days = sys_days(__y/January/1)
					      + days(__dayofyear - 1);
			      if (__need_ymd)
				_M_ymd = year_month_day(_M_sys_days);
			    }
			  else
			    __err |= ios_base::failbit;
			}
		      else if (__can_use_iso_wk)
			{
			  // Calculate y/m/d from ISO week date.

			  if (__iso_wk == 53)
			    {
			      // A year has 53 weeks iff Jan 1st is a Thursday
			      // or Jan 1 is a Wednesday and it's a leap year.
			      const sys_days __jan4(__iso_y/January/4);
			      weekday __wd1(__jan4 - days(3));
			      if (__wd1 != Thursday)
				if (__wd1 != Wednesday || !__iso_y.is_leap())
				  __err |= ios_base::failbit;
			    }

			  if (!__is_failed(__err)) [[likely]]
			    {
			      // First Thursday is always in week one:
			      sys_days __w(Thursday[1]/January/__iso_y);
			      // First day of week-based year:
			      __w -= Thursday - Monday;
			      __w += days(weeks(__iso_wk - 1));
			      __w += __wday - Monday;
			      _M_sys_days = __w;

			      if (__need_ymd)
				_M_ymd = year_month_day(_M_sys_days);
			    }
			}
		      else if (__can_use_sun_wk)
			{
			  // Calculate y/m/d from week number + weekday.
			  sys_days __wk1(__y/January/Sunday[1]);
			  _M_sys_days = __wk1 + weeks(__sunday_wk - 1)
					+ days(__wday.c_encoding());
			  _M_ymd = year_month_day(_M_sys_days);
			  if (_M_ymd.year() != __y) [[unlikely]]
			    __err |= ios_base::failbit;
			}
		      else if (__can_use_mon_wk)
			{
			  // Calculate y/m/d from week number + weekday.
			  sys_days __wk1(__y/January/Monday[1]);
			  _M_sys_days = __wk1 + weeks(__monday_wk - 1)
					+ days(__wday.c_encoding() - 1);
			  _M_ymd = year_month_day(_M_sys_days);
			  if (_M_ymd.year() != __y) [[unlikely]]
			    __err |= ios_base::failbit;
			}
		      else // Should not be able to get here.
			__err |= ios_base::failbit;
		    }
		  else
		    {
		      // We know that all fields the caller needs are present,
		      // but check that their values are in range.
		      // Make unwanted fields valid so that _M_ymd.ok() is true.

		      if ((_M_need & _ChronoParts::_Year) != 0)
			{
			  if (!__y.ok()) [[unlikely]]
			    __err |= ios_base::failbit;
			}
		      else if (__y == __bad_y)
			__y = 1972y; // Leap year so that Feb 29 is valid.

		      if ((_M_need & _ChronoParts::_Month) != 0)
			{
			  if (!__m.ok()) [[unlikely]]
			    __err |= ios_base::failbit;
			}
		      else if (__m == __bad_mon)
			__m = January;

		      if ((_M_need & _ChronoParts::_Day) != 0)
			{
			  if (__d < day(1) || __d > (__y/__m/last).day())
			    __err |= ios_base::failbit;
			}
		      else if (__d == __bad_day)
			__d = 1d;

		      if (year_month_day __ymd(__y, __m, __d); __ymd.ok())
			{
			  _M_ymd = __ymd;
			  if (__need_wday || __need_time)
			    _M_sys_days = sys_days(_M_ymd);
			}
		      else [[unlikely]]
			__err |= ios_base::failbit;
		    }

		  if (__need_wday)
		    _M_wd = weekday(_M_sys_days);
		}

	      // Need to set _M_time for both durations and time_points.
	      if (__need_time)
		{
		  if (__h == __bad_h && __h12 != __bad_h)
		    {
		      if (__ampm == 1)
			__h = __h12 == hours(12) ? hours(0) : __h12;
		      else if (__ampm == 2)
			__h = __h12 == hours(12) ? __h12 : __h12 + hours(12);
		      else [[unlikely]]
			__err |= ios_base::failbit;
		    }

		  auto __t = _M_time.zero();
		  bool __ok = false;

		  if (__h != __bad_h)
		    {
		      __ok = true;
		      __t += __h;
		    }

		  if (__min != __bad_min)
		    {
		      __ok = true;
		      __t += __min;
		    }

		  if (__s != __bad_sec)
		    {
		      __ok = true;
		      __t += __s;
		      _M_is_leap_second = __s >= seconds(60);
		    }

		  if (__ok)
		    _M_time = __t;
		  else
		    __err |= ios_base::failbit;
		}

	      if (!__is_failed(__err)) [[likely]]
		{
		  if (__offset && __tz_offset != __bad_min)
		    *__offset = __tz_offset;
		  if (__abbrev && !__tz_abbr.empty())
		    *__abbrev = std::move(__tz_abbr);
		}
	    }
	  else
	    __err |= ios_base::failbit;
	}
      if (__err)
	__is.setstate(__err);
      return __is;
    }
  /// @endcond
#undef _GLIBCXX_WIDEN

  /// @} group chrono
} // namespace chrono

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++20

#endif //_GLIBCXX_CHRONO_IO_H
