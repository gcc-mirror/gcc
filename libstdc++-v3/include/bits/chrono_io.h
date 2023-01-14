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

#pragma GCC system_header

#if __cplusplus >= 202002L

#include <sstream> // ostringstream
#include <iomanip> // setw, setfill
#include <format>

#include <bits/charconv.h>

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
  // STATICALLY-WIDEN, see C++20 [time.general]
  // It doesn't matter for format strings (which can only be char or wchar_t)
  // but this returns the narrow string for anything that isn't wchar_t. This
  // is done because const char* can be inserted into any ostream type, and
  // will be widened at runtime if necessary.
  template<typename _CharT>
    consteval auto
    _Widen(const char* __narrow, const wchar_t* __wide)
    {
      if constexpr (is_same_v<_CharT, wchar_t>)
	return __wide;
      else
	return __narrow;
    }
#define _GLIBCXX_WIDEN_(C, S) ::std::chrono::__detail::_Widen<C>(S, L##S)
#define _GLIBCXX_WIDEN(S) _GLIBCXX_WIDEN_(_CharT, S)


  // Write an arbitrary duration suffix into the buffer.
  template<typename _Period>
    constexpr const char*
    __units_suffix_misc(char* __buf, size_t /* TODO check length? */) noexcept
    {
      namespace __tc = std::__detail;
      char* __p = __buf;
      __p[0] = '[';
      unsigned __nlen = __tc::__to_chars_len((uintmax_t)_Period::num);
      __tc::__to_chars_10_impl(__p + 1, __nlen, (uintmax_t)_Period::num);
      __p += 1 + __nlen;
      if constexpr (_Period::den != 1)
	{
	  __p[0] = '/';
	  unsigned __dlen = __tc::__to_chars_len((uintmax_t)_Period::den);
	  __tc::__to_chars_10_impl(__p + 1, __dlen, (uintmax_t)_Period::den);
	  __p += 1 + __dlen;
	}
      __p[0] = ']';
      __p[1] = 's';
      __p[2] = '\0';
      return __buf;
    }

  template<typename _Period, typename _CharT>
    constexpr auto
    __units_suffix(char* __buf, size_t __n) noexcept
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
      return __detail::__units_suffix_misc<_Period>(__buf, __n);
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
      using period = typename _Period::type;
      char __buf[sizeof("[/]s") + 2 * numeric_limits<intmax_t>::digits10];
      std::basic_ostringstream<_CharT, _Traits> __s;
      __s.flags(__os.flags());
      __s.imbue(__os.getloc());
      __s.precision(__os.precision());
      __s << __d.count();
      __s << __detail::__units_suffix<period, _CharT>(__buf, sizeof(__buf));
      __os << std::move(__s).str();
      return __os;
    }

/// @cond undocumented
namespace __detail
{
  // An unspecified type returned by `chrono::local_time_format`.
  template<typename _Duration>
    struct __local_time_fmt
    {
      local_time<_Duration> _M_time;
      const string* _M_abbrev;
      const seconds* _M_offset_sec;
    };

  struct __local_fmt_t;
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
  __no_timezone_available()
  { __throw_format_error("format error: no timezone available for %Z or %z"); }

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

  template<typename _CharT>
    struct _ChronoSpec : _Spec<_CharT>
    {
      basic_string_view<_CharT> _M_chrono_specs;
    };

  // Represents the information provided by a chrono type.
  // e.g. month_weekday has month and weekday but no year or time of day,
  // hh_mm_ss has time of day but no date, sys_time is time_point+timezone.
  enum _ChronoParts {
    _Year = 1, _Month = 2, _Day = 4, _Weekday = 8, _TimeOfDay = 16,
    _TimeZone = 32,
    _Date = _Year | _Month | _Day | _Weekday,
    _DateTime = _Date | _TimeOfDay,
    _ZonedDateTime = _DateTime | _TimeZone,
    _Duration = 128 // special case
  };

  constexpr _ChronoParts
  operator|(_ChronoParts __x, _ChronoParts __y)
  { return static_cast<_ChronoParts>((int)__x | (int)__y); }

  // TODO rename this to chrono::__formatter? or chrono::__detail::__formatter?
  template<typename _CharT>
    struct __formatter_chrono
    {
      using __string_view = basic_string_view<_CharT>;
      using __string = basic_string<_CharT>;

      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	_M_parse(_ParseContext& __pc, _ChronoParts __parts)
	{
	  auto __first = __pc.begin();
	  auto __last = __pc.end();

	  _ChronoSpec<_CharT> __spec{};

	  auto __finalize = [this, &__spec] {
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

	  if (__parts & _ChronoParts::_Duration)
	    {
	      __first = __spec._M_parse_precision(__first, __last, __pc);
	      if (__finished())
		return __first;
	    }

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

	  const auto __chrono_specs = __first++; // Skip leading '%'
	  if (*__chrono_specs != '%')
	    __throw_format_error("chrono format error: no '%' at start of "
				     "chrono-specs");

	  _CharT __mod{};
	  bool __conv = true;
	  int __needed = 0;

	  while (__first != __last)
	    {
	      enum _Mods { _Mod_none, _Mod_E, _Mod_O, _Mod_E_O };
	      _Mods __allowed_mods = _Mod_none;

	      _CharT __c = *__first++;
	      switch (__c)
		{
		case 'a':
		case 'A':
		  __needed = _Weekday;
		  break;
		case 'b':
		case 'h':
		case 'B':
		  __needed = _Month;
		  break;
		case 'c':
		  __needed = _DateTime;
		  __allowed_mods = _Mod_E;
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
		  __needed = _Date;
		  break;
		case 'g':
		case 'G':
		  __needed = _Date;
		  break;
		case 'H':
		case 'I':
		  __needed = _TimeOfDay;
		  __allowed_mods = _Mod_O;
		  break;
		case 'j':
		  if (!(__parts & _Duration))
		    __needed = _Date;
		  break;
		case 'm':
		  __needed = _Month;
		  __allowed_mods = _Mod_O;
		  break;
		case 'M':
		  __needed = _TimeOfDay;
		  __allowed_mods = _Mod_O;
		  break;
		case 'p':
		case 'r':
		case 'R':
		case 'T':
		  __needed = _TimeOfDay;
		  break;
		case 'q':
		case 'Q':
		  __needed = _Duration;
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
		case 'V':
		case 'W':
		  __needed = _Date;
		  __allowed_mods = _Mod_O;
		  break;
		case 'x':
		  __needed = _Date;
		  __allowed_mods = _Mod_E;
		  break;
		case 'X':
		  __needed = _TimeOfDay;
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
		  __needed = _TimeZone;
		  __allowed_mods = _Mod_E;
		  break;
		case 'Z':
		  __needed = _TimeZone;
		  __allowed_mods = _Mod_E_O;
		  break;
		case 'n':
		case 't':
		case '%':
		  break;
		case 'O':
		case 'E':
		  __mod = __c;
		  continue;
		default:
		  __throw_format_error("chrono format error: invalid "
				       " specifier in chrono-specs");
		}

	      if ((__mod == 'E' && !(__allowed_mods & _Mod_E))
		    || __mod == 'O' && !(__allowed_mods & _Mod_O))
		__throw_format_error("chrono format error: invalid "
				     " modifier in chrono-specs");
	      __mod = _CharT();

	      if ((__parts & __needed) != __needed)
		__throw_format_error("chrono format error: format argument "
				     "does not contain the information "
				     "required by the chrono-specs");

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

	  _M_spec = __spec;
	  _M_spec._M_chrono_specs = {__chrono_specs, __first - __chrono_specs};

	  return __first;
	}

      // TODO this function template is instantiated for every different _Tp.
      // Consider creating a polymorphic interface for calendar types so
      // that we instantiate fewer different specializations. Similar to
      // _Sink_iter for std::format. Replace each _S_year, _S_day etc. with
      // member functions of that type.
      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_format(const _Tp& __t, _FormatContext& __fc,
		  bool __is_neg = false) const
	{
	  if constexpr (__is_specialization_of<_Tp, chrono::hh_mm_ss>)
	    __is_neg = __t.is_negative();
	  else if constexpr (!chrono::__is_duration_v<_Tp>)
	    __is_neg = false;

	  auto __first = _M_spec._M_chrono_specs.begin();
	  const auto __last = _M_spec._M_chrono_specs.end();
	  if (__first == __last)
	    return _M_format_to_ostream(__t, __fc, __is_neg);

	  _Sink_iter<_CharT> __out;
	  __format::_Str_sink<_CharT> __sink;
	  bool __write_direct = false;
	  if constexpr (is_same_v<typename _FormatContext::iterator,
				  _Sink_iter<_CharT>>)
	    {
	      if (_M_spec._M_width_kind == __format::_WP_none)
		{
		  __out = __fc.out();
		  __write_direct = true;
		}
	      else
		__out = __sink.out();
	    }
	  else
	    __out = __sink.out();

	  auto __print_sign = [&__is_neg, &__out] {
	    if (__is_neg)
	      {
		*__out++ = _S_plus_minus[1];
		__is_neg = false;
	      }
	    return std::move(__out);
	  };

	  // Characters to output for "%n", "%t" and "%%" specifiers.
	  constexpr const _CharT* __literals = _GLIBCXX_WIDEN("\n\t%");

	  ++__first; // Skip leading '%' at start of chrono-specs.

	  _CharT __mod{};
	  do
	    {
	      _CharT __c = *__first++;
	      switch (__c)
		{
		case 'a':
		case 'A':
		  __out = _M_a_A(__t, std::move(__out), __fc, __c == 'A');
		  break;
		case 'b':
		case 'h':
		case 'B':
		  __out = _M_b_B(__t, std::move(__out), __fc, __c == 'B');
		  break;
		case 'c':
		  __out = _M_c(__t, std::move(__out), __fc, __mod == 'E');
		  break;
		case 'C':
		case 'y':
		case 'Y':
		  __out = _M_C_y_Y(__t, std::move(__out), __fc, __c, __mod);
		  break;
		case 'd':
		  // %d  The day of month as a decimal number.
		  // %Od Locale's alternative representation.
		  __out = _S_dd_zero_fill((unsigned)_S_day(__t),
					  std::move(__out),
					  __fc, __mod == 'O');
		  break;
		case 'D':
		  __out = _M_D(__t, std::move(__out), __fc);
		  break;
		case 'e':
		  __out = _M_e(__t, std::move(__out), __fc, __mod == 'O');
		  break;
		case 'F':
		  __out = _M_F(__t, std::move(__out), __fc);
		  break;
		case 'g':
		case 'G':
		  __out = _M_g_G(__t, std::move(__out), __fc, __c == 'G');
		  break;
		case 'H':
		  // %H  The hour (24-hour clock) as a decimal number.
		  // %OH Locale's alternative representation.
		  __out = _S_dd_zero_fill(_S_hms(__t).hours().count(),
					  __print_sign(), __fc, __mod == 'O');
		  break;
		case 'I':
		  __out = _M_I(__t, __print_sign(), __fc, __mod == 'O');
		  break;
		case 'j':
		  __out = _M_j(__t, __print_sign(), __fc);
		  break;
		case 'm':
		  // %m  month as a decimal number.
		  // %Om Locale's alternative representation.
		  __out = _S_dd_zero_fill((unsigned)_S_month(__t),
					  std::move(__out), __fc,
					  __mod == 'O');
		  break;
		case 'M':
		  // %M  The minute as a decimal number.
		  // %OM Locale's alternative representation.
		  __out = _S_dd_zero_fill(_S_hms(__t).minutes().count(),
					  __print_sign(), __fc, __mod == 'O');
		  break;
		case 'p':
		  __out = _M_p(__t, std::move(__out), __fc);
		  break;
		case 'q':
		  __out = _M_q(__t, std::move(__out), __fc);
		  break;
		case 'Q':
		  // %Q The duration's numeric value.
		  if constexpr (chrono::__is_duration_v<_Tp>)
		    __out = std::format_to(__print_sign(), _S_empty_spec,
					   __t.count());
		  else
		    __throw_format_error("chrono format error: argument is "
					 "not a duration");
		  break;
		case 'r':
		  __out = _M_r(__t, __print_sign(), __fc);
		  break;
		case 'R':
		case 'T':
		  __out = _M_R_T(__t, __print_sign(), __fc, __c == 'T');
		  break;
		case 'S':
		  __out = _M_S(__t, __print_sign(), __fc, __mod == 'O');
		  break;
		case 'u':
		case 'w':
		  __out = _M_u_w(__t, std::move(__out), __fc, __c, __mod == 'O');
		  break;
		case 'U':
		case 'V':
		case 'W':
		  __out = _M_U_V_W(__t, std::move(__out), __fc, __c,
				   __mod == 'O');
		  break;
		case 'x':
		  __out = _M_x(__t, std::move(__out), __fc, __mod == 'E');
		  break;
		case 'X':
		  __out = _M_X(__t, __print_sign(), __fc, __mod == 'E');
		  break;
		case 'z':
		  __out = _M_z(__t, std::move(__out), __fc, (bool)__mod);
		  break;
		case 'Z':
		  __out = _M_Z(__t, std::move(__out), __fc);
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

	  if constexpr (is_same_v<typename _FormatContext::iterator,
				  _Sink_iter<_CharT>>)
	    if (__write_direct)
	      return __out;

	  auto __str = std::move(__sink).get();
	  return __format::__write_padded_as_spec(__str, __str.size(),
						  __fc, _M_spec);
	}

      _ChronoSpec<_CharT> _M_spec;

    private:
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

      // TODO: consider moving body of every operator<< into this function
      // and use std::format("{}", t) to implement those operators. That
      // would avoid std::format("{}", t) calling operator<< which calls
      // std::format again.
      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_format_to_ostream(const _Tp& __t, _FormatContext& __fc,
			     bool __is_neg) const
	{
	  using ::std::chrono::__detail::__utc_leap_second;
	  using ::std::chrono::__detail::__local_time_fmt;

	  if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    return _M_format_to_ostream(__t._M_time, __fc, false);
	  else
	    {
	      basic_ostringstream<_CharT> __os;
	      __os.imbue(_M_locale(__fc));

	      if constexpr (__is_specialization_of<_Tp, __utc_leap_second>)
		__os << __t._M_date << ' ' << __t._M_time;
	      else
		{
		  if (__is_neg) [[unlikely]]
		    __os << _S_plus_minus[1];
		  __os << __t;
		}

	      auto __str = std::move(__os).str();
	      return __format::__write_padded_as_spec(__str, __str.size(),
						      __fc, _M_spec);
	    }
	}

      static constexpr const _CharT* _S_chars
	= _GLIBCXX_WIDEN("0123456789+-:/ {}");
      static constexpr const _CharT* _S_plus_minus = _S_chars + 10;
      static constexpr _CharT _S_colon = _S_chars[12];
      static constexpr _CharT _S_slash = _S_chars[13];
      static constexpr _CharT _S_space = _S_chars[14];
      static constexpr const _CharT* _S_empty_spec = _S_chars + 15;

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_a_A(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, bool __full) const
	{
	  // %a Locale's abbreviated weekday name.
	  // %A Locale's full weekday name.
	  chrono::weekday __wd = _S_weekday(__t);
	  if (!__wd.ok())
	    __throw_format_error("format error: invalid weekday");

	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __days[7];
	  if (__full)
	    __tp._M_days(__days);
	  else
	    __tp._M_days_abbreviated(__days);
	  __string_view __str(__days[__wd.c_encoding()]);
	  return __format::__write(std::move(__out), __str);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_b_B(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, bool __full) const
	{
	  // %b Locale's abbreviated month name.
	  // %B Locale's full month name.
	  chrono::month __m = _S_month(__t);
	  if (!__m.ok())
	    __throw_format_error("format error: invalid month");
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __months[12];
	  if (__full)
	    __tp._M_months(__months);
	  else
	    __tp._M_months_abbreviated(__months);
	  __string_view __str(__months[(unsigned)__m - 1]);
	  return __format::__write(std::move(__out), __str);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_c(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %c  Locale's date and time representation.
	  // %Ec Locale's alternate date and time representation.

	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __formats[2];
	  __tp._M_date_time_formats(__formats);
	  const _CharT* __rep = __formats[__mod];
	  if (!*__rep)
	    __rep = _GLIBCXX_WIDEN("%a %b %e %H:%M:%S %Y");
	  basic_string<_CharT> __fmt(_S_empty_spec);
	  __fmt.insert(1u, 1u, _S_colon);
	  __fmt.insert(2u, __rep);
	  return std::vformat_to(std::move(__out), __loc, __fmt,
				 std::make_format_args<_FormatContext>(__t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_C_y_Y(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, char __conv, char __mod = 0) const
	{
	  // %C  Year divided by 100 using floored division.
	  // %EC Locale's alternative preresentation of the century (era name).
	  // %y  Last two decimal digits of the year.
	  // %OY Locale's alternative represenation.
	  // %Ey Locale's alternative representation of offset from %EC.
	  // %Y  Year as a decimal number.
	  // %EY Locale's alternative full year represenation.

	  chrono::year __y = _S_year(__t);

	  if (__mod == 'E')
	    {
	      // TODO: %EC, %Ey or %EY
	      // return __out;
	    }

	  basic_string<_CharT> __s;
	  int __yi = (int)__y;
	  const bool __is_neg = __yi < 0;
	  __yi = __builtin_abs(__yi);

	  if (__conv == 'Y' || __conv == 'C')
	    {
	      if (__is_neg)
		__s.assign(1, _S_plus_minus[1]);
	      int __ci = __yi / 100;
	      if (__ci >= 100) [[unlikely]]
		{
		  __s += std::format(_S_empty_spec, __ci / 100);
		  __ci %= 100;
		}
	      __s += _S_two_digits(__ci);
	    }

	  if (__conv == 'Y' || __conv == 'y')
	    __s += _S_two_digits(__yi % 100);

	  if (__mod == 'O') // %OY
	    _S_altnum(_M_locale(__ctx), __s, __is_neg);

	  return __format::__write(std::move(__out), __string_view(__s));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_D(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext&) const
	{
	  auto __ymd = _S_date(__t);
	  basic_string<_CharT> __s;
#if ! _GLIBCXX_USE_CXX11_ABI
	  __s.reserve(8);
#endif
	  __s = _S_two_digits((unsigned)__ymd.month());
	  __s += _S_slash;
	  __s += _S_two_digits((unsigned)__ymd.day());
	  __s += _S_slash;
	  __s += _S_two_digits(__builtin_abs((int)__ymd.year()) % 100);
	  return __format::__write(std::move(__out), __string_view(__s));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_e(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %e  Day of month as decimal number, padded with space.
	  // %Oe Locale's alternative digits.
	  chrono::day __d = _S_day(__t);
	  unsigned __i = (unsigned)__d;
	  auto __sv = _S_two_digits(__i);
	  basic_string<_CharT> __s;
	  if (__mod)
	    __sv = _S_altnum(_M_locale(__ctx), __s.assign(__sv));
	  if (__i < 10)
	    __sv = __s = {_S_space, __sv[1]};
	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_F(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext&) const
	{
	  auto __ymd = _S_date(__t);
	  basic_string<_CharT> __s;
#if ! _GLIBCXX_USE_CXX11_ABI
	  __s.reserve(11);
#endif
	  __s += std::format(_GLIBCXX_WIDEN("{:04d}-  -  "), (int)__ymd.year());
	  auto __sv = _S_two_digits((unsigned)__ymd.month());
	  __s[__s.size() - 5] = __sv[0];
	  __s[__s.size() - 4] = __sv[1];
	  __sv = _S_two_digits((unsigned)__ymd.day());
	  __s[__s.size() - 2] = __sv[0];
	  __s[__s.size() - 1] = __sv[1];
	  __sv = __s;
	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_g_G(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __full) const
	{
	  // %g last two decimal digits of the ISO week-based year.
	  // %G ISO week-based year.
	  using namespace chrono;
	  auto __d = _S_days(__t);
	  // Move to nearest Thursday:
	  __d -= (weekday(__d) - Monday) - days(3);
	  // ISO week-based year is the year that contains that Thursday:
	  year __y = year_month_day(__d).year();
	  return _M_C_y_Y(__y, std::move(__out), __ctx, "yY"[__full]);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_I(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  auto __hms = _S_hms(__t);
	  int __i = __hms.hours().count();
	  if (__i == 0)
	    __i = 12;
	  else if (__i > 12)
	    __i -= 12;
	  auto __sv = _S_two_digits(__i);
	  basic_string<_CharT> __s;
	  if (__mod)
	    __sv = _S_altnum(_M_locale(__ctx), __s.assign(__sv));
	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_j(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx) const
	{
	  if constexpr (chrono::__is_duration_v<_Tp>)
	    {
	      // Decimal number of days, without padding.
	      unsigned __d = chrono::duration_cast<chrono::days>(__t).count();
	      return std::format_to(std::move(__out), _S_empty_spec, __d);
	    }
	  else
	    {
	      // Day of the year as a decimal number, padding with zero.
	      using namespace chrono;
	      auto __day = _S_days(__t);
	      auto __ymd = _S_date(__t);
	      days __d;
	      // See "Calculating Ordinal Dates" at
	      // https://github.com/HowardHinnant/date/wiki/Examples-and-Recipes
	      if constexpr (is_same_v<typename decltype(__day)::clock, local_t>)
		__d = __day - local_days(__ymd.year()/January/0);
	      else
		__d = __day - sys_days(__ymd.year()/January/0);
	      return std::format_to(std::move(__out), _GLIBCXX_WIDEN("{:03d}"),
				    __d.count());
	    }
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_p(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx) const
	{
	  // %p The locale's equivalent of the AM/PM designations.
	  auto __hms = _S_hms(__t);
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __ampm[2];
	  __tp._M_am_pm(__ampm);
	  return std::format_to(std::move(__out), _S_empty_spec,
				__ampm[__hms.hours().count() >= 12]);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_q(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx) const
	{
	  // %q The duration's unit suffix
	  if constexpr (!chrono::__is_duration_v<_Tp>)
	    __throw_format_error("format error: argument is not a duration");
	  else
	    {
	      using period = typename _Tp::period;
	      char __buf[sizeof("[/]s") + 2 * numeric_limits<intmax_t>::digits10];
	      constexpr size_t __n = sizeof(__buf);
	      auto __s = chrono::__detail::__units_suffix<period, _CharT>(__buf,
									  __n);
	      if constexpr (is_same_v<decltype(__s), const _CharT*>)
		return std::format_to(std::move(__out), _S_empty_spec, __s);
	      else
		{
		  // Suffix was written to __buf as narrow string.
		  _CharT __wbuf[__n];
		  size_t __len = __builtin_strlen(__buf);
		  locale __loc = _M_locale(__ctx);
		  auto& __ct = use_facet<ctype<_CharT>>(__loc);
		  __ct.widen(__buf, __len, __wbuf);
		  __wbuf[__len] = 0;
		  return std::format_to(std::move(__out), _S_empty_spec,
					__wbuf);
		}
	    }
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_r(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx) const
	{
	  // %r locale's 12-hour clock time.
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __ampm_fmt;
	  __tp._M_am_pm_format(&__ampm_fmt);
	  basic_string<_CharT> __fmt(_S_empty_spec);
	  __fmt.insert(1u, 1u, _S_colon);
	  __fmt.insert(2u, __ampm_fmt);
	  return std::vformat_to(std::move(__out), __fmt,
				 std::make_format_args<_FormatContext>(__t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_R_T(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, bool __secs) const
	{
	  // %R Equivalent to %H:%M
	  // %T Equivalent to %H:%M:%S
	  auto __hms = _S_hms(__t);

	  basic_string<_CharT> __s;
#if ! _GLIBCXX_USE_CXX11_ABI
	  __s.reserve(11);
#endif
	  __s = std::format(_GLIBCXX_WIDEN("{:02d}:00"), __hms.hours().count());
	  auto __sv = _S_two_digits(__hms.minutes().count());
	  __s[__s.size() - 2] = __sv[0];
	  __s[__s.size() - 1] = __sv[1];
	  __sv = __s;
	  __out = __format::__write(std::move(__out), __sv);
	  if (__secs)
	    {
	      *__out++ = _S_colon;
	      __out = _M_S(__hms, std::move(__out), __ctx);
	    }
	  return __out;
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_S(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %S  Seconds as a decimal number.
	  // %OS (TODO) The locale's alternative representation.
	  auto __hms = _S_hms(__t);
	  __out = _S_dd_zero_fill(__hms.seconds().count(),
				  std::move(__out), __ctx, __mod);
	  using rep = typename decltype(__hms)::precision::rep;
	  if constexpr (__hms.fractional_width != 0)
	    {
	      locale __loc = _M_locale(__ctx);
	      auto __ss = __hms.subseconds();
	      if constexpr (is_floating_point_v<rep>)
		{
		  __out = std::format_to(__loc, std::move(__out),
					 _GLIBCXX_WIDEN("{:.{}Lg}"),
					 __ss.count(),
					 __hms.fractional_width);
		}
	      else if constexpr (is_integral_v<rep>)
		{
		  const auto& __np
		    = use_facet<numpunct<_CharT>>(__loc);
		  __out = std::format_to(std::move(__out),
					 _GLIBCXX_WIDEN("{}{:0{}}"),
					 __np.decimal_point(),
					 __ss.count(),
					 __hms.fractional_width);
		}
	      else
		{
		  const auto& __np
		    = use_facet<numpunct<_CharT>>(__loc);
		  *__out++ = __np.decimal_point();
		  auto __str = std::format(_S_empty_spec, __ss.count());
		  __out = std::format_to(_GLIBCXX_WIDEN("{:0>{}s}"),
							__str,
							__hms.fractional_width);
		}
	    }
	  return __out;
	}

      // %t handled in _M_format

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_u_w(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, _CharT __conv, bool __mod = false) const
	{
	  // %u  ISO weekday as a decimal number (1-7), where Monday is 1.
	  // %Ou Locale's alternative numeric rep.
	  // %w  Weekday as a decimal number (0-6), where Sunday is 0.
	  // %Ow Locale's alternative numeric rep.
	  chrono::weekday __wd = _S_weekday(__t);
	  unsigned __wdi = __conv == 'u' ? __wd.iso_encoding()
					 : __wd.c_encoding();
	  basic_string<_CharT> __s(1, _S_digit(__wdi));
	  if (__mod)
	    _S_altnum(_M_locale(__ctx), __s);
	  return __format::__write(std::move(__out), __string_view(__s));
	  return __out;
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_U_V_W(const _Tp& __t, typename _FormatContext::iterator __out,
		 _FormatContext& __ctx, _CharT __conv, bool __mod = false) const
	{
	  // %U  Week number of the year as a decimal number, from first Sunday.
	  // %OU Locale's alternative numeric rep.
	  // %V  ISO week-based week number as a decimal number.
	  // %OV Locale's alternative numeric rep.
	  // %W  Week number of the year as a decimal number, from first Monday.
	  // %OW Locale's alternative numeric rep.
	  using namespace chrono;
	  auto __d = _S_days(__t);
	  using _TDays = decltype(__d); // Either sys_days or local_days.

	  _TDays __first; // First day of week 1.
	  if (__conv == 'V') // W01 begins on Monday before first Thursday.
	    {
	      // Move to nearest Thursday:
	      __d -= (weekday(__d) - Monday) - days(3);
	      // ISO week of __t is number of weeks since January 1 of the
	      // same year as that nearest Thursday.
	      __first = _TDays(year_month_day(__d).year()/January/1);
	    }
	  else
	    {
	      year __y;
	      if constexpr (requires { __t.year(); })
		__y = __t.year();
	      else
		__y = year_month_day(__d).year();
	      const weekday __weekstart = __conv == 'U' ? Sunday : Monday;
	      __first = _TDays(__y/January/__weekstart[1]);
	    }
	  auto __weeks = chrono::floor<weeks>(__d - __first);
	  __string_view __sv = _S_two_digits(__weeks.count() + 1);
	  basic_string<_CharT> __s;
	  if (__mod)
	    __sv = _S_altnum(_M_locale(__ctx), __s.assign(__sv));
	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_x(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %x  Locale's date rep
	  // %Ex Locale's alternative date representation.
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __date_reps[2];
	  __tp._M_date_formats(__date_reps);
	  const _CharT* __rep = __date_reps[__mod];
	  if (!*__rep)
	    return _M_D(__t, std::move(__out), __ctx);

	  basic_string<_CharT> __fmt(_S_empty_spec);
	  __fmt.insert(1u, 1u, _S_colon);
	  __fmt.insert(2u, __rep);
	  return std::vformat_to(std::move(__out), __fmt,
				 std::make_format_args<_FormatContext>(__t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_X(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %X  Locale's time rep
	  // %EX Locale's alternative time representation.
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __time_reps[2];
	  __tp._M_time_formats(__time_reps);
	  const _CharT* __rep = __time_reps[__mod];
	  if (!*__rep)
	    return _M_R_T(__t, std::move(__out), __ctx, true);

	  basic_string<_CharT> __fmt(_S_empty_spec);
	  __fmt.insert(1u, 1u, _S_colon);
	  __fmt.insert(2u, __rep);
	  return std::vformat_to(std::move(__out), __fmt,
				 std::make_format_args<_FormatContext>(__t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_z(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  using ::std::chrono::__detail::__utc_leap_second;
	  using ::std::chrono::__detail::__local_time_fmt;

	  auto __utc = __mod ? __string_view(_GLIBCXX_WIDEN("+00:00"), 6)
			     : __string_view(_GLIBCXX_WIDEN("+0000"), 5);

	  if constexpr (chrono::__is_time_point_v<_Tp>)
	    {
	      if constexpr (is_same_v<typename _Tp::clock,
				      chrono::system_clock>)
		return __format::__write(std::move(__out), __utc);
	    }
	  else if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    {
	      if (__t._M_offset_sec)
		{
		  auto __sv = __utc;
		  basic_string<_CharT> __s;
		  if (*__t._M_offset_sec != 0s)
		    {
		      chrono:: hh_mm_ss __hms(*__t._M_offset_sec);
		      __s = _S_plus_minus[__hms.is_negative()];
		      __s += _S_two_digits(__hms.hours().count());
		      if (__mod)
			__s += _S_colon;
		      __s += _S_two_digits(__hms.minutes().count());
		      __sv = __s;
		    }
		  return __format::__write(std::move(__out), __sv);
		}
	    }
	  else if constexpr (__is_specialization_of<_Tp, __utc_leap_second>)
	    return __format::__write(std::move(__out), __utc);

	  __no_timezone_available();
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_Z(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx) const
	{
	  using ::std::chrono::__detail::__utc_leap_second;
	  using ::std::chrono::__detail::__local_time_fmt;

	  __string_view __utc(_GLIBCXX_WIDEN("UTC"), 3);
	  if constexpr (chrono::__is_time_point_v<_Tp>)
	    {
	      if constexpr (is_same_v<typename _Tp::clock,
				      chrono::system_clock>)
		return __format::__write(std::move(__out), __utc);
	    }
	  else if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    {
	      if (__t._M_abbrev)
		{
		  __string_view __wsv;
		  if constexpr (is_same_v<_CharT, char>)
		    __wsv = *__t._M_abbrev;
		  else
		    {
		      string_view __sv = *__t._M_abbrev;
		      basic_string<_CharT> __ws(__sv.size());
		      auto& __ct = use_facet<ctype<_CharT>>(_M_locale(__ctx));
		      __ct.widen(__sv.data(), __sv.size(), __ws.data());
		      __wsv = __ws;
		    }
		  return __format::__write(std::move(__out), __wsv);
		}
	    }
	  else if constexpr (__is_specialization_of<_Tp, __utc_leap_second>)
	    return __format::__write(std::move(__out), __utc);

	  __no_timezone_available();
	}

      // %% handled in _M_format

      // A single digit character in the range '0'..'9'.
      static _CharT
      _S_digit(int __n) noexcept
      {
	// Extra 9s avoid past-the-end read on bad input.
	return _GLIBCXX_WIDEN("0123456789999999")[__n & 0xf];
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

      // Convert a numeric string to the locale's alternative numeric symbols.
      static basic_string_view<_CharT>
      _S_altnum(const locale& __loc, basic_string<_CharT>& __s,
		bool __is_neg = false)
      {
	if (__loc == locale::classic())
	  return __s;

#if 0 // TODO how can we access numpunct_cache?! Need to go via std::time_put?
	auto& __np = use_facet<__numpunct_cache<_CharT>>(__loc);
	auto __nums = __np._M_atoms_out; // alts for "-+xX01234..."
	if (__is_neg)
	  __s[0] = __nums[0];
	__nums += 4; // now points to alternate digits
	for (int __i = __is_neg; __i < __s.size(); ++__i)
	  __s[__i] = __nums[__s[__i] - '0'];
#endif
	return __s;
      }

      // Write two digits, zero-filled.
      template<typename _FormatContext>
	typename _FormatContext::iterator
	_S_dd_zero_fill(int __val, typename _FormatContext::iterator __out,
			_FormatContext& __ctx, bool __alt_num) const
	{
	  auto __sv = _S_two_digits(__val);
	  basic_string<_CharT> __s;
	  if (__alt_num)
	    __sv = _S_altnum(_M_locale(__ctx), __s.assign(__sv));
	  return __format::__write(std::move(__out), __sv);
	}

      // Accessors for the components of chrono types:

      // Returns a hh_mm_ss.
      template<typename _Tp>
	static decltype(auto)
	_S_hms(const _Tp& __t)
	{
	  using ::std::chrono::__detail::__utc_leap_second;
	  using ::std::chrono::__detail::__local_time_fmt;

	  if constexpr (__is_specialization_of<_Tp, chrono::hh_mm_ss>)
	    return __t;
	  else if constexpr (__is_specialization_of<_Tp, __utc_leap_second>)
	    return __t._M_time;
	  else if constexpr (chrono::__is_duration_v<_Tp>)
	    return chrono::hh_mm_ss<_Tp>(__t);
	  else if constexpr (chrono::__is_time_point_v<_Tp>)
	    return chrono::hh_mm_ss(__t - chrono::floor<chrono::days>(__t));
	  else if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    return _S_hms(__t._M_time);
	  else
	    {
	      __invalid_chrono_spec();
	      return chrono::hh_mm_ss<chrono::seconds>();
	    }
	}

      // Returns a sys_days or local_days.
      template<typename _Tp>
	static auto
	_S_days(const _Tp& __t)
	{
	  using namespace chrono;
	  using ::std::chrono::__detail::__utc_leap_second;
	  using ::std::chrono::__detail::__local_time_fmt;

	  if constexpr (__is_time_point_v<_Tp>)
	    return chrono::floor<days>(__t);
	  else if constexpr (__is_specialization_of<_Tp, __utc_leap_second>)
	    return __t._M_date;
	  else if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    return chrono::floor<days>(__t._M_time);
	  else if constexpr (is_same_v<_Tp, year_month_day>
			       || is_same_v<_Tp, year_month_day_last>
			       || is_same_v<_Tp, year_month_weekday>
			       || is_same_v<_Tp, year_month_weekday_last>)
	    return sys_days(__t);
	  else
	    {
	      if constexpr (__is_duration_v<_Tp>)
		__not_valid_for_duration();
	      else
		__invalid_chrono_spec();
	      return chrono::sys_days();
	    }
	}

      // Returns a year_month_day.
      template<typename _Tp>
	static chrono::year_month_day
	_S_date(const _Tp& __t)
	{
	  if constexpr (is_same_v<_Tp, chrono::year_month_day>)
	    return __t;
	  else
	    return chrono::year_month_day(_S_days(__t));
	}

      template<typename _Tp>
	static chrono::day
	_S_day(const _Tp& __t)
	{
	  using namespace chrono;

	  if constexpr (is_same_v<_Tp, day>)
	    return __t;
	  else if constexpr (requires { __t.day(); })
	    return __t.day();
	  else
	    return _S_date(__t).day();
	}

      template<typename _Tp>
	static chrono::month
	_S_month(const _Tp& __t)
	{
	  using namespace chrono;

	  if constexpr (is_same_v<_Tp, month>)
	    return __t;
	  else if constexpr (requires { __t.month(); })
	    return __t.month();
	  else
	    return _S_date(__t).month();
	}

      template<typename _Tp>
	static chrono::year
	_S_year(const _Tp& __t)
	{
	  using namespace chrono;

	  if constexpr (is_same_v<_Tp, year>)
	    return __t;
	  else if constexpr (requires { __t.year(); })
	    return __t.year();
	  else
	    return _S_date(__t).year();
	}

      template<typename _Tp>
	static chrono::weekday
	_S_weekday(const _Tp& __t)
	{
	  using namespace ::std::chrono;
	  using ::std::chrono::__detail::__local_time_fmt;

	  if constexpr (is_same_v<_Tp, weekday>)
	    return __t;
	  else if constexpr (requires { __t.weekday(); })
	    return __t.weekday();
	  else if constexpr (is_same_v<_Tp, month_weekday>)
	    return __t.weekday_indexed().weekday();
	  else if constexpr (is_same_v<_Tp, month_weekday_last>)
	    return __t.weekday_last().weekday();
	  else
	    return weekday(_S_days(__t));
	}
    };

} // namespace __format
/// @endcond

  template<typename _Rep, typename _Period, typename _CharT>
    struct formatter<chrono::duration<_Rep, _Period>, _CharT>
    {
      constexpr typename basic_format_parse_context<_CharT>::iterator
      parse(basic_format_parse_context<_CharT>& __pc)
      {
	using namespace __format;
	auto __it = _M_f._M_parse(__pc, _Duration|_TimeOfDay);
	if constexpr (!is_floating_point_v<_Rep>)
	  if (_M_f._M_spec._M_prec_kind != __format::_WP_none)
	    __throw_format_error("format error: invalid precision for duration");
	return __it;
      }

      template<typename _Out>
	typename basic_format_context<_Out, _CharT>::iterator
	format(const chrono::duration<_Rep, _Period>& __d,
	       basic_format_context<_Out, _CharT>& __fc) const
	{
	  return _M_f._M_format(chrono::abs(__d), __fc, __d < __d.zero());
	}

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::day, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Day); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::day& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::month, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Month); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::month& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::year, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Year); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::year& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::weekday, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Weekday); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::weekday& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::weekday_indexed, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Weekday); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::weekday_indexed& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::weekday_last, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Weekday); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::weekday_last& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::month_day, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Month|__format::_Day); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::month_day& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::month_day_last, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Month|__format::_Day); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::month_day_last& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::month_weekday, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Month|__format::_Weekday); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::month_weekday& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::month_weekday_last, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Month|__format::_Weekday); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::month_weekday_last& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::year_month, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Year|__format::_Month); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::year_month& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::year_month_day, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Date); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::year_month_day& __t, _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::year_month_day_last, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Date); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::year_month_day_last& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::year_month_weekday, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Date); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::year_month_weekday& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::year_month_weekday_last, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_Date); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::year_month_weekday_last& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Rep, typename _Period, typename _CharT>
    struct formatter<chrono::hh_mm_ss<chrono::duration<_Rep, _Period>>, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_TimeOfDay); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::hh_mm_ss<chrono::duration<_Rep, _Period>>& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  template<typename _CharT>
    struct formatter<chrono::sys_info, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ChronoParts{}); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::sys_info& __i, _FormatContext& __fc) const
	{ return _M_f._M_format(__i, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _CharT>
    struct formatter<chrono::local_info, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ChronoParts{}); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::local_info& __i, _FormatContext& __fc) const
	{ return _M_f._M_format(__i, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };
#endif

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::sys_time<_Duration>, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ZonedDateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::sys_time<_Duration>& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::utc_time<_Duration>, _CharT>
    : __format::__formatter_chrono<_CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ZonedDateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::utc_time<_Duration>& __t,
	       _FormatContext& __fc) const
	{
	  // Adjust by removing leap seconds to get equivalent sys_time.
	  // We can't just use clock_cast because we want to know if the time
	  // falls within a leap second insertion, and format seconds as "60".
	  using chrono::__detail::__utc_leap_second;
	  using chrono::seconds;
	  using chrono::sys_time;
	  using _CDur = common_type_t<_Duration, seconds>;
	  const auto __li = chrono::get_leap_second_info(__t);
	  sys_time<_CDur> __s{__t.time_since_epoch() - __li.elapsed};
	  if (!__li.is_leap_second) [[likely]]
	    return _M_f._M_format(__s, __fc);
	  else
	    return _M_f._M_format(__utc_leap_second(__s), __fc);
	}

    private:
      friend formatter<chrono::__detail::__utc_leap_second<_Duration>, _CharT>;

      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::tai_time<_Duration>, _CharT>
    : __format::__formatter_chrono<_CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ZonedDateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::tai_time<_Duration>& __t,
	       _FormatContext& __fc) const
	{
	  // Convert to __local_time_fmt with abbrev "TAI" and offset 0s.

	  // Offset is 1970y/January/1 - 1958y/January/1
	  constexpr chrono::days __tai_offset = chrono::days(4383);
	  using _CDur = common_type_t<_Duration, chrono::days>;
	  chrono::local_time<_CDur> __lt(__t.time_since_epoch() - __tai_offset);
	  const string __abbrev("TAI", 3);
	  const chrono::seconds __off = 0s;
	  const auto __lf = chrono::local_time_format(__lt, &__abbrev, &__off);
	  return _M_f._M_format(__lf, __fc);
	}

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::gps_time<_Duration>, _CharT>
    : __format::__formatter_chrono<_CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ZonedDateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::gps_time<_Duration>& __t,
	       _FormatContext& __fc) const
	{
	  // Convert to __local_time_fmt with abbrev "GPS" and offset 0s.

	  // Offset is 1980y/January/Sunday[1] - 1970y/January/1
	  constexpr chrono::days __gps_offset = chrono::days(3657);
	  using _CDur = common_type_t<_Duration, chrono::days>;
	  chrono::local_time<_CDur> __lt(__t.time_since_epoch() + __gps_offset);
	  const string __abbrev("GPS", 3);
	  const chrono::seconds __off = 0s;
	  const auto __lf = chrono::local_time_format(__lt, &__abbrev, &__off);
	  return _M_f._M_format(__lf, __fc);
	}

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::file_time<_Duration>, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ZonedDateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::file_time<_Duration>& __t,
	       _FormatContext& __ctx) const
	{
	  using namespace chrono;
	  return _M_f._M_format(chrono::clock_cast<system_clock>(__t), __ctx);
	}

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::local_time<_Duration>, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_DateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::local_time<_Duration>& __t,
	       _FormatContext& __ctx) const
	{ return _M_f._M_format(__t, __ctx); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

  template<typename _Duration, typename _CharT>
    struct formatter<chrono::__detail::__local_time_fmt<_Duration>, _CharT>
    {
      template<typename _ParseContext>
	constexpr typename _ParseContext::iterator
	parse(_ParseContext& __pc)
	{ return _M_f._M_parse(__pc, __format::_ZonedDateTime); }

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::__detail::__local_time_fmt<_Duration>& __t,
	       _FormatContext& __ctx) const
	{ return _M_f._M_format(__t, __ctx); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  template<typename _Duration, typename _TimeZonePtr, typename _CharT>
    struct formatter<chrono::zoned_time<_Duration, _TimeZonePtr>, _CharT>
    : formatter<chrono::__detail::__local_time_fmt<_Duration>, _CharT>
    {
      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::zoned_time<_Duration, _TimeZonePtr>& __tp,
	       _FormatContext& __ctx) const
	{
	  using chrono::__detail::__local_time_fmt;
	  using _Base = formatter<__local_time_fmt<_Duration>, _CharT>;
	  const chrono::sys_info __info = __tp.get_info();
	  const auto __lf = chrono::local_time_format(__tp.get_local_time(),
						      &__info.abbrev,
						      &__info.offset);
	  return _Base::format(__lf, __ctx);
	}
    };
#endif

  // Partial specialization needed for %c formatting of __utc_leap_second.
  template<typename _Duration, typename _CharT>
    struct formatter<chrono::__detail::__utc_leap_second<_Duration>, _CharT>
    : formatter<chrono::utc_time<_Duration>, _CharT>
    {
      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::__detail::__utc_leap_second<_Duration>& __t,
	       _FormatContext& __fc) const
	{ return this->_M_f._M_format(__t, __fc); }
    };

namespace chrono
{
/// @addtogroup chrono
/// @{

  // TODO: from_stream for duration
#if 0
  template<typename _CharT, typename _Traits, typename _Rep, typename _Period,
	   typename _Alloc = allocator<_CharT>>
    basic_istream<_CharT, _Traits>&
    from_stream(basic_istream<_CharT, _Traits>& __is, const _CharT* __fmt,
		duration<_Rep, _Period>& __d,
		basic_string<_CharT, _Traits, _Alloc>* __abbrev = nullptr,
		minutes* __offset = nullptr)
    {
    }
#endif

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const day& __d)
    {
      using _Ctx = __conditional_t<is_same_v<_CharT, char>,
				   format_context, wformat_context>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:02d} is not a valid day");
      if (__d.ok())
	__s = __s.substr(0, 6);
      __os << std::vformat(__s, make_format_args<_Ctx>((unsigned)__d));
      return __os;
    }

  // TODO from_stream for day

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const month& __m)
    {
      using _Ctx = __conditional_t<is_same_v<_CharT, char>,
				   format_context, wformat_context>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:L%b}{} is not a valid month");
      if (__m.ok())
	__os << std::vformat(__os.getloc(), __s.substr(0, 6),
			     make_format_args<_Ctx>(__m));
      else
	__os << std::vformat(__s.substr(6),
			     make_format_args<_Ctx>((unsigned)__m));
      return __os;
    }

  // TODO from_stream for month

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const year& __y)
    {
      using _Ctx = __conditional_t<is_same_v<_CharT, char>,
				   format_context, wformat_context>;
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

  // TODO from_stream for year

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const weekday& __wd)
    {
      using _Ctx = __conditional_t<is_same_v<_CharT, char>,
				   format_context, wformat_context>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:L%a}{} is not a valid weekday");
      if (__wd.ok())
	__os << std::vformat(__os.getloc(), __s.substr(0, 6),
			     make_format_args<_Ctx>(__wd));
      else
	__os << std::vformat(__s.substr(6),
			     make_format_args<_Ctx>(__wd.c_encoding()));
      return __os;
    }

  // TODO from_stream for weekday

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
      if constexpr (is_same_v<_CharT, char>)
	__os2 << std::format("[{}", __i);
      else
	__os2 << std::format(L"[{}", __i);
      basic_string_view<_CharT> __s = _GLIBCXX_WIDEN(" is not a valid index]");
      if (__i >= 1 && __i <= 5)
	__os2 << __s.back();
      else
	__os2 << __s;
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

  // TODO from_stream for month_day

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const month_day_last& __mdl)
    {
      // As above, just write straight to a stringstream, as if by "{:L}/last"
      basic_stringstream<_CharT> __os2;
      __os2.imbue(__os.getloc());
      __os2 << __mdl.month();
      if constexpr (is_same_v<_CharT, char>)
	__os2 << "/last";
      else
	__os2 << L"/last";
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

  // TODO from_stream for year_month

  template<typename _CharT, typename _Traits>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const year_month_day& __ymd)
    {
      using _Ctx = __conditional_t<is_same_v<_CharT, char>,
				   format_context, wformat_context>;
      using _Str = basic_string_view<_CharT>;
      _Str __s = _GLIBCXX_WIDEN("{:%F} is not a valid date");
      __os << std::vformat(__ymd.ok() ? __s.substr(0, 5) : __s,
			   make_format_args<_Ctx>(__ymd));
      return __os;
    }

  // TODO from_stream for year_month_day

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
      __os << '[' << __i.begin << ',' << __i.end
	   << ',' << hh_mm_ss(__i.offset) << ',' << __i.save
	   << ',' << __i.abbrev << ']';
      return __os;
    }

  /// Writes a local_info object to an ostream in an unspecified format.
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os, const local_info& __li)
    {
      __os << '[';
      if (__li.result == local_info::unique)
	__os << __li.first;
      else
	{
	  if (__li.result == local_info::nonexistent)
	    __os << "nonexistent";
	  else
	    __os << "ambiguous";
	  __os << " local time between " << __li.first;
	  __os << " and " << __li.second;
	}
      __os << ']';
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

  // TODO: from_stream for sys_time

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const utc_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  // TODO: from_stream for utc_time

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const tai_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  // TODO: from_stream for tai_time

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const gps_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  // TODO: from_stream for gps_time


  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const file_time<_Duration>& __t)
    {
      __os << std::format(__os.getloc(), _GLIBCXX_WIDEN("{:L%F %T}"), __t);
      return __os;
    }

  // TODO: from_stream for file_time

  template<typename _CharT, typename _Traits, typename _Duration>
    inline basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __os,
	       const local_time<_Duration>& __lt)
    {
      __os << sys_time<_Duration>{__lt.time_since_epoch()};
      return __os;
    }

  // TODO: from_stream for local_time
#undef _GLIBCXX_WIDEN

  /// @} group chrono
} // namespace chrono

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++20

#endif //_GLIBCXX_CHRONO_IO_H
