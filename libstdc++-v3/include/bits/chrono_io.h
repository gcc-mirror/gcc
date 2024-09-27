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
#include <iomanip> // setw, setfill
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

      // Use one of the reserved bits in __format::_Spec<C>.
      // This indicates that a locale-dependent conversion specifier such as
      // %a is used in the chrono-specs. This is not the same as the
      // _Spec<C>::_M_localized member which indicates that "L" was present
      // in the format-spec, e.g. "{:L%a}" is localized and locale-specific,
      // but "{:L}" is only localized and "{:%a}" is only locale-specific.
      constexpr bool
      _M_locale_specific() const noexcept
      { return this->_M_reserved; }

      constexpr void
      _M_locale_specific(bool __b) noexcept
      { this->_M_reserved = __b; }
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
  operator|(_ChronoParts __x, _ChronoParts __y) noexcept
  { return static_cast<_ChronoParts>((int)__x | (int)__y); }

  constexpr _ChronoParts&
  operator|=(_ChronoParts& __x, _ChronoParts __y) noexcept
  { return __x = __x | __y; }

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
	  bool __locale_specific = false;

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
		  __locale_specific = true;
		  break;
		case 'b':
		case 'h':
		case 'B':
		  __needed = _Month;
		  __locale_specific = true;
		  break;
		case 'c':
		  __needed = _DateTime;
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
		  __locale_specific = true;
		  [[fallthrough]];
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
		  __locale_specific = true;
		  __allowed_mods = _Mod_E;
		  break;
		case 'X':
		  __needed = _TimeOfDay;
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
		  __needed = _TimeZone;
		  __allowed_mods = _Mod_E_O;
		  break;
		case 'Z':
		  __needed = _TimeZone;
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
		  __throw_format_error("chrono format error: invalid "
				       " specifier in chrono-specs");
		}

	      if ((__mod == 'E' && !(__allowed_mods & _Mod_E))
		    || (__mod == 'O' && !(__allowed_mods & _Mod_O)))
		__throw_format_error("chrono format error: invalid "
				     " modifier in chrono-specs");
	      if (__mod && __c != 'z')
		__locale_specific = true;
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
	  _M_spec._M_chrono_specs
		 = __string_view(__chrono_specs, __first - __chrono_specs);
	  _M_spec._M_locale_specific(__locale_specific);

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
	  auto __first = _M_spec._M_chrono_specs.begin();
	  const auto __last = _M_spec._M_chrono_specs.end();
	  if (__first == __last)
	    return _M_format_to_ostream(__t, __fc, __is_neg);

#if defined _GLIBCXX_USE_NL_LANGINFO_L && __CHAR_BIT__ == 8
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 3565. Handling of encodings in localized formatting
	  //       of chrono types is underspecified
	  if constexpr (is_same_v<_CharT, char>)
	    if constexpr (__unicode::__literal_encoding_is_utf8())
	      if (_M_spec._M_localized && _M_spec._M_locale_specific())
		{
		  extern locale __with_encoding_conversion(const locale&);

		  // Allocate and cache the necessary state to convert strings
		  // in the locale's encoding to UTF-8.
		  locale __loc = __fc.locale();
		  if (__loc != locale::classic())
		    __fc._M_loc =  __with_encoding_conversion(__loc);
		}
#endif

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

	  // formatter<duration> passes the correct value of __is_neg
	  // for durations but for hh_mm_ss we decide it here.
	  if constexpr (__is_specialization_of<_Tp, chrono::hh_mm_ss>)
	    __is_neg = __t.is_negative();

	  auto __print_sign = [&__is_neg, &__out] {
	    if constexpr (chrono::__is_duration_v<_Tp>
			    || __is_specialization_of<_Tp, chrono::hh_mm_ss>)
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
		case 'e':
		  __out = _M_d_e(__t, std::move(__out), __fc, __c, __mod == 'O');
		  break;
		case 'D':
		  __out = _M_D(__t, std::move(__out), __fc);
		  break;
		case 'F':
		  __out = _M_F(__t, std::move(__out), __fc);
		  break;
		case 'g':
		case 'G':
		  __out = _M_g_G(__t, std::move(__out), __fc, __c == 'G');
		  break;
		case 'H':
		case 'I':
		  __out = _M_H_I(__t, __print_sign(), __fc, __c, __mod == 'O');
		  break;
		case 'j':
		  __out = _M_j(__t, __print_sign(), __fc);
		  break;
		case 'm':
		  __out = _M_m(__t, std::move(__out), __fc, __mod == 'O');
		  break;
		case 'M':
		  __out = _M_M(__t, __print_sign(), __fc, __mod == 'O');
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
		    // _GLIBCXX_RESOLVE_LIB_DEFECTS
		    // 4118. How should duration formatters format custom rep?
		    __out = std::format_to(__print_sign(), _S_empty_spec,
					   +__t.count());
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

      // Format for empty chrono-specs, e.g. "{}" (C++20 [time.format] p6).
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

	  basic_ostringstream<_CharT> __os;
	  __os.imbue(_M_locale(__fc));

	  if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    {
	      // Format as "{:L%F %T}"
	      auto __days = chrono::floor<chrono::days>(__t._M_time);
	      __os << chrono::year_month_day(__days) << ' '
		   << chrono::hh_mm_ss(__t._M_time - __days);

	      // For __local_time_fmt the __is_neg flags says whether to
	      // append " %Z" to the result.
	      if (__is_neg)
		{
		  if (!__t._M_abbrev) [[unlikely]]
		    __format::__no_timezone_available();
		  else if constexpr (is_same_v<_CharT, char>)
		    __os << ' ' << *__t._M_abbrev;
		  else
		    {
		      __os << L' ';
		      for (char __c : *__t._M_abbrev)
			__os << __c;
		    }
		}
	    }
	  else
	    {
	      if constexpr (__is_specialization_of<_Tp, __utc_leap_second>)
		__os << __t._M_date << ' ' << __t._M_time;
	      else if constexpr (chrono::__is_time_point_v<_Tp>)
		{
		  // Need to be careful here because not all specializations
		  // of chrono::sys_time can be written to an ostream.
		  // For the specializations of time_point that can be
		  // formatted with an empty chrono-specs, either it's a
		  // sys_time with period greater or equal to days:
		  if constexpr (is_convertible_v<_Tp, chrono::sys_days>)
		    __os << _S_date(__t);
		  else // Or it's formatted as "{:L%F %T}":
		    {
		      auto __days = chrono::floor<chrono::days>(__t);
		      __os << chrono::year_month_day(__days) << ' '
			 << chrono::hh_mm_ss(__t - __days);
		    }
		}
	      else
		{
		  if constexpr (chrono::__is_duration_v<_Tp>)
		    if (__is_neg) [[unlikely]]
		      __os << _S_plus_minus[1];
		  __os << __t;
		}
	    }

	  auto __str = std::move(__os).str();
	  return __format::__write_padded_as_spec(__str, __str.size(),
						  __fc, _M_spec);
	}

      static constexpr const _CharT* _S_chars
	= _GLIBCXX_WIDEN("0123456789+-:/ {}");
      static constexpr const _CharT* _S_plus_minus = _S_chars + 10;
      static constexpr _CharT _S_colon = _S_chars[12];
      static constexpr _CharT _S_slash = _S_chars[13];
      static constexpr _CharT _S_space = _S_chars[14];
      static constexpr const _CharT* _S_empty_spec = _S_chars + 15;

      template<typename _OutIter>
	_OutIter
	_M_write(_OutIter __out, const locale& __loc, __string_view __s) const
	{
#if defined _GLIBCXX_USE_NL_LANGINFO_L && __CHAR_BIT__ == 8
	  __sso_string __buf;
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 3565. Handling of encodings in localized formatting
	  //       of chrono types is underspecified
	  if constexpr (is_same_v<_CharT, char>)
	    if constexpr (__unicode::__literal_encoding_is_utf8())
	      if (_M_spec._M_localized && _M_spec._M_locale_specific()
		    && __loc != locale::classic())
		{
		  extern string_view
		  __locale_encoding_to_utf8(const locale&, string_view, void*);

		  __s = __locale_encoding_to_utf8(__loc, __s, &__buf);
		}
#endif
	  return __format::__write(std::move(__out), __s);
	}

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
	  return _M_write(std::move(__out), __loc, __str);
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
	  return _M_write(std::move(__out), __loc, __str);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_c(const _Tp& __tt, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %c  Locale's date and time representation.
	  // %Ec Locale's alternate date and time representation.

	  auto __t = _S_floor_seconds(__tt);
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __formats[2];
	  __tp._M_date_time_formats(__formats);
	  const _CharT* __rep = __formats[__mod];
	  if (!*__rep) [[unlikely]]
	    __rep = _GLIBCXX_WIDEN("%a %b %e %T %Y");
	  basic_string<_CharT> __fmt(_S_empty_spec);
	  __fmt.insert(1u, 1u, _S_colon);
	  __fmt.insert(2u, __rep);
	  return std::vformat_to(std::move(__out), __loc, __fmt,
				 std::make_format_args<_FormatContext>(__t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_C_y_Y(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, _CharT __conv, _CharT __mod = 0) const
	{
	  // %C  Year divided by 100 using floored division.
	  // %EC Locale's alternative preresentation of the century (era name).
	  // %y  Last two decimal digits of the year.
	  // %Oy Locale's alternative representation.
	  // %Ey Locale's alternative representation of offset from %EC.
	  // %Y  Year as a decimal number.
	  // %EY Locale's alternative full year representation.

	  chrono::year __y = _S_year(__t);

	  if (__mod && _M_spec._M_localized) [[unlikely]]
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		struct tm __tm{};
		__tm.tm_year = (int)__y - 1900;
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     __conv, __mod);
	      }

	  basic_string<_CharT> __s;
	  int __yi = (int)__y;
	  const bool __is_neg = __yi < 0;
	  __yi = __builtin_abs(__yi);

	  if (__conv == 'Y' || __conv == 'C')
	    {
	      int __ci = __yi / 100;
	      if (__is_neg) [[unlikely]]
		{
		  __s.assign(1, _S_plus_minus[1]);
		  // For floored division -123//100 is -2 and -100//100 is -1
		  if (__conv == 'C' && (__ci * 100) != __yi)
		    ++__ci;
		}
	      if (__ci >= 100) [[unlikely]]
		{
		  __s += std::format(_S_empty_spec, __ci / 100);
		  __ci %= 100;
		}
	      __s += _S_two_digits(__ci);
	    }

	  if (__conv == 'Y' || __conv == 'y')
	    __s += _S_two_digits(__yi % 100);

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
	_M_d_e(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, _CharT __conv, bool __mod = false) const
	{
	  // %d  The day of month as a decimal number.
	  // %Od Locale's alternative representation.
	  // %e  Day of month as decimal number, padded with space.
	  // %Oe Locale's alternative digits.

	  chrono::day __d = _S_day(__t);
	  unsigned __i = (unsigned)__d;

	  if (__mod && _M_spec._M_localized) [[unlikely]]
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		struct tm __tm{};
		__tm.tm_mday = __i;
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     (char)__conv, 'O');
	      }

	  auto __sv = _S_two_digits(__i);
	  _CharT __buf[2];
	  if (__conv == _CharT('e') && __i < 10)
	    {
	      __buf[0] = _S_space;
	      __buf[1] = __sv[1];
	      __sv = {__buf, 2};
	    }
	  return __format::__write(std::move(__out), __sv);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_F(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext&) const
	{
	  auto __ymd = _S_date(__t);
	  auto __s = std::format(_GLIBCXX_WIDEN("{:04d}-  -  "),
				 (int)__ymd.year());
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
	_M_H_I(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, _CharT __conv, bool __mod = false) const
	{
	  // %H  The hour (24-hour clock) as a decimal number.
	  // %OH Locale's alternative representation.
	  // %I  The hour (12-hour clock) as a decimal number.
	  // %OI Locale's alternative representation.

	  const auto __hms = _S_hms(__t);
	  int __i = __hms.hours().count();

	  if (__mod && _M_spec._M_localized) [[unlikely]]
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		struct tm __tm{};
		__tm.tm_hour = __i;
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     (char)__conv, 'O');
	      }

	  if (__conv == _CharT('I'))
	    {
	      if (__i == 0)
		__i = 12;
	      else if (__i > 12)
		__i -= 12;
	    }
	  return __format::__write(std::move(__out), _S_two_digits(__i));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_j(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext&) const
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
	_M_m(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod) const
	{
	  // %m  month as a decimal number.
	  // %Om Locale's alternative representation.

	  auto __m = _S_month(__t);
	  auto __i = (unsigned)__m;

	  if (__mod && _M_spec._M_localized) [[unlikely]] // %Om
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		struct tm __tm{};
		__tm.tm_mon = __i - 1;
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     'm', 'O');
	      }

	  return __format::__write(std::move(__out), _S_two_digits(__i));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_M(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod) const
	{
	  // %M  The minute as a decimal number.
	  // %OM Locale's alternative representation.

	  auto __m = _S_hms(__t).minutes();
	  auto __i = __m.count();

	  if (__mod && _M_spec._M_localized) [[unlikely]] // %OM
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		struct tm __tm{};
		__tm.tm_min = __i;
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     'M', 'O');
	      }

	  return __format::__write(std::move(__out), _S_two_digits(__i));
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
	  return _M_write(std::move(__out), __loc,
			  __ampm[__hms.hours().count() >= 12]);
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_q(const _Tp&, typename _FormatContext::iterator __out,
	     _FormatContext&) const
	{
	  // %q The duration's unit suffix
	  if constexpr (!chrono::__is_duration_v<_Tp>)
	    __throw_format_error("format error: argument is not a duration");
	  else
	    {
	      namespace __d = chrono::__detail;
	      using period = typename _Tp::period;
	      return __d::__fmt_units_suffix<period, _CharT>(std::move(__out));
	    }
	}

      // %Q handled in _M_format

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_r(const _Tp& __tt, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx) const
	{
	  // %r locale's 12-hour clock time.
	  auto __t = _S_floor_seconds(__tt);
	  locale __loc = _M_locale(__ctx);
	  const auto& __tp = use_facet<__timepunct<_CharT>>(__loc);
	  const _CharT* __ampm_fmt;
	  __tp._M_am_pm_format(&__ampm_fmt);
	  basic_string<_CharT> __fmt(_S_empty_spec);
	  __fmt.insert(1u, 1u, _S_colon);
	  __fmt.insert(2u, __ampm_fmt);
	  using _FmtStr = _Runtime_format_string<_CharT>;
	  return _M_write(std::move(__out), __loc,
			  std::format(__loc, _FmtStr(__fmt), __t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_R_T(const _Tp& __t, typename _FormatContext::iterator __out,
	       _FormatContext& __ctx, bool __secs) const
	{
	  // %R Equivalent to %H:%M
	  // %T Equivalent to %H:%M:%S
	  auto __hms = _S_hms(__t);

	  auto __s = std::format(_GLIBCXX_WIDEN("{:02d}:00"),
				 __hms.hours().count());
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
	  // %OS The locale's alternative representation.
	  auto __hms = _S_hms(__t);
	  auto __s = __hms.seconds();

	  if (__mod) [[unlikely]] // %OS
	    {
	      if (_M_spec._M_localized)
		if (auto __loc = __ctx.locale(); __loc != locale::classic())
		  {
		    struct tm __tm{};
		    __tm.tm_sec = (int)__s.count();
		    return _M_locale_fmt(std::move(__out), __loc, __tm,
					 'S', 'O');
		  }

	      // %OS formats don't include subseconds, so just format that:
	      return __format::__write(std::move(__out),
				       _S_two_digits(__s.count()));
	    }

	  if constexpr (__hms.fractional_width == 0)
	    __out = __format::__write(std::move(__out),
				      _S_two_digits(__s.count()));
	  else
	    {
	      locale __loc = _M_locale(__ctx);
	      auto __ss = __hms.subseconds();
	      using rep = typename decltype(__ss)::rep;
	      if constexpr (is_floating_point_v<rep>)
		{
		  chrono::duration<rep> __fs = __s + __ss;
		  __out = std::format_to(std::move(__out), __loc,
					 _GLIBCXX_WIDEN("{:#0{}.{}Lf}"),
					 __fs.count(),
					 3 + __hms.fractional_width,
					 __hms.fractional_width);
		}
	      else
		{
		  const auto& __np
		    = use_facet<numpunct<_CharT>>(__loc);
		  __out = __format::__write(std::move(__out),
					    _S_two_digits(__s.count()));
		  *__out++ = __np.decimal_point();
		  if constexpr (is_integral_v<rep>)
		    __out = std::format_to(std::move(__out),
					   _GLIBCXX_WIDEN("{:0{}}"),
					   __ss.count(),
					   __hms.fractional_width);
		  else
		    {
		      auto __str = std::format(_S_empty_spec, __ss.count());
		      __out = std::format_to(_GLIBCXX_WIDEN("{:0>{}s}"),
					     __str,
					     __hms.fractional_width);
		    }
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

	  if (__mod && _M_spec._M_localized) [[unlikely]]
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		struct tm __tm{};
		__tm.tm_wday = __wd.c_encoding();
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     (char)__conv, 'O');
	      }

	  unsigned __wdi = __conv == 'u' ? __wd.iso_encoding()
					 : __wd.c_encoding();
	  const _CharT __d = _S_digit(__wdi);
	  return __format::__write(std::move(__out), __string_view(&__d, 1));
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

	  if (__mod && _M_spec._M_localized) [[unlikely]]
	    if (auto __loc = __ctx.locale(); __loc != locale::classic())
	      {
		const year_month_day __ymd(__d);
		const year __y = __ymd.year();
		struct tm __tm{};
		__tm.tm_year = (int)__y - 1900;
		__tm.tm_yday = (__d - _TDays(__y/January/1)).count();
		__tm.tm_wday = weekday(__d).c_encoding();
		return _M_locale_fmt(std::move(__out), __loc, __tm,
				     (char)__conv, 'O');
	      }

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
	  using _FmtStr = _Runtime_format_string<_CharT>;
	  return _M_write(std::move(__out), __loc,
			  std::format(__loc, _FmtStr(__fmt), __t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_X(const _Tp& __tt, typename _FormatContext::iterator __out,
	     _FormatContext& __ctx, bool __mod = false) const
	{
	  // %X  Locale's time rep
	  // %EX Locale's alternative time representation.
	  auto __t = _S_floor_seconds(__tt);
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
	  using _FmtStr = _Runtime_format_string<_CharT>;
	  return _M_write(std::move(__out), __loc,
			  std::format(__loc, _FmtStr(__fmt), __t));
	}

      template<typename _Tp, typename _FormatContext>
	typename _FormatContext::iterator
	_M_z(const _Tp& __t, typename _FormatContext::iterator __out,
	     _FormatContext&, bool __mod = false) const
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
		  string_view __sv = *__t._M_abbrev;
		  if constexpr (is_same_v<_CharT, char>)
		    return __format::__write(std::move(__out), __sv);
		  else
		    {
		      // TODO use resize_and_overwrite
		      basic_string<_CharT> __ws(__sv.size(), _CharT());
		      auto& __ct = use_facet<ctype<_CharT>>(_M_locale(__ctx));
		      __ct.widen(__sv.begin(), __sv.end(), __ws.data());
		      __string_view __wsv = __ws;
		      return __format::__write(std::move(__out), __wsv);
		    }
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

      // Remove subsecond precision from a time_point.
      template<typename _Tp>
	static auto
	_S_floor_seconds(const _Tp& __t)
	{
	  using chrono::__detail::__local_time_fmt;
	  if constexpr (chrono::__is_time_point_v<_Tp>
			  || chrono::__is_duration_v<_Tp>)
	    {
	      if constexpr (_Tp::period::den != 1)
		return chrono::floor<chrono::seconds>(__t);
	      else
		return __t;
	    }
	  else if constexpr (__is_specialization_of<_Tp, chrono::hh_mm_ss>)
	    {
	      if constexpr (_Tp::fractional_width != 0)
		return chrono::floor<chrono::seconds>(__t.to_duration());
	      else
		return __t;
	    }
	  else if constexpr (__is_specialization_of<_Tp, __local_time_fmt>)
	    return _S_floor_seconds(__t._M_time);
	  else
	    return __t;
	}

      // Use the formatting locale's std::time_put facet to produce
      // a locale-specific representation.
      template<typename _Iter>
	_Iter
	_M_locale_fmt(_Iter __out, const locale& __loc, const struct tm& __tm,
		      char __fmt, char __mod) const
	{
	  basic_ostringstream<_CharT> __os;
	  const auto& __tp = use_facet<time_put<_CharT>>(__loc);
	  __tp.put(__os, __os, _S_space, &__tm, __fmt, __mod);
	  if (__os)
	    __out = _M_write(std::move(__out), __loc, __os.view());
	  return __out;
	}
    };

} // namespace __format
/// @endcond

  template<typename _Rep, typename _Period, typename _CharT>
    requires __format::__formattable_impl<_Rep, _CharT>
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
		    return _M_f._M_format(__ud, __fc, true);
		  }
		else
		  return _M_f._M_format(-__d, __fc, true);
	      }
	  return _M_f._M_format(__d, __fc, false);
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
	{
	  auto __next = _M_f._M_parse(__pc, __format::_ZonedDateTime);
	  if constexpr (!__stream_insertable)
	    if (_M_f._M_spec._M_chrono_specs.empty())
	      __format::__invalid_chrono_spec(); // chrono-specs can't be empty
	  return __next;
	}

      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::sys_time<_Duration>& __t,
	       _FormatContext& __fc) const
	{ return _M_f._M_format(__t, __fc); }

    private:
      static constexpr bool __stream_insertable
	= requires (basic_ostream<_CharT>& __os,
		    chrono::sys_time<_Duration> __t) { __os << __t; };

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
	  // We use __local_time_fmt and not sys_time (as the standard implies)
	  // because %Z for sys_time would print "UTC" and we want "TAI" here.

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
	  // We use __local_time_fmt and not sys_time (as the standard implies)
	  // because %Z for sys_time would print "UTC" and we want "GPS" here.

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
	{ return _M_f._M_format(__t, __ctx, /* use %Z for {} */ true); }

    private:
      __format::__formatter_chrono<_CharT> _M_f;
    };

#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  template<typename _Duration, typename _TimeZonePtr, typename _CharT>
    struct formatter<chrono::zoned_time<_Duration, _TimeZonePtr>, _CharT>
    : formatter<chrono::__detail::__local_time_fmt_for<_Duration>, _CharT>
    {
      template<typename _FormatContext>
	typename _FormatContext::iterator
	format(const chrono::zoned_time<_Duration, _TimeZonePtr>& __tp,
	       _FormatContext& __ctx) const
	{
	  using _Ltf = chrono::__detail::__local_time_fmt_for<_Duration>;
	  using _Base = formatter<_Ltf, _CharT>;
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
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month
		    | _ChronoParts::_Day | _ChronoParts::_TimeOfDay;
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
      auto __need = _ChronoParts::_Year | _ChronoParts::_Month
		    | _ChronoParts::_Day | _ChronoParts::_TimeOfDay;
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

	  if ((_M_need & _ChronoParts::_TimeOfDay)
		&& (_M_need & _ChronoParts::_Year))
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
			  if (_M_need & _ChronoParts::_TimeOfDay)
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
			  if (_M_need & _ChronoParts::_TimeOfDay)
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
			  if (_M_need & _ChronoParts::_TimeOfDay)
			    __err |= ios_base::failbit;
			  break;
			}
		      if (!__read_chr(':')) [[unlikely]]
			break;
		      __h = hours(__val);

		      __val = __read_unsigned(2);
		      if (__val == -1 || __val > 60) [[unlikely]]
			{
			  if (_M_need & _ChronoParts::_TimeOfDay)
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
			  if (_M_need & _ChronoParts::_TimeOfDay)
			    __err |= ios_base::failbit;
			  break;
			}
		    }
		  else // Read fractional seconds
		    {
		      basic_stringstream<_CharT> __buf;
		      auto __digit = _S_try_read_digit(__is, __err);
		      if (__digit != -1)
			{
			  __buf.put(_CharT('0') + __digit);
			  __digit = _S_try_read_digit(__is, __err);
			  if (__digit != -1)
			    __buf.put(_CharT('0') + __digit);
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
				    __buf.put(_CharT('0') + __digit);
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
	      const bool __need_wday = _M_need & _ChronoParts::_Weekday;

	      // Whether the caller wants _M_sys_days and _M_time.
	      // Only true for durations and time_points.
	      const bool __need_time = _M_need & _ChronoParts::_TimeOfDay;

	      if (__need_wday && __wday != __bad_wday)
		_M_wd = __wday; // Caller only wants a weekday and we have one.
	      else if (_M_need & _ChronoParts::_Date) // subsumes __need_wday
		{
		  // Whether the caller wants _M_ymd.
		  // True for chrono::year etc., false for time_points.
		  const bool __need_ymd = !__need_wday && !__need_time;

		  if ((_M_need & _ChronoParts::_Year && __y == __bad_y)
		     || (_M_need & _ChronoParts::_Month && __m == __bad_mon)
		     || (_M_need & _ChronoParts::_Day && __d == __bad_day))
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

		      if (_M_need & _ChronoParts::_Year)
			{
			  if (!__y.ok()) [[unlikely]]
			    __err |= ios_base::failbit;
			}
		      else if (__y == __bad_y)
			__y = 1972y; // Leap year so that Feb 29 is valid.

		      if (_M_need & _ChronoParts::_Month)
			{
			  if (!__m.ok()) [[unlikely]]
			    __err |= ios_base::failbit;
			}
		      else if (__m == __bad_mon)
			__m = January;

		      if (_M_need & _ChronoParts::_Day)
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
