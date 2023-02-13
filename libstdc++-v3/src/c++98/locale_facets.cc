// Copyright (C) 1997-2023 Free Software Foundation, Inc.
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

#define _GLIBCXX_USE_CXX11_ABI 0
#include <locale>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Definitions for static const data members of time_base.
  template<>
    const char*
    __timepunct_cache<char>::_S_timezones[14] =
    {
      "GMT", "HST", "AKST", "PST", "MST", "CST", "EST", "AST", "NST", "CET",
      "IST", "EET", "CST", "JST"
    };

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    const wchar_t*
    __timepunct_cache<wchar_t>::_S_timezones[14] =
    {
      L"GMT", L"HST", L"AKST", L"PST", L"MST", L"CST", L"EST", L"AST",
      L"NST", L"CET", L"IST", L"EET", L"CST", L"JST"
    };
#endif

  // Definitions for static const data members of money_base.
  const money_base::pattern
  money_base::_S_default_pattern =  { {symbol, sign, none, value} };

  const char* money_base::_S_atoms = "-0123456789";

  const char* __num_base::_S_atoms_in = "-+xX0123456789abcdefABCDEF";
  const char* __num_base::_S_atoms_out ="-+xX0123456789abcdef0123456789ABCDEF";

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // According to the resolution of DR 231, about 22.2.2.2.2, p11,
  // "str.precision() is specified in the conversion specification".
  void
  __num_base::_S_format_float(const ios_base& __io, char* __fptr,
			      char __mod) throw()
  {
    ios_base::fmtflags __flags = __io.flags();
    *__fptr++ = '%';
    // [22.2.2.2.2] Table 60
    if (__flags & ios_base::showpos)
      *__fptr++ = '+';
    if (__flags & ios_base::showpoint)
      *__fptr++ = '#';

    ios_base::fmtflags __fltfield = __flags & ios_base::floatfield;

#if _GLIBCXX_USE_C99_STDIO
    // Precision is always used except for hexfloat format.
    if (__fltfield != (ios_base::fixed | ios_base::scientific))
#endif
      {
        // As per DR 231: not only when __flags & ios_base::fixed || __prec > 0
        *__fptr++ = '.';
        *__fptr++ = '*';
      }

    if (__mod)
      *__fptr++ = __mod;
    // [22.2.2.2.2] Table 58
    if (__fltfield == ios_base::fixed)
      *__fptr++ = 'f';
    else if (__fltfield == ios_base::scientific)
      *__fptr++ = (__flags & ios_base::uppercase) ? 'E' : 'e';
#if _GLIBCXX_USE_C99_STDIO
    else if (__fltfield == (ios_base::fixed | ios_base::scientific))
      *__fptr++ = (__flags & ios_base::uppercase) ? 'A' : 'a';
#endif
    else
      *__fptr++ = (__flags & ios_base::uppercase) ? 'G' : 'g';
    *__fptr = '\0';
  }

  // This function is not exported but is needed internally, by the versions
  // of __verify_grouping below and in src/c++11/cxx11-locale-inst.cc
  extern bool
  __verify_grouping_impl(const char* __grouping, size_t __grouping_size,
                         const char* __grouping_tmp, size_t __grouping_tmp_size)
  {
    const size_t __n = __grouping_tmp_size - 1;
    const size_t __min = std::min(__n, size_t(__grouping_size - 1));
    size_t __i = __n;
    bool __test = true;

    // Parsed number groupings have to match the
    // numpunct::grouping string exactly, starting at the
    // right-most point of the parsed sequence of elements ...
    for (size_t __j = 0; __j < __min && __test; --__i, ++__j)
      __test = __grouping_tmp[__i] == __grouping[__j];
    for (; __i && __test; --__i)
      __test = __grouping_tmp[__i] == __grouping[__min];
    // ... but the first parsed grouping can be <= numpunct
    // grouping (only do the check if the numpunct char is > 0
    // because <= 0 means any size is ok).
    if (static_cast<signed char>(__grouping[__min]) > 0
	&& __grouping[__min] != __gnu_cxx::__numeric_traits<char>::__max)
      __test &= __grouping_tmp[0] <= __grouping[__min];
    return __test;
  }

  bool
  __verify_grouping(const char* __grouping, size_t __grouping_size,
		    const string& __grouping_tmp) throw()
  {
    return __verify_grouping_impl(__grouping, __grouping_size,
                                  __grouping_tmp.c_str(),
                                  __grouping_tmp.size());
  }

  namespace
  {
    bool
    is_leap(int year)
    {
      return (year % 100 != 0 || year % 400 == 0) && year % 4 == 0;
    }

    const unsigned short int mon_yday[2][13] =
    {
      // Normal years.
      { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
      // Leap years.
      { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
    };

    int
    day_of_the_week (int year, int mon, int mday)
    {
      // We know that January 1st 1970 was a Thursday (= 4).  Compute the
      // difference between this date and the one in arguments and so
      // determine the weekday.
      int corr_year = 1900 + year - (mon < 2);
      int wday = (-473
		  + (365 * (year - 70))
		  + (corr_year / 4)
		  - ((corr_year / 4) / 25) + ((corr_year / 4) % 25 < 0)
		  + (((corr_year / 4) / 25) / 4)
		  + mon_yday[0][mon]
		  + mday - 1);
      return ((wday % 7) + 7) % 7;
    }

    // Compute the day of the year.
    int
    day_of_the_year (tm *tm)
    {
      return (mon_yday[is_leap (1900 + tm->tm_year)][tm->tm_mon]
	      + (tm->tm_mday - 1));
    }
  }

  // Finalize time_get state.
  void
  __time_get_state::
  _M_finalize_state(tm* tm)
  {
    if (_M_have_I && _M_is_pm)
      tm->tm_hour += 12;
    if (_M_have_century)
      {
	if (_M_want_century)
	  tm->tm_year = tm->tm_year % 100;
	else
	  tm->tm_year = 0;
	tm->tm_year += (_M_century - 19) * 100;
      }
    if (_M_want_xday && !_M_have_wday)
      {
	if (!(_M_have_mon && _M_have_mday) && _M_have_yday)
	  {
	    // We don't have tm_mon and/or tm_mday, compute them.
	    int t_mon = 0;
	    while (mon_yday[is_leap(1900 + tm->tm_year)][t_mon]
		   <= tm->tm_yday)
	      ++t_mon;
	    if (!_M_have_mon)
	      tm->tm_mon = t_mon - 1;
	    if (!_M_have_mday)
	      tm->tm_mday
		= (tm->tm_yday
		   - mon_yday[is_leap(1900 + tm->tm_year)][t_mon - 1] + 1);
	    _M_have_mon = 1;
	    _M_have_mday = 1;
	  }
	// Don't crash in day_of_the_week if tm_mon is uninitialized.
	if (_M_have_mon || (unsigned) tm->tm_mon <= 11)
	  tm->tm_wday
	    = day_of_the_week (tm->tm_year, tm->tm_mon, tm->tm_mday);
      }
    if (_M_want_xday
	&& !_M_have_yday
	&& (_M_have_mon || (unsigned) tm->tm_mon <= 11))
      tm->tm_yday = day_of_the_year (tm);
    if ((_M_have_uweek || _M_have_wweek) && _M_have_wday)
      {
	int w_offset = _M_have_uweek ? 0 : 1;
	int wday = day_of_the_week (tm->tm_year, 0, 1);

	if (!_M_have_yday)
	  tm->tm_yday = ((7 - (wday - w_offset)) % 7
			 + (_M_week_no - 1) * 7
			 + (tm->tm_wday - w_offset + 7) % 7);

	if (!_M_have_mday || !_M_have_mon)
	  {
	    int t_mon = 0;
	    while (mon_yday[is_leap(1900 + tm->tm_year)][t_mon]
		   <= tm->tm_yday)
	      ++t_mon;
	    if (!_M_have_mon)
	      tm->tm_mon = t_mon - 1;
	    if (!_M_have_mday)
	      tm->tm_mday
		= (tm->tm_yday
		   - mon_yday[is_leap(1900 + tm->tm_year)][t_mon - 1] + 1);
	  }
      }
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
