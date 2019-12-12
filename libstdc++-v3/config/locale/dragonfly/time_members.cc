// std::time_get, std::time_put implementation, DragonFly version -*- C++ -*-

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 22.2.5.1.2 - time_get virtual functions
// ISO C++ 14882: 22.2.5.3.2 - time_put virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>
// Modified for DragonFly by John Marino <gnugcc@marino.st>

#include <locale>
#include <ctime>
#include <cwchar>
#include <stdlib.h>
#include <langinfo.h>
#include <xlocale.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    void
    __timepunct<char>::
    _M_put(char* __s, size_t __maxlen, const char* __format,
	   const tm* __tm) const throw()
    {
      const size_t __len = strftime_l(__s, __maxlen, __format, __tm,
				      (locale_t)_M_c_locale_timepunct);
      // Make sure __s is null terminated.
      if (__len == 0)
	__s[0] = '\0';
    }

  template<>
    void
    __timepunct<char>::_M_initialize_timepunct(__c_locale __cloc)
    {
      if (!_M_data)
	_M_data = new __timepunct_cache<char>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_c_locale_timepunct = _S_get_c_locale();

	  _M_data->_M_date_format = "%m/%d/%y";
	  _M_data->_M_date_era_format = "%m/%d/%y";
	  _M_data->_M_time_format = "%H:%M:%S";
	  _M_data->_M_time_era_format = "%H:%M:%S";
	  _M_data->_M_date_time_format = "";
	  _M_data->_M_date_time_era_format = "";
	  _M_data->_M_am = "AM";
	  _M_data->_M_pm = "PM";
	  _M_data->_M_am_pm_format = "";

	  // Day names, starting with "C"'s Sunday.
	  _M_data->_M_day1 = "Sunday";
	  _M_data->_M_day2 = "Monday";
	  _M_data->_M_day3 = "Tuesday";
	  _M_data->_M_day4 = "Wednesday";
	  _M_data->_M_day5 = "Thursday";
	  _M_data->_M_day6 = "Friday";
	  _M_data->_M_day7 = "Saturday";

	  // Abbreviated day names, starting with "C"'s Sun.
	  _M_data->_M_aday1 = "Sun";
	  _M_data->_M_aday2 = "Mon";
	  _M_data->_M_aday3 = "Tue";
	  _M_data->_M_aday4 = "Wed";
	  _M_data->_M_aday5 = "Thu";
	  _M_data->_M_aday6 = "Fri";
	  _M_data->_M_aday7 = "Sat";

	  // Month names, starting with "C"'s January.
	  _M_data->_M_month01 = "January";
	  _M_data->_M_month02 = "February";
	  _M_data->_M_month03 = "March";
	  _M_data->_M_month04 = "April";
	  _M_data->_M_month05 = "May";
	  _M_data->_M_month06 = "June";
	  _M_data->_M_month07 = "July";
	  _M_data->_M_month08 = "August";
	  _M_data->_M_month09 = "September";
	  _M_data->_M_month10 = "October";
	  _M_data->_M_month11 = "November";
	  _M_data->_M_month12 = "December";

	  // Abbreviated month names, starting with "C"'s Jan.
	  _M_data->_M_amonth01 = "Jan";
	  _M_data->_M_amonth02 = "Feb";
	  _M_data->_M_amonth03 = "Mar";
	  _M_data->_M_amonth04 = "Apr";
	  _M_data->_M_amonth05 = "May";
	  _M_data->_M_amonth06 = "Jun";
	  _M_data->_M_amonth07 = "Jul";
	  _M_data->_M_amonth08 = "Aug";
	  _M_data->_M_amonth09 = "Sep";
	  _M_data->_M_amonth10 = "Oct";
	  _M_data->_M_amonth11 = "Nov";
	  _M_data->_M_amonth12 = "Dec";
	}
      else
	{
	  _M_c_locale_timepunct = _S_clone_c_locale(__cloc);

	  _M_data->_M_date_format = nl_langinfo_l(D_FMT, (locale_t)__cloc);
	  _M_data->_M_date_era_format = nl_langinfo_l(ERA_D_FMT,
	  					      (locale_t)__cloc);
	  _M_data->_M_time_format = nl_langinfo_l(T_FMT, (locale_t)__cloc);
	  _M_data->_M_time_era_format = nl_langinfo_l(ERA_T_FMT,
						      (locale_t)__cloc);
	  _M_data->_M_date_time_format = nl_langinfo_l(D_T_FMT,
						       (locale_t)__cloc);
	  _M_data->_M_date_time_era_format = nl_langinfo_l(ERA_D_T_FMT,
	  						   (locale_t)__cloc);
	  _M_data->_M_am = nl_langinfo_l(AM_STR, (locale_t)__cloc);
	  _M_data->_M_pm = nl_langinfo_l(PM_STR, (locale_t)__cloc);
	  _M_data->_M_am_pm_format = nl_langinfo_l(T_FMT_AMPM,
	  					   (locale_t)__cloc);

	  // Day names, starting with "C"'s Sunday.
	  _M_data->_M_day1 = nl_langinfo_l(DAY_1, (locale_t)__cloc);
	  _M_data->_M_day2 = nl_langinfo_l(DAY_2, (locale_t)__cloc);
	  _M_data->_M_day3 = nl_langinfo_l(DAY_3, (locale_t)__cloc);
	  _M_data->_M_day4 = nl_langinfo_l(DAY_4, (locale_t)__cloc);
	  _M_data->_M_day5 = nl_langinfo_l(DAY_5, (locale_t)__cloc);
	  _M_data->_M_day6 = nl_langinfo_l(DAY_6, (locale_t)__cloc);
	  _M_data->_M_day7 = nl_langinfo_l(DAY_7, (locale_t)__cloc);

	  // Abbreviated day names, starting with "C"'s Sun.
	  _M_data->_M_aday1 = nl_langinfo_l(ABDAY_1, (locale_t)__cloc);
	  _M_data->_M_aday2 = nl_langinfo_l(ABDAY_2, (locale_t)__cloc);
	  _M_data->_M_aday3 = nl_langinfo_l(ABDAY_3, (locale_t)__cloc);
	  _M_data->_M_aday4 = nl_langinfo_l(ABDAY_4, (locale_t)__cloc);
	  _M_data->_M_aday5 = nl_langinfo_l(ABDAY_5, (locale_t)__cloc);
	  _M_data->_M_aday6 = nl_langinfo_l(ABDAY_6, (locale_t)__cloc);
	  _M_data->_M_aday7 = nl_langinfo_l(ABDAY_7, (locale_t)__cloc);

	  // Month names, starting with "C"'s January.
	  _M_data->_M_month01 = nl_langinfo_l(MON_1, (locale_t)__cloc);
	  _M_data->_M_month02 = nl_langinfo_l(MON_2, (locale_t)__cloc);
	  _M_data->_M_month03 = nl_langinfo_l(MON_3, (locale_t)__cloc);
	  _M_data->_M_month04 = nl_langinfo_l(MON_4, (locale_t)__cloc);
	  _M_data->_M_month05 = nl_langinfo_l(MON_5, (locale_t)__cloc);
	  _M_data->_M_month06 = nl_langinfo_l(MON_6, (locale_t)__cloc);
	  _M_data->_M_month07 = nl_langinfo_l(MON_7, (locale_t)__cloc);
	  _M_data->_M_month08 = nl_langinfo_l(MON_8, (locale_t)__cloc);
	  _M_data->_M_month09 = nl_langinfo_l(MON_9, (locale_t)__cloc);
	  _M_data->_M_month10 = nl_langinfo_l(MON_10, (locale_t)__cloc);
	  _M_data->_M_month11 = nl_langinfo_l(MON_11, (locale_t)__cloc);
	  _M_data->_M_month12 = nl_langinfo_l(MON_12, (locale_t)__cloc);

	  // Abbreviated month names, starting with "C"'s Jan.
	  _M_data->_M_amonth01 = nl_langinfo_l(ABMON_1, (locale_t)__cloc);
	  _M_data->_M_amonth02 = nl_langinfo_l(ABMON_2, (locale_t)__cloc);
	  _M_data->_M_amonth03 = nl_langinfo_l(ABMON_3, (locale_t)__cloc);
	  _M_data->_M_amonth04 = nl_langinfo_l(ABMON_4, (locale_t)__cloc);
	  _M_data->_M_amonth05 = nl_langinfo_l(ABMON_5, (locale_t)__cloc);
	  _M_data->_M_amonth06 = nl_langinfo_l(ABMON_6, (locale_t)__cloc);
	  _M_data->_M_amonth07 = nl_langinfo_l(ABMON_7, (locale_t)__cloc);
	  _M_data->_M_amonth08 = nl_langinfo_l(ABMON_8, (locale_t)__cloc);
	  _M_data->_M_amonth09 = nl_langinfo_l(ABMON_9, (locale_t)__cloc);
	  _M_data->_M_amonth10 = nl_langinfo_l(ABMON_10, (locale_t)__cloc);
	  _M_data->_M_amonth11 = nl_langinfo_l(ABMON_11, (locale_t)__cloc);
	  _M_data->_M_amonth12 = nl_langinfo_l(ABMON_12, (locale_t)__cloc);
	}
    }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    void
    __timepunct<wchar_t>::
    _M_put(wchar_t* __s, size_t __maxlen, const wchar_t* __format,
	   const tm* __tm) const throw()
    {
      const size_t __len = wcsftime_l(__s, __maxlen, __format, __tm,
				      (locale_t)_M_c_locale_timepunct);
      // Make sure __s is null terminated.
      if (__len == 0)
	__s[0] = L'\0';
    }

#define WIDE_LANGINFO(M,FMT) \
	fmtlen = mbstowcs_l (holder, nl_langinfo_l(FMT, (locale_t)__cloc), \
		128, (locale_t)__cloc); \
	langstring = new wchar_t[fmtlen + 1]; \
	wcsncpy (langstring, holder, fmtlen); \
	langstring[fmtlen] = L'\0'; \
	_M_data->M = langstring;

  template<>
    void
    __timepunct<wchar_t>::_M_initialize_timepunct(__c_locale __cloc)
    {
      if (!_M_data)
	_M_data = new __timepunct_cache<wchar_t>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_c_locale_timepunct = _S_get_c_locale();

	  _M_data->_M_date_format = L"%m/%d/%y";
	  _M_data->_M_date_era_format = L"%m/%d/%y";
	  _M_data->_M_time_format = L"%H:%M:%S";
	  _M_data->_M_time_era_format = L"%H:%M:%S";
	  _M_data->_M_date_time_format = L"";
	  _M_data->_M_date_time_era_format = L"";
	  _M_data->_M_am = L"AM";
	  _M_data->_M_pm = L"PM";
	  _M_data->_M_am_pm_format = L"";

	  // Day names, starting with "C"'s Sunday.
	  _M_data->_M_day1 = L"Sunday";
	  _M_data->_M_day2 = L"Monday";
	  _M_data->_M_day3 = L"Tuesday";
	  _M_data->_M_day4 = L"Wednesday";
	  _M_data->_M_day5 = L"Thursday";
	  _M_data->_M_day6 = L"Friday";
	  _M_data->_M_day7 = L"Saturday";

	  // Abbreviated day names, starting with "C"'s Sun.
	  _M_data->_M_aday1 = L"Sun";
	  _M_data->_M_aday2 = L"Mon";
	  _M_data->_M_aday3 = L"Tue";
	  _M_data->_M_aday4 = L"Wed";
	  _M_data->_M_aday5 = L"Thu";
	  _M_data->_M_aday6 = L"Fri";
	  _M_data->_M_aday7 = L"Sat";

	  // Month names, starting with "C"'s January.
	  _M_data->_M_month01 = L"January";
	  _M_data->_M_month02 = L"February";
	  _M_data->_M_month03 = L"March";
	  _M_data->_M_month04 = L"April";
	  _M_data->_M_month05 = L"May";
	  _M_data->_M_month06 = L"June";
	  _M_data->_M_month07 = L"July";
	  _M_data->_M_month08 = L"August";
	  _M_data->_M_month09 = L"September";
	  _M_data->_M_month10 = L"October";
	  _M_data->_M_month11 = L"November";
	  _M_data->_M_month12 = L"December";

	  // Abbreviated month names, starting with "C"'s Jan.
	  _M_data->_M_amonth01 = L"Jan";
	  _M_data->_M_amonth02 = L"Feb";
	  _M_data->_M_amonth03 = L"Mar";
	  _M_data->_M_amonth04 = L"Apr";
	  _M_data->_M_amonth05 = L"May";
	  _M_data->_M_amonth06 = L"Jun";
	  _M_data->_M_amonth07 = L"Jul";
	  _M_data->_M_amonth08 = L"Aug";
	  _M_data->_M_amonth09 = L"Sep";
	  _M_data->_M_amonth10 = L"Oct";
	  _M_data->_M_amonth11 = L"Nov";
	  _M_data->_M_amonth12 = L"Dec";
	}
      else
	{
	  wchar_t *langstring = 0;
	  wchar_t holder[128];
	  size_t fmtlen;

	  _M_c_locale_timepunct = _S_clone_c_locale(__cloc);

	  WIDE_LANGINFO(_M_date_format, D_FMT)
	  WIDE_LANGINFO(_M_date_era_format, ERA_D_FMT)
	  WIDE_LANGINFO(_M_time_format, T_FMT)
	  WIDE_LANGINFO(_M_time_era_format, ERA_T_FMT)
	  WIDE_LANGINFO(_M_date_time_format, D_T_FMT)
	  WIDE_LANGINFO(_M_date_time_era_format, ERA_D_T_FMT)
	  WIDE_LANGINFO(_M_am, AM_STR)
	  WIDE_LANGINFO(_M_pm, PM_STR)
	  WIDE_LANGINFO(_M_am_pm_format, T_FMT_AMPM)

	  // Day names, starting with "C"'s Sunday.
	  WIDE_LANGINFO(_M_day1, DAY_1)
	  WIDE_LANGINFO(_M_day2, DAY_2)
	  WIDE_LANGINFO(_M_day3, DAY_3)
	  WIDE_LANGINFO(_M_day4, DAY_4)
	  WIDE_LANGINFO(_M_day5, DAY_5)
	  WIDE_LANGINFO(_M_day6, DAY_6)
	  WIDE_LANGINFO(_M_day7, DAY_7)

	  // Abbreviated day names, starting with "C"'s Sun.
	  WIDE_LANGINFO(_M_aday1, ABDAY_1)
	  WIDE_LANGINFO(_M_aday2, ABDAY_2)
	  WIDE_LANGINFO(_M_aday3, ABDAY_3)
	  WIDE_LANGINFO(_M_aday4, ABDAY_4)
	  WIDE_LANGINFO(_M_aday5, ABDAY_5)
	  WIDE_LANGINFO(_M_aday6, ABDAY_6)
	  WIDE_LANGINFO(_M_aday7, ABDAY_7)

	  // Month names, starting with "C"'s January.
	  WIDE_LANGINFO(_M_month01, MON_1)
	  WIDE_LANGINFO(_M_month02, MON_2)
	  WIDE_LANGINFO(_M_month03, MON_3)
	  WIDE_LANGINFO(_M_month04, MON_4)
	  WIDE_LANGINFO(_M_month05, MON_5)
	  WIDE_LANGINFO(_M_month06, MON_6)
	  WIDE_LANGINFO(_M_month07, MON_7)
	  WIDE_LANGINFO(_M_month08, MON_8)
	  WIDE_LANGINFO(_M_month09, MON_9)
	  WIDE_LANGINFO(_M_month10, MON_10)
	  WIDE_LANGINFO(_M_month11, MON_11)
	  WIDE_LANGINFO(_M_month12, MON_12)

	  // Abbreviated month names, starting with "C"'s Jan.
	  WIDE_LANGINFO(_M_amonth01, ABMON_1)
	  WIDE_LANGINFO(_M_amonth02, ABMON_2)
	  WIDE_LANGINFO(_M_amonth03, ABMON_3)
	  WIDE_LANGINFO(_M_amonth04, ABMON_4)
	  WIDE_LANGINFO(_M_amonth05, ABMON_5)
	  WIDE_LANGINFO(_M_amonth06, ABMON_6)
	  WIDE_LANGINFO(_M_amonth07, ABMON_7)
	  WIDE_LANGINFO(_M_amonth08, ABMON_8)
	  WIDE_LANGINFO(_M_amonth09, ABMON_9)
	  WIDE_LANGINFO(_M_amonth10, ABMON_10)
	  WIDE_LANGINFO(_M_amonth11, ABMON_11)
	  WIDE_LANGINFO(_M_amonth12, ABMON_12)
	}
    }

  template<>
    __timepunct<wchar_t>::~__timepunct()
    {
      delete [] _M_data->_M_date_format;
      delete [] _M_data->_M_date_era_format;
      delete [] _M_data->_M_time_format;
      delete [] _M_data->_M_time_era_format;
      delete [] _M_data->_M_date_time_format;
      delete [] _M_data->_M_date_time_era_format;
      delete [] _M_data->_M_am;
      delete [] _M_data->_M_pm;
      delete [] _M_data->_M_am_pm_format;
      delete [] _M_data->_M_day1;
      delete [] _M_data->_M_day2;
      delete [] _M_data->_M_day3;
      delete [] _M_data->_M_day4;
      delete [] _M_data->_M_day5;
      delete [] _M_data->_M_day6;
      delete [] _M_data->_M_day7;
      delete [] _M_data->_M_aday1;
      delete [] _M_data->_M_aday2;
      delete [] _M_data->_M_aday3;
      delete [] _M_data->_M_aday4;
      delete [] _M_data->_M_aday5;
      delete [] _M_data->_M_aday6;
      delete [] _M_data->_M_aday7;
      delete [] _M_data->_M_month01;
      delete [] _M_data->_M_month02;
      delete [] _M_data->_M_month03;
      delete [] _M_data->_M_month04;
      delete [] _M_data->_M_month05;
      delete [] _M_data->_M_month06;
      delete [] _M_data->_M_month07;
      delete [] _M_data->_M_month08;
      delete [] _M_data->_M_month09;
      delete [] _M_data->_M_month10;
      delete [] _M_data->_M_month11;
      delete [] _M_data->_M_month12;
      delete [] _M_data->_M_amonth01;
      delete [] _M_data->_M_amonth02;
      delete [] _M_data->_M_amonth03;
      delete [] _M_data->_M_amonth04;
      delete [] _M_data->_M_amonth05;
      delete [] _M_data->_M_amonth06;
      delete [] _M_data->_M_amonth07;
      delete [] _M_data->_M_amonth08;
      delete [] _M_data->_M_amonth09;
      delete [] _M_data->_M_amonth10;
      delete [] _M_data->_M_amonth11;
      delete [] _M_data->_M_amonth12;
      delete _M_data;
    }

#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
