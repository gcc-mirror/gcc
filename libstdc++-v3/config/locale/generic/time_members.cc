// std::time_get, std::time_put implementation, generic version -*- C++ -*-

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

#include <locale>
#include <cstdlib>
#include <cstring>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    void
    __timepunct<char>::
    _M_put(char* __s, size_t __maxlen, const char* __format,
	   const tm* __tm) const throw()
    {
      char* __old = setlocale(LC_ALL, 0);
      const size_t __llen = strlen(__old) + 1;
      char* __sav = new char[__llen];
      memcpy(__sav, __old, __llen);
      setlocale(LC_ALL, _M_name_timepunct);
      const size_t __len = strftime(__s, __maxlen, __format, __tm);
      setlocale(LC_ALL, __sav);
      delete [] __sav;
      // Make sure __s is null terminated.
      if (__len == 0)
	__s[0] = '\0';
    }

  template<>
    void
    __timepunct<char>::_M_initialize_timepunct(__c_locale)
    {
      // "C" locale.
      if (!_M_data)
	_M_data = new __timepunct_cache<char>;

      _M_data->_M_date_format = "%m/%d/%y";
      _M_data->_M_date_era_format = "%m/%d/%y";
      _M_data->_M_time_format = "%H:%M:%S";
      _M_data->_M_time_era_format = "%H:%M:%S";
      _M_data->_M_date_time_format = "";
      _M_data->_M_date_time_era_format = "";
      _M_data->_M_am = "AM";
      _M_data->_M_pm = "PM";
      _M_data->_M_am_pm_format = "%I:%M:%S %p";

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

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    void
    __timepunct<wchar_t>::
    _M_put(wchar_t* __s, size_t __maxlen, const wchar_t* __format,
	   const tm* __tm) const throw()
    {
      char* __old = setlocale(LC_ALL, 0);
      const size_t __llen = strlen(__old) + 1;
      char* __sav = new char[__llen];
      memcpy(__sav, __old, __llen);
      setlocale(LC_ALL, _M_name_timepunct);
      const size_t __len = wcsftime(__s, __maxlen, __format, __tm);
      setlocale(LC_ALL, __sav);
      delete [] __sav;
      // Make sure __s is null terminated.
      if (__len == 0)
	__s[0] = L'\0';
    }

  template<>
    void
    __timepunct<wchar_t>::_M_initialize_timepunct(__c_locale)
    {
      // "C" locale.
      if (!_M_data)
	_M_data = new __timepunct_cache<wchar_t>;

      _M_data->_M_date_format = L"%m/%d/%y";
      _M_data->_M_date_era_format = L"%m/%d/%y";
      _M_data->_M_time_format = L"%H:%M:%S";
      _M_data->_M_time_era_format = L"%H:%M:%S";
      _M_data->_M_date_time_format = L"";
      _M_data->_M_date_time_era_format = L"";
      _M_data->_M_am = L"AM";
      _M_data->_M_pm = L"PM";
      _M_data->_M_am_pm_format = L"%I:%M:%S %p";

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
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
