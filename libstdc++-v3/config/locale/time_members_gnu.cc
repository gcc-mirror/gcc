// std::time_get, std::time_put implementation, GNU version -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 22.2.5.1.2 - time_get virtual functions
// ISO C++ 14882: 22.2.5.3.2 - time_put virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>

namespace std
{
  template<> 
    void
    __timepunct<char>::_M_initialize_timepunct(__c_locale __cloc)
    {
      if (!__cloc)
	{
	  // "C" locale
	  _M_date_format = "%m/%d/%Y";
	  _M_date_era_format = "%m/%d/%Y";
	  _M_time_format = "%H:%M:%S";
	  _M_time_era_format = "%H:%M:%S";
	  _M_am = "AM";
	  _M_pm = "PM";
	  
	  // Day names, starting with "C"'s Sunday.
	  _M_day1 = "Sunday";
	  _M_day2 = "Monday";
	  _M_day3 = "Tuesday";
	  _M_day4 = "Wednesday";
	  _M_day5 = "Thursday";
	  _M_day6 = "Friday";
	  _M_day7 = "Saturday";

	  // Abbreviated day names, starting with "C"'s Sun.
	  _M_day_a1 = "Sun";
	  _M_day_a2 = "Mon";
	  _M_day_a3 = "Tue";
	  _M_day_a4 = "Wed";
	  _M_day_a5 = "Thu";
	  _M_day_a6 = "Fri";
	  _M_day_a7 = "Sat";

	  // Month names, starting with "C"'s January.
	  _M_month01 = "January";
	  _M_month02 = "February";
	  _M_month03 = "March";
	  _M_month04 = "April";
	  _M_month05 = "May";
	  _M_month06 = "June";
	  _M_month07 = "July";
	  _M_month08 = "August";
	  _M_month09 = "September";
	  _M_month10 = "October";
	  _M_month11 = "November";
	  _M_month12 = "December";

	  // Abbreviated month names, starting with "C"'s Jan.
	  _M_month_a01 = "Jan";
	  _M_month_a02 = "Feb";
	  _M_month_a03 = "Mar";
	  _M_month_a04 = "Apr";
	  _M_month_a05 = "May";
	  _M_month_a06 = "Jun";
	  _M_month_a07 = "July";
	  _M_month_a08 = "Aug";
	  _M_month_a09 = "Sep";
	  _M_month_a10 = "Oct";
	  _M_month_a11 = "Nov";
	  _M_month_a12 = "Dec";
	}
      else
	{
	  _M_c_locale_timepunct = _S_clone_c_locale(__cloc); 

	  _M_date_format = __nl_langinfo_l(D_FMT, __cloc);
	  _M_date_era_format = __nl_langinfo_l(ERA_D_FMT, __cloc);
	  _M_time_format = __nl_langinfo_l(T_FMT, __cloc);
	  _M_time_era_format = __nl_langinfo_l(ERA_T_FMT, __cloc);
	  _M_am = __nl_langinfo_l(AM_STR, __cloc);
	  _M_pm = __nl_langinfo_l(PM_STR, __cloc);

	  // Day names, starting with "C"'s Sunday.
	  _M_day1 = __nl_langinfo_l(DAY_1, __cloc);
	  _M_day2 = __nl_langinfo_l(DAY_2, __cloc);
	  _M_day3 = __nl_langinfo_l(DAY_3, __cloc);
	  _M_day4 = __nl_langinfo_l(DAY_4, __cloc);
	  _M_day5 = __nl_langinfo_l(DAY_5, __cloc);
	  _M_day6 = __nl_langinfo_l(DAY_6, __cloc);
	  _M_day7 = __nl_langinfo_l(DAY_7, __cloc);

	  // Abbreviated day names, starting with "C"'s Sun.
	  _M_day_a1 = __nl_langinfo_l(ABDAY_1, __cloc);
	  _M_day_a2 = __nl_langinfo_l(ABDAY_2, __cloc);
	  _M_day_a3 = __nl_langinfo_l(ABDAY_3, __cloc);
	  _M_day_a4 = __nl_langinfo_l(ABDAY_4, __cloc);
	  _M_day_a5 = __nl_langinfo_l(ABDAY_5, __cloc);
	  _M_day_a6 = __nl_langinfo_l(ABDAY_6, __cloc);
	  _M_day_a7 = __nl_langinfo_l(ABDAY_7, __cloc);

	  // Month names, starting with "C"'s January.
	  _M_month01 = __nl_langinfo_l(MON_1, __cloc);
	  _M_month02 = __nl_langinfo_l(MON_2, __cloc);
	  _M_month03 = __nl_langinfo_l(MON_3, __cloc);
	  _M_month04 = __nl_langinfo_l(MON_4, __cloc);
	  _M_month05 = __nl_langinfo_l(MON_5, __cloc);
	  _M_month06 = __nl_langinfo_l(MON_6, __cloc);
	  _M_month07 = __nl_langinfo_l(MON_7, __cloc);
	  _M_month08 = __nl_langinfo_l(MON_8, __cloc);
	  _M_month09 = __nl_langinfo_l(MON_9, __cloc);
	  _M_month10 = __nl_langinfo_l(MON_10, __cloc);
	  _M_month11 = __nl_langinfo_l(MON_11, __cloc);
	  _M_month12 = __nl_langinfo_l(MON_12, __cloc);

	  // Abbreviated month names, starting with "C"'s Jan.
	  _M_month_a01 = __nl_langinfo_l(ABMON_1, __cloc);
	  _M_month_a02 = __nl_langinfo_l(ABMON_2, __cloc);
	  _M_month_a03 = __nl_langinfo_l(ABMON_3, __cloc);
	  _M_month_a04 = __nl_langinfo_l(ABMON_4, __cloc);
	  _M_month_a05 = __nl_langinfo_l(ABMON_5, __cloc);
	  _M_month_a06 = __nl_langinfo_l(ABMON_6, __cloc);
	  _M_month_a07 = __nl_langinfo_l(ABMON_7, __cloc);
	  _M_month_a08 = __nl_langinfo_l(ABMON_8, __cloc);
	  _M_month_a09 = __nl_langinfo_l(ABMON_9, __cloc);
	  _M_month_a10 = __nl_langinfo_l(ABMON_10, __cloc);
	  _M_month_a11 = __nl_langinfo_l(ABMON_11, __cloc);
	  _M_month_a12 = __nl_langinfo_l(ABMON_12, __cloc);
	}
    }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<> 
    void
    __timepunct<wchar_t>::_M_initialize_timepunct(__c_locale __cloc)
    {
      if (!__cloc)
	{
	  // "C" locale
	}
      else
	{
	  _M_c_locale_timepunct = _S_clone_c_locale(__cloc); 
	}
    }
#endif
}

