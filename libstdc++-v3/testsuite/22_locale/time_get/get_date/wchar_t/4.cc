// { dg-require-namedlocale "zh_TW.UTF-8" }

// 2003-12-17  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

static bool debian_date_format();

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  locale loc_tw = locale("zh_TW.UTF-8");

  iterator_type end;

  wistringstream iss;
  iss.imbue(loc_tw);
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc());

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  const wchar_t wstr[] = { 0x897f, 0x5143, L'2', L'0', L'0', L'3',
			   0x5e74, L'1', L'2', 0x6708, L'1', L'7',
			   0x65e5 , 0x0 };

  iss.str(debian_date_format() ? wstr+2 : wstr);
  iterator_type is_it01(iss);
  tm time01;
  tim_get.get_date(is_it01, end, iss, errorstate, &time01);
  VERIFY( errorstate == ios_base::eofbit );
  VERIFY( time01.tm_mon == 11 );
  VERIFY( time01.tm_mday == 17 );
  VERIFY( time01.tm_year == 103 );
}

#include <locale.h>
#if __has_include(<langinfo.h>)
# include <langinfo.h>
#endif

static bool debian_date_format()
{
#ifdef D_FMT
  std::string orig = setlocale(LC_TIME, NULL);
  if (setlocale(LC_TIME, "zh_TW.UTF-8") != NULL)
  {
    // See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=31413
    // and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=71641#c2
    std::string d_fmt = nl_langinfo(D_FMT);
    setlocale(LC_TIME, orig.c_str());
    return d_fmt[0] == '%';
  }
#endif
  return false;
}

int main()
{
  test01();
  return 0;
}
