// { dg-require-namedlocale "en_HK.ISO8859-1" }

// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

static bool ampm_time_format();

void test02()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_hk = locale(ISO_8859(1,en_HK));
  VERIFY( loc_hk != loc_c );

  const int pm = ampm_time_format() ? 12 : 0;
  const wstring empty;
  const tm time_bday = __gnu_test::test_tm(1, 2, 0+pm, 4, 3, 71, 0, 93, 0);

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;
  wistringstream iss;
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc());
  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  if (pm)
    iss.str(L"12:02:01 PM PST");
  else
    iss.str(L"12:02:01 PST"); // %I means 12-hour clock, so parsed as 12am
  // Hong Kong in California! Well, they have Paris in Vegas... this
  // is all a little disney-esque anyway. Besides, you can get decent
  // Dim Sum in San Francisco.
  iterator_type is_it20(iss);
  tm time20;
  errorstate = good;
  tim_get.get_time(is_it20, end, iss, errorstate, &time20);
  VERIFY( time20.tm_sec == time_bday.tm_sec );
  VERIFY( time20.tm_min == time_bday.tm_min );
  VERIFY( time20.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == ios_base::eofbit );
}

#include <locale.h>
#if __has_include(<langinfo.h>)
# include <langinfo.h>
#endif
#include <string.h>

static bool ampm_time_format()
{
#ifdef T_FMT
  std::string orig = setlocale(LC_TIME, NULL);
  if (setlocale(LC_TIME, ISO_8859(1,en_HK)) != NULL)
  {
    // See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=103687
    std::string t_fmt = nl_langinfo(T_FMT);
    setlocale(LC_TIME, orig.c_str());
    return t_fmt.find("%p") != std::string::npos;
  }
#endif
  return false;
}

int main()
{
  test02();
  return 0;
}
