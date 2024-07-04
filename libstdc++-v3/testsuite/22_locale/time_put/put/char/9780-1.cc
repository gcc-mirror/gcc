// { dg-require-namedlocale "de_DE.ISO8859-15" }
// { dg-require-namedlocale "es_ES.ISO8859-15" }

// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

int main()
{
  using namespace std;

  locale l1 = locale(ISO_8859(15,de_DE));
  locale l2 = locale(ISO_8859(15,es_ES));
  
  const time_put<char> &tp = use_facet<time_put<char> >(l1);  
  ostringstream oss;
  oss.imbue(l2);
  
  tm t = tm();  
  tp.put(oss.rdbuf(), oss, ' ', &t, 'A');
  string res = oss.str();
  
  VERIFY( res == "domingo" );

  return 0;
}

// Two interpretations of the standard.

// 1 : time_get, time_put each have their own data internally
//   use internal data for time and date specifics
//   use getloc for ctype info

// 2 : time_get, time_put use the ios_base& argument and getloc to
// retrieve the necessary data.
//   use getloc for ctype, time and date specifics

// It is my opinion that the language in the standard is sufficiently
// vague to permit both interpretations. In particular, the interface
// for time_get and time_put is based on strftime, which as
// POSIX notes is dependent on LC_TIME. The C++ standard, however,
// does not specify the equivalent mappings of LC_TIME to time_get and
// time_put.

/*
The problems with the first approach, as above, are numerous.

1) The locale usage and design for formatters and parsers becomes
   fragmented. On one side, num_put and money_put, and on the other,
   time_put. This inconsistency is not useful.

2) The data structures for time and date formatting are the largest in
   the locale library. Making time_put and time_get keep separate
   copies is inefficient. (Note that time_put and time_get are in the
   same locale::category).
*/


/*
22.2.5 - The time category [lib.category.time]

-1- Templates time_get<charT,InputIterator> and
 time_put<charT,OutputIterator> provide date and time formatting and
 parsing. All specifications of member functions for time_put and
 time_get in the subclauses of lib.category.time only apply to the
 instantiations required in Tables 51 and 52
 (lib.locale.category). Their members use their ios_base&,
 ios_base::iostate&, and fill arguments as described in
 (lib.locale.categories), and the ctype<> facet, to determine
 formatting details.
*/

/*
22.2 - Standard locale categories [lib.locale.categories]

-1- Each of the standard categories includes a family of facets. Some
 of these implement formatting or parsing of a datum, for use by
 standard or users' iostream operators << and >>, as members put() and
 get(), respectively. Each such member function takes an ios_base&
 argument whose members flags(), precision(), and width(), specify the
 format of the corresponding datum. (lib.ios.base). Those functions
 which need to use other facets call its member getloc() to retrieve
 the locale imbued there. Formatting facets use the character argument
 fill to fill out the specified width where necessary.
*/

/*
With GCC/libstdc++, the output of the program with the arguments
of de_DE.ISO8859-15 es_ES is:
     domingo
     lunes
     martes
     miércoles
     jueves
     viernes
     sábado

With Intel C++, it is: (this is clearly wrong)
     Sunday
     Monday
     Tuesday
     Wednesday
     Thursday
     Friday
     Saturday

And with RogueWave C++
     Sonntag
     Montag
     Dienstag
     Mittwoch
     Donnerstag
     Freitag
     Samstag
*/
