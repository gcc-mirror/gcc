// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000, 2002 Free Software Foundation
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

// 22.1.1.5 locale static members [lib.locale.statics]

#include <cwchar> // for mbstate_t
#include <locale>
#include <iostream>
//#include <testsuite_hooks.h>
#define VERIFY(x) test &= x

typedef std::codecvt<char, char, std::mbstate_t> ccodecvt;
class gnu_codecvt: public ccodecvt { }; 

void test01()
{
  using namespace std;

  bool test = true;
  string str1, str2;

  // Construct a locale object with the C facet.
  const locale loc_env("");
  const locale loc01 = locale::classic();

  // Construct a locale object with the specialized facet.
  locale loc02(locale::classic(), new gnu_codecvt);
  VERIFY ( loc01 != loc02 );
  VERIFY ( !(loc01 == loc02) );

  // classic
  locale loc06("C");
  VERIFY (loc06 == loc01);
  str1 = loc06.name();
  VERIFY( str1 == "C" );

  // global
  locale loc03;
  VERIFY ( loc03 == loc01);
  locale loc04 = locale::global(loc02);
  locale loc05;
  VERIFY (loc05 != loc03);
  VERIFY (loc05 == loc02);

  // Reset global locale.
  locale::global(loc_env);
}

// Sanity check locale::global(loc) and setlocale.
void test02()
{
  using namespace std;
  bool test = true;
  
  const string ph("en_PH");
  const string mx("es_MX");

  const locale loc_ph(ph.c_str());
  const locale loc_mx(mx.c_str());

  // Get underlying current locale and environment settings.
  const string lc_all_orig = std::setlocale(LC_ALL, NULL);
  const locale loc_orig("");

  // setlocale to en_PH
  string lc_all_ph = std::setlocale(LC_ALL, ph.c_str());

  const locale loc_env("");
  VERIFY( loc_env == loc_orig );

  locale::global(loc_mx);
  string lc_all_mx = std::setlocale(LC_ALL, NULL);
  VERIFY( lc_all_mx == mx.c_str() );

  // Restore global info.
  locale::global(loc_orig);
}

int main ()
{
  test01();
  test02();

  return 0;
}
