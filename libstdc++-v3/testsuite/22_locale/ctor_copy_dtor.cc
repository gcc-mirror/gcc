// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000 Free Software Foundation
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <locale>
#include <stdexcept>
#include <debug_assert.h>

typedef std::codecvt<char, char, mbstate_t> ccodecvt;
class gnu_codecvt: public ccodecvt { }; 

void test01()
{
  using namespace std;

  bool test = true;
  string str1, str2;

  // construct a locale object with the C facet
  const locale& 	loc01 = locale::classic();

  // 1
  // template <class Facet> locale(const locale& other, Facet* f)
  // construct a locale object with the specialized facet.
  locale loc02(locale::classic(), new gnu_codecvt);
  VERIFY (loc01 != loc02);
  VERIFY (loc02.name() == "*");

  // 2
  // locale() throw()
  locale loc03;
  VERIFY (loc03 == loc01);
  VERIFY (loc03.name() == "C");
  locale loc04 = locale::global(loc02);
  locale loc05;
  VERIFY (loc05 != loc03);
  VERIFY (loc05 == loc02);

  // 3
  // explicit locale(const char* std_name)
  locale loc06("fr_FR");
  VERIFY (loc06 != loc01);  
  VERIFY (loc06 != loc02);  
  VERIFY (loc06.name() == "fr_FR");
  locale loc07("");
  VERIFY (loc07 != loc01);  
  VERIFY (loc07 != loc02);  
  VERIFY (loc06.name() == "");
  try
    { locale loc08(static_cast<const char*>(NULL)); }
  catch(runtime_error& obj)
    { VERIFY (true); }
  catch(...)
    { VERIFY (false); }

  // 4
  // locale(const locale& other, const char* std_name, category)
  


}

int main ()
{
  test01();

  return 0;
}
