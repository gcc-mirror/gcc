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

// Make sure that formatted output uses the locale in the output stream.
using namespace std;
locale l1 = locale(ISO_8859(15,de_DE));
const num_put<char>& np = use_facet<num_put<char> >(l1);
const numpunct<char>& npunct = use_facet<numpunct<char> >(l1);

void test01()
{
  locale l2 = locale("C");
  const numpunct<char>& npunct2 = use_facet<numpunct<char> >(l2);
  char c __attribute__((unused)) = npunct2.thousands_sep();
  string s = npunct2.grouping();

  ostringstream oss;
  oss.imbue(l2);

  long l = 1234567890;
  np.put(oss.rdbuf(), oss, ' ', l);
  string res = oss.str();

  VERIFY( res == "1234567890" );
}

void test02()
{
  locale l2 = locale(ISO_8859(15,es_ES));
  const numpunct<char>& npunct3 = use_facet<numpunct<char> >(l2);
  char c __attribute__((unused)) = npunct3.thousands_sep();
  string s = npunct3.grouping();

  ostringstream oss;
  oss.imbue(l2);

  long l = 1234567890;
  np.put(oss.rdbuf(), oss, ' ', l);
  string res = oss.str();

  if (!s.empty())
    VERIFY( res == "1.234.567.890" );
  else
    VERIFY( res == "1234567890" );
}

int main()
{
  // Sanity check.
  char c __attribute__((unused)) = npunct.thousands_sep();
  string s = npunct.grouping();

  test01();
  test02();
  return 0;
}
