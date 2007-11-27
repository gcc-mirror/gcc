// 2007-11-26  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2007-11/msg00074.html
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wostringstream oss1, oss2, oss3;
  const num_put<wchar_t>& np1 = use_facet<num_put<wchar_t> >(oss1.getloc());
  const num_put<wchar_t>& np2 = use_facet<num_put<wchar_t> >(oss2.getloc());
  const num_put<wchar_t>& np3 = use_facet<num_put<wchar_t> >(oss3.getloc());

  wstring result1, result2, result3;

  long int li1 = 0;
  long int li2 = 5;
  double d1 = 0.0;

  oss1.setf(ios_base::showpos);
  np1.put(oss1.rdbuf(), oss1, L'*', li1);
  result1 = oss1.str();
  VERIFY( result1 == L"+0" );

  oss2.setf(ios_base::showpos);
  np2.put(oss2.rdbuf(), oss2, L'*', li2);
  result2 = oss2.str();
  VERIFY( result2 == L"+5" );

  oss3.setf(ios_base::showpos);
  np3.put(oss3.rdbuf(), oss3, L'*', d1);
  result3 = oss3.str();
  VERIFY( result3 == L"+0" );
}

int main()
{
  test01();
  return 0;
}
