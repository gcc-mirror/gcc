// 2007-01-19  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.2.5 [lib.alg.fill] Fill.

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  const int A1[] = {3, 3, 3, 3, 3, 3, 3, 3, 3, 3};
  const int N1 = sizeof(A1) / sizeof(int);
  
  int i1[N1];
  fill(i1, i1 + N1, 3);
  VERIFY( equal(i1, i1 + N1, A1) );

  vector<int> v1(N1);
  fill(v1.begin(), v1.end(), 3);
  VERIFY( equal(v1.begin(), v1.end(), A1) );

  const char A2[] = {'\3', '\3', '\3', '\3', '\3',
		     '\3', '\3', '\3', '\3', '\3'};
  const int N2 = sizeof(A2) / sizeof(char);

  char i2[N2];
  fill(i2, i2 + N2, '\3');
  VERIFY( equal(i2, i2 + N2, A2) );

  vector<char> v2(N2);
  fill(v2.begin(), v2.end(), '\3');
  VERIFY( equal(v2.begin(), v2.end(), A2) );

#ifdef _GLIBCXX_USE_WCHAR_T
  const wchar_t A3[] = {L'\3', L'\3', L'\3', L'\3', L'\3',
			L'\3', L'\3', L'\3', L'\3', L'\3'};
  const int N3 = sizeof(A3) / sizeof(wchar_t);

  wchar_t i3[N3];
  fill(i3, i3 + N3, L'\3');
  VERIFY( equal(i3, i3 + N3, A3) );

  vector<wchar_t> v3(N3);
  fill(v3.begin(), v3.end(), L'\3');
  VERIFY( equal(v3.begin(), v3.end(), A3) );
#endif
}

int
main()
{
  test01();
  return 0;
}
