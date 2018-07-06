// 2003-06-30 peturr02@ru.is

// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include <iostream>
#include <locale>
#include <testsuite_hooks.h>

#ifdef _GLIBCXX_USE_WCHAR_T
// libstdc++/9828
void test01()
{
  using namespace std;

  typedef num_put<char> np_t;

  ostringstream stream;
  const np_t& np = use_facet<np_t>(stream.getloc());

  np.put(stream, wcout, ' ', static_cast<long>(10));
  VERIFY( stream.str() == "10" );
}
#endif

int main()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  test01();
#endif
  return 0;
}
