// 990519 bkoz

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

// Include all the headers except for iostream.
#include <algorithm>
#include <bitset>
#include <complex>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <typeinfo>
#include <utility>
#include <valarray>
#include <vector>
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfloat>
#include <ciso646>
#include <climits>
#include <clocale>
#include <cmath>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#if _GLIBCPP_USE_WCHAR_T
  #include <bits/c++config.h>
  #include <cwchar>
  #include <cwctype>
#endif

// Include iostream last, just to make is as difficult as possible to
// properly initialize the standard iostream objects.
#include <iostream>

// Make sure all the standard streams are defined.
bool test01()
{
  bool test = true;

  char array1[20];
  typedef std::ios::traits_type ctraits_type;
  ctraits_type::int_type i = 15;
  ctraits_type::copy(array1, "testing istream", i);
  array1[i] = '\0';
  std::cout << "testing cout" << std::endl;
  std::cerr << "testing cerr" << std::endl;
  test &= std::cerr.flags() & std::ios_base::unitbuf;
  std::clog << "testing clog" << std::endl;
  // std::cin >> array1; // requires somebody to type something in.
  test &= std::cin.tie() == &std::cout;

#ifdef _GLIBCPP_USE_WCHAR_T
  wchar_t array2[20];
  typedef std::wios::traits_type wtraits_type;
  wtraits_type::int_type wi = 15;
  wtraits_type::copy(array2, L"testing istream", wi);
  std::wcout << L"testing wcout" << std::endl;
  std::wcerr << L"testing wcerr" << std::endl;
  test &= std::wcerr.flags() & std::ios_base::unitbuf;
  std::wclog << L"testing wclog" << std::endl;
  // std::wcin >> array2; // requires somebody to type something in.
  test &= std::wcin.tie() == &std::wcout;
#endif

  return true;
}


int main(void)
{
  test01();
  return 0;
}
