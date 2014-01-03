// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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


#include <iostream>
#include <string>
#include <sstream>
#include <complex>
#include <testsuite_hooks.h>
#include <cmath>

template<typename R>
inline bool flteq(R x, R y)
{
  if (x == R(0)) return y == R(0);
  else return std::fabs(x-y) < 1e-6*std::fabs(x);
}

template<typename R>
int
test_good(std::wstring str, R x, R y)
{
  bool test __attribute__((unused)) = true;
  std::complex<R> z;
  wchar_t ch;
  std::wistringstream iss(str);
  iss >> z >> ch;
  VERIFY( iss.good() );
  VERIFY( flteq(z.real(), x) );
  VERIFY( flteq(z.imag(), y) );
  VERIFY( ch == L'#' );
  return 0;
}

template<typename R>
int
test_fail(std::wstring str)
{
  bool test __attribute__((unused)) = true;
  std::complex<R> z;
  std::wistringstream iss(str);
  iss >> z;
  VERIFY( iss.fail() && !iss.bad() );
  return 0;
}

template<typename R>
int
testall()
{
  test_good<R>(L"(-1.1,3.7)#", -1.1, 3.7);
  test_good<R>(L"(  .7e6  ,  \n-3.1)#", .7e6, -3.1);
  test_good<R>(L"(\t0,-1)#", 0.0, -1.0);
  test_good<R>(L"(-3.14)#", -3.14, 0.0);
  test_good<R>(L"-.1#", -.1, 0.0);
  test_good<R>(L" ( -2.7e3 )#", -2.7e3, 0.0);
  test_good<R>(L" -.1#", -.1, 0.0);
  test_fail<R>(L"(a,1)");
  test_fail<R>(L"(,1)");
  test_fail<R>(L"(1,a)");
  test_fail<R>(L"(1, )");
  test_fail<R>(L"|1,1)");
  test_fail<R>(L"(1|1)");
  test_fail<R>(L"(1,1|");
  return 0;
}

// libstdc++/2970
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  complex<float> cf01(-1.1, -333.2);
  wstringstream ss;
  ss << cf01;
  wstring str = ss.str();
  VERIFY( str == L"(-1.1,-333.2)" );
}

// libstdc++/2985
struct gnu_char_traits : public std::char_traits<wchar_t>
{ };

typedef std::basic_ostringstream<wchar_t, gnu_char_traits> gnu_sstream;
template class std::basic_string<wchar_t, gnu_char_traits,
				 std::allocator<wchar_t> >;

void test02()
{
  bool test __attribute__((unused)) = true;

  // Construct locale with specialized facets.
  typedef gnu_sstream::__num_put_type numput_type;
  typedef gnu_sstream::__num_get_type numget_type;
  std::locale loc_c = std::locale::classic();
  std::locale loc_1(loc_c, new numput_type);
  std::locale loc_2(loc_1, new numget_type);
  VERIFY( std::has_facet<numput_type>(loc_2) );
  VERIFY( std::has_facet<numget_type>(loc_2) );

  gnu_sstream sstr;
  sstr.imbue(loc_2);


  std::complex<double> x(3, 4);
  sstr << x; 
  VERIFY( sstr.str() == L"(3,4)" );
}

int
main()
{
  testall<float>();
  testall<double>();
  testall<long double>();

  test01();
  test02();

  return 0;
}
