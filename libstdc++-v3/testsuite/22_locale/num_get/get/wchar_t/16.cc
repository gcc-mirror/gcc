// 2005-04-26  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;
  
  bool test __attribute__((unused)) = true;

  wstringstream ss;
  const num_get<wchar_t>& ng = use_facet<num_get<wchar_t> >(ss.getloc()); 
  ios_base::iostate err;
  iterator_type end;

  unsigned short us0, us1 = numeric_limits<unsigned short>::max();
  unsigned int ui0, ui1 = numeric_limits<unsigned int>::max();
  unsigned long ul0, ul1 = numeric_limits<unsigned long>::max();
  long l01, l1 = numeric_limits<long>::max();
  long l02, l2 = numeric_limits<long>::min();
#ifdef _GLIBCXX_USE_LONG_LONG
  unsigned long long ull0, ull1 = numeric_limits<unsigned long long>::max();
  long long ll01, ll1 = numeric_limits<long long>::max();
  long long ll02, ll2 = numeric_limits<long long>::min();
#endif

  const wstring empty;

  us0 = 0;
  ss << us1;
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, us0);
  VERIFY( err == ios_base::eofbit );
  VERIFY( us0 == us1 );

  us0 = 0;
  ss.clear();
  ss.str(empty);
  ss << us1 << L'0';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, us0);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( us0 == numeric_limits<unsigned short>::max() );

  ui0 = 0U;
  ss.clear();
  ss.str(empty);
  ss << ui1 << ' ';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ui0);
  VERIFY( err == ios_base::goodbit );
  VERIFY( ui0 == ui1 );

  ui0 = 0U;
  ss.clear();
  ss.str(empty);
  ss << ui1 << L'1';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ui0);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( ui0 == numeric_limits<unsigned int>::max() );

  ul0 = 0UL;
  ss.clear();
  ss.str(empty);
  ss << ul1;
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ul0);
  VERIFY( err == ios_base::eofbit );
  VERIFY( ul0 == ul1 );

  ul0 = 0UL;
  ss.clear();
  ss.str(empty);
  ss << ul1 << L'2';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ul0);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( ul0 == numeric_limits<unsigned long>::max() );

  l01 = 0L;
  ss.clear();
  ss.str(empty);
  ss << l1 << L' ';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, l01);
  VERIFY( err == ios_base::goodbit );
  VERIFY( l01 == l1 );

  l01 = 0L;
  ss.clear();
  ss.str(empty);
  ss << l1 << L'3';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, l01);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( l01 == numeric_limits<long>::max() );

  l02 = 0L;
  ss.clear();
  ss.str(empty);
  ss << l2;
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, l02);
  VERIFY( err == ios_base::eofbit );
  VERIFY( l02 == l2 );

  l02 = 0L;
  ss.clear();
  ss.str(empty);
  ss << l2 << L'4';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, l02);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( l02 == numeric_limits<long>::min() );

#ifdef _GLIBCXX_USE_LONG_LONG
  ull0 = 0ULL;
  ss.clear();
  ss.str(empty);
  ss << ull1 << L' ';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ull0);
  VERIFY( err == ios_base::goodbit );
  VERIFY( ull0 == ull1 );

  ull0 = 0ULL;
  ss.clear();
  ss.str(empty);
  ss << ull1 << L'5';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ull0);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( ull0 == numeric_limits<unsigned long long>::max() );

  ll01 = 0LL;
  ss.clear();
  ss.str(empty);
  ss << ll1;
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ll01);
  VERIFY( err == ios_base::eofbit );
  VERIFY( ll01 == ll1 );

  ll01 = 0LL;
  ss.clear();
  ss.str(empty);
  ss << ll1 << L'6';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ll01);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( ll01 == numeric_limits<long long>::max() );

  ll02 = 0LL;
  ss.clear();
  ss.str(empty);
  ss << ll2 << L' ';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ll02);
  VERIFY( err == ios_base::goodbit );
  VERIFY( ll02 == ll2 );

  ll02 = 0LL;
  ss.clear();
  ss.str(empty);
  ss << ll2 << L'7';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ll02);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( ll02 == numeric_limits<long long>::min() );
#endif
}

int main()
{
  test01();
  return 0;
}
