// 2008-10-31  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

struct Punct1: std::numpunct<wchar_t>
{
  std::wstring do_truename() const { return L"a"; }
  std::wstring do_falsename() const { return L"abb"; }
};

struct Punct2: std::numpunct<wchar_t>
{
  std::wstring do_truename() const { return L"1"; }
  std::wstring do_falsename() const { return L"0"; }
};

struct Punct3: std::numpunct<wchar_t>
{
  std::wstring do_truename() const { return L""; }
  std::wstring do_falsename() const { return L""; }
};

struct Punct4: std::numpunct<wchar_t>
{
  std::wstring do_truename() const { return L"one"; }
  std::wstring do_falsename() const { return L"two"; }
};

// libstdc++/37958
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  wistringstream iss0, iss1, iss2, iss3, iss4;
  iss1.imbue(locale(iss1.getloc(), new Punct1));
  iss2.imbue(locale(iss2.getloc(), new Punct2));
  iss3.imbue(locale(iss3.getloc(), new Punct3));
  iss4.imbue(locale(iss4.getloc(), new Punct4));
  const num_get<wchar_t>& ng0 = use_facet<num_get<wchar_t> >(iss0.getloc());
  const num_get<wchar_t>& ng1 = use_facet<num_get<wchar_t> >(iss1.getloc());
  const num_get<wchar_t>& ng2 = use_facet<num_get<wchar_t> >(iss2.getloc());
  const num_get<wchar_t>& ng3 = use_facet<num_get<wchar_t> >(iss3.getloc());
  const num_get<wchar_t>& ng4 = use_facet<num_get<wchar_t> >(iss4.getloc());

  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  bool b0 = false;
  bool b1 = false;
  bool b2 = false;
  bool b3 = true;
  bool b4 = false;

  iss0.str(L"true");
  iss0.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng0.get(iss0.rdbuf(), 0, iss0, err, b0);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b0 == true );

  iss0.str(L"false");
  iss0.clear();
  err = ios_base::goodbit;
  end = ng0.get(iss0.rdbuf(), 0, iss0, err, b0);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b0 == false );

  iss1.str(L"a");
  iss1.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == ios_base::eofbit );
  VERIFY( b1 == true );

  iss1.str(L"abb");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b1 == false );

  iss1.str(L"abc");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == ios_base::failbit );
  VERIFY( b1 == false );
  VERIFY( *end == L'c' );

  iss1.str(L"ab");
  iss1.clear();
  b1 = true;
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( b1 == false );

  iss2.str(L"1");
  iss2.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, b2);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b2 == true );

  iss2.str(L"0");
  iss2.clear();
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, b2);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b2 == false );

  iss2.str(L"2");
  iss2.clear();
  b2 = true;
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, b2);
  VERIFY( err == ios_base::failbit );
  VERIFY( b2 == false );
  VERIFY( *end == L'2' );

  iss3.str(L"blah");
  iss3.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng3.get(iss3.rdbuf(), 0, iss3, err, b3);
  VERIFY( err == ios_base::failbit );
  VERIFY( b3 == false );
  VERIFY( *end == L'b' );

  iss3.str(L"");
  iss3.clear();
  b3 = true;
  err = ios_base::goodbit;
  end = ng3.get(iss3.rdbuf(), 0, iss3, err, b3);
  VERIFY( err == ios_base::failbit );
  VERIFY( b3 == false );

  iss4.str(L"one");
  iss4.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng4.get(iss4.rdbuf(), 0, iss4, err, b4);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b4 == true );

  iss4.str(L"two");
  iss4.clear();
  err = ios_base::goodbit;
  end = ng4.get(iss4.rdbuf(), 0, iss4, err, b4);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b4 == false );

  iss4.str(L"three");
  iss4.clear();
  b4 = true;
  err = ios_base::goodbit;
  end = ng4.get(iss4.rdbuf(), 0, iss4, err, b4);
  VERIFY( err == ios_base::failbit );
  VERIFY( b4 == false );
  VERIFY( *end == L'h' );

  iss4.str(L"on");
  iss4.clear();
  b4 = true;
  err = ios_base::goodbit;
  end = ng4.get(iss4.rdbuf(), 0, iss4, err, b4);
  VERIFY( err == (ios_base::failbit | ios_base::eofbit) );
  VERIFY( b4 == false );
}

int main()
{
  test01();
  return 0;
}
