// 2008-10-31  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008 Free Software Foundation
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

// libstdc++/37958
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;
  
  bool test __attribute__((unused)) = true;

  wistringstream iss1, iss2, iss3;
  iss1.imbue(locale(iss1.getloc(), new Punct1));
  iss2.imbue(locale(iss2.getloc(), new Punct2));
  iss3.imbue(locale(iss3.getloc(), new Punct3));
  const num_get<wchar_t>& ng1 = use_facet<num_get<wchar_t> >(iss1.getloc());
  const num_get<wchar_t>& ng2 = use_facet<num_get<wchar_t> >(iss2.getloc());
  const num_get<wchar_t>& ng3 = use_facet<num_get<wchar_t> >(iss3.getloc());

  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  bool b1 = false;
  bool b2 = false;
  bool b3 = true;

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

  iss2.str(L"1");
  iss2.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, b2);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b2 == true );

  iss3.str(L"blah");
  iss3.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng3.get(iss3.rdbuf(), 0, iss3, err, b3);
  VERIFY( err == ios_base::failbit );
  VERIFY( b3 == false );
  VERIFY( *end == L'b' );
}

int main()
{
  test01();
  return 0;
}
