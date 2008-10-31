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

struct Punct1: std::numpunct<char>
{
  std::string do_truename() const { return "a"; }
  std::string do_falsename() const { return "abb"; }
};

struct Punct2: std::numpunct<char>
{
  std::string do_truename() const { return "1"; }
  std::string do_falsename() const { return "0"; }
};

struct Punct3: std::numpunct<char>
{
  std::string do_truename() const { return ""; }
  std::string do_falsename() const { return ""; }
};

// libstdc++/37958
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;
  
  bool test __attribute__((unused)) = true;

  istringstream iss1, iss2, iss3;
  iss1.imbue(locale(iss1.getloc(), new Punct1));
  iss2.imbue(locale(iss2.getloc(), new Punct2));
  iss3.imbue(locale(iss3.getloc(), new Punct3));
  const num_get<char>& ng1 = use_facet<num_get<char> >(iss1.getloc());
  const num_get<char>& ng2 = use_facet<num_get<char> >(iss2.getloc());
  const num_get<char>& ng3 = use_facet<num_get<char> >(iss3.getloc());

  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  bool b1 = false;
  bool b2 = false;
  bool b3 = true;

  iss1.str("a");
  iss1.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == ios_base::eofbit );
  VERIFY( b1 == true );

  iss1.str("abb");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b1 == false );

  iss1.str("abc");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, b1);
  VERIFY( err == ios_base::failbit );
  VERIFY( b1 == false );
  VERIFY( *end == 'c' );

  iss2.str("1");
  iss2.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, b2);
  VERIFY( err == ios_base::goodbit );
  VERIFY( b2 == true );

  iss3.str("blah");
  iss3.setf(ios_base::boolalpha);
  err = ios_base::goodbit;
  end = ng3.get(iss3.rdbuf(), 0, iss3, err, b3);
  VERIFY( err == ios_base::failbit );
  VERIFY( b3 == false );
  VERIFY( *end == 'b' );
}

int main()
{
  test01();
  return 0;
}
