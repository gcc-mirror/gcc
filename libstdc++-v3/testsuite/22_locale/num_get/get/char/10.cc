// 2003-12-19  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;
  
  bool test __attribute__((unused)) = true;

  istringstream iss;
  const num_get<char>& ng = use_facet<num_get<char> >(iss.getloc()); 
  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  float f = 0.0f;
  double d = 0.0;
  long double ld = 0.0l;
  float f1 = 1.0f;
  double d1 = 3.0;
  long double ld1 = 6.0l;
  
  iss.str("1e.");
  err = ios_base::goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, f);
  VERIFY( err == ios_base::goodbit );
  VERIFY( *end == '.' );
  VERIFY( f == f1 );

  iss.str("3e+");
  iss.clear();
  err = ios_base::goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, d);
  VERIFY( err == ios_base::eofbit );
  VERIFY( d == d1 );

  iss.str("6e ");
  iss.clear();
  err = ios_base::goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, ld);
  VERIFY( err == ios_base::goodbit );
  VERIFY( *end == ' ' );
  VERIFY( ld == ld1 );
}

int main()
{
  test01();
  return 0;
}
