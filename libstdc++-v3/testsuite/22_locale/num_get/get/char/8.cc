// 2003-12-15  Paolo Carlini  <pcarlini@suse.de>

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

  bool b;

  // cache the num_get facet
  istringstream iss;
  const num_get<char>& ng = use_facet<num_get<char> >(iss.getloc()); 
  const ios_base::iostate goodbit = ios_base::goodbit;
  const ios_base::iostate failbit = ios_base::failbit;
  ios_base::iostate err;
  iterator_type end;

  iss.setf(ios_base::boolalpha);
  iss.str("faLse");
  err = goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, b);
  VERIFY( *end == 'L' );
  VERIFY( err == failbit );

  iss.str("falsr");
  iss.clear();  
  err = goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, b);
  VERIFY( *end == 'r' );
  VERIFY( err == failbit );

  iss.str("trus");
  iss.clear();
  err = goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, b);
  VERIFY( *end == 's' );
  VERIFY( err == failbit );
}

int main()
{
  test01();
  return 0;
}
