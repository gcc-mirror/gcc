// 2004-06-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation
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

// 27.6.1.3 unformatted input functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <fstream>
#include <limits>
#include <testsuite_hooks.h>

// istream& ignore(streamsize n)
void
test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  const char filename[] ="istream_unformatted-1.txt";
  ios_base::iostate state1, state2;

  ifstream ifstrm;
  ifstrm.open(filename);  

  state1 = ifstrm.rdstate();
  VERIFY( state1 == ios_base::goodbit );
  VERIFY( ifstrm.peek() == '1' );
  state2 = ifstrm.rdstate();
  VERIFY( state1 == state2 );

  state1 = ifstrm.rdstate();
  ifstrm.ignore(1);
  VERIFY( ifstrm.gcount() == 1 );
  state2 = ifstrm.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( ifstrm.peek() == '2' );

  state1 = ifstrm.rdstate();
  ifstrm.ignore(10);
  VERIFY( ifstrm.gcount() == 10 );
  state2 = ifstrm.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( ifstrm.peek() == '1' );

  state1 = ifstrm.rdstate();
  ifstrm.ignore(100);
  VERIFY( ifstrm.gcount() == 100 );
  state2 = ifstrm.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( ifstrm.peek() == '2' );
  
  state1 = ifstrm.rdstate();
  ifstrm.ignore(1000);
  VERIFY( ifstrm.gcount() == 1000 );
  state2 = ifstrm.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( ifstrm.peek() == '1' );
  
  state1 = ifstrm.rdstate();
  ifstrm.ignore(10000);
  VERIFY( ifstrm.gcount() == 10000 );
  state2 = ifstrm.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( ifstrm.peek() == '2' );

  state1 = ifstrm.rdstate();
  ifstrm.ignore(numeric_limits<streamsize>::max());
  VERIFY( ifstrm.gcount() == 5389 );
  state2 = ifstrm.rdstate();
  VERIFY( state1 != state2 );
  VERIFY( state2 == ios_base::eofbit );
}

int 
main()
{
  test01();
  return 0;
}
