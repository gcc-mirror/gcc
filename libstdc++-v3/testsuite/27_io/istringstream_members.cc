// 2000-01-10 bkoz

// Copyright (C) 2000 Free Software Foundation, Inc.
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

// 27.7.2 template class basic_istringstream
// 27.7.2.2 member functions (istringstream_members)

// stringbuf* rdbuf() const

#include <sstream>
#ifdef DEBUG_ASSERT
  #include <assert.h>
#endif


void test01()
{
  bool test = true;
  std::istringstream is01;
  const std::string str01 = "123";
  std::string str02;
  const int i01 = 123;
  int a,b;

  std::ios_base::iostate state1, state2, statefail, stateeof;
  statefail = std::ios_base::failbit;
  stateeof = std::ios_base::eofbit;
  
  // void str(const basic_string&)
  is01.str(str01);
  state1 = is01.rdstate();
  is01 >> a;
  state2 = is01.rdstate();
  test &=  a = i01;
  // 22.2.2.1.2 num_get virtual functions
  // p 13
  // in any case, if stage 2 processing was terminated by the test for
  // in == end then err != ios_base::eofbit is performed.
  test &= state1 != state2;
  test &= state2 == stateeof; 

  is01.str(str01);
  is01 >> b;
  test &=  b != a; 
  // as is01.good() is false, istream::sentry blocks extraction.

  is01.clear();
  state1 = is01.rdstate();
  is01 >> b;
  state2 = is01.rdstate();
  test &=  b == a; 
  test &= state1 != state2;
  test &= state2 == stateeof; 

  // string str() const
  str02 = is01.str();
  test &= str01 == str02;

 #ifdef DEBUG_ASSERT
  assert(test);
#endif
}

int main()
{
  test01();
  return 0;
}


