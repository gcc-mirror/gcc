// 1999-08-16 bkoz

// Copyright (C) 2000, 1999 Free Software Foundation
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

// 27.6.2.5.4 basic_ostream character inserters

#include <string>
#include <ostream>
#include <sstream>
#include <fstream>
#ifdef DEBUG_ASSERT
  #include <assert.h>
#endif

// ofstream
bool test01()
{
  bool test = true;
  std::string str01;
  const int size = 1000;
  const char name_02[] = "testsuite/ostream_inserter_char-1.txt";

  // initialize string
  for(int i=0 ; i < size; i++) {
    str01 += '1';
    str01 += '2';
    str01 += '3';
    str01 += '4';
    str01 += '5';
    str01 += '6';
    str01 += '7';
    str01 += '8';
    str01 += '9';
    str01 += '\n';
  }
  std::ofstream f(name_02);

  f << str01;
  f.close();

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

// ostringstream width() != zero
// left
bool test02(void) 
{
  bool test = true;
  std::string tmp;
  
  std::string str01 = "";
  std::ostringstream oss01;
  oss01.width(5);
  oss01.fill('0');
  oss01.flags(std::ios_base::left);
  oss01 << str01;
  tmp = oss01.str();
  test &= tmp == "00000";

  std::string str02 = "1";
  std::ostringstream oss02;
  oss02.width(5);
  oss02.fill('0');
  oss02.flags(std::ios_base::left);
  oss02 << str02;
  tmp = oss02.str();
  test &= tmp == "10000";

  std::string str03 = "909909";
  std::ostringstream oss03;
  oss03.width(5);
  oss03.fill('0');
  oss03.flags(std::ios_base::left);
  oss03 << str03;
  tmp = oss03.str();
  test &= tmp == "909909";

#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

// width() != zero
// right
bool test03(void) 
{
  bool test = true;
  std::string tmp;

  std::string str01 = "";
  std::ostringstream oss01;
  oss01.width(5);
  oss01.fill('0');
  oss01.flags(std::ios_base::right);
  oss01 << str01;
  tmp = oss01.str();
  test &= tmp == "00000";

  std::string str02 = "1";
  std::ostringstream oss02;
  oss02.width(5);
  oss02.fill('0');
  oss02.flags(std::ios_base::right);
  oss02 << str02;
  tmp = oss02.str();
  test &= tmp == "00001";

  std::string str03 = "909909";
  std::ostringstream oss03;
  oss03.width(5);
  oss03.fill('0');
  oss03.flags(std::ios_base::right);
  oss03 << str03;
  tmp = oss03.str();
  test &= tmp == "909909";

#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

// stringstream and large strings
bool test04() {

  bool test = true;
  std::string str_01;
  const std::string str_02("coltrane playing 'softly as a morning sunrise'");
  const std::string str_03("coltrane");
  std::string str_tmp;
  const int i_max=250;

  std::ostringstream oss_01(std::ios_base::out);
  std::ostringstream oss_02(str_01, std::ios_base::out);

  std::ios_base::iostate state1, state2, statefail;
  statefail = std::ios_base::failbit;

  // template<_CharT, _Traits>
  //  basic_ostream& operator<<(ostream&, const char*)
  for (int i = 0; i < i_max; ++i) 
    oss_02 << "Test: " << i << std::endl;
  str_tmp = oss_02.str();
  test &= !oss_02.bad();
  test &= oss_02.good();
  test &= str_tmp != str_01;
  test &= str_tmp.size() == 2390;

#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

// ostringstream and large strings number 2
bool test05()
{
  bool test = true;
  std::string str05, str10;

  typedef std::ostream::pos_type	pos_type;
  typedef std::ostream::off_type	off_type;
  std::string str01;
  const int size = 1000;

  // initialize string
  for(int i=0 ; i < size; i++) {
    str01 += '1';
    str01 += '2';
    str01 += '3';
    str01 += '4';
    str01 += '5';
    str01 += '6';
    str01 += '7';
    str01 += '8';
    str01 += '9';
    str01 += '\n';
  }

  // test 1: out
  std::ostringstream sstr01(str01, std::ios_base::out);
  std::ostringstream sstr02;
  sstr02 << str01;
  str05 = sstr01.str();
  str10 = sstr02.str();
  test &= str05 == str01;
  test &= str10 == str01;

  // test 2: in | out 
  std::ostringstream sstr04(str01,  std::ios_base::out | std::ios_base::in);
  std::ostringstream sstr05(std::ios_base::in | std::ios_base::out);
  sstr05 << str01;
  str05 = sstr04.str();
  str10 = sstr05.str();
  test &= str05 == str01;
  test &= str10 == str01;

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}


// ostringstream and positioning, multiple writes
// http://sourceware.cygnus.com/ml/libstdc++/2000-q1/msg00326.html
void test06()
{
  bool test = true;
  const char carray01[] = "mos def & talib kweli are black star";

  // normal
  std::ostringstream ostr1("mos def");
  test &= ostr1.str() == "mos def"; 
  ostr1 << " & talib kweli";  // should overwrite first part of buffer
  test &= ostr1.str() == " & talib kweli";
  ostr1 << " are black star";  // should append to string from above
  test &= ostr1.str() != carray01;
  test &= ostr1.str() == " & talib kweli are black star";

  // appending
  std::ostringstream ostr2("blackalicious", 
			   std::ios_base::out | std::ios_base::ate);
  test &= ostr2.str() == "blackalicious"; 
  ostr2 << " NIA ";  // should not overwrite first part of buffer
  test &= ostr2.str() == "blackalicious NIA ";
  ostr2 << "4: deception (5:19)";  // should append to full string from above
  test &= ostr2.str() == "blackalicious NIA 4: deception (5:19)";

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}


int main()
{

  test02();
  test03();
  test04();
  test05();
  test06();
  return 0;
}

