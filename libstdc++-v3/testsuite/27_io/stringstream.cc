// 981015 bkoz
// i,o,stringstream usage

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.


#include <vector>
#include <string>
#include <sstream>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

// 01: sanity checks for strings, stringbufs
std::string test01()
{
  bool test = false;

  // Empty string sanity check.
  std::string str01;
  std::string::iterator __i_start = str01.begin();
  char* __p_start = __i_start.base();
  std::string::iterator __i_end = str01.end();
  char* __p_end = __i_end.base();
  std::string::size_type len = str01.size();
  test = __p_start == __p_end;
  test &= len == 0;

  // Full string sanity check.
  std::string str02("these golden days, i spend waiting for you:\n 
              Betty Carter on Verve with I'm Yours and You're Mine.");
  __i_start = str02.begin();
  __p_start = __i_start.base();
  __i_end = str02.end();
  __p_end = __i_end.base();
  len = str02.size();
  test &= __p_start != __p_end;
  test &= len != 0;
 
  // Test an empty ostring stream for sanity.
  std::ostringstream ostrstream0;
  std::string str03 = ostrstream0.str();
  __i_start = str03.begin();
  __p_start = __i_start.base();
  __i_end = str03.end();
  __p_end = __i_end.base();
  len = str03.size();
  test &= __p_start == __p_end;
  test &= len == 0;
  test &= str01 == str03;

  return str02;
}


bool test02(void) {

  bool test = true;

  //
  // 1: Automatic formatting of a compound string
  //
  int i = 1024;
  int *pi = &i;
  double d = 3.14159;
  double *pd = &d;
  std::string blank;
  std::ostringstream ostrst01; 
  std::ostringstream ostrst02(blank); 
  
  // No buffer, so nothing should be added.
  ostrst01 << "i: " << i << " i's address:  " << pi << "\n"
	     << "d: " << d << " d's address: " << pd << std::endl;
  // Buffer, so this should be ok.
  ostrst02 << "i: " << i << " i's address:  " << pi << "\n"
	     << "d: " << d << " d's address: " << pd << std::endl;

  std::string msg01 = ostrst01.str();
  std::string msg02 = ostrst02.str();
  test &= msg01 != msg02;
  test &= msg02 != blank;

  //
  // 2: istringstream
  //
  // extracts the stored ascii values, placing them in turn in the four vars
#if 0
  int i2 = 0;
  int *pi2 = &i2;
  double d2 = 0.0;
  double *pd2 = &d2;
  std::istringstream istrst01(ostrst02.str());

  istrst01 >> i2 >> pi2 >> d2 >> pd2;
  //istrst01 >> i2;
  //istrst01 >> pi2;
  test &= i2 == i;
  test &= d2 == d;
  test &= pd2 == pd;
  test &= pi2 == pi;
#endif

  // stringstream
  std::string str1("");
  std::string str3("this is a somewhat  string");
  std::stringstream ss1(str1, std::ios_base::in|std::ios_base::out);
  std::stringstream ss2(str3, std::ios_base::in|std::ios_base::out);

  return test;
}

// user-reported error
class derived_oss: public std::ostringstream 
{
public:
  derived_oss() : std::ostringstream() {}
};

bool test03()
{
  bool test = true;
  derived_oss yy;
  yy << "buena vista social club\n";
  test &= yy.str() == std::string("buena vista social club\n");

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

int main() 
{
  test01();
  test02();
  test03();
}











