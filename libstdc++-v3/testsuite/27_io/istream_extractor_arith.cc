// 1999-04-12 bkoz

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

// 27.6.1.2.2 arithmetic extractors

#include <istream>
#include <ostream>
#include <sstream>
#include <locale>
#ifdef DEBUG_ASSERT
  #include <assert.h>
#endif

std::string str_01;
std::string str_02("true false 0 1 110001");
std::string str_03("-19999999 777777 -234234 233 -234 33 1 66300.25 .315 1.5");

std::stringbuf isbuf_01(std::ios_base::in);
std::stringbuf isbuf_02(str_02, std::ios_base::in);
std::stringbuf isbuf_03(str_03, std::ios_base::in);
 
std::istream is_01(NULL);
std::istream is_02(&isbuf_02);
std::istream is_03(&isbuf_03);
std::stringstream ss_01(str_01);
 
// minimal sanity check
bool test01() {

  bool test = true;

  // Integral Types:
  bool 			b1  = false;
  bool 			b2  = false;
  short 		s1  = 0;
  int	 		i1  = 0;
  long	 		l1  = 0;
  unsigned short 	us1 = 0;
  unsigned int 		ui1 = 0;
  unsigned long 	ul1 = 0;

  // Floating-point Types:
  float 		f1  = 0;
  double 		d1  = 0;
  long double 		ld1 = 0;

  // process alphanumeric versions of bool values
  std::ios_base::fmtflags fmt = is_02.flags();
  bool testfmt = fmt & std::ios_base::boolalpha;
  is_02.setf(std::ios_base::boolalpha);
  fmt = is_02.flags();
  testfmt = fmt & std::ios_base::boolalpha;
  is_02 >> b1;
  test &= b1 == 1;
  is_02 >> b1;
  test &= b1 == 0;

  // process numeric versions of of bool values
  is_02.unsetf(std::ios_base::boolalpha);
  fmt = is_02.flags();
  testfmt = fmt & std::ios_base::boolalpha;
  is_02 >> b1;
  test &= b1 == 0;
  is_02 >> b1;
  test &= b1 == 1;

  // is_03 == "-19999999 777777 -234234 233 -234 33 1 66300.25 .315 1.5"
  is_03 >> l1;
  test &= l1 == -19999999;
  is_03 >> ul1;
  test &= ul1 == 777777;
  is_03 >> i1;
  test &= i1 == -234234;
  is_03 >> ui1;
  test &= ui1 == 233;
  is_03 >> s1;
  test &= s1 == -234;
  is_03 >> us1;
  test &= us1 == 33;
  is_03 >> b1;
  test &= b1 == 1;
  is_03 >> ld1;
  test &= ld1 == 66300.25;
  is_03 >> d1;
  test &= d1 == .315;
  is_03 >> f1;
  test &= f1 == 1.5;

  // test void pointers
  int i = 55;
  void* po = &i;
  void* pi;

  ss_01 << po;
  ss_01 >> pi;
  test &= po == pi;
  
#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

// elaborated test for ints
bool test02() {

  bool test = true;
  const std::string str_01("20000AB");
  std::stringbuf strb_01(str_01, std::ios_base::in);
  std::istream is(&strb_01);

  int n = 15;
  is >> n;
  test &= n == 20000;
  char c = is.peek();
  test &= c == 65;

#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

bool test03()
{
  std::stringbuf sbuf;
  std::istream istr(&sbuf);
  std::ostream ostr(&sbuf);

  bool test = true;
  long l01;
  ostr <<  "12220101";
  istr >>  l01; // _M_in_end set completely incorrectly here.
  test &= l01 == 12220101;
  test &= istr.rdstate() == std::ios_base::eofbit;

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

// http://sourceware.cygnus.com/ml/libstdc++/2000-q1/msg00081.html
// Jim Parsons
void test06()
{
  // default locale, grouping is turned off
  bool test = true;
  unsigned int h4, h3, h2;
  char c;
  std::string s("205,199,144");
  std::istringstream is(s);
  
  is >> h4; // 205
  test &= h4 == 205;
  is >> c; // ','
  test &= c == ',';

  is >> h4; // 199
  test &= h4 == 199;
  is >> c; // ','
  test &= c == ',';

  is >> h4; // 144
  test &= is.rdstate() == std::ios_base::eofbit;
  test &= h4 == 144;
  is >> c; // EOF
  test &= c == ',';
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

namespace std {
  class test_numpunct1 : public numpunct<char>
  {
  protected:
    string
    do_grouping() const 
    { return string(1, '\003'); }
  };
} // namespace std

void test07()
{
  // manufactured locale, grouping is turned on
  bool test = true;
  unsigned int h4 = 0, h3 = 0, h2 = 0;
  float f1 = 0.0;
  const std::string s1("205,199 23,445.25 1,024,365 123,22,24");
  std::istringstream is(s1);
  is.imbue(std::locale(std::locale(), new std::test_numpunct1));  

  // Basic operation.
  is >> h4; 
  test &= h4 == 205199;
  test &= is.good();

  is.clear();
  is >> f1; 
  test &= f1 == 23445.25;
  test &= is.good();

  is.clear();
  is >> h3; 
  test &= h3 == 1024365;
  test &= is.good();

  is.clear();
  is >> h2; 
  test &= h2 == 0;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);
  test &= static_cast<bool>(is.rdstate() & std::ios_base::eofbit);

  // Stress tests for explicit errors in grouping corner cases.  The
  // validity of these tests and results have been hammered out in
  // private email between bkoz and ncm between Jan 25 and Jan 27, 2000.
  // Thanks nate -- benjamin
  const std::string s2(",111 4,,4 0.25,345 5..25 156,, 1,000000 1000000 1234,567");
  h3 = h4 = h2 = 0;
  f1 = 0.0;
  const char c_control = '?';
  char c = c_control;
  is.clear();
  is.str(s2);

  is >> h4; 
  test &= h4 == 0;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);
  is.clear();
  is >> c;
  test &= c == ',';
  test &= is.good();

  is.ignore(3);
  is >> f1; 
  test &= f1 == 0.0;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);
  is.clear();
  is >> c;
  test &= c == ',';
  is >> c;
  test &= c == '4';
  test &= is.good();

  is >> f1; 
  test &= f1 == 0.25;
  test &= is.good();
  is >> c;
  test &= c == ',';
  is >> h2;
  test &= h2 == 345;
  test &= is.good();
  f1 = 0.0;
  h2 = 0;

  is >> f1; 
  test &= f1 == 5.0;
  test &= is.good();
  is >> f1; 
  test &= f1 == .25;
  test &= is.good();

  is >> h3; 
  test &= h3 == 0;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);
  is.clear();
  is >> c;
  test &= c == ','; // second one
  test &= is.good();

  is >> h2; 
  test &= h2 == 0;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);
  is.clear();

  is >> h2; 
  test &= h2 == 1000000;
  test &= is.good();
  h2 = 0;

  is >> h2; 
  test &= h2 == 0;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::failbit);
  test &= static_cast<bool>(is.rdstate() & std::ios_base::eofbit);
  is.clear();

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

namespace std {
  class test_numpunct2 : public numpunct<char>
  {
  protected:
    string
    do_grouping() const 
    { return string("\002\003"); }
  };
} // namespace std

void test08()
{
  // manufactured locale, grouping is turned on
  bool test = true;
  unsigned int h4 = 0, h3 = 0, h2 = 0;
  float f1 = 0.0;
  const std::string s1("1,22 205,19 22,123,22");
  const std::string s2("1,220 2050,19 202,123,22");

  std::istringstream is(s1);
  is.imbue(std::locale(std::locale(), new std::test_numpunct2));  

  // Basic operation.
  is >> h4; 
  test &= h4 == 122;
  test &= is.good();

  is.clear();
  is >> h3; 
  test &= h3 == 20519;
  test &= is.good();

  is.clear();
  is >> h2; 
  test &= h2 == 2212322;
  test &= static_cast<bool>(is.rdstate() & std::ios_base::eofbit);


#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

int main()
{
  test01();
  test02();
  test03();

  test06();
  test07();
  test08();
  return 0;
}





// paul miller was right on with riddim warfare!













