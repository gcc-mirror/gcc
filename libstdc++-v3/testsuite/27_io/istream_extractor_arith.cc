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

#include <cstdio> // for printf
#include <istream>
#include <ostream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

std::string str_01;
std::string str_02("true false 0 1 110001");
std::string str_03("-19999999 777777 -234234 233 -234 33 1 66300.25 .315 1.5");
std::string str_04("0123");

std::stringbuf isbuf_01(std::ios_base::in);
std::stringbuf isbuf_02(str_02, std::ios_base::in);
std::stringbuf isbuf_03(str_03, std::ios_base::in);
std::stringbuf isbuf_04(str_04, std::ios_base::in);

std::istream is_01(NULL);
std::istream is_02(&isbuf_02);
std::istream is_03(&isbuf_03);
std::istream is_04(&isbuf_04);
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
  VERIFY( b1 == 1 );
  is_02 >> b1;
  VERIFY( b1 == 0 );

  // process numeric versions of of bool values
  is_02.unsetf(std::ios_base::boolalpha);
  fmt = is_02.flags();
  testfmt = fmt & std::ios_base::boolalpha;
  is_02 >> b1;
  VERIFY( b1 == 0 );
  is_02 >> b1;
  VERIFY( b1 == 1 );

  // is_03 == "-19999999 777777 -234234 233 -234 33 1 66300.25 .315 1.5"
  is_03 >> l1;
  VERIFY( l1 == -19999999 );
  is_03 >> ul1;
  VERIFY( ul1 == 777777 );
  is_03 >> i1;
  VERIFY( i1 == -234234 );
  is_03 >> ui1;
  VERIFY( ui1 == 233 );
  is_03 >> s1;
  VERIFY( s1 == -234 );
  is_03 >> us1;
  VERIFY( us1 == 33 );
  is_03 >> b1;
  VERIFY( b1 == 1 );
  is_03 >> ld1;
  VERIFY( ld1 == 66300.25 );
  is_03 >> d1;
  VERIFY( d1 == .315 );
  is_03 >> f1;
  VERIFY( f1 == 1.5 );

  is_04 >> std::hex >> i1;
  std::printf ("%d %d %d\n", i1, i1 == 0x123, test);
  VERIFY( i1 == 0x123 );
  std::printf ("%d %d %d\n", i1, i1 == 0x123, test);

  // test void pointers
  int i = 55;
  void* po = &i;
  void* pi;

  ss_01 << po;
  ss_01 >> pi;
  std::printf ("%x %x\n", pi, po);
  VERIFY( po == pi );
  
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
  VERIFY( n == 20000 );
  char c = is.peek();
  VERIFY( c == 65 );

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
  VERIFY( l01 == 12220101 );
  VERIFY( istr.rdstate() == std::ios_base::eofbit );

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

// http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00081.html
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
  VERIFY( h4 == 205 );
  is >> c; // ','
  VERIFY( c == ',' );

  is >> h4; // 199
  VERIFY( h4 == 199 );
  is >> c; // ','
  VERIFY( c == ',' );

  is >> h4; // 144
  VERIFY( is.rdstate() == std::ios_base::eofbit );
  VERIFY( h4 == 144 );
  is >> c; // EOF
  VERIFY( c == ',' );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );

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
  VERIFY( h4 == 205199 );
  VERIFY( is.good() );

  is.clear();
  is >> f1; 
  VERIFY( f1 == 23445.25 );
  VERIFY( is.good() );

  is.clear();
  is >> h3; 
  VERIFY( h3 == 1024365 );
  VERIFY( is.good() );

  is.clear();
  is >> h2; 
  VERIFY( h2 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::eofbit) );

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
  VERIFY( h4 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();
  is >> c;
  VERIFY( c == ',' );
  VERIFY( is.good() );

  is.ignore(3);
  is >> f1; 
  VERIFY( f1 == 0.0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();
  is >> c;
  VERIFY( c == ',' );
  is >> c;
  VERIFY( c == '4' );
  VERIFY( is.good() );

  is >> f1; 
  VERIFY( f1 == 0.25 );
  VERIFY( is.good() );
  is >> c;
  VERIFY( c == ',' );
  is >> h2;
  VERIFY( h2 == 345 );
  VERIFY( is.good() );
  f1 = 0.0;
  h2 = 0;

  is >> f1; 
  VERIFY( f1 == 5.0 );
  VERIFY( is.good() );
  is >> f1; 
  VERIFY( f1 == .25 );
  VERIFY( is.good() );

  is >> h3; 
  VERIFY( h3 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();
  is >> c;
  VERIFY( c == ',' ); // second one
  VERIFY( is.good() );

  is >> h2; 
  VERIFY( h2 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  is.clear();

  is >> h2; 
  VERIFY( h2 == 1000000 );
  VERIFY( is.good() );
  h2 = 0;

  is >> h2; 
  VERIFY( h2 == 0 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::failbit) );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::eofbit) );
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
  VERIFY( h4 == 122 );
  VERIFY( is.good() );

  is.clear();
  is >> h3; 
  VERIFY( h3 == 20519 );
  VERIFY( is.good() );

  is.clear();
  is >> h2; 
  VERIFY( h2 == 2212322 );
  VERIFY( static_cast<bool>(is.rdstate() & std::ios_base::eofbit) );


#ifdef DEBUG_ASSERT
  assert(test);
#endif
}


bool test09()
{
   bool test = true;

   std::string st("2.456e3-+0.567e-2");
   std::stringbuf sb(st);
   std::istream is(&sb);
   double f1 = 0, f2 = 0;
   char c;
   (is>>std::ws) >> f1;
   (is>>std::ws) >> c;
   (is>>std::ws) >> f2;
   test = f1 == 2456;
   VERIFY( f2 == 0.00567 );
   VERIFY( c == '-' );
#ifdef DEBUG_ASSERT
  assert(test);
#endif
 
  return test;
}

bool test10() {
  std::string str_01("0 00 000 +0 +0 -0");
  std::stringbuf isbuf_01(str_01);
  std::istream is_01(&isbuf_01);

  bool test = true;

  int n = 365;
  is_01 >> n;
  VERIFY( n == 0 );
  n = 364;
  is_01 >> n;
  VERIFY( n == 0 );
  n = 363;
  is_01 >> n;
  VERIFY( n == 0 );
  n = 362;
  is_01 >> n;
  VERIFY( n == 0 );
  n = 361;
  is_01 >> n;
  VERIFY( n == 0 );
  n = 360;
  is_01 >> n;
  VERIFY( n == 0 );
  VERIFY( is_01.rdstate() == std::ios_base::eofbit );

  std::string str_02("0x32 0X33 033 33");
  std::stringbuf isbuf_02(str_02);
  std::istream is_02(&isbuf_02);
  is_02.unsetf(std::ios_base::basefield);
  is_02 >> n;
  VERIFY( n == 50 );
  is_02 >> n;
  VERIFY( n == 51 );
  is_02 >> n;
  VERIFY( n == 27 );
  is_02 >> n;
  VERIFY( n == 33 );
  VERIFY( is_02.rdstate() == std::ios_base::eofbit );

  std::stringbuf isbuf_03(str_02);
  std::istream is_03(&isbuf_03);
  char c;
  int m;

  is_03 >> std::dec >> n >> c >> m;
  VERIFY( n == 0 );
  VERIFY( c == 'x' );
  VERIFY( m == 32 );

  is_03 >> std::oct >> m >> c >> n;
  VERIFY( m == 0 );
  VERIFY( c == 'X' );
  VERIFY( n == 27 );

  is_03 >> std::dec >> m >> n;
  VERIFY( m == 33 );
  VERIFY( n == 33 );
  VERIFY( is_03.rdstate() == std::ios_base::eofbit );

  std::string str_04("3. 4.5E+2a5E-3 .6E1");
  std::stringbuf isbuf_04(str_04);
  std::istream is_04(&isbuf_04);

  double f;
  is_04 >> f;
  VERIFY( f == 3.0 );
  is_04 >> f;
  VERIFY( f == 450.0 );
  is_04.ignore();
  is_04 >> f;
  VERIFY( f == 0.005 );
  is_04 >> f;
  VERIFY( f == 6 );
  VERIFY( is_03.rdstate() == std::ios_base::eofbit );

  std::string str_05("0E20 5Ea E16");
  std::stringbuf isbuf_05(str_05);
  std::istream is_05(&isbuf_05);

  is_05 >> f;
  VERIFY( f == 0 );
  is_05 >> f;
  VERIFY( f == 0 );
  VERIFY( is_05.rdstate() == std::ios_base::failbit );
  is_05.clear();
  is_05 >> c;
  VERIFY( c == 'a' );
  is_05 >> f;
  VERIFY( f == 0 );
  VERIFY( is_05.rdstate() == std::ios_base::failbit );
  is_05.clear();
  is_05.ignore();
  is_05 >> n;
  VERIFY( n == 16 );

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

// In the presence of no fmtflags, the input operator should behave
// like strtol(x, y, 0)
// libstdc++/90
bool test11()
{
  bool test = true;
  const char* cstrlit = "0x2a";

  // sanity check via 'C' library call
  char* err;
  long l = strtol(cstrlit, &err, 0);

  std::istringstream iss(cstrlit);
  iss.setf(std::ios::fmtflags(0), std::ios::basefield);
  int i;
  iss >> i;

  VERIFY (!iss.fail());
  VERIFY (l == i);

  return test;
}

// libstdc++/3720
// excess input should not cause a core dump
template<typename T>
bool test12_aux(bool integer_type)
{
  bool test = true;
  
  int digits_overflow;
  if (integer_type)
    // This many digits will overflow integer types in base 10.
    digits_overflow = std::numeric_limits<T>::digits10 + 2;
  else
    // This might do it, unsure.
    digits_overflow = std::numeric_limits<T>::max_exponent10 + 1;
  
  std::string st;
  std::string part = "1234567890123456789012345678901234567890";
  for (int i = 0; i < digits_overflow / part.size() + 1; ++i)
    st += part;
  std::stringbuf sb(st);
  std::istream is(&sb);
  T t;
  is >> t;
  VERIFY(is.fail());
  return test;
}

bool test12()
{
  bool test = true;
  VERIFY(test12_aux<short>(true));
  VERIFY(test12_aux<int>(true));
  VERIFY(test12_aux<long>(true));
  VERIFY(test12_aux<float>(false));
  VERIFY(test12_aux<double>(false));
  VERIFY(test12_aux<long double>(false));
  return test;
}

// libstdc++/3720 part two
void test13()
{
  using namespace std;
  bool test = true;
  const char* l1 = "12345678901234567890123456789012345678901234567890123456";
  const char* l2 = "1.2345678901234567890123456789012345678901234567890123456"
                   "  "
                   "1246.9";

  // 1 
  // used to core.
  double d;
  istringstream iss1(l2);
  iss1 >> d;
  iss1 >> d;
  VERIFY (d > 1246 && d < 1247);

  // 2
  // quick test for failbit on maximum length extraction.
  int i;
  int max_digits = numeric_limits<int>::digits10 + 1;
  string digits;
  for (int j = 0; j < max_digits; ++j)
    digits += '1';
  istringstream iss2(digits);
  iss2 >> i;
  VERIFY( !iss2.fail() );

  digits += '1';
  i = 0;
  iss2.str(digits);
  iss2.clear();
  iss2 >> i; 
  VERIFY( i == 0 );
  VERIFY( iss2.fail() );
}

int main()
{
  test01();
  test02();
  test03();

  test06();
  test07();
  test08();
  test09();
  test10();
  
  test11();
  test12();
  test13();
  return 0;
}

// paul miller was right on with riddim warfare!
