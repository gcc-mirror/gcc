// 1999-11-15 Kevin Ediger  <kediger@licor.com>
// test the floating point inserters (facet num_put)

// Copyright (C) 1999 Free Software Foundation, Inc.
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

#include <iostream>
#include <iomanip>
#include <locale>
#include <sstream>
#include <limits>
#include <assert.h>

using namespace std;

#define __TEST_NUMPUT_VERBOSE 1

struct _TestCase
{
  double val;
    
  int precision;
  int width;
  char decimal;
  char fill;

  bool fixed;
  bool scientific;
  bool showpos;
  bool showpoint;
  bool uppercase;
  bool internal;
  bool left;
  bool right;

  const char* result;
#if _GLIBCPP_USE_WCHAR_T
  const wchar_t* wresult;
#endif
};

static bool T=true;
static bool F=false;

static _TestCase testcases[] =
{
#if _GLIBCPP_USE_WCHAR_T
  // standard output (no formatting applied)
  { 1.2, 6,0,'.',' ', F,F,F,F,F,F,F,F, "1.2",L"1.2" },
  { 54, 6,0,'.',' ', F,F,F,F,F,F,F,F, "54",L"54" },
  { -.012, 6,0,'.',' ', F,F,F,F,F,F,F,F, "-0.012",L"-0.012" },
  { -.00000012, 6,0,'.',' ', F,F,F,F,F,F,F,F, "-1.2e-07",L"-1.2e-07" },
    
  // fixed formatting
  { 10.2345, 0,0,'.',' ', T,F,F,F,F,F,F,F, "10",L"10" },
  { 10.2345, 0,0,'.',' ', T,F,F,T,F,F,F,F, "10.",L"10." },
  { 10.2345, 1,0,'.',' ', T,F,F,F,F,F,F,F, "10.2",L"10.2" },
  { 10.2345, 4,0,'.',' ', T,F,F,F,F,F,F,F, "10.2345",L"10.2345" },
  { 10.2345, 6,0,'.',' ', T,F,T,F,F,F,F,F, "+10.234500",L"+10.234500" },
  { -10.2345, 6,0,'.',' ', T,F,F,F,F,F,F,F, "-10.234500",L"-10.234500" },
  { -10.2345, 6,0,',',' ', T,F,F,F,F,F,F,F, "-10,234500",L"-10,234500" },

  // fixed formatting with width
  { 10.2345, 4,5,'.',' ', T,F,F,F,F,F,F,F, "10.2345",L"10.2345" },
  { 10.2345, 4,6,'.',' ', T,F,F,F,F,F,F,F, "10.2345",L"10.2345" },
  { 10.2345, 4,7,'.',' ', T,F,F,F,F,F,F,F, "10.2345",L"10.2345" },
  { 10.2345, 4,8,'.',' ', T,F,F,F,F,F,F,F, " 10.2345",L" 10.2345" },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,F,F,F, "   10.2345",L"   10.2345" },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,F,T,F, "10.2345   ",L"10.2345   " },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,F,F,T, "   10.2345",L"   10.2345" },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,T,F,F, "   10.2345",L"   10.2345" },
  { -10.2345, 4,10,'.',' ', T,F,F,F,F,T,F,F, "-  10.2345",L"-  10.2345" },
  { -10.2345, 4,10,'.','A', T,F,F,F,F,T,F,F, "-AA10.2345",L"-AA10.2345" },
  { 10.2345, 4,10,'.','#', T,F,T,F,F,T,F,F, "+##10.2345",L"+##10.2345" },

  // scientific formatting
  { 1.23e+12, 1,0,'.',' ', F,T,F,F,F,F,F,F, "1.2e+12",L"1.2e+12" },
  { 1.23e+12, 1,0,'.',' ', F,T,F,F,T,F,F,F, "1.2E+12",L"1.2E+12" },
  { 1.23e+12, 2,0,'.',' ', F,T,F,F,F,F,F,F, "1.23e+12",L"1.23e+12" },
  { 1.23e+12, 3,0,'.',' ', F,T,F,F,F,F,F,F, "1.230e+12",L"1.230e+12" },
  { 1.23e+12, 3,0,'.',' ', F,T,T,F,F,F,F,F, "+1.230e+12",L"+1.230e+12" },
  { -1.23e-12, 3,0,'.',' ', F,T,F,F,F,F,F,F, "-1.230e-12",L"-1.230e-12" },
  { 1.23e+12, 3,0,',',' ', F,T,F,F,F,F,F,F, "1,230e+12",L"1,230e+12" },
#else
  // standard output (no formatting applied)
  { 1.2, 6,0,'.',' ', F,F,F,F,F,F,F,F, "1.2" },
  { 54, 6,0,'.',' ', F,F,F,F,F,F,F,F, "54" },
  { -.012, 6,0,'.',' ', F,F,F,F,F,F,F,F, "-0.012" },
  { -.00000012, 6,0,'.',' ', F,F,F,F,F,F,F,F, "-1.2e-07" },
    
  // fixed formatting
  { 10.2345, 0,0,'.',' ', T,F,F,F,F,F,F,F, "10" },
  { 10.2345, 0,0,'.',' ', T,F,F,T,F,F,F,F, "10." },
  { 10.2345, 1,0,'.',' ', T,F,F,F,F,F,F,F, "10.2" },
  { 10.2345, 4,0,'.',' ', T,F,F,F,F,F,F,F, "10.2345" },
  { 10.2345, 6,0,'.',' ', T,F,T,F,F,F,F,F, "+10.234500" },
  { -10.2345, 6,0,'.',' ', T,F,F,F,F,F,F,F, "-10.234500" },
  { -10.2345, 6,0,',',' ', T,F,F,F,F,F,F,F, "-10,234500" },

  // fixed formatting with width
  { 10.2345, 4,5,'.',' ', T,F,F,F,F,F,F,F, "10.2345" },
  { 10.2345, 4,6,'.',' ', T,F,F,F,F,F,F,F, "10.2345" },
  { 10.2345, 4,7,'.',' ', T,F,F,F,F,F,F,F, "10.2345" },
  { 10.2345, 4,8,'.',' ', T,F,F,F,F,F,F,F, " 10.2345" },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,F,F,F, "   10.2345" },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,F,T,F, "10.2345   " },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,F,F,T, "   10.2345" },
  { 10.2345, 4,10,'.',' ', T,F,F,F,F,T,F,F, "   10.2345" },
  { -10.2345, 4,10,'.',' ', T,F,F,F,F,T,F,F, "-  10.2345" },
  { -10.2345, 4,10,'.','A', T,F,F,F,F,T,F,F, "-AA10.2345" },
  { 10.2345, 4,10,'.','#', T,F,T,F,F,T,F,F, "+##10.2345" },

  // scientific formatting
  { 1.23e+12, 1,0,'.',' ', F,T,F,F,F,F,F,F, "1.2e+12" },
  { 1.23e+12, 1,0,'.',' ', F,T,F,F,T,F,F,F, "1.2E+12" },
  { 1.23e+12, 2,0,'.',' ', F,T,F,F,F,F,F,F, "1.23e+12" },
  { 1.23e+12, 3,0,'.',' ', F,T,F,F,F,F,F,F, "1.230e+12" },
  { 1.23e+12, 3,0,'.',' ', F,T,T,F,F,F,F,F, "+1.230e+12" },
  { -1.23e-12, 3,0,'.',' ', F,T,F,F,F,F,F,F, "-1.230e-12" },
  { 1.23e+12, 3,0,',',' ', F,T,F,F,F,F,F,F, "1,230e+12" },
#endif
};

template<typename _CharT>
class testpunct : public numpunct<_CharT>
{
public:
  typedef _CharT  char_type;

  explicit
  testpunct(char_type decimal_char) : numpunct<_CharT>() 
    { 
      _M_init(decimal_char, ',', "");
    }
};
 
template<typename _CharT>  
void apply_formatting(const _TestCase & tc, basic_ostream<_CharT> & os)
{
  os.precision(tc.precision);
  os.width(tc.width);
  os.fill(static_cast<_CharT>(tc.fill));
  if (tc.fixed)
    os.setf(ios::fixed);
  if (tc.scientific)
    os.setf(ios::scientific);
  if (tc.showpos)
    os.setf(ios::showpos);
  if (tc.showpoint)
    os.setf(ios::showpoint);
  if (tc.uppercase)
    os.setf(ios::uppercase);
  if (tc.internal)
    os.setf(ios::internal);
  if (tc.left)
    os.setf(ios::left);
  if (tc.right)
    os.setf(ios::right);
}

void test01()
{
  for (int j=0; j<sizeof(testcases)/sizeof(testcases[0]); j++)
    {
      _TestCase & tc = testcases[j];
#ifdef __TEST_NUMPUT_VERBOSE
      cout << "expect: " << tc.result << endl;
#endif
      // test double with char type
      {
        testpunct<char>* __tp = new testpunct<char>(tc.decimal);
        ostringstream os;
        locale __loc(os.getloc(), __tp);
        os.imbue(__loc);
        apply_formatting(tc, os);
        os << tc.val;
#ifdef __TEST_NUMPUT_VERBOSE
        cout << "result: " << os.str() << endl;
#endif
        assert(os && os.str() == tc.result);
      }
      // test long double with char type
      {
        testpunct<char>* __tp = new testpunct<char>(tc.decimal);
        ostringstream os;
        locale __loc(os.getloc(), __tp);
        os.imbue(__loc);
        apply_formatting(tc, os);
        os << (long double)tc.val;
#ifdef __TEST_NUMPUT_VERBOSE
        cout << "result: " << os.str() << endl;
#endif
        assert(os && os.str() == tc.result);
      }
#if _GLIBCPP_USE_WCHAR_T
      // test double with wchar_t type
      {
        testpunct<wchar_t>* __tp = new testpunct<wchar_t>(tc.decimal);
        wostringstream os;
        locale __loc(os.getloc(), __tp);
        os.imbue(__loc);
        apply_formatting(tc, os);
        os << tc.val;
        assert(os && os.str() == tc.wresult);
      }
      // test long double with wchar_t type
      {
        testpunct<wchar_t>* __tp = new testpunct<wchar_t>(tc.decimal);
        wostringstream os;
        locale __loc(os.getloc(), __tp);
        os.imbue(__loc);
        apply_formatting(tc, os);
        os << (long double)tc.val;
        assert(os && os.str() == tc.wresult);
      }
#endif
    }
}

void test02()
{
  // make sure we can output a very long float
  long double val = 1.2345678901234567890123456789e+1000L;
  int prec = numeric_limits<long double>::digits10;

  ostringstream os;
  os.precision(prec);
  os.setf(ios::scientific);
  os << val;

  char largebuf[512];
  sprintf(largebuf, "%.*Le", prec, val);
#ifdef __TEST_NUMPUT_VERBOSE
  cout << "expect: " << largebuf << endl;
  cout << "result: " << os.str() << endl;
#endif
  assert(os && os.str() == largebuf);
}

void test03()
{
  short s = -1;
  int i = -1;
  long l = -1;
  bool test = true;

  const string str_blank;
  string str_tmp;
  stringbuf strbuf;
  ostream o(&strbuf);

  o << oct << s << ' ' << hex << s; 
  test &= strbuf.str() == "177777 ffff"; // Assuming 2byte-shorts
  strbuf.str(str_blank);

  o << oct << i << ' ' << hex << i; 
  test &= strbuf.str() == "37777777777 ffffffff";
  strbuf.str(str_blank);

  o << oct << l << ' ' << hex << l; 
  test &= strbuf.str() == "37777777777 ffffffff";
  strbuf.str(str_blank);

  o << showpos << hex << showbase << 11;
  test &= strbuf.str() == "0xb";
  
  assert(test);
}

int main()
{
  test01();
  test02();
#ifdef __TEST_NUMPUT_VERBOSE
  cout << "Test passed!" << endl;
#endif
  return 0;
}










