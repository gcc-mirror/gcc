// 1999-10-11 bkoz

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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <ostream>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

class testbuf : public std::streambuf
{
public:

  // Typedefs:
  typedef std::streambuf::traits_type traits_type;
  typedef std::streambuf::char_type char_type;

  testbuf(): std::streambuf() 
  { _M_mode = (std::ios_base::in | std::ios_base::out); }

  bool
  check_pointers()
  { 
    bool test = true;
    test &= this->eback() == NULL;
    test &= this->gptr() == NULL;
    test &= this->egptr() == NULL;
    test &= this->pbase() == NULL;
    test &= this->pptr() == NULL;
    test &= this->epptr() == NULL;
    return test;
  }

  int_type 
  pub_uflow() 
  { return (this->uflow()); }

  int_type 
  pub_overflow(int_type __c = traits_type::eof()) 
  { return (this->overflow(__c)); }

  int_type 
  pub_pbackfail(int_type __c) 
  { return (this->pbackfail(__c)); }

  void 
  pub_setg(char* beg, char* cur, char *end) 
  { this->setg(beg, cur, end); }

  void 
  pub_setp(char* beg, char* end) 
  { this->setp(beg, end); }

protected:
  int_type 
  underflow() 
  { 
    int_type __retval = traits_type::eof();
    if (this->gptr() < this->egptr())
      __retval = traits_type::not_eof(0); 
    return __retval;
  }
};

void test01()
{
  typedef testbuf::traits_type traits_type;
  typedef testbuf::int_type int_type;

  bool test = true;
  char* lit01 = "chicago underground trio/possible cube on delmark";
  testbuf buf01;

  // 27.5.2.1 basic_streambuf ctors
  // default ctor initializes 
  // - all pointer members to null pointers
  // - locale to current global locale
  test &= buf01.check_pointers();
  test &= buf01.getloc() == std::locale();

  // 27.5.2.3.1 get area
  // 27.5.2.2.3 get area
  // 27.5.2.4.3 get area
  int i01 = 3;
  buf01.pub_setg(lit01, lit01, (lit01 + i01));
  test &= i01 == buf01.in_avail();

  test &= buf01.pub_uflow() == lit01[0];
  test &= buf01.sgetc() == traits_type::to_int_type(lit01[1]);
  test &= buf01.pub_uflow() == lit01[1];
  test &= buf01.sgetc() == traits_type::to_int_type(lit01[2]);
  test &= buf01.pub_uflow() == lit01[2];
  test &= buf01.sgetc() == traits_type::eof();

  // pbackfail
  buf01.pub_setg(lit01, lit01, (lit01 + i01));
  test &= i01 == buf01.in_avail();
  int_type intt01 = traits_type::to_int_type('b');
  test &= traits_type::eof() == buf01.pub_pbackfail(intt01);

  // overflow
  test &= traits_type::eof() == buf01.pub_overflow(intt01);
  test &= traits_type::eof() == buf01.pub_overflow();
  test &= buf01.sgetc() == traits_type::to_int_type(lit01[0]);

  // sputn/xsputn
  char* lit02 = "isotope 217: the unstable molecule on thrill jockey";
  int i02 = strlen(lit02);
  char carray[i02 + 1];
  memset(carray, 0, i02 + 1);

  buf01.pub_setp(carray, (carray + i02));
  buf01.sputn(lit02, 0);
  test &= carray[0] == 0;
  test &= lit02[0] == 'i';
  buf01.sputn(lit02, 1);
  test &= lit02[0] == carray[0];
  test &= lit02[1] == 's';
  test &= carray[1] == 0;
  buf01.sputn(lit02 + 1, 10);
  test &= memcmp(lit02, carray, 10) == 0;
  buf01.sputn(lit02 + 11, 20);
  test &= memcmp(lit02, carray, 30) == 0;

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

void test02()
{
  typedef testbuf::traits_type traits_type;
  typedef testbuf::int_type int_type;

  bool test = true;
  char* lit01 = "chicago underground trio/possible cube on delmark";
  testbuf buf01;

  // 27.5.2.1 basic_streambuf ctors
  // default ctor initializes 
  // - all pointer members to null pointers
  // - locale to current global locale
  test &= buf01.check_pointers();
  test &= buf01.getloc() == std::locale();

  // 27.5.2.2.5 Put area
  size_t i01 = traits_type::length(lit01);
  char carray01[i01];
  memset(carray01, 0, i01);
  
  buf01.pub_setg(lit01, lit01, lit01 + i01);
  buf01.sgetn(carray01, 0);
  test &= carray01[0] == 0;
  buf01.sgetn(carray01, 1);
  test &= carray01[0] == 'c';
  buf01.sgetn(carray01 + 1, i01 - 1);
  test &= carray01[0] == 'c';
  test &= carray01[1] == 'h';
  test &= carray01[i01 - 1] == 'k';

#ifdef DEBUG_ASSERT
  assert(test);
#endif
}
 
// test03
// http://sourceware.cygnus.com/ml/libstdc++/2000-q1/msg00151.html
template<typename charT, typename traits = std::char_traits<charT> >
  class basic_nullbuf : public std::basic_streambuf<charT, traits>
  {
  protected:
    virtual int_type 
    overflow(int_type c) 
    {  return traits::not_eof(c); }
  };

typedef basic_nullbuf<char> nullbuf;
typedef basic_nullbuf<wchar_t> wnullbuf;

template<typename T>
  char
  print(const T& x) 
  {
   nullbuf ob;
   std::ostream out(&ob); 
   out << x << std::endl;
   return (!out ? '0' : '1');
 }

void test03() 
{
  bool test = true;
  const std::string control01("11111");
  std::string test01;

  test01 += print(true);
  test01 += print(3.14159);
  test01 += print(10);
  test01 += print('x');
  test01 += print("pipo");

  test &= test01 == control01;
#ifdef DEBUG_ASSERT
  assert(test);
#endif
}


int main() 
{
  test01();
  test02();
  test03();

  return 0;
}













