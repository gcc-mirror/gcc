// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  bool test = true;
  char buf[512];
  const char* strlit = "how to tell a story and other essays: mark twain";
  const size_t strlitsize = std::strlen(strlit);
  stringbuf sbuf(ios_base::out);

  sbuf.pubsetbuf(buf, strlitsize);
  sbuf.sputn(strlit, strlitsize);
  VERIFY( std::strncmp(strlit, buf, strlitsize) != 0 );
}

void test02(std::stringbuf& in, bool pass)
{
  using namespace std;
  typedef streambuf::pos_type pos_type;
  typedef streambuf::off_type off_type;
  pos_type bad = pos_type(off_type(-1));
  pos_type p = 0;

  // seekoff
  p = in.pubseekoff(0, ios_base::beg, ios_base::in);
  if (pass)
    VERIFY( p != bad );

  p = in.pubseekoff(0, ios_base::beg, ios_base::out); 
  VERIFY( p == bad );

  p = in.pubseekoff(0, ios_base::beg); 
  VERIFY( p == bad );


  // seekpos
  p = in.pubseekpos(0, ios_base::in);
  if (pass)
    VERIFY( p != bad );

  p = in.pubseekpos(0, ios_base::out); 
  VERIFY( p == bad );

  p = in.pubseekpos(0); 
  VERIFY( p == bad );
}

// libstdc++/9322
void test08()
{
  using std::locale;
  bool test = true;

  locale loc;
  std::stringbuf ob;
  VERIFY( ob.getloc() == loc );

  locale::global(locale("en_US"));
  VERIFY( ob.getloc() == loc );

  locale loc_de ("de_DE");
  locale ret = ob.pubimbue(loc_de);
  VERIFY( ob.getloc() == loc_de );
  VERIFY( ret == loc );

  locale::global(loc);
  VERIFY( ob.getloc() == loc_de );
}

bool over_called;

class Derived_stringbuf : public std::stringbuf
{
public:
  int_type overflow(int_type c)
  {
    over_called = true;
    return std::stringbuf::overflow(c);
  }
  
  const char_type* pub_epptr() const
  {
    return epptr();
  }
  
  const char_type* pub_pptr() const
  {
    return pptr();
  }
};

// libstdc++/9404
void test09()
{
  bool test = true;

  bool over_expected;

  // sputc
  Derived_stringbuf dsbuf_01;
  over_called = false;
  dsbuf_01.sputc('i');
  VERIFY( over_called );
  over_expected = dsbuf_01.pub_epptr() == dsbuf_01.pub_pptr();
  over_called = false;
  dsbuf_01.sputc('v');
  VERIFY( (!over_expected && !over_called)
	  || (over_expected && over_called) );
  dsbuf_01.sputc('i');
  VERIFY( dsbuf_01.str() == "ivi" ); // Sanity check.

  // sputn
  Derived_stringbuf dsbuf_02;
  over_called = false;
  dsbuf_02.sputn("sonne's", 7);
  VERIFY( over_called );
  over_expected = dsbuf_02.pub_epptr() == dsbuf_02.pub_pptr();
  over_called = false;
  dsbuf_02.sputn(" peak", 5);
  VERIFY( (!over_expected && !over_called)
	  || (over_expected && over_called) );
  VERIFY( dsbuf_02.str() == "sonne's peak" ); // Sanity check.
}

int main() 
{
  using namespace std;
  test01();

  // movie star, submarine scientist!
  stringbuf in1("Hedy Lamarr", ios_base::in);
  stringbuf in2(ios_base::in);
  stringbuf in3("", ios_base::in);
  test02(in1, true);
  test02(in2, false);
  test02(in3, false);

  test08();
  test09();
  return 0;
}
