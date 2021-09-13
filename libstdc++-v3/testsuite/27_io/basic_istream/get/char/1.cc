// 1999-08-11 bkoz

// Copyright (C) 1999-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 27.6.1.3 unformatted input functions

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void
test03()
{
  typedef std::char_traits<char>	traits_type;

  const char str_lit01[] = 
  "   sun*ra \n\t\t\t   & his arkestra, featuring john gilmore: \n"
  "                         "
    "jazz in silhouette: images and forecasts of tomorrow";

  std::string str01(str_lit01);
  std::string strtmp;

  std::stringbuf sbuf_03;
  std::stringbuf sbuf_04(str01, std::ios_base::in);
  std::stringbuf sbuf_05(str01, std::ios_base::in);

  std::istream is_00(0);
  std::istream is_04(&sbuf_04);
  std::istream is_05(&sbuf_05);
  std::ios_base::iostate statefail, stateeof;
  statefail = std::ios_base::failbit;
  stateeof = std::ios_base::eofbit;
  char carray1[400] = "";

  // int_type get()
  // istream& get(char*, streamsize, char delim)
  // istream& get(char*, streamsize)
  // istream& get(streambuf&, char delim)
  // istream& get(streambuf&)
  is_00.get(carray1, 2);
  VERIFY( static_cast<bool>(is_00.rdstate() & statefail) ); 
  VERIFY( is_00.gcount() == 0 );

  is_04.get(carray1, 4);
  VERIFY( !(is_04.rdstate() & statefail) );
  VERIFY( !traits_type::compare(carray1, "   ", 4) );
  VERIFY( is_04.gcount() == 3 );

  is_04.clear();
  is_04.get(carray1 + 3, 200);
  VERIFY( !(is_04.rdstate() & statefail) );
  VERIFY( !(is_04.rdstate() & stateeof) );
  VERIFY( !traits_type::compare(carray1, str_lit01, 10) );
  VERIFY( is_04.gcount() == 7 );

  is_04.clear();
  is_04.get(carray1, 200);
  VERIFY( !(is_04.rdstate() & stateeof) );
  VERIFY( static_cast<bool>(is_04.rdstate() & statefail) ); // delimiter
  VERIFY( is_04.gcount() == 0 );
  is_04.clear();
  is_04.get(carray1, 200, '[');
  VERIFY( static_cast<bool>(is_04.rdstate() & stateeof) );
  VERIFY( !(is_04.rdstate() & statefail) );
  VERIFY( is_04.gcount() == 125 );
  is_04.clear();  
  is_04.get(carray1, 200);
  VERIFY( static_cast<bool>(is_04.rdstate() & stateeof) );
  VERIFY( static_cast<bool>(is_04.rdstate() & statefail) ); 
  VERIFY( is_04.gcount() == 0 );

  std::stringbuf sbuf_02(std::ios_base::in);
  is_05.clear();
  is_05.get(sbuf_02);
  VERIFY( is_05.gcount() == 0 );
  VERIFY( static_cast<bool>(is_05.rdstate() & statefail) ); 
  VERIFY( !(is_05.rdstate() & stateeof) ); 

  is_05.clear();
  is_05.get(sbuf_03);
  VERIFY( is_05.gcount() == 10 );
  VERIFY( sbuf_03.str() == "   sun*ra " );
  VERIFY( !(is_05.rdstate() & statefail) ); 
  VERIFY( !(is_05.rdstate() & stateeof) ); 

  is_05.clear();
  is_05.get(sbuf_03, '|');
  VERIFY( is_05.gcount() == 125 );
  VERIFY( sbuf_03.str() == str_lit01 );
  VERIFY( !(is_05.rdstate() & statefail) ); 
  VERIFY( static_cast<bool>(is_05.rdstate() & stateeof) ); 

  is_05.clear();
  is_05.get(sbuf_03, '|');
  VERIFY( is_05.gcount() == 0 );
  VERIFY( static_cast<bool>(is_05.rdstate() & stateeof) ); 
  VERIFY( static_cast<bool>(is_05.rdstate() & statefail) ); 
}

int 
main()
{
  test03();
  return 0;
}
