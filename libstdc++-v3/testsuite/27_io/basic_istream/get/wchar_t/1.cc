// Copyright (C) 2004, 2009 Free Software Foundation
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
  typedef std::char_traits<wchar_t>	traits_type;

  bool test __attribute__((unused)) = true;
  const wchar_t str_lit01[] = 
  L"   sun*ra \n\t\t\t   & his arkestra, featuring john gilmore: \n"
  L"                         "
    L"jazz in silhouette: images and forecasts of tomorrow";

  std::wstring str01(str_lit01);
  std::wstring strtmp;

  std::wstringbuf sbuf_03;
  std::wstringbuf sbuf_04(str01, std::ios_base::in);
  std::wstringbuf sbuf_05(str01, std::ios_base::in);

  std::wistream is_00(NULL);
  std::wistream is_04(&sbuf_04);
  std::wistream is_05(&sbuf_05);
  std::ios_base::iostate statefail, stateeof;
  statefail = std::ios_base::failbit;
  stateeof = std::ios_base::eofbit;
  wchar_t carray1[400] = L"";

  // int_type get()
  // istream& get(wchar_t*, streamsize, wchar_t delim)
  // istream& get(wchar_t*, streamsize)
  // istream& get(streambuf&, wchar_t delim)
  // istream& get(streambuf&)
  is_00.get(carray1, 2);
  VERIFY( static_cast<bool>(is_00.rdstate() & statefail) ); 
  VERIFY( is_00.gcount() == 0 );

  is_04.get(carray1, 4);
  VERIFY( !(is_04.rdstate() & statefail) );
  VERIFY( !traits_type::compare(carray1, L"   ", 4) );
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
  is_04.get(carray1, 200, L'[');
  VERIFY( static_cast<bool>(is_04.rdstate() & stateeof) );
  VERIFY( !(is_04.rdstate() & statefail) );
  VERIFY( is_04.gcount() == 125 );
  is_04.clear();  
  is_04.get(carray1, 200);
  VERIFY( static_cast<bool>(is_04.rdstate() & stateeof) );
  VERIFY( static_cast<bool>(is_04.rdstate() & statefail) ); 
  VERIFY( is_04.gcount() == 0 );

  std::wstringbuf sbuf_02(std::ios_base::in);
  is_05.clear();
  is_05.get(sbuf_02);
  VERIFY( is_05.gcount() == 0 );
  VERIFY( static_cast<bool>(is_05.rdstate() & statefail) ); 
  VERIFY( !(is_05.rdstate() & stateeof) ); 

  is_05.clear();
  is_05.get(sbuf_03);
  VERIFY( is_05.gcount() == 10 );
  VERIFY( sbuf_03.str() == L"   sun*ra " );
  VERIFY( !(is_05.rdstate() & statefail) ); 
  VERIFY( !(is_05.rdstate() & stateeof) ); 

  is_05.clear();
  is_05.get(sbuf_03, L'|');
  VERIFY( is_05.gcount() == 125 );
  VERIFY( sbuf_03.str() == str_lit01 );
  VERIFY( !(is_05.rdstate() & statefail) ); 
  VERIFY( static_cast<bool>(is_05.rdstate() & stateeof) ); 

  is_05.clear();
  is_05.get(sbuf_03, L'|');
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
