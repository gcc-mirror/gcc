// 1999-07-08 bkoz

// Copyright (C) 1999, 2003, 2009 Free Software Foundation, Inc.
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

// 21.3.5.2 basic_string::append

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

bool test01(void)
{
  bool test __attribute__((unused)) = true;
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::const_reference cref;
  typedef std::wstring::reference ref;
  csize_type csz01;

  const wchar_t str_lit01[] = L"point bolivar, texas";
  const std::wstring str01(str_lit01);
  const std::wstring str02(L"corpus, ");
  const std::wstring str03;
  std::wstring str05;


  // wstring& append(const wstring&)
  str05 = str02;
  str05.append(str05); 
  VERIFY( str05 == L"corpus, corpus, " );
  str05.append(str01);
  VERIFY( str05 == L"corpus, corpus, point bolivar, texas" );
  str05.append(str03);
  VERIFY( str05 == L"corpus, corpus, point bolivar, texas" );
  std::wstring str06;
  str06.append(str05);
  VERIFY( str06 == str05 );


  // wstring& append(const wstring&, size_type pos, size_type n)
  str05.erase();
  str06.erase();
  csz01 = str03.size();
  try {
    str06.append(str03, csz01 + 1, 0);
    VERIFY( false ); 
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  csz01 = str01.size();
  try {
    str06.append(str01, csz01 + 1, 0);
    VERIFY( false ); 
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  str05 = str02;
  str05.append(str01, 0, std::wstring::npos);
  VERIFY( str05 == L"corpus, point bolivar, texas" );
  VERIFY( str05 != str02 );

  str06 = str02;
  str06.append(str01, 15, std::wstring::npos);
  VERIFY( str06 == L"corpus, texas" );
  VERIFY( str02 != str06 );


  // wstring& append(const wchar_t* s)
  str05.erase();
  str06.erase();
  str05.append(L"");
  VERIFY( str05 == str03 );

  str05.append(str_lit01);
  VERIFY( str05 == str01 );

  str06 = str02;
  str06.append(L"corpus, ");
  VERIFY( str06 == L"corpus, corpus, " );


  // wstring& append(const wchar_t* s, size_type n)
  str05.erase();
  str06.erase();
  str05.append(L"", 0);
  VERIFY( str05.size() == 0 );
  VERIFY( str05 == str03 );
  
  str05.append(str_lit01, sizeof(str_lit01) / sizeof(wchar_t) - 1);
  VERIFY( str05 == str01 );

  str06 = str02;
  str06.append(L"corpus, ", 6);
  VERIFY( str06 == L"corpus, corpus" );

  str06 = str02;
  str06.append(L"corpus, ", 12);
  VERIFY( str06 != L"corpus, corpus, " );


  // wstring& append(size_type n, char c)
  str05.erase();
  str06.erase();
  str05.append(0, L'a');
  VERIFY( str05 == str03 );
  str06.append(8, L'.');
  VERIFY( str06 == L"........" );


  // template<typename InputIter>
  //  wstring& append(InputIter first, InputIter last)
  str05.erase();
  str06.erase();
  str05.append(str03.begin(), str03.end());
  VERIFY( str05 == str03 );

  str06 = str02;
  str06.append(str01.begin(), str01.begin() + str01.find(L'r')); 
  VERIFY( str06 == L"corpus, point boliva" );
  VERIFY( str06 != str01 );
  VERIFY( str06 != str02 );

  str05 = str01;
  str05.append(str05.begin(), str05.begin() + str05.find(L'r')); 
  VERIFY( str05 ==  L"point bolivar, texaspoint boliva" );
  VERIFY( str05 != str01 );
  return test;
}

int main()
{ 
  test01();
  return 0;
}
