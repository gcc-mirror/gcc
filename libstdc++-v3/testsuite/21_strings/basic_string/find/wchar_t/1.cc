// 1999-06-09 bkoz

// Copyright (C) 1994, 1999, 2000, 2003 Free Software Foundation, Inc.
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

// 21.3.6.1 basic_string find

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

bool test01(void)
{
  bool test = true;
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::const_reference cref;
  typedef std::wstring::reference ref;
  csize_type npos = std::wstring::npos;
  csize_type csz01, csz02;

  const wchar_t str_lit01[] = L"mave";
  const std::wstring str01(L"mavericks, santa cruz");
  std::wstring str02(str_lit01);
  std::wstring str03(L"s, s");
  std::wstring str04;

  // size_type find(const wstring&, size_type pos = 0) const;
  csz01 = str01.find(str01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str01, 4);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str02, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str02, 3);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str03, 0);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 3);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 12);
  VERIFY( csz01 == npos );

  // An empty string consists of no characters
  // therefore it should be found at every point in a string,
  // except beyond the end
  csz01 = str01.find(str04, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str04, 5);
  VERIFY( csz01 == 5 );
  csz01 = str01.find(str04, str01.size());
  VERIFY( csz01 == str01.size() ); 
  csz01 = str01.find(str04, str01.size()+1);
  VERIFY( csz01 == npos ); 
  
  // size_type find(const wchar_t* s, size_type pos, size_type n) const;
  csz01 = str01.find(str_lit01, 0, 3);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3, 0);
  VERIFY( csz01 == 3 );

  // size_type find(const wchar_t* s, size_type pos = 0) const;
  csz01 = str01.find(str_lit01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3);
  VERIFY( csz01 == npos );

  // size_type find(wchar_t c, size_type pos = 0) const;
  csz01 = str01.find(L'z');
  csz02 = str01.size() - 1;
  VERIFY( csz01 == csz02 );
  csz01 = str01.find(L'/');
  VERIFY( csz01 == npos );
   
  // size_type find_first_of(const wstring&, size_type pos = 0) const;
  std::wstring str05(L"xena rulez");
  csz01 = str01.find_first_of(str01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_of(str01, 4);
  VERIFY( csz01 == 4 );
  csz01 = str01.find_first_of(str02, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_of(str02, 3);
  VERIFY( csz01 == 3 );
  csz01 = str01.find_first_of(str03, 0);
  VERIFY( csz01 == 8 );
  csz01 = str01.find_first_of(str03, 3);
  VERIFY( csz01 == 8 );
  csz01 = str01.find_first_of(str03, 12);
  VERIFY( csz01 == 16 );
  csz01 = str01.find_first_of(str05, 0);
  VERIFY( csz01 == 1 );
  csz01 = str01.find_first_of(str05, 4);
  VERIFY( csz01 == 4 );

  // An empty string consists of no characters
  // therefore it should be found at every point in a string,
  // except beyond the end
  // However, str1.find_first_of(str2,pos) finds the first character in 
  // str1 (starting at pos) that exists in str2, which is none for empty str2
  csz01 = str01.find_first_of(str04, 0);
  VERIFY( csz01 == npos );
  csz01 = str01.find_first_of(str04, 5);
  VERIFY( csz01 == npos );
  
  // size_type find_first_of(const wchar_t* s, size_type pos, size_type n) const;
  csz01 = str01.find_first_of(str_lit01, 0, 3);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_of(str_lit01, 3, 0);
  VERIFY( csz01 == npos );

  // size_type find_first_of(const char* s, size_type pos = 0) const;
  csz01 = str01.find_first_of(str_lit01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_of(str_lit01, 3);
  VERIFY( csz01 == 3 );

  // size_type find_first_of(wchar_t c, size_type pos = 0) const;
  csz01 = str01.find_first_of(L'z');
  csz02 = str01.size() - 1;
  VERIFY( csz01 == csz02 );

  // size_type find_last_of(const wstring& str, size_type pos = 0) const;
  // size_type find_last_of(const wchar_t* s, size_type pos, size_type n) const;
  // size_type find_last_of(const wchar_t* s, size_type pos = 0) const;
  // size_type find_last_of(wchar_t c, size_type pos = 0) const;

#if 1
// from tstring.cc, from jason merrill, et. al.
  std::wstring x;
  std::wstring::size_type pos;
  pos = x.find_last_not_of(L'X');
  VERIFY( pos == npos );
  pos = x.find_last_not_of(L"XYZ");
  VERIFY( pos == npos );

  std::wstring y(L"a");
  pos = y.find_last_not_of(L'X');
  VERIFY( pos == 0 );
  pos = y.find_last_not_of(L'a');
  VERIFY( pos == npos );
  pos = y.find_last_not_of(L"XYZ");
  VERIFY( pos == 0 );
  pos = y.find_last_not_of(L"a");
  VERIFY( pos == npos );

  std::wstring z(L"ab");
  pos = z.find_last_not_of(L'X');
  VERIFY( pos == 1 );
  pos = z.find_last_not_of(L"XYZ");
  VERIFY( pos == 1 );
  pos = z.find_last_not_of(L'b');
  VERIFY( pos == 0 );
  pos = z.find_last_not_of(L"Xb");
  VERIFY( pos == 0 );
  pos = z.find_last_not_of(L"Xa");
  VERIFY( pos == 1 );
  pos = z.find_last_of(L"ab");
  VERIFY( pos == 1 );
  pos = z.find_last_of(L"Xa");
  VERIFY( pos == 0 );
  pos = z.find_last_of(L"Xb");
  VERIFY( pos == 1 );
  pos = z.find_last_of(L"XYZ");
  VERIFY( pos == std::wstring::npos );
  pos = z.find_last_of(L'a');
  VERIFY( pos == 0 );
  pos = z.find_last_of(L'b');
  VERIFY( pos == 1 );
  pos = z.find_last_of(L'X');
  VERIFY( pos == std::wstring::npos );
#endif

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}

int main()
{ 
  test01();
  return 0;
}
