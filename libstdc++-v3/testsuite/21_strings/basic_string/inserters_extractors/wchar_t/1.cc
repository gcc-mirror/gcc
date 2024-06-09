// 1999-07-01 bkoz

// Copyright (C) 1999-2024 Free Software Foundation, Inc.
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

// 21.3.7.9 inserters and extractors

// NB: This file is predicated on sstreams, istreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string class.

#include <string>
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <iostream>
#include <testsuite_hooks.h>

void test01(void)
{
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::const_reference cref;
  typedef std::wstring::reference ref;

  const std::wstring str01(L"sailing grand traverse bay\n"
	       L"\t\t\t    from Elk Rapids to the point reminds me of miles");
  const std::wstring str02(L"sailing");
  const std::wstring str03(L"grand");
  const std::wstring str04(L"traverse");
  const std::wstring str05;
  std::wstring str10;
  
  // istream& operator>>(istream&, string&)
  std::wistringstream istrs01(str01);
  istrs01 >> str10;
  VERIFY( str10 == str02 );
  try 
    {
      std::wistringstream::int_type i01 = istrs01.peek(); //a-boo
      VERIFY( std::wistringstream::traits_type::to_char_type(i01) == L' ' );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false ); // shouldn't throw
    }

  istrs01.clear();
  istrs01 >> str10; 
  VERIFY( str10 == str03 ); 
  istrs01.clear();
  istrs01 >> str10; 
  VERIFY( str10 == str04 ); // sentry picks out the white spaces. . 

  std::wistringstream istrs02(str05); // empty
  istrs02 >> str10;
  VERIFY( str10 == str04 );
 
  // istream& getline(istream&, string&, char)
  // istream& getline(istream&, string&)
  try 
    {
      istrs01.clear();
      getline(istrs01, str10);
      VERIFY( !istrs01.fail() );
      VERIFY( !istrs01.eof() );
      VERIFY( istrs01.good() );
      VERIFY( str10 == L" bay" );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false ); // shouldn't throw
    }

  try 
    {
      istrs01.clear();
      getline(istrs01, str10, L'\t');
      VERIFY( !istrs01.fail() );
      VERIFY( !istrs01.eof() );
      VERIFY( istrs01.good() );
      VERIFY( str10 == str05 );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false ); // shouldn't throw
    }
  
  try 
    {
      istrs01.clear();
      getline(istrs01, str10, L'\t');
      VERIFY( !istrs01.fail() );
      VERIFY( !istrs01.eof() );
      VERIFY( istrs01.good() );
      VERIFY( str10 == str05 );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false ); // shouldn't throw
    }
  
  try 
    {
      istrs01.clear();
      getline(istrs01, str10, L'.');
      VERIFY( !istrs01.fail() );
      VERIFY( istrs01.eof() );
      VERIFY( !istrs01.good() );
      VERIFY( str10 == L"\t    from Elk Rapids to the point reminds me of miles" );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false ); // shouldn't throw
    }

  try 
    {
      getline(istrs02, str10);
      VERIFY( istrs02.fail() );
      VERIFY( istrs02.eof() );
      VERIFY( str10 == L"\t    from Elk Rapids to the point reminds me of miles" );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false ); // shouldn't throw
    }
  
  // ostream& operator<<(ostream&, const basic_string&)
  std::wostringstream ostrs01;
  try 
    {
      ostrs01 << str01;
      VERIFY( ostrs01.str() == str01 );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false );
    }
  
  std::wstring hello_world;
  std::wcout << hello_world;
}

int main()
{ 
  test01();
  return 0;
}
