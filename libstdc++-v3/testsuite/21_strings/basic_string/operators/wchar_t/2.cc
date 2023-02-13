// 1998-10-01, 1999-06-25 bkoz

// Copyright (C) 1998-2023 Free Software Foundation, Inc.
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

// 21.3.7.1 basic_string non-member functions

// 21.3.7.2 operator==
/*
template<class charT, class traits, class Allocator>
  bool operator==(const basic_string<charT,traits,Allocator>& lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator==(const charT* lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator==(const basic_string<charT,traits,Allocator>& lhs,
                  const charT* rhs);
*/

// 21.3.7.3 operator!=
/*
template<class charT, class traits, class Allocator>
  bool operator!=(const basic_string<charT,traits,Allocator>& lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator!=(const charT* lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator!=(const basic_string<charT,traits,Allocator>& lhs, 
                  const charT* rhs);
*/

// 21.3.7.4 operator<
/*
template<class charT, class traits, class Allocator>
  bool operator< (const basic_string<charT,traits,Allocator>& lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator< (const basic_string<charT,traits,Allocator>& lhs,
                  const charT* rhs);

template<class charT, class traits, class Allocator>
  bool operator< (const charT* lhs, 
                  const basic_string<charT,traits,Allocator>& rhs);
*/

// 21.3.7.5 operator>
/*
template<class charT, class traits, class Allocator>
  bool operator> (const basic_string<charT,traits,Allocator>& lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator> (const basic_string<charT,traits,Allocator>& lhs,
                  const charT* rhs);

template<class charT, class traits, class Allocator>
  bool operator> (const charT* lhs,
                  const basic_string<charT,traits,Allocator>& rhs);
*/

//21.3.7.6 operator<=
/*
template<class charT, class traits, class Allocator>
  bool operator<=(const basic_string<charT,traits,Allocator>& lhs,
                  const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator<=(const basic_string<charT,traits,Allocator>& lhs,
                  const charT* rhs);

template<class charT, class traits, class Allocator>
  bool operator<=(const charT* lhs,
                  const basic_string<charT,traits,Allocator>& rhs);
*/

// 21.3.7.7 operator>=
/*
template<class charT, class traits, class Allocator>
  bool operator>=(const basic_string<charT,traits,Allocator>& lhs,
                const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  bool operator>=(const basic_string<charT,traits,Allocator>& lhs,
                  const charT* rhs);

template<class charT, class traits, class Allocator>
  bool operator>=(const charT* lhs,
                  const basic_string<charT,traits,Allocator>& rhs);
*/

#include <string>
#include <testsuite_hooks.h>

int test01(void)
{
  std::wstring 	str_0(L"costa rica");
  std::wstring 	str_1(L"costa marbella");
  std::wstring 	str_2(L"cost");
  std::wstring	str_3(L"costa ricans");
  std::wstring  str_4;
  
  str_4 = str_0;
  //comparisons between string objects
  VERIFY( !(str_0 == str_1) );
  VERIFY( !(str_0 == str_2) );
  VERIFY( !(str_0 == str_3) );
  VERIFY( !(str_1 == str_0) );
  VERIFY( !(str_2 == str_0) );
  VERIFY( !(str_3 == str_0) );
  VERIFY( str_4 == str_0 );
  VERIFY( str_0 == str_4 );

  VERIFY( str_0 != str_1 );
  VERIFY( str_0 != str_2 );
  VERIFY( str_0 != str_3 );
  VERIFY( str_1 != str_0 );
  VERIFY( str_2 != str_0 );
  VERIFY( str_3 != str_0 );
  VERIFY( !(str_0 != str_4) );
  VERIFY( !(str_4 != str_0) );
   
  VERIFY( str_0 > str_1 ); //true cuz r>m
  VERIFY( str_0 > str_2 );
  VERIFY( !(str_0 > str_3) );
  VERIFY( !(str_1 > str_0) ); //false cuz m<r
  VERIFY( !(str_2 > str_0) );
  VERIFY( str_3 > str_0 );
  VERIFY( !(str_0 > str_4) );
  VERIFY( !(str_4 > str_0) );

  VERIFY( !(str_0 < str_1) ); //false cuz r>m
  VERIFY( !(str_0 < str_2) );
  VERIFY( str_0 < str_3 );
  VERIFY( str_1 < str_0 ); //true cuz m<r
  VERIFY( str_2 < str_0 );
  VERIFY( !(str_3 < str_0) );
  VERIFY( !(str_0 < str_4) );
  VERIFY( !(str_4 < str_0) );

  VERIFY( str_0 >= str_1 ); //true cuz r>m
  VERIFY( str_0 >= str_2 );
  VERIFY( !(str_0 >= str_3) );
  VERIFY( !(str_1 >= str_0) );//false cuz m<r
  VERIFY( !(str_2 >= str_0) );
  VERIFY( str_3 >= str_0 );
  VERIFY( str_0 >= str_4 );
  VERIFY( str_4 >= str_0 );

  VERIFY( !(str_0 <= str_1) );//false cuz r>m
  VERIFY( !(str_0 <= str_2) );
  VERIFY( str_0 <= str_3 );
  VERIFY( str_1 <= str_0 );//true cuz m<r
  VERIFY( str_2 <= str_0 );
  VERIFY( !(str_3 <= str_0) );
  VERIFY( str_0 <= str_4 );
  VERIFY( str_4 <= str_0 );

  //comparisons between string object and string literal
  VERIFY( !(str_0 == L"costa marbella") );
  VERIFY( !(str_0 == L"cost") );
  VERIFY( !(str_0 == L"costa ricans") );
  VERIFY( !(L"costa marbella" == str_0) );
  VERIFY( !(L"cost" == str_0) );
  VERIFY( !(L"costa ricans" == str_0) );
  VERIFY( L"costa rica" == str_0 );
  VERIFY( str_0 == L"costa rica" );

  VERIFY( str_0 != L"costa marbella" );
  VERIFY( str_0 != L"cost" );
  VERIFY( str_0 != L"costa ricans" );
  VERIFY( L"costa marbella" != str_0 );
  VERIFY( L"cost" != str_0 );
  VERIFY( L"costa ricans" != str_0 );
  VERIFY( !(L"costa rica" != str_0) );
  VERIFY( !(str_0 != L"costa rica") );

  VERIFY( str_0 > L"costa marbella" ); //true cuz r>m
  VERIFY( str_0 > L"cost" );
  VERIFY( !(str_0 > L"costa ricans") );
  VERIFY( !(L"costa marbella" > str_0) );//false cuz m<r
  VERIFY( !(L"cost" > str_0) );
  VERIFY( L"costa ricans" > str_0 );
  VERIFY( !(L"costa rica" > str_0) );
  VERIFY( !(str_0 > L"costa rica") );

  VERIFY( !(str_0 < L"costa marbella") );//false cuz r>m
  VERIFY( !(str_0 < L"cost") );
  VERIFY( str_0 < L"costa ricans" );
  VERIFY( L"costa marbella" < str_0 );//true cuz m<r
  VERIFY( L"cost" < str_0 );
  VERIFY( !(L"costa ricans" < str_0) );
  VERIFY( !(L"costa rica" < str_0) );
  VERIFY( !(str_0 < L"costa rica") );

  VERIFY( str_0 >= L"costa marbella" );//true cuz r>m
  VERIFY( str_0 >= L"cost" );
  VERIFY( !(str_0 >= L"costa ricans") );
  VERIFY( !(L"costa marbella" >= str_0) );//false cuz m<r
  VERIFY( !(L"cost" >= str_0) );
  VERIFY( L"costa ricans" >= str_0 );
  VERIFY( L"costa rica" >= str_0 );
  VERIFY( str_0 >= L"costa rica" );

  VERIFY( !(str_0 <= L"costa marbella") );//false cuz r>m
  VERIFY( !(str_0 <= L"cost") );
  VERIFY( str_0 <= L"costa ricans" );
  VERIFY( L"costa marbella" <= str_0 );//true cuz m<r
  VERIFY( L"cost" <= str_0 );
  VERIFY( !(L"costa ricans" <= str_0) );
  VERIFY( L"costa rica" <= str_0 );
  VERIFY( str_0 <= L"costa rica" );

  // 21.3.7.1 operator+
/*
template<class charT, class traits, class Allocator>
  basic_string<charT,traits,Allocator>
    operator+(const basic_string<charT,traits,Allocator>& lhs,
              const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  basic_string<charT,traits,Allocator>
    operator+(const charT* lhs,
              const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  basic_string<charT,traits,Allocator>
    operator+(const basic_string<charT,traits,Allocator>& lhs,
              const charT* rhs);

template<class charT, class traits, class Allocator>
  basic_string<charT,traits,Allocator>
    operator+(charT lhs, const basic_string<charT,traits,Allocator>& rhs);

template<class charT, class traits, class Allocator>
  basic_string<charT,traits,Allocator>
    operator+(const basic_string<charT,traits,Allocator>& lhs, charT rhs);
*/

  str_4 = str_0 + L"ns";
  VERIFY( str_4 == str_3 );

  const std::wstring str_5(L" marbella");
  str_4 = L"costa" + str_5;
  VERIFY( str_4 == str_1 );

  std::wstring str_6(L"ns");
  str_4 = str_0 + str_6;
  VERIFY( str_4 == str_3 );

  str_4 = str_0 + L'n';
  str_4 = str_4 + L's';
  VERIFY( str_4 == str_3 );

  str_4 = L'a' + str_6;
  str_4 = L'c' + str_4;
  str_4 = L'i' + str_4;
  str_4 = L'r' + str_4;
  str_4 = L' ' + str_4;
  str_4 = L'a' + str_4;
  str_4 = L't' + str_4;
  str_4 = L's' + str_4;
  str_4 = L'o' + str_4;
  str_4 = L'c' + str_4;
  VERIFY( str_4 == str_3 );
  return 0;
}

int main() 
{
  test01();
  return 0;
}
