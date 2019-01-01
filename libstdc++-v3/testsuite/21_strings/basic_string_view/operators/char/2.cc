// { dg-options "-std=gnu++17" }

// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

// basic_string non-member functions

// operator==
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

// operator!=
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

// operator<
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

// operator>
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

// operator<=
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

// operator>=
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

#include <string_view>
#include <testsuite_hooks.h>

void
test01()
{
  std::string_view 	str_0("costa rica");
  std::string_view 	str_1("costa marbella");
  std::string_view 	str_2("cost");
  std::string_view	str_3("costa ricans");
  std::string_view        str_4;

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
  VERIFY( !(str_0 == "costa marbella") );
  VERIFY( !(str_0 == "cost") );
  VERIFY( !(str_0 == "costa ricans") );
  VERIFY( !("costa marbella" == str_0) );
  VERIFY( !("cost" == str_0) );
  VERIFY( !("costa ricans" == str_0) );
  VERIFY( "costa rica" == str_0 );
  VERIFY( str_0 == "costa rica" );

  VERIFY( str_0 != "costa marbella" );
  VERIFY( str_0 != "cost" );
  VERIFY( str_0 != "costa ricans" );
  VERIFY( "costa marbella" != str_0 );
  VERIFY( "cost" != str_0 );
  VERIFY( "costa ricans" != str_0 );
  VERIFY( !("costa rica" != str_0) );
  VERIFY( !(str_0 != "costa rica") );

  VERIFY( str_0 > "costa marbella" ); //true cuz r>m
  VERIFY( str_0 > "cost" );
  VERIFY( !(str_0 > "costa ricans") );
  VERIFY( !("costa marbella" > str_0) );//false cuz m<r
  VERIFY( !("cost" > str_0) );
  VERIFY( "costa ricans" > str_0 );
  VERIFY( !("costa rica" > str_0) );
  VERIFY( !(str_0 > "costa rica") );

  VERIFY( !(str_0 < "costa marbella") );//false cuz r>m
  VERIFY( !(str_0 < "cost") );
  VERIFY( str_0 < "costa ricans" );
  VERIFY( "costa marbella" < str_0 );//true cuz m<r
  VERIFY( "cost" < str_0 );
  VERIFY( !("costa ricans" < str_0) );
  VERIFY( !("costa rica" < str_0) );
  VERIFY( !(str_0 < "costa rica") );

  VERIFY( str_0 >= "costa marbella" );//true cuz r>m
  VERIFY( str_0 >= "cost" );
  VERIFY( !(str_0 >= "costa ricans") );
  VERIFY( !("costa marbella" >= str_0) );//false cuz m<r
  VERIFY( !("cost" >= str_0) );
  VERIFY( "costa ricans" >= str_0 );
  VERIFY( "costa rica" >= str_0 );
  VERIFY( str_0 >= "costa rica" );

  VERIFY( !(str_0 <= "costa marbella") );//false cuz r>m
  VERIFY( !(str_0 <= "cost") );
  VERIFY( str_0 <= "costa ricans" );
  VERIFY( "costa marbella" <= str_0 );//true cuz m<r
  VERIFY( "cost" <= str_0 );
  VERIFY( !("costa ricans" <= str_0) );
  VERIFY( "costa rica" <= str_0 );
  VERIFY( str_0 <= "costa rica" );
}

constexpr bool
test02()
{
  std::string_view 	str_0("costa rica");
  std::string_view 	str_1("costa marbella");
  std::string_view 	str_2("cost");
  std::string_view	str_3("costa ricans");
  std::string_view        str_4;

#undef VERIFY
#define VERIFY(x) if (!(x)) return false

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
  VERIFY( !(str_0 == "costa marbella") );
  VERIFY( !(str_0 == "cost") );
  VERIFY( !(str_0 == "costa ricans") );
  VERIFY( !("costa marbella" == str_0) );
  VERIFY( !("cost" == str_0) );
  VERIFY( !("costa ricans" == str_0) );
  VERIFY( "costa rica" == str_0 );
  VERIFY( str_0 == "costa rica" );

  VERIFY( str_0 != "costa marbella" );
  VERIFY( str_0 != "cost" );
  VERIFY( str_0 != "costa ricans" );
  VERIFY( "costa marbella" != str_0 );
  VERIFY( "cost" != str_0 );
  VERIFY( "costa ricans" != str_0 );
  VERIFY( !("costa rica" != str_0) );
  VERIFY( !(str_0 != "costa rica") );

  VERIFY( str_0 > "costa marbella" ); //true cuz r>m
  VERIFY( str_0 > "cost" );
  VERIFY( !(str_0 > "costa ricans") );
  VERIFY( !("costa marbella" > str_0) );//false cuz m<r
  VERIFY( !("cost" > str_0) );
  VERIFY( "costa ricans" > str_0 );
  VERIFY( !("costa rica" > str_0) );
  VERIFY( !(str_0 > "costa rica") );

  VERIFY( !(str_0 < "costa marbella") );//false cuz r>m
  VERIFY( !(str_0 < "cost") );
  VERIFY( str_0 < "costa ricans" );
  VERIFY( "costa marbella" < str_0 );//true cuz m<r
  VERIFY( "cost" < str_0 );
  VERIFY( !("costa ricans" < str_0) );
  VERIFY( !("costa rica" < str_0) );
  VERIFY( !(str_0 < "costa rica") );

  VERIFY( str_0 >= "costa marbella" );//true cuz r>m
  VERIFY( str_0 >= "cost" );
  VERIFY( !(str_0 >= "costa ricans") );
  VERIFY( !("costa marbella" >= str_0) );//false cuz m<r
  VERIFY( !("cost" >= str_0) );
  VERIFY( "costa ricans" >= str_0 );
  VERIFY( "costa rica" >= str_0 );
  VERIFY( str_0 >= "costa rica" );

  VERIFY( !(str_0 <= "costa marbella") );//false cuz r>m
  VERIFY( !(str_0 <= "cost") );
  VERIFY( str_0 <= "costa ricans" );
  VERIFY( "costa marbella" <= str_0 );//true cuz m<r
  VERIFY( "cost" <= str_0 );
  VERIFY( !("costa ricans" <= str_0) );
  VERIFY( "costa rica" <= str_0 );
  VERIFY( str_0 <= "costa rica" );

  return true;
}

int
main()
{
  test01();
  static_assert( test02() );
}
