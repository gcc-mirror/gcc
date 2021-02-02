// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// C++20 21.3.3.2 Non-member comparison functions [string.cmp]

// operator==
/*
template<class charT, class traits, class Allocator>
  constexpr bool
  operator==(const basic_string<charT, traits, Allocator>& lhs,
	     const basic_string<charT, traits, Allocator>& rhs);

template<class charT, class traits, class Allocator>
  constexpr bool
  operator==(const basic_string<charT, traits, Allocator>& lhs,
	     const charT* rhs);
*/

// operator<=>
/*
template<class charT, class traits, class Allocator>
  constexpr [see below]
  operator<=>(const basic_string<charT, traits, Allocator>& lhs,
	      const basic_string<charT, traits, Allocator>& rhs);

template<class charT, class traits, class Allocator>
  constexpr [see below]
  operator<=>(const basic_string<charT,traits,Allocator>& lhs,
	      const charT* rhs);
*/

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  std::string 	str_0("costa rica");
  std::string 	str_1("costa marbella");
  std::string 	str_2("cost");
  std::string	str_3("costa ricans");
  std::string        str_4;

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

  VERIFY( std::is_gt(str_0 <=> str_1) );
  VERIFY( std::is_gt(str_0 <=> str_2) );
  VERIFY( std::is_lt(str_0 <=> str_3) );
  VERIFY( std::is_eq(str_0 <=> str_4) );
  VERIFY( std::is_lt(str_1 <=> str_0) );
  VERIFY( std::is_lt(str_2 <=> str_0) );
  VERIFY( std::is_gt(str_3 <=> str_0) );
  VERIFY( std::is_eq(str_4 <=> str_0) );

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

  VERIFY( std::is_gt(str_0 <=> "costa marbella") );
  VERIFY( std::is_gt(str_0 <=> "cost") );
  VERIFY( std::is_lt(str_0 <=> "costa ricans") );
  VERIFY( std::is_eq(str_0 <=> "costa rica") );
  VERIFY( std::is_lt("costa marbella" <=> str_0) );
  VERIFY( std::is_lt("cost" <=> str_0) );
  VERIFY( std::is_gt("costa ricans" <=> str_0) );
  VERIFY( std::is_eq("costa rica" <=> str_0) );
}

int main()
{
  test01();
}
