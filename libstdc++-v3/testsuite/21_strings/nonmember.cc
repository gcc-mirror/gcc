// 1998-10-01, 1999-06-25 bkoz

// Copyright (C) 1998-1999 Free Software Foundation, Inc.
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
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

int test01(void)
{
  bool 		test = true;
  std::string 	str_0("costa rica");
  std::string 	str_1("costa marbella");
  std::string 	str_2("cost");
  std::string	str_3("costa ricans");
  std::string        str_4;
  
  str_4 = str_0;
  //comparisons between string objects
  test &= !(str_0 == str_1);
  test &= !(str_0 == str_2);
  test &= !(str_0 == str_3);
  test &= !(str_1 == str_0);
  test &= !(str_2 == str_0);
  test &= !(str_3 == str_0);
  test &= str_4 == str_0;
  test &= str_0 == str_4;

  test &= str_0 != str_1;
  test &= str_0 != str_2;
  test &= str_0 != str_3;
  test &= str_1 != str_0;
  test &= str_2 != str_0;
  test &= str_3 != str_0;
  test &= !(str_0 != str_4);
  test &= !(str_4 != str_0);
   
  test &= str_0 > str_1; //true cuz r>m
  test &= str_0 > str_2;
  test &= !(str_0 > str_3);
  test &= !(str_1 > str_0); //false cuz m<r
  test &= !(str_2 > str_0);
  test &= str_3 > str_0;
  test &= !(str_0 > str_4);
  test &= !(str_4 > str_0);

  test &= !(str_0 < str_1); //false cuz r>m
  test &= !(str_0 < str_2);
  test &= str_0 < str_3;
  test &= str_1 < str_0; //true cuz m<r
  test &= str_2 < str_0;
  test &= !(str_3 < str_0);
  test &= !(str_0 < str_4);
  test &= !(str_4 < str_0);

  test &= str_0 >= str_1; //true cuz r>m
  test &= str_0 >= str_2;
  test &= !(str_0 >= str_3);
  test &= !(str_1 >= str_0);//false cuz m<r
  test &= !(str_2 >= str_0);
  test &= str_3 >= str_0;
  test &= str_0 >= str_4;
  test &= str_4 >= str_0;

  test &= !(str_0 <= str_1);//false cuz r>m
  test &= !(str_0 <= str_2);
  test &= str_0 <= str_3;
  test &= str_1 <= str_0;//true cuz m<r
  test &= str_2 <= str_0;
  test &= !(str_3 <= str_0);
  test &= str_0 <= str_4;
  test &= str_4 <= str_0;

  //comparisons between string object and string literal
  test &= !(str_0 == "costa marbella");
  test &= !(str_0 == "cost");
  test &= !(str_0 == "costa ricans");
  test &= !("costa marbella" == str_0);
  test &= !("cost" == str_0);
  test &= !("costa ricans" == str_0);
  test &= "costa rica" == str_0;
  test &= str_0 == "costa rica";

  test &= str_0 != "costa marbella";
  test &= str_0 != "cost";
  test &= str_0 != "costa ricans";
  test &= "costa marbella" != str_0;
  test &= "cost" != str_0;
  test &= "costa ricans" != str_0;
  test &= !("costa rica" != str_0);
  test &= !(str_0 != "costa rica");

  test &= str_0 > "costa marbella"; //true cuz r>m
  test &= str_0 > "cost";
  test &= !(str_0 > "costa ricans");
  test &= !("costa marbella" > str_0);//false cuz m<r
  test &= !("cost" > str_0);
  test &= "costa ricans" > str_0;
  test &= !("costa rica" > str_0);
  test &= !(str_0 > "costa rica");

  test &= !(str_0 < "costa marbella");//false cuz r>m
  test &= !(str_0 < "cost");
  test &= str_0 < "costa ricans";
  test &= "costa marbella" < str_0;//true cuz m<r
  test &= "cost" < str_0;
  test &= !("costa ricans" < str_0);
  test &= !("costa rica" < str_0);
  test &= !(str_0 < "costa rica");

  test &= str_0 >= "costa marbella";//true cuz r>m
  test &= str_0 >= "cost";
  test &= !(str_0 >= "costa ricans");
  test &= !("costa marbella" >= str_0);//false cuz m<r
  test &= !("cost" >= str_0);
  test &= "costa ricans" >= str_0;
  test &= "costa rica" >= str_0;
  test &= str_0 >= "costa rica";

  test &= !(str_0 <= "costa marbella");//false cuz r>m
  test &= !(str_0 <= "cost");
  test &= str_0 <= "costa ricans";
  test &= "costa marbella" <= str_0;//true cuz m<r
  test &= "cost" <= str_0;
  test &= !("costa ricans" <= str_0);
  test &= "costa rica" <= str_0;
  test &= str_0 <= "costa rica";

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

  str_4 = str_0 + "ns";
  test &= str_4 == str_3;

  const std::string str_5(" marbella");
  str_4 = "costa" + str_5;
  test &= str_4 == str_1;

  std::string str_6("ns");
  str_4 = str_0 + str_6;
  test &= str_4 == str_3;

  str_4 = str_0 + 'n';
  str_4 = str_4 + 's';
  test &= str_4 == str_3;

  str_4 = 'a' + str_6;
  str_4 = 'c' + str_4;
  str_4 = 'i' + str_4;
  str_4 = 'r' + str_4;
  str_4 = ' ' + str_4;
  str_4 = 'a' + str_4;
  str_4 = 't' + str_4;
  str_4 = 's' + str_4;
  str_4 = 'o' + str_4;
  str_4 = 'c' + str_4;
  test &= str_4 == str_3;

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return 0;
}

int main() {
  test01();
}






