// { dg-do run { target c++11 } }
//
// Copyright (C) 2008-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>
#include <testsuite_hooks.h>

#define JOIN( X, Y ) DO_JOIN( X, Y )
#define DO_JOIN( X, Y ) DO_JOIN2(X,Y)
#define DO_JOIN2( X, Y ) X##Y

#define COMMON_TYPE_TEST_1(type1, uid) \
  typedef common_type<type1>::type JOIN(test_t,uid); \
  VERIFY( (is_same<JOIN(test_t,uid), JOIN(test_t,uid)>::value) ); \
  typedef common_type<const type1>::type JOIN(test_t,JOIN(uid,c)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,c)), \
                   JOIN(test_t,JOIN(uid,c))>::value) ); \
  typedef common_type<volatile type1>::type JOIN(test_t,JOIN(uid,v)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,v)), \
                   JOIN(test_t,JOIN(uid,v))>::value) ); \
  typedef common_type<const volatile type1>::type JOIN(test_t,JOIN(uid,cv)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,cv)), \
                   JOIN(test_t,JOIN(uid,cv))>::value) ); \
  typedef common_type<type1 &>::type JOIN(test_t,JOIN(uid,l)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,l)), \
                   JOIN(test_t,JOIN(uid,l))>::value) ); \
  typedef common_type<const type1 &>::type JOIN(test_t,JOIN(uid,lc)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,lc)), \
                   JOIN(test_t,JOIN(uid,lc))>::value) ); \
  typedef common_type<volatile type1 &>::type JOIN(test_t,JOIN(uid,lv)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,lv)), \
                   JOIN(test_t,JOIN(uid,lv))>::value) ); \
  typedef common_type<const volatile type1 &>::type JOIN(test_t,JOIN(uid,lcv)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,lcv)), \
                   JOIN(test_t,JOIN(uid,lcv))>::value) ); \
  typedef common_type<type1 &&>::type JOIN(test_t,JOIN(uid,r)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,r)), \
                   JOIN(test_t,JOIN(uid,r))>::value) ); \
  typedef common_type<const type1 &&>::type JOIN(test_t,JOIN(uid,rc)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,rc)), \
                   JOIN(test_t,JOIN(uid,rc))>::value) ); \
  typedef common_type<volatile type1 &&>::type JOIN(test_t,JOIN(uid,rv)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,rv)), \
                   JOIN(test_t,JOIN(uid,rv))>::value) ); \
  typedef common_type<const volatile type1 &&>::type JOIN(test_t,JOIN(uid,rcv)); \
  VERIFY( (is_same<JOIN(test_t,JOIN(uid,rcv)), \
                   JOIN(test_t,JOIN(uid,rcv))>::value) )
    
struct A { };
struct B : A { };

void test01()
{
  using std::common_type;
  using std::is_same;

  // Positive tests.
  COMMON_TYPE_TEST_1(int, 1);
  COMMON_TYPE_TEST_1(double, 2);
  COMMON_TYPE_TEST_1(A, 3);
  COMMON_TYPE_TEST_1(B, 4);
}

#define COMMON_TYPE_TEST_2_IMPL(type1, type2, type3, uid) \
  typedef common_type<type1, type2>::type  	JOIN(JOIN(test, uid),_t1); \
  typedef common_type<type2, type1>::type  	JOIN(JOIN(test, uid),_t2); \
  VERIFY( (is_same<JOIN(JOIN(test, uid),_t1), type3>::value) ); \
  VERIFY( (is_same<JOIN(JOIN(test, uid),_t2), type3>::value) )

#define NO_CV
  
#define COMMON_TYPE_TEST_2(cv_qual, type1, type2, type3, uid) \
  COMMON_TYPE_TEST_2_IMPL(cv_qual type1, type2, type3, uid); \
  COMMON_TYPE_TEST_2_IMPL(cv_qual type1 &, type2, type3, JOIN(uid,l)); \
  COMMON_TYPE_TEST_2_IMPL(cv_qual type1 &&, type2, type3, JOIN(uid,r))

#define COMMON_TYPE_TEST_ALL_2(type1, type2, type3, uid) \
  COMMON_TYPE_TEST_2(NO_CV, type1, type2, type3, uid); \
  COMMON_TYPE_TEST_2(const, type1, type2, type3, uid); \
  COMMON_TYPE_TEST_2(volatile, type1, type2, type3, uid); \
  COMMON_TYPE_TEST_2(const volatile, type1, type2, type3, uid)

void test02()
{
  using std::common_type;
  using std::is_same;
  
  COMMON_TYPE_TEST_ALL_2(int, int, int, 1);
  COMMON_TYPE_TEST_ALL_2(int, double, double, 2);
  COMMON_TYPE_TEST_2(NO_CV, A, A, A, 3);
  COMMON_TYPE_TEST_2(const, A, A, A, 4);
  COMMON_TYPE_TEST_2(NO_CV, B, A, A, 5);  
}

int main()
{
  test01();
  test02();
  return 0;
}
