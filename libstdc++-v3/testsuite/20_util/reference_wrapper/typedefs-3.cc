// { dg-do compile { target c++11 } }
// { dg-skip-if "argument_type removed for C++20" { c++2a } }

// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

#include <functional>
#include <type_traits>

struct S { };

struct S0
{
  typedef int argument_type;
};

struct S1
{
  typedef float first_argument_type;
};

struct S2
{
  typedef char second_argument_type;
};

struct S01 : S0, S1 { };
struct S02 : S0, S2 { };
struct S12 : S1, S2 { };

struct S012 : S0, S1, S2 { };

using std::true_type;
using std::false_type;
using std::__void_t;

_GLIBCXX_HAS_NESTED_TYPE(argument_type)
_GLIBCXX_HAS_NESTED_TYPE(first_argument_type)
_GLIBCXX_HAS_NESTED_TYPE(second_argument_type)

template<typename T>
  struct has_arg_type : __has_argument_type<T>
  { };

template<typename T>
  struct has_1st_arg_type : __has_first_argument_type<T>
  { };

template<typename T>
  struct has_2nd_arg_type : __has_second_argument_type<T>
  { };

template<typename T, bool = has_arg_type<T>::value>
struct test_arg_type
{
  static_assert( !has_arg_type<std::reference_wrapper<T>>::value,
      "reference_wrapper has no nested argument_type");
};

template<typename T>
struct test_arg_type<T, true>
{
  typedef std::reference_wrapper<T> ref;

  static_assert( has_arg_type<ref>::value,
      "reference_wrapper has nested argument_type");

  static_assert(
      std::is_same< typename T::argument_type,
                    typename ref::argument_type >::value,
      "reference_wrapper has the correct argument_type");
};

template<typename T,
         bool = has_1st_arg_type<T>::value && has_2nd_arg_type<T>::value>
struct test_1st_2nd_arg_types
{
  typedef std::reference_wrapper<T> ref;

  static_assert( !has_1st_arg_type<ref>::value,
      "reference_wrapper has no nested first_argument_type");

  static_assert( !has_2nd_arg_type<ref>::value,
      "reference_wrapper has no nested second_argument_type");
};

template<typename T>
struct test_1st_2nd_arg_types<T, true>
{
  typedef std::reference_wrapper<T> ref;

  static_assert( has_1st_arg_type<ref>::value,
      "reference_wrapper has nested first_argument_type");

  static_assert( has_2nd_arg_type<ref>::value,
      "reference_wrapper has nested second_argument_type");

  static_assert(
      std::is_same< typename T::first_argument_type,
                    typename ref::first_argument_type>::value,
      "reference_wrapper has correct first_argument_type");

  static_assert(
      std::is_same< typename T::second_argument_type,
                    typename ref::second_argument_type>::value,
      "reference_wrapper has correct second_argument_type");
};


template<typename T>
  void test()
  {
    test_arg_type<T> t __attribute__((unused));
    test_arg_type<const T> tc __attribute__((unused));
    test_arg_type<volatile T> tv __attribute__((unused));
    test_arg_type<const volatile T> tcv __attribute__((unused));
    test_1st_2nd_arg_types<T> t12 __attribute__((unused));
    test_1st_2nd_arg_types<const T> t12c __attribute__((unused));
    test_1st_2nd_arg_types<volatile T> t12v __attribute__((unused));
    test_1st_2nd_arg_types<const volatile T> t12cv __attribute__((unused));
  }

int main()
{
  test<S>();
  test<S0>();
  test<S1>();
  test<S2>();
  test<S01>();
  test<S02>();
  test<S12>();
  test<S012>();
}

