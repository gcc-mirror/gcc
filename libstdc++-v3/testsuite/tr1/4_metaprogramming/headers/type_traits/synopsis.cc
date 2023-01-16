// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <tr1/type_traits>

namespace std {
namespace tr1 {

  // [4.3] helper class:
  template <class T, T v> struct integral_constant;
  typedef integral_constant<bool, true> true_type;
  typedef integral_constant<bool, false> false_type;

  // [4.5.1] primary type categories:
  template <class T> struct is_void;
  template <class T> struct is_integral;
  template <class T> struct is_floating_point;
  template <class T> struct is_array;
  template <class T> struct is_pointer;
  template <class T> struct is_reference;
  template <class T> struct is_member_object_pointer;
  template <class T> struct is_member_function_pointer;
  template <class T> struct is_enum;
  template <class T> struct is_union;
  template <class T> struct is_class;
  template <class T> struct is_function;

  // [4.5.2] composite type categories:
  template <class T> struct is_arithmetic;
  template <class T> struct is_fundamental;
  template <class T> struct is_object;
  template <class T> struct is_scalar;
  template <class T> struct is_compound;
  template <class T> struct is_member_pointer;

  // [4.5.3] type properties:
  template <class T> struct is_const;
  template <class T> struct is_volatile;
  template <class T> struct is_pod;
  template <class T> struct is_empty;
  template <class T> struct is_polymorphic;
  template <class T> struct is_abstract;
  template <class T> struct has_trivial_constructor;
  template <class T> struct has_trivial_copy;
  template <class T> struct has_trivial_assign;
  template <class T> struct has_trivial_destructor;
  template <class T> struct has_nothrow_constructor;
  template <class T> struct has_nothrow_copy;
  template <class T> struct has_nothrow_assign;
  template <class T> struct has_virtual_destructor;
  template <class T> struct is_signed;
  template <class T> struct is_unsigned;
  template <class T> struct alignment_of;
  template <class T> struct rank;
  template <class T, unsigned I> struct extent;

  // [4.6] type relations:
  template <class T, class U> struct is_same;
  template <class Base, class Derived> struct is_base_of;
  template <class From, class To> struct is_convertible;

  // [4.7.1] const-volatile modifications:
  template <class T> struct remove_const;
  template <class T> struct remove_volatile;
  template <class T> struct remove_cv;
  template <class T> struct add_const;
  template <class T> struct add_volatile;
  template <class T> struct add_cv;

  // [4.7.2] reference modifications:
  template <class T> struct remove_reference;
  template <class T> struct add_reference;

  // [4.7.3] array modifications:
  template <class T> struct remove_extent;
  template <class T> struct remove_all_extents;

  // [4.7.4] pointer modifications:
  template <class T> struct remove_pointer;
  template <class T> struct add_pointer;

  // [4.8] other transformations:
  template <std::size_t Len, std::size_t Align> struct aligned_storage;
} // namespace tr1
} // namespace std
