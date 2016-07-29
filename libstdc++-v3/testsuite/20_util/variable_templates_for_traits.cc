// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>

using namespace std;

// These tests are rather simple, the front-end tests already test
// variable templates, and the library tests for the underlying
// traits are more elaborate. These are just simple sanity tests.

static_assert(is_void_v<void> && is_void<void>::value, "");
static_assert(!is_void_v<int> && !is_void<int>::value, "");

static_assert(is_null_pointer_v<nullptr_t>
	      && is_null_pointer<nullptr_t>::value, "");
static_assert(!is_null_pointer_v<void*>
	      && !is_null_pointer<void*>::value, "");

static_assert(is_integral_v<int> && is_integral<int>::value, "");
static_assert(!is_integral_v<int*> && !is_integral<int*>::value, "");

static_assert(is_floating_point_v<float>
	      && is_floating_point<float>::value, "");
static_assert(!is_floating_point_v<int>
	      && !is_floating_point<int>::value, "");

static_assert(is_array_v<char[42]> && is_array<char[42]>::value, "");
static_assert(!is_array_v<char*> && !is_array<char*>::value, "");

static_assert(is_pointer_v<int*> && is_pointer<int*>::value, "");
static_assert(!is_pointer_v<int> && !is_pointer<int>::value, "");

static_assert(is_lvalue_reference_v<int&>
	      && is_lvalue_reference<int&>::value, "");
static_assert(!is_lvalue_reference_v<int>
	      && !is_lvalue_reference<int>::value, "");

static_assert(is_rvalue_reference_v<int&&>
	      && is_rvalue_reference<int&&>::value, "");
static_assert(!is_rvalue_reference_v<int>
	      && !is_rvalue_reference<int>::value, "");

struct EmptyFinal final {};

static_assert(is_member_object_pointer_v<int (EmptyFinal::*)>
	      && is_member_object_pointer<int (EmptyFinal::*)>::value, "");
static_assert(!is_member_object_pointer_v<void*>
	      && !is_member_object_pointer<void*>::value, "");

static_assert(is_member_function_pointer_v<int (EmptyFinal::*)()>
	      && is_member_function_pointer<int (EmptyFinal::*)()>::value, "");
static_assert(!is_member_function_pointer_v<void*>
	      && !is_member_function_pointer<void*>::value, "");

enum Enum {};

static_assert(is_enum_v<Enum> && is_enum<Enum>::value, "");
static_assert(!is_enum_v<int> && !is_enum<int>::value, "");

union Union;

static_assert(is_union_v<Union> && is_union<Union>::value, "");
static_assert(!is_union_v<int> && !is_union<int>::value, "");

static_assert(is_class_v<EmptyFinal> && is_class<EmptyFinal>::value, "");
static_assert(!is_class_v<int> && !is_class<int>::value, "");

static_assert(is_function_v<void()> && is_function<void()>::value, "");
static_assert(!is_function_v<void(*)()> && !is_function<void(*)()>::value, "");

static_assert(is_reference_v<int&> && is_reference<int&>::value, "");
static_assert(!is_reference_v<int> && !is_reference<int>::value, "");

static_assert(is_arithmetic_v<int> && is_arithmetic<int>::value, "");
static_assert(!is_arithmetic_v<void*> && !is_arithmetic<void*>::value, "");

static_assert(is_fundamental_v<int> && is_fundamental<int>::value, "");
static_assert(!is_fundamental_v<EmptyFinal>
	      && !is_fundamental<EmptyFinal>::value, "");

static_assert(is_object_v<int> && is_object<int>::value, "");
static_assert(!is_object_v<int&> && !is_object<int&>::value, "");

static_assert(is_scalar_v<int> && is_scalar<int>::value, "");
static_assert(!is_scalar_v<int&> && !is_scalar<int&>::value, "");

static_assert(is_compound_v<EmptyFinal>
	      && is_compound<EmptyFinal>::value, "");
static_assert(!is_compound_v<int> && !is_compound<int>::value, "");

static_assert(is_member_pointer_v<int (EmptyFinal::*)>
	      && is_member_pointer<int (EmptyFinal::*)>::value, "");
static_assert(!is_member_pointer_v<void*>
	      && !is_member_pointer<void*>::value, "");

static_assert(is_const_v<const int> && is_const<const int>::value, "");
static_assert(!is_const_v<int> && !is_const<int>::value, "");

static_assert(is_volatile_v<volatile int>
	      && is_volatile<volatile int>::value, "");
static_assert(!is_volatile_v<int> && !is_volatile<int>::value, "");

struct NType
{
  NType(int);
  ~NType();
  int i;
private:
  NType(const NType&);
  NType& operator=(const NType&);
  int i2;
};

static_assert(is_trivial_v<int> && is_trivial<int>::value, "");
static_assert(!is_trivial_v<NType> && !is_trivial<NType>::value, "");

static_assert(is_trivially_copyable_v<int>
	      && is_trivially_copyable<int>::value, "");
static_assert(!is_trivially_copyable_v<NType>
	      && !is_trivially_copyable<NType>::value, "");

static_assert(is_standard_layout_v<int>
	      && is_standard_layout<int>::value, "");
static_assert(!is_standard_layout_v<NType>
	      && !is_standard_layout<NType>::value, "");

static_assert(is_pod_v<int>
	      && is_pod<int>::value, "");
static_assert(!is_pod_v<NType>
	      && !is_pod<NType>::value, "");

static_assert(is_literal_type_v<int>
	      && is_literal_type<int>::value, "");
static_assert(!is_literal_type_v<NType>
	      && !is_literal_type<NType>::value, "");

static_assert(is_empty_v<EmptyFinal>
	      && is_empty<EmptyFinal>::value, "");
static_assert(!is_empty_v<NType>
	      && !is_empty<NType>::value, "");

struct Abstract {protected: virtual ~Abstract() = 0;};
struct Poly : Abstract {virtual ~Poly();};

static_assert(is_polymorphic_v<Poly>
	      && is_polymorphic<Poly>::value, "");
static_assert(!is_polymorphic_v<EmptyFinal>
	      && !is_polymorphic<EmptyFinal>::value, "");



static_assert(is_abstract_v<Abstract>
	      && is_abstract<Abstract>::value, "");
static_assert(!is_abstract_v<EmptyFinal>
	      && !is_abstract<EmptyFinal>::value, "");

static_assert(is_final_v<EmptyFinal>
	      && is_final<EmptyFinal>::value, "");
static_assert(!is_final_v<Abstract>
	      && !is_final<Abstract>::value, "");

static_assert(is_signed_v<int> && is_signed<int>::value, "");
static_assert(!is_signed_v<unsigned int>
	      && !is_signed<unsigned int>::value, "");

static_assert(is_constructible_v<int, int>
	      && is_constructible<int, int>::value, "");
static_assert(!is_constructible_v<int, void*>
	      && !is_constructible<int, void*>::value, "");

static_assert(is_default_constructible_v<int>
	      && is_default_constructible<int>::value, "");
static_assert(!is_default_constructible_v<NType>
	      && !is_default_constructible<NType>::value, "");

static_assert(is_copy_constructible_v<int>
	      && is_copy_constructible<int>::value, "");
static_assert(!is_copy_constructible_v<NType>
	      && !is_copy_constructible<NType>::value, "");

static_assert(is_move_constructible_v<int>
	      && is_copy_constructible<int>::value, "");
static_assert(!is_move_constructible_v<NType>
	      && !is_copy_constructible<NType>::value, "");

static_assert(is_assignable_v<int&, int>
	      && is_assignable<int&, int>::value, "");
static_assert(!is_assignable_v<int, int>
	      && !is_assignable<int, int>::value, "");

static_assert(is_copy_assignable_v<int>
	      && is_copy_assignable<int>::value, "");
static_assert(!is_copy_assignable_v<NType>
	      && !is_copy_assignable<NType>::value, "");

static_assert(is_move_assignable_v<int>
	      && is_move_assignable<int>::value, "");
static_assert(!is_move_assignable_v<NType>
	      && !is_move_assignable<NType>::value, "");

static_assert(is_destructible_v<int>
	      && is_destructible<int>::value, "");
static_assert(!is_destructible_v<Abstract>
	      && !is_destructible<Abstract>::value, "");

static_assert(is_trivially_constructible_v<int, int>
	      && is_trivially_constructible<int, int>::value, "");
static_assert(!is_trivially_constructible_v<NType, NType>
	      && !is_trivially_constructible<NType, NType>::value, "");

static_assert(is_trivially_default_constructible_v<int>
	      && is_trivially_default_constructible<int>::value, "");
static_assert(!is_trivially_default_constructible_v<NType>
	      && !is_trivially_default_constructible<NType>::value, "");

static_assert(is_trivially_copy_constructible_v<int>
	      && is_trivially_copy_constructible<int>::value, "");
static_assert(!is_trivially_copy_constructible_v<NType>
	      && !is_trivially_copy_constructible<NType>::value, "");

static_assert(is_trivially_move_constructible_v<int>
	      && is_trivially_move_constructible<int>::value, "");
static_assert(!is_trivially_move_constructible_v<NType>
	      && !is_trivially_move_constructible<NType>::value, "");

static_assert(is_trivially_assignable_v<int&, int>
	      && is_trivially_assignable<int&, int>::value, "");
static_assert(!is_trivially_assignable_v<NType, NType>
	      && !is_trivially_assignable<NType, NType>::value, "");

static_assert(is_trivially_copy_assignable_v<int>
	      && is_trivially_copy_assignable<int>::value, "");
static_assert(!is_trivially_copy_assignable_v<NType>
	      && !is_trivially_copy_assignable<NType>::value, "");

static_assert(is_trivially_move_assignable_v<int>
	      && is_trivially_move_assignable<int>::value, "");
static_assert(!is_trivially_move_assignable_v<NType>
	      && !is_trivially_move_assignable<NType>::value, "");

static_assert(is_trivially_destructible_v<int>
	      && is_trivially_destructible<int>::value, "");
static_assert(!is_trivially_destructible_v<Abstract>
	      && !is_trivially_destructible<Abstract>::value, "");

static_assert(is_nothrow_constructible_v<int, int>
	      && is_nothrow_constructible<int, int>::value, "");
static_assert(!is_nothrow_constructible_v<NType, NType>
	      && !is_nothrow_constructible<NType, NType>::value, "");

static_assert(is_nothrow_default_constructible_v<int>
	      && is_nothrow_default_constructible<int>::value, "");
static_assert(!is_nothrow_default_constructible_v<NType>
	      && !is_nothrow_default_constructible<NType>::value, "");

static_assert(is_nothrow_copy_constructible_v<int>
	      && is_nothrow_copy_constructible<int>::value, "");
static_assert(!is_nothrow_copy_constructible_v<NType>
	      && !is_nothrow_copy_constructible<NType>::value, "");

static_assert(is_nothrow_move_constructible_v<int>
	      && is_nothrow_move_constructible<int>::value, "");
static_assert(!is_nothrow_move_constructible_v<NType>
	      && !is_nothrow_move_constructible<NType>::value, "");

static_assert(is_nothrow_assignable_v<int&, int>
	      && is_nothrow_assignable<int&, int>::value, "");
static_assert(!is_nothrow_assignable_v<NType, NType>
	      && !is_nothrow_assignable<NType, NType>::value, "");

static_assert(is_nothrow_copy_assignable_v<int>
	      && is_nothrow_copy_assignable<int>::value, "");
static_assert(!is_nothrow_copy_assignable_v<NType>
	      && !is_nothrow_copy_assignable<NType>::value, "");

static_assert(is_nothrow_move_assignable_v<int>
	      && is_nothrow_move_assignable<int>::value, "");
static_assert(!is_nothrow_move_assignable_v<NType>
	      && !is_nothrow_move_assignable<NType>::value, "");

static_assert(has_virtual_destructor_v<Abstract>
	      && has_virtual_destructor<Abstract>::value, "");
static_assert(!has_virtual_destructor_v<NType>
	      && !has_virtual_destructor<NType>::value, "");

static_assert(alignment_of_v<int> == alignof(int)
	      && alignment_of<int>::value == alignof(int) , "");

static_assert(rank_v<int[1][1]> == rank<int[1][1]>::value, "");

static_assert(extent_v<int[1][2], 1> == 2
	      && extent<int[1][2], 1>::value == 2, "");

static_assert(is_same_v<int, int> && is_same<int, int>::value, "");
static_assert(!is_same_v<int, char> && !is_same<int, char>::value, "");

static_assert(is_base_of_v<Abstract, Poly>
	      && is_base_of<Abstract, Poly>::value, "");
static_assert(!is_base_of_v<Abstract, NType>
	      && !is_base_of<Abstract, NType>::value, "");

static_assert(is_convertible_v<int&, const int&>
	      && is_convertible<int&, const int&>::value, "");
static_assert(!is_convertible_v<const int&, int&>
	      && !is_convertible<const int&, int&>::value, "");

static_assert(negation_v<false_type>, "");
static_assert(!negation_v<true_type>, "");
static_assert(conjunction_v<>, "");
static_assert(!disjunction_v<>, "");
static_assert(conjunction_v<true_type>, "");
static_assert(!conjunction_v<false_type>, "");
static_assert(disjunction_v<true_type>, "");
static_assert(!disjunction_v<false_type>, "");
static_assert(conjunction_v<true_type, true_type>, "");
static_assert(!conjunction_v<true_type, false_type>, "");
static_assert(disjunction_v<false_type, true_type>, "");
static_assert(!disjunction_v<false_type, false_type>, "");
static_assert(conjunction_v<true_type, true_type,
              true_type>, "");
static_assert(!conjunction_v<true_type, true_type,
              false_type>, "");
static_assert(disjunction_v<false_type, false_type,
              true_type>, "");
static_assert(!disjunction_v<false_type, false_type,
              false_type>, "");
