// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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

#include <type_traits>
#include <initializer_list>

// Helper types:
struct has_type_impl
{
  template<typename T, typename = typename T::type>
  static std::true_type test(int);
  template<typename>
  static std::false_type test(...);
};

template<typename T>
struct has_type : public decltype(has_type_impl::test<T>(0))
{};

template<typename T, typename Expected>
struct is_expected_type : public std::is_same<typename T::type, Expected>
{};

template<typename P1, typename P2>
struct and_ : public std::conditional<P1::value, P2, std::false_type>::type
{};

template<typename T, typename Expected>
struct is_type : public and_<has_type<T>, is_expected_type<T, Expected>>
{};

// Inspection types:

struct S {};

struct B {};
struct D : B {};

struct F1 { operator void*(); };
struct F2 { operator void*(); };

struct G1 { operator const void*(); };
struct G2 { operator volatile void*(); };

template<typename T>
struct ImplicitTo
{
  operator T();
};

template<typename T>
struct ExplicitTo
{
  explicit operator T();
};

template<typename T>
struct PrivateImplicitTo
{
private:
  operator T();
};

auto lmd1 = [](int, double) {};
auto lmd2 = [](int, double) {};

struct Abstract
{
  virtual ~Abstract() = 0;
};

enum class ScEn;

enum UnscEn : int;

struct Ukn;

union U
{
  int i;
};

union U2
{
  long i;
};

union UConv1
{
  operator Abstract*();
};

union UConv2
{
  operator Abstract*();
};

struct X1 {};
struct X2 {};
struct RX12 {};
struct RX21 {};
struct Y1 {};
struct Y2 {};
struct Y3 {};
struct Y4 {};

namespace std {

  template<>
  struct common_type<X1, X2>
  {
    typedef RX12 type;
  };

  template<>
  struct common_type<X2, X1>
  {
    typedef RX21 type;
  };

  template<>
  struct common_type<RX12, X1>
  {
    typedef Y1 type;
  };

  template<>
  struct common_type<X1, RX12>
  {
    typedef Y2 type;
  };

  template<>
  struct common_type<RX21, X1>
  {
    typedef Y3 type;
  };

  template<>
  struct common_type<X1, RX21>
  {
    typedef Y4 type;
  };
}

static_assert(is_type<std::common_type<int>, int>(), "");
static_assert(is_type<std::common_type<const int>, int>(), "");
static_assert(is_type<std::common_type<int, int>, int>(), "");
static_assert(is_type<std::common_type<const int, int>, int>(), "");
static_assert(is_type<std::common_type<ScEn, ScEn>, ScEn>(), "");
static_assert(is_type<std::common_type<UnscEn, UnscEn>, UnscEn>(), "");
static_assert(is_type<std::common_type<UnscEn, int>, int>(), "");
static_assert(is_type<std::common_type<int, int, int>, int>(), "");
static_assert(is_type<std::common_type<int, int, int, int>, int>(), "");
static_assert(is_type<std::common_type<int, int, int, int, int>, int>(), "");
static_assert(is_type<std::common_type<S, S>, S>(), "");
static_assert(is_type<std::common_type<const S, const S>, S>(), "");
static_assert(is_type<std::common_type<std::initializer_list<int>,
	      std::initializer_list<int>>, std::initializer_list<int>>(), "");
static_assert(is_type<std::common_type<B, D>, B>(), "");
static_assert(is_type<std::common_type<D, B>, B>(), "");
static_assert(is_type<std::common_type<F1, F2>, void*>(), "");
static_assert(is_type<std::common_type<F2, F1>, void*>(), "");
static_assert(is_type<std::common_type<G1, G2>, const volatile void*>(), "");
static_assert(is_type<std::common_type<G2, G1>, const volatile void*>(), "");
static_assert(is_type<std::common_type<int*, const volatile int*>,
	      const volatile int*>(), "");
static_assert(is_type<std::common_type<void*, const volatile int*>,
	      const volatile void*>(), "");
static_assert(is_type<std::common_type<void>, void>(), "");
static_assert(is_type<std::common_type<const void>, void>(), "");
static_assert(is_type<std::common_type<void, void>, void>(), "");
static_assert(is_type<std::common_type<const void, const void>, void>(), "");
static_assert(is_type<std::common_type<int&, int&&>, int>(), "");
static_assert(is_type<std::common_type<int&, int&>, int>(), "");
static_assert(is_type<std::common_type<int&&, int&&>, int>(), "");
static_assert(is_type<std::common_type<int&&, const int&&>, int>(), "");
static_assert(is_type<std::common_type<U&, const U&&>, U>(), "");
static_assert(is_type<std::common_type<U&, U&>, U>(), "");
static_assert(is_type<std::common_type<U&&, U&&>, U>(), "");
static_assert(is_type<std::common_type<int B::*, int D::*>, int D::*>(), "");
static_assert(is_type<std::common_type<int D::*, int B::*>, int D::*>(), "");
static_assert(is_type<std::common_type<const int B::*, volatile int D::*>,
	      const volatile int D::*>(), "");
static_assert(is_type<std::common_type<int (B::*)(), int (D::*)()>,
	      int (D::*)()>(), "");
static_assert(is_type<std::common_type<int (B::*)() const, int (D::*)() const>,
	      int (D::*)() const>(), "");
static_assert(is_type<std::common_type<int[3], int[3]>, int*>(), "");
static_assert(is_type<std::common_type<int[1], const int[3]>,
	      const int*>(), "");
static_assert(is_type<std::common_type<void(), void()>, void(*)()>(), "");
static_assert(is_type<std::common_type<void(&)(), void(&)()>, void(*)()>(), "");
static_assert(is_type<std::common_type<void(&)(), void(&&)()>,
	      void(*)()>(), "");
static_assert(is_type<std::common_type<void(&&)(), void(&)()>,
	      void(*)()>(), "");
static_assert(is_type<std::common_type<void(&&)(), void(&&)()>,
	      void(*)()>(), "");
static_assert(is_type<std::common_type<ImplicitTo<int>, int>, int>(), "");
static_assert(is_type<std::common_type<ImplicitTo<int>, ImplicitTo<int>>,
	      ImplicitTo<int>>(), "");
static_assert(is_type<std::common_type<ImplicitTo<int>, int,
	      ImplicitTo<int>>, int>(), "");
static_assert(is_type<std::common_type<ExplicitTo<int>, ExplicitTo<int>>,
	      ExplicitTo<int>>(), "");
static_assert(is_type<std::common_type<decltype(lmd1), decltype(lmd1)>,
	      decltype(lmd1)>(), "");
static_assert(is_type<std::common_type<decltype(lmd1)&, decltype(lmd1)&>,
	      decltype(lmd1)>(), "");
static_assert(is_type<std::common_type<decltype(lmd1)&, decltype(lmd2)&>,
	      void(*)(int, double)>(), "");
static_assert(is_type<std::common_type<decltype(nullptr), void*>, void*>(), "");
static_assert(is_type<std::common_type<decltype(nullptr), int*>, int*>(), "");
static_assert(is_type<std::common_type<const decltype(nullptr)&, int*>,
	      int*>(), "");
static_assert(is_type<std::common_type<decltype(nullptr), const volatile int*>,
	      const volatile int*>(), "");
static_assert(is_type<std::common_type<decltype(nullptr), int (B::*)()>,
	      int (B::*)()>(), "");
static_assert(is_type<std::common_type<decltype(nullptr), int (B::*)() const>,
	      int (B::*)() const>(), "");
static_assert(is_type<std::common_type<decltype(nullptr), const int B::*>,
	      const int B::*>(), "");
static_assert(is_type<std::common_type<Abstract&, Abstract&>, Abstract>(), "");
static_assert(is_type<std::common_type<Ukn&, Ukn&>, Ukn>(), "");
static_assert(is_type<std::common_type<ImplicitTo<B&>, B&>, B>(), "");
static_assert(is_type<std::common_type<ImplicitTo<B&>&, B&&>, B>(), "");
static_assert(is_type<std::common_type<UConv1, const Abstract*&>,
	      const Abstract*>(), "");
static_assert(is_type<std::common_type<UConv1, UConv2>, Abstract*>(), "");
static_assert(is_type<std::common_type<UConv1&, UConv2&>, Abstract*>(), "");

static_assert(is_type<std::common_type<Abstract&&, Abstract&&>,
	      Abstract>(), "");
static_assert(is_type<std::common_type<const Abstract&&,
				       const Abstract&&>, Abstract>(), "");
static_assert(is_type<std::common_type<volatile Abstract&&,
				       volatile Abstract&&>, Abstract>(), "");
static_assert(is_type<std::common_type<Ukn&&, Ukn&&>, Ukn>(), "");
static_assert(is_type<std::common_type<const Ukn&&, const Ukn&&>,
	      Ukn>(), "");
static_assert(is_type<std::common_type<volatile Ukn&&, volatile Ukn&&>,
	      Ukn>(), "");

static_assert(is_type<std::common_type<X1, X2>, RX12>(), "");
static_assert(is_type<std::common_type<X2, X1>, RX21>(), "");

static_assert(is_type<std::common_type<X1, X2, X1>, Y1>(), "");
static_assert(is_type<std::common_type<X2, X1, X1>, Y3>(), "");

static_assert(is_type<std::common_type<X1, X1, X2>, RX12>(), "");
static_assert(is_type<std::common_type<X1, X1, X2, X1>, Y1>(), "");

static_assert(!has_type<std::common_type<>>(), "");
static_assert(!has_type<std::common_type<int, S>>(), "");
static_assert(!has_type<std::common_type<U, S>>(), "");
static_assert(!has_type<std::common_type<U, U2>>(), "");
static_assert(!has_type<std::common_type<const ImplicitTo<int>, int>>(), "");
static_assert(!has_type<std::common_type<PrivateImplicitTo<int>, int>>(), "");
static_assert(!has_type<std::common_type<const PrivateImplicitTo<int>,
	      int>>(), "");
static_assert(!has_type<std::common_type<int, Ukn>>(), "");
static_assert(!has_type<std::common_type<int, Abstract>>(), "");
static_assert(!has_type<std::common_type<Ukn, Abstract>>(), "");
static_assert(!has_type<std::common_type<int, void>>(), "");
static_assert(!has_type<std::common_type<int, const volatile void>>(), "");
static_assert(!has_type<std::common_type<Abstract, void>>(), "");
static_assert(!has_type<std::common_type<Ukn, void>>(), "");
static_assert(!has_type<std::common_type<int[4], void>>(), "");
static_assert(!has_type<std::common_type<ScEn, void>>(), "");
static_assert(!has_type<std::common_type<UnscEn, void>>(), "");
static_assert(!has_type<std::common_type<U, void>>(), "");
static_assert(!has_type<std::common_type<std::initializer_list<int>,
	      void>>(), "");
static_assert(!has_type<std::common_type<int, int, int, S>>(), "");
static_assert(!has_type<std::common_type<int, int, S, int>>(), "");
static_assert(!has_type<std::common_type<int, S, int, int>>(), "");
static_assert(!has_type<std::common_type<S, int, int, int>>(), "");
static_assert(!has_type<std::common_type<int, int, void, int, int>>(), "");
static_assert(!has_type<std::common_type<B, S>>(), "");
static_assert(!has_type<std::common_type<int, B, S>>(), "");
static_assert(!has_type<std::common_type<B, int, S>>(), "");
static_assert(!has_type<std::common_type<B, S, int>>(), "");
static_assert(!has_type<std::common_type<int*, double*>>(), "");
static_assert(!has_type<std::common_type<void*, void(*)(...)>>(), "");
static_assert(!has_type<std::common_type<void(*)(), void(*)(...)>>(), "");
static_assert(!has_type<std::common_type<void(*)(), void(S::*)()>>(), "");
static_assert(!has_type<std::common_type<void(S::*)() const,
	      void(S::*)()>>(), "");
static_assert(!has_type<std::common_type<int S::*, long S::*>>(), "");
static_assert(!has_type<std::common_type<int S::*, void(S::*)()>>(), "");
static_assert(!has_type<std::common_type<int (B::*)(),
	      int (D::*)() const>>(), "");
static_assert(!has_type<std::common_type<int (B::*)() const,
	      int (D::*)()>>(), "");
static_assert(!has_type<std::common_type<int, ExplicitTo<int>>>(), "");
static_assert(!has_type<std::common_type<ImplicitTo<int>,
					 ExplicitTo<int>>>(), "");
static_assert(!has_type<std::common_type<ScEn, int>>(), "");
static_assert(!has_type<std::common_type<ScEn, UnscEn>>(), "");
static_assert(!has_type<std::common_type<U, S, Abstract, void, D,
	      int (B::*)(), int[5]>>(), "");
static_assert(!has_type<std::common_type<UConv1, Abstract&&>>(), "");
static_assert(!has_type<std::common_type<std::initializer_list<int>,
					 std::initializer_list<long>>>(), "");

// PR libstdc++/89102
static_assert(!has_type<std::common_type<int() &>>(), "");
static_assert(!has_type<std::common_type<int() & noexcept>>(), "");
static_assert(!has_type<std::common_type<int() const>>(), "");
static_assert(!has_type<std::common_type<int(...) &>>(), "");
static_assert(!has_type<std::common_type<int(...) & noexcept>>(), "");
static_assert(!has_type<std::common_type<int(...) const>>(), "");

void test(int i)
{
  auto local_lmd1 = [=](int, double) { return i + i; };
  auto local_lmd2 = [=](int, double) { return i - i; };

  static_assert(is_type<std::common_type<decltype(local_lmd1),
		        decltype(local_lmd1)>, decltype(local_lmd1)>(), "");
  static_assert(is_type<std::common_type<decltype(local_lmd1)&,
		        decltype(local_lmd1)>, decltype(local_lmd1)>(), "");
  static_assert(is_type<std::common_type<decltype(local_lmd1)&,
			decltype(local_lmd1)&>, decltype(local_lmd1)>(), "");

  static_assert(!has_type<std::common_type<decltype(local_lmd1),
		decltype(lmd1)>>(), "");
  static_assert(!has_type<std::common_type<decltype(local_lmd1)&,
		decltype(lmd1)&>>(), "");
  static_assert(!has_type<std::common_type<decltype(local_lmd1),
		decltype(local_lmd2)>>(), "");
  static_assert(!has_type<std::common_type<decltype(local_lmd1)&,
		decltype(local_lmd2)&>>(), "");
}
