// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

#include <memory>
#include <cstddef>
#include <type_traits>

// TODO: Uncomment the following define once gcc has fixed bug 52748
// (incomplete types in function call expressions):
//#define HAS_52748_FIXED

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

template<typename T, typename Res>
struct is_expected_type : public std::is_same<typename T::type, Res>
{};

template<typename P1, typename P2>
struct and_ : public std::conditional<P1::value, P2, std::false_type>::type
{};

template<typename T, typename Res>
struct is_type : public and_<has_type<T>, is_expected_type<T, Res>>
{};

// Types under inspection:

typedef bool (&PF1)();
typedef short (*PF2)(long);

struct S {
  operator PF2() const;
  double operator()(char, int&);
  void calc(long) const;
};

typedef void (S::*PMS)(long) const;
typedef void (S::*PMSnonconst)(long);

typedef int S::* PMI;

struct B {
  int i;
  void f1() const;
  bool f2(int) const volatile;
};

struct D : B {};

typedef void (B::*base_func_void)() const;
typedef bool (B::*base_func_bool_int)(int) const volatile;

struct ident_functor {
  template<typename T>
  T operator()(T&& x);
};

template<typename Ret = void>
struct variable_functor {
  template<typename... T>
  Ret operator()(T&&...);
};

struct ident_functor_noref {
  template<typename T>
  typename std::remove_reference<T>::type operator()(T&& x);
};

enum class ScEn;

enum UnScEn : int;

union U {
  int i;
  double d;
};

union U2 {
  int i;
  bool b;
  void operator()() const;
  int operator()(double) const;
  bool operator()(double);
  U operator()(int, int);
};

struct Ukn;

typedef Ukn (S::*PMSIncomplete)(long) const;
typedef Ukn (S::*PMSIncompletenonconst)(long);
typedef Ukn (*FuncIncomplete)(long);

struct Abstract {
  virtual ~Abstract() = 0;
};

struct Private {
private:
  void operator()();
  int operator()(int);
public:
  bool operator()(std::nullptr_t);
};

union PrivateUnion {
  double d;
private:
  void operator()();
  int operator()(int);
public:
  bool operator()(std::nullptr_t);
};

template<typename T>
struct ImplicitTo {
  operator T();
};

template<typename>
struct never { static const bool value = false; };

template<typename T>
struct BrokenTrait {
  static_assert(never<T>::value, "Error!");
  typedef T type;
};

template<typename T>
struct BadSmartPtr : T {
  T& operator*() const noexcept(typename BrokenTrait<T>::type());
};

template<typename Ret>
using FuncEllipses = Ret(...);

static_assert(is_type<std::result_of<S(int)>, short>::value, "Error!");
static_assert(is_type<std::result_of<S&(unsigned char, int&)>,
	      double>::value, "Error!");
static_assert(is_type<std::result_of<PF1()>, bool>::value, "Error!");
static_assert(is_type<std::result_of<PF1&()>, bool>::value, "Error!");
static_assert(is_type<std::result_of<PMS(std::unique_ptr<S>, int)>,
	      void>::value, "Error!");
static_assert(is_type<std::result_of<PMS(std::unique_ptr<S>&, unsigned&)>,
	      void>::value, "Error!");
static_assert(is_type<std::result_of<PMS&(std::unique_ptr<S>, int)>,
	      void>::value, "Error!");
static_assert(is_type<std::result_of<PMS&(std::unique_ptr<S>&, unsigned&)>,
	      void>::value, "Error!");

static_assert(is_type<std::result_of<ident_functor(int)>,
	      int>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(const int)>,
	      int>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(const int&&)>,
	      int>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(int&&)>,
	      int>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(int&)>,
	      int&>::value, "Error!");

static_assert(is_type<std::result_of<ident_functor(const B)>,
	      B>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(const B&&)>,
	      const B>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(B&&)>, B>::value, "Error!");
static_assert(is_type<std::result_of<ident_functor(B&)>, B&>::value, "Error!");

static_assert(is_type<std::result_of<int B::*(B&)>, int&>::value, "Error!");

// This is expected as of CWG 616 P/R:
static_assert(is_type<std::result_of<int B::*(B)>, int&&>::value, "Error!");

static_assert(is_type<std::result_of<volatile int B::*(const B&&)>,
	      const volatile int&&>::value, "Error!");
static_assert(is_type<std::result_of<const int B::*(volatile B&&)>,
	      const volatile int&&>::value, "Error!");

static_assert(is_type<std::result_of<int B::*(const B&)>,
	      const int&>::value, "Error!");
static_assert(is_type<std::result_of<volatile int B::*(const B&)>,
	      const volatile int&>::value, "Error!");
static_assert(is_type<std::result_of<const int B::*(volatile B&)>,
	      const volatile int&>::value, "Error!");

static_assert(is_type<std::result_of<int B::*(B*)>, int&>::value, "Error!");
static_assert(is_type<std::result_of<int B::*(B*&)>, int&>(), "Error!");
static_assert(is_type<std::result_of<int B::*(const B*)>,
	      const int&>::value, "Error!");
static_assert(is_type<std::result_of<int B::*(const B*&)>,
	      const int&>::value, "Error!");
static_assert(is_type<std::result_of<volatile int B::*(const B*)>,
	      const volatile int&>::value, "Error!");
static_assert(is_type<std::result_of<const int B::*(volatile B*)>,
	      const volatile int&>::value, "Error!");

static_assert(is_type<std::result_of<base_func_void(const B&)>,
	      void>::value, "Error!");
static_assert(is_type<std::result_of<base_func_void(const B*)>,
	      void>::value, "Error!");
static_assert(is_type<std::result_of<base_func_void(B&)>,
	      void>::value, "Error!");
static_assert(is_type<std::result_of<base_func_void(B*)>,
	      void>::value, "Error!");

static_assert(!has_type<std::result_of<base_func_void(volatile B&)>>::value,
	      "Error!");
static_assert(!has_type<std::result_of<base_func_void(volatile B*)>>::value,
	      "Error!");

static_assert(is_type<std::result_of<base_func_bool_int(B&, long)>,
	      bool>::value, "Error!");
static_assert(is_type<std::result_of<base_func_bool_int(B*, long)>,
	      bool>::value, "Error!");
static_assert(is_type<std::result_of<base_func_bool_int(volatile B&, long)>,
	      bool>::value, "Error!");
static_assert(is_type<std::result_of<base_func_bool_int(volatile B*, long)>,
	      bool>::value, "Error!");

static_assert(!has_type<std::result_of<int()>>(), "Error!");
static_assert(!has_type<std::result_of<void()>>(), "Error!");
static_assert(!has_type<std::result_of<int(int)>>(), "Error!");
static_assert(!has_type<std::result_of<void(int)>>(), "Error!");
static_assert(!has_type<std::result_of<PF1(int)>>(), "Error!");
static_assert(is_type<std::result_of<PF2(long)>, short>(), "Error!");
static_assert(!has_type<std::result_of<PF2()>>(), "Error!");
static_assert(!has_type<std::result_of<PF2(B)>>(), "Error!");
static_assert(!has_type<std::result_of<PF2(ScEn)>>(), "Error!");
static_assert(is_type<std::result_of<PF2(UnScEn)>, short>(), "Error!");
static_assert(!has_type<std::result_of<PF2(long, int)>>(), "Error!");
static_assert(is_type<std::result_of<PMS(std::unique_ptr<S>, int)>, void>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMS(int)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(int, std::unique_ptr<S>)>>(), "Error!");

// Argument number mismatch:
static_assert(!has_type<std::result_of<PMS(std::unique_ptr<S>)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(std::unique_ptr<S>&)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(std::unique_ptr<S>, int, bool)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMS(std::unique_ptr<S>&, int, bool)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMS(S)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(S&)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(S, int, bool)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(S&, int, bool)>>(), "Error!");

// Non-convertible arguments:
static_assert(!has_type<std::result_of<PMS(std::unique_ptr<S>, S)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMS(std::unique_ptr<S>&, S)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMS(S, S)>>(), "Error!");
static_assert(!has_type<std::result_of<PMS(S&, S)>>(), "Error!");

// cv-violations:
static_assert(!has_type<std::result_of<PMSnonconst(const S&, long)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMSnonconst(const S&&, long)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMSnonconst(const S*, long)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PMSnonconst(const S*&, long)>>(),
	      "Error!");

static_assert(is_type<std::result_of<PMI(S*)>, int&>(), "Error!");
static_assert(is_type<std::result_of<PMI(S&)>, int&>(), "Error!");
static_assert(is_type<std::result_of<PMI(S&&)>, int&&>(), "Error!");
static_assert(is_type<std::result_of<PMI(S)>, int&&>(), "Error!");

static_assert(!has_type<std::result_of<PMI()>>(), "Error!");

static_assert(!has_type<std::result_of<PMI(S*, int)>>(), "Error!");
static_assert(!has_type<std::result_of<PMI(S&, int)>>(), "Error!");
static_assert(!has_type<std::result_of<PMI(S*, int, S, bool)>>(), "Error!");
static_assert(!has_type<std::result_of<PMI(S&, int, S, bool)>>(), "Error!");

static_assert(!has_type<std::result_of<PMI(B*)>>(), "Error!");
static_assert(!has_type<std::result_of<PMI(B&)>>(), "Error!");

static_assert(is_type<std::result_of<int U::*(U)>, int&&>(), "Error!");
static_assert(is_type<std::result_of<int U::*(U&)>, int&>(), "Error!");
static_assert(is_type<std::result_of<int U::*(const U&)>, const int&>(),
	      "Error!");
static_assert(is_type<std::result_of
	      <volatile int U::*(const U&)>, const volatile int&>(), "Error!");
static_assert(is_type<std::result_of
	      <const int U::*(volatile U&)>, const volatile int&>(), "Error!");

static_assert(is_type<std::result_of<int Ukn::*(Ukn*)>, int&>(), "Error!");
static_assert(is_type<std::result_of<int Ukn::*(Ukn&)>, int&>(), "Error!");
static_assert(is_type<std::result_of<int Ukn::*(Ukn&&)>, int&&>(), "Error!");
static_assert(is_type<std::result_of<int Ukn::*(const Ukn*)>, const int&>(),
	      "Error!");
static_assert(is_type<std::result_of<int Ukn::*(const Ukn&)>, const int&>(),
	      "Error!");
static_assert(is_type<std::result_of<int Ukn::*(const Ukn&&)>, const int&&>(),
	      "Error!");

typedef void (Ukn::* PUfnMF)();
typedef void (Ukn::* PUfnConstMF)() const;

static_assert(is_type<std::result_of<PUfnMF(Ukn*)>, void>(), "Error!");
static_assert(is_type<std::result_of<PUfnMF(Ukn&)>, void>(), "Error!");
static_assert(is_type<std::result_of<PUfnMF(Ukn&&)>, void>(), "Error!");
static_assert(is_type<std::result_of<PUfnConstMF(Ukn*)>, void>(), "Error!");
static_assert(is_type<std::result_of<PUfnConstMF(Ukn&)>, void>(), "Error!");
static_assert(is_type<std::result_of<PUfnConstMF(Ukn&&)>, void>(), "Error!");
static_assert(is_type<std::result_of<PUfnConstMF(const Ukn*)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<PUfnConstMF(const Ukn&)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<PUfnConstMF(const Ukn&&)>, void>(),
	      "Error!");

static_assert(!has_type<std::result_of<S()>>(), "Error!");
static_assert(!has_type<std::result_of<S(int, S)>>(), "Error!");
static_assert(!has_type<std::result_of<S(S)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <S(double, bool, std::nullptr_t, Ukn&)>>(), "Error!");

static_assert(is_type<std::result_of<U2()>, void>(), "Error!");
static_assert(is_type<std::result_of<const U2&()>, void>(), "Error!");
static_assert(is_type<std::result_of<U2&()>, void>(), "Error!");
static_assert(is_type<std::result_of<U2(double)>, bool>(), "Error!");
static_assert(is_type<std::result_of<const U2&(double)>, int>(), "Error!");
static_assert(is_type<std::result_of<U2&(double)>, bool>(), "Error!");
static_assert(is_type<std::result_of<U2(int)>, bool>(), "Error!");
static_assert(is_type<std::result_of<U2&(int)>, bool>(), "Error!");
static_assert(is_type<std::result_of<const U2&(int)>, int>(), "Error!");
static_assert(is_type<std::result_of<U2(int, int)>, U>(), "Error!");
static_assert(is_type<std::result_of<U2&(int, int)>, U>(), "Error!");

static_assert(!has_type<std::result_of<const U2&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<U2(int, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<U2&(int, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<const U2&(int, int, int)>>(), "Error!");

static_assert(is_type<std::result_of<ident_functor(int)>, int>(), "Error!");
static_assert(is_type<std::result_of<ident_functor(const volatile int)>,
	      int>(), "Error!");
static_assert(is_type<std::result_of<ident_functor(int&)>, int&>(), "Error!");
static_assert(is_type<std::result_of<ident_functor(const volatile int&)>,
	      const volatile int&>(), "Error!");
static_assert(is_type<std::result_of<ident_functor(int&&)>, int>(), "Error!");
static_assert(is_type<std::result_of<ident_functor(const volatile int&&)>,
	      int>(), "Error!");
static_assert(is_type<std::result_of<ident_functor(Abstract&)>, Abstract&>(),
	      "Error!");
static_assert(is_type<std::result_of<ident_functor(const volatile Abstract&)>,
	      const volatile Abstract&>(), "Error!");

static_assert(!has_type<std::result_of<ident_functor(int(&&)[1])>>(), "Error!");
static_assert(!has_type<std::result_of<ident_functor(Abstract&&)>>(), "Error!");
static_assert(!has_type<std::result_of<ident_functor(const int(&&)[1])>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ident_functor(const Abstract&&)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ident_functor_noref(int(&)[1])>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ident_functor_noref
	      (const int(&)[1])>>(), "Error!");
static_assert(!has_type<std::result_of<ident_functor_noref(Abstract&)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <ident_functor_noref(const Abstract&)>>(), "Error!");
static_assert(!has_type<std::result_of<ident_functor_noref(void(&)())>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ident_functor_noref(void(&&)())>>(),
	      "Error!");

static_assert(!has_type<std::result_of<ident_functor()>>(), "Error!");
static_assert(!has_type<std::result_of<ident_functor(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<const ident_functor&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<const ident_functor&&(int)>>(),
	      "Error!");

// Border-line case:
static_assert(!has_type<std::result_of<int S::*(Ukn*)>>(), "Error!");
static_assert(!has_type<std::result_of<void (S::*(Ukn*))()>>(), "Error!");

// We want to allow this, it seems to be required by the order described 
// in [func.require] p1:
static_assert(is_type<std::result_of<int S::*(BadSmartPtr<S>&)>, int&>(),
	      "Error!");

static_assert(is_type<std::result_of<Private(std::nullptr_t)>, bool>(),
	      "Error!");
static_assert(is_type<std::result_of<Private&(std::nullptr_t)>, bool>(),
	      "Error!");
static_assert(is_type<std::result_of<Private&&(std::nullptr_t)>, bool>(),
	      "Error!");
static_assert(is_type<std::result_of<Private(ImplicitTo<std::nullptr_t>)>,
	      bool>(), "Error!");
static_assert(is_type<std::result_of<Private&(ImplicitTo<std::nullptr_t>)>,
	      bool>(), "Error!");
static_assert(is_type<std::result_of<Private&&(ImplicitTo<std::nullptr_t>)>,
	      bool>(), "Error!");

static_assert(!has_type<std::result_of<const Private&(std::nullptr_t)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const Private&&(std::nullptr_t)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<Private()>>(), "Error!");
static_assert(!has_type<std::result_of<Private(int)>>(), "Error!");
static_assert(!has_type<std::result_of<Private(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<Private&()>>(), "Error!");
static_assert(!has_type<std::result_of<Private&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<Private&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&()>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<Private&&()>>(), "Error!");
static_assert(!has_type<std::result_of<Private&&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<Private&&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&&()>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&&(int, int)>>(), "Error!");

static_assert(!has_type<std::result_of<Private(ScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<Private(UnScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&(ScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<const Private&(UnScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<Private(ImplicitTo<ScEn>)>>(), "Error!");
static_assert(!has_type<std::result_of<Private(ImplicitTo<UnScEn>)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const Private&(ImplicitTo<ScEn>)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const Private&(ImplicitTo<UnScEn>)>>(),
	      "Error!");

static_assert(is_type<std::result_of<PrivateUnion(std::nullptr_t)>, bool>(),
	      "Error!");
static_assert(is_type<std::result_of<PrivateUnion&(std::nullptr_t)>, bool>(),
	      "Error!");
static_assert(is_type<std::result_of<PrivateUnion&&(std::nullptr_t)>, bool>(),
	      "Error!");
static_assert(is_type<std::result_of<PrivateUnion(ImplicitTo<std::nullptr_t>)>,
	      bool>(), "Error!");
static_assert(is_type<std::result_of
	      <PrivateUnion&(ImplicitTo<std::nullptr_t>)>, bool>(), "Error!");
static_assert(is_type<std::result_of
	      <PrivateUnion&&(ImplicitTo<std::nullptr_t>)>, bool>(), "Error!");

static_assert(!has_type<std::result_of<const PrivateUnion&(std::nullptr_t)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const PrivateUnion&&(std::nullptr_t)>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion()>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion(int)>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion&()>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&()>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&(int, int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PrivateUnion&&()>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion&&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion&&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&&()>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&&(int)>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&&(int, int)>>(),
	      "Error!");

static_assert(!has_type<std::result_of<PrivateUnion(ScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<PrivateUnion(UnScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&(ScEn)>>(), "Error!");
static_assert(!has_type<std::result_of<const PrivateUnion&(UnScEn)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PrivateUnion(ImplicitTo<ScEn>)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<PrivateUnion(ImplicitTo<UnScEn>)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const PrivateUnion&(ImplicitTo<ScEn>)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const PrivateUnion&(ImplicitTo<UnScEn>)>>(), "Error!");

static_assert(is_type<std::result_of<void(*(bool))(int)>, void>(), "Error!");
static_assert(is_type<std::result_of<void(*(UnScEn))(int)>, void>(), "Error!");
static_assert(is_type<std::result_of<void(*(ImplicitTo<int>))(int)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<void(*(ImplicitTo<int>&))(int)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<void(*(ImplicitTo<int>&&))(int)>, void>(),
	      "Error!");

static_assert(!has_type<std::result_of<void(*(ScEn))(int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <void(*(const ImplicitTo<int>&))(int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <void(*(const ImplicitTo<int>&&))(int)>>(), "Error!");

static_assert(is_type<std::result_of<ImplicitTo<void(*)()>()>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<ImplicitTo<void(&)()>()>, void>(),
	      "Error!");

static_assert(!has_type<std::result_of<ImplicitTo<void(*)()>(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ImplicitTo<void(*)(int)>()>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ImplicitTo<void(&)()>(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<ImplicitTo<void(&)(int)>()>>(),
	      "Error!");

// Conversion operators of types are not considered in call expressions
// (except for conversion to function pointer/reference):
static_assert(!has_type<std::result_of<ImplicitTo<S>(char, int&)>>(), "Error!");
static_assert(!has_type<std::result_of<ImplicitTo<ident_functor>(int)>>(),
	      "Error!");

static_assert(is_type<std::result_of<variable_functor<>()>, void>(), "Error!");
static_assert(is_type<std::result_of<variable_functor<>(int)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<>(int, int)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<>(int, int, int)>,
	      void>(), "Error!");

static_assert(is_type<std::result_of<variable_functor<>&()>, void>(), "Error!");
static_assert(is_type<std::result_of<variable_functor<>&(int)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<>&(int, int)>, void>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<>&(int, int, int)>,
	      void>(), "Error!");

static_assert(!has_type<std::result_of<const variable_functor<>()>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const variable_functor<>(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const variable_functor<>(int, int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<>(int, int, int)>>(), "Error!");

static_assert(!has_type<std::result_of<const variable_functor<>&()>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const variable_functor<>&(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<>&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<>&(int, int, int)>>(), "Error!");

static_assert(is_type<std::result_of<variable_functor<S>()>, S>(), "Error!");
static_assert(is_type<std::result_of<variable_functor<S>(int)>, S>(), "Error!");
static_assert(is_type<std::result_of<variable_functor<S>(int, int)>, S>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<S>(int, int, int)>, S>(),
	      "Error!");

static_assert(is_type<std::result_of<variable_functor<S>&()>, S>(), "Error!");
static_assert(is_type<std::result_of<variable_functor<S>&(int)>, S>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<S>&(int, int)>, S>(),
	      "Error!");
static_assert(is_type<std::result_of
	      <variable_functor<S>&(int, int, int)>, S>(), "Error!");

static_assert(!has_type<std::result_of
	      <const variable_functor<S>()>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<S>(int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<S>(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<S>(int, int, int)>>(), "Error!");

static_assert(!has_type<std::result_of<const variable_functor<S>&()>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const variable_functor<S>&(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<S>&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<S>&(int, int, int)>>(), "Error!");

#if defined(HAS_52748_FIXED)
static_assert(has_type<std::result_of<variable_functor<Ukn>()>>(), "Error!");
static_assert(is_type<std::result_of<variable_functor<Ukn>()>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<Ukn>(int)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<Ukn>(int, int)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of
	      <variable_functor<Ukn>(int, int, int)>, Ukn>(), "Error!");

static_assert(is_type<std::result_of<variable_functor<Ukn>&()>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of<variable_functor<Ukn>&(int)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of
	      <variable_functor<Ukn>&(int, int)>, Ukn>(), "Error!");
static_assert(is_type<std::result_of
	      <variable_functor<Ukn>&(int, int, int)>, Ukn>(), "Error!");

static_assert(is_type<std::result_of<PMSIncomplete(int)>, Ukn>(), "Error!");
static_assert(is_type<std::result_of<PMSIncomplete&(int)>, Ukn>(), "Error!");
static_assert(is_type<std::result_of<PMSIncomplete&&(int)>, Ukn>(), "Error!");

static_assert(is_type<std::result_of<FuncIncomplete(int)>, Ukn>(), "Error!");
static_assert(is_type<std::result_of<FuncIncomplete&(int)>, Ukn>(), "Error!");
static_assert(is_type<std::result_of<FuncIncomplete&&(int)>, Ukn>(), "Error!");

static_assert(is_type<std::result_of<FuncEllipses<Ukn>*()>, Ukn>(), "Error!");
static_assert(is_type<std::result_of<FuncEllipses<Ukn>&()>, Ukn>(), "Error!");
static_assert(is_type<std::result_of<FuncEllipses<Ukn>&&()>, Ukn>(), "Error!");

static_assert(is_type<std::result_of<FuncEllipses<Ukn>*(bool)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of<FuncEllipses<Ukn>&(bool)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of<FuncEllipses<Ukn>&&(bool)>, Ukn>(),
	      "Error!");

static_assert(is_type<std::result_of<FuncEllipses<Ukn>*(bool, int, S)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of<FuncEllipses<Ukn>&(bool, int, S)>, Ukn>(),
	      "Error!");
static_assert(is_type<std::result_of
	      <FuncEllipses<Ukn>&&(bool, int, S)>, Ukn>(), "Error!");

static_assert(!has_type<std::result_of<PMSIncompletenonconst(const S*)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*, int, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*&)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*&, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*&, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S*&, int, int, int)>>(), "Error!");

static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&, int, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&&)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&&, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&&, int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <PMSIncompletenonconst(const S&&, int, int, int)>>(), "Error!");
#endif

static_assert(!has_type<std::result_of<const variable_functor<Ukn>()>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const variable_functor<Ukn>(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<Ukn>(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<Ukn>(int, int, int)>>(), "Error!");

static_assert(!has_type<std::result_of<const variable_functor<Ukn>&()>>(),
	      "Error!");
static_assert(!has_type<std::result_of<const variable_functor<Ukn>&(int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<Ukn>&(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of
	      <const variable_functor<Ukn>&(int, int, int)>>(), "Error!");

static_assert(!has_type<std::result_of<FuncIncomplete()>>(), "Error!");
static_assert(!has_type<std::result_of<FuncIncomplete(S)>>(), "Error!");
static_assert(!has_type<std::result_of<FuncIncomplete(int, int)>>(), "Error!");
static_assert(!has_type<std::result_of<FuncIncomplete(int, int, int)>>(),
	      "Error!");

static_assert(!has_type<std::result_of<FuncIncomplete&&()>>(), "Error!");
static_assert(!has_type<std::result_of<FuncIncomplete&&(S)>>(), "Error!");
static_assert(!has_type<std::result_of<FuncIncomplete&&(int, int)>>(),
	      "Error!");
static_assert(!has_type<std::result_of<FuncIncomplete&&(int, int, int)>>(),
	      "Error!");

static_assert(is_type<std::result_of<FuncEllipses<int>*()>, int>(), "Error!");
static_assert(is_type<std::result_of<FuncEllipses<int>&()>, int>(), "Error!");
static_assert(is_type<std::result_of<FuncEllipses<int>&&()>, int>(), "Error!");

static_assert(is_type<std::result_of<FuncEllipses<int>*(bool)>, int>(),
	      "Error!");
static_assert(is_type<std::result_of<FuncEllipses<int>&(bool)>, int>(),
	      "Error!");
static_assert(is_type<std::result_of<FuncEllipses<int>&&(bool)>, int>(), 
	      "Error!");

static_assert(is_type<std::result_of<FuncEllipses<int>*(bool, int, S)>, int>(),
	      "Error!");
static_assert(is_type<std::result_of<FuncEllipses<int>&(bool, int, S)>, int>(),
	      "Error!");
static_assert(is_type<std::result_of
	      <FuncEllipses<int>&&(bool, int, S)>, int>(), "Error!");

