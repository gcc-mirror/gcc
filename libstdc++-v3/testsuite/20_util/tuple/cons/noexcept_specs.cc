//{ dg-do run { target c++11 } }

// 2019-04-10  Nina Dinka Ranns  <dinka.ranns@gmail.com>
//
// Copyright (C) 2019 Free Software Foundation, Inc.
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

#include <tuple>
#include <testsuite_tr1.h>
#include <utility>

using namespace __gnu_test;

bool throwing_ctor_called = false;

namespace test_trait{
   template <bool B>
   using bool_constant = std::integral_constant<bool, B>;

   template<typename From, typename To,
           bool = std::__or_<std::is_void<From>, std::is_function<To>,
		   std::is_array<To>>::value>
    struct is_nt_convertible_helper
    : std::is_void<To>
    { };

  template<typename From, typename To>
    class is_nt_convertible_helper<From, To, false>
    {
      template<typename To1>
	static void test_aux(To1) noexcept;

      template<typename From1, typename To1>
	static bool_constant<noexcept(test_aux<To1>({std::declval<From1>()}))>
	test(int);

      template<typename, typename>
	static std::false_type
	test(...);

    public:
      using type = decltype(test<From, To>(0));
    };

  // Similar to std::is_nothrow_convertible, but only considers whether the
  // actual conversion can throw (and not any potential copies of From).
  // This means the result is not affected by copy elision of From in C++17.
  template<typename From, typename To>
    struct is_nothrow_convertible
    : public is_nt_convertible_helper<From, To>::type
    { };
}

template<typename T>
bool  checkDefaultThrowConstruct()
{
  throwing_ctor_called = false;
  bool deduced_nothrow = std::is_nothrow_constructible<T>::value;
  T t{};
  return throwing_ctor_called != deduced_nothrow;
}
template<typename T, typename U>
bool  checkCopyThrowConstruct()
{
  throwing_ctor_called = false;
  bool deduced_nothrow = std::is_nothrow_constructible<T, U&>::value;
  U u;
  T t{u};
  return throwing_ctor_called != deduced_nothrow;
}
template<typename T, typename U>
bool  checkMoveThrowConstruct()
{
  throwing_ctor_called = false;
  bool deduced_nothrow = std::is_nothrow_constructible<T, U&&>::value;
  U u;
  T t{std::move(u)};
  return throwing_ctor_called != deduced_nothrow;
}

typedef std::tuple<int> IT;
typedef std::tuple<const int> CIT;
typedef std::tuple<int&&> RVIT;
typedef std::tuple<int, int> IIT;
typedef std::pair<int, int> IIP;
typedef std::tuple<int, int, int> IIIT;

namespace DefaultConstructionTests
{
  struct NoexceptDC
  {
    NoexceptDC() noexcept(true){}
  };

  struct ExceptDC
  {
    ExceptDC() noexcept(false)
      {  throwing_ctor_called = true; }
  };

  struct ExplicitNoexceptDC
  {
    explicit ExplicitNoexceptDC() noexcept(true)
        {}
  };

  struct ExplicitExceptDC
  {
    explicit ExplicitExceptDC() noexcept(false)
        {  throwing_ctor_called = true; }
  };

  typedef std::tuple<NoexceptDC> NDT;
  typedef std::tuple<ExceptDC> EDT;
  typedef std::tuple<ExplicitNoexceptDC> X_NDT;
  typedef std::tuple<ExplicitExceptDC> X_EDT;

  typedef std::tuple<NoexceptDC,NoexceptDC> NNDT;
  typedef std::tuple<ExceptDC,ExceptDC> EEDT;
  typedef std::tuple<ExceptDC,NoexceptDC> ENDT;
  typedef std::tuple<ExplicitNoexceptDC,NoexceptDC> X_NNDT;
  typedef std::tuple<ExplicitExceptDC,ExceptDC> X_EEDT;
  typedef std::tuple<ExceptDC,ExplicitNoexceptDC> X_ENDT;

  typedef std::tuple<long, NoexceptDC, NoexceptDC> LNDNDT;
  typedef std::tuple<long, NoexceptDC, ExceptDC> LNDEDT;
  typedef std::tuple<long, ExplicitNoexceptDC, NoexceptDC> X_LNEDNDT;
  typedef std::tuple<long, ExplicitNoexceptDC, ExceptDC> X_LNEDEDT;
  typedef std::tuple<long, ExplicitExceptDC, ExceptDC> X_LEEDEDT;


  /* if it has E in the name, it contains a type that throws when default constructed */
  static_assert(std::is_nothrow_constructible<IT>::value, "");
  static_assert(std::is_nothrow_constructible<NDT>::value, "");
  static_assert(!std::is_nothrow_constructible<EDT>::value, "");
  static_assert(std::is_nothrow_constructible<X_NDT>::value, "");
  static_assert(!std::is_nothrow_constructible<X_EDT>::value, "");

  static_assert(std::is_nothrow_constructible<IIT>::value, "");
  static_assert(std::is_nothrow_constructible<NNDT>::value, "");
  static_assert(!std::is_nothrow_constructible<EEDT>::value, "");
  static_assert(!std::is_nothrow_constructible<ENDT>::value, "");
  static_assert(std::is_nothrow_constructible<X_NNDT>::value, "");
  static_assert(!std::is_nothrow_constructible<X_EEDT>::value, "");
  static_assert(!std::is_nothrow_constructible<X_ENDT>::value, "");

  static_assert(std::is_nothrow_constructible<IIIT>::value, "");
  static_assert(std::is_nothrow_constructible<LNDNDT>::value, "");
  static_assert(!std::is_nothrow_constructible<LNDEDT>::value, "");
  static_assert(std::is_nothrow_constructible<X_LNEDNDT>::value, "");
  static_assert(!std::is_nothrow_constructible<X_LNEDEDT>::value, "");
  static_assert(!std::is_nothrow_constructible<X_LEEDEDT>::value, "");

  void Run()
  {
    VERIFY( checkDefaultThrowConstruct<IT>() );
    VERIFY( checkDefaultThrowConstruct<NDT>() );
    VERIFY( checkDefaultThrowConstruct<EDT>() );
    VERIFY( checkDefaultThrowConstruct<X_NDT>() );
    VERIFY( checkDefaultThrowConstruct<X_EDT>() );

    VERIFY( checkDefaultThrowConstruct<IIT>() );
    VERIFY( checkDefaultThrowConstruct<NNDT>() );
    VERIFY( checkDefaultThrowConstruct<EEDT>() );
    VERIFY( checkDefaultThrowConstruct<ENDT>() );
    VERIFY( checkDefaultThrowConstruct<X_NNDT>() );
    VERIFY( checkDefaultThrowConstruct<X_EEDT>() );
    VERIFY( checkDefaultThrowConstruct<X_ENDT>() );

    VERIFY( checkDefaultThrowConstruct<IIIT>() );
    VERIFY( checkDefaultThrowConstruct<LNDNDT>() );
    VERIFY( checkDefaultThrowConstruct<LNDEDT>() );
    VERIFY( checkDefaultThrowConstruct<X_LNEDNDT>() );
    VERIFY( checkDefaultThrowConstruct<X_LNEDEDT>() );
  }
}
namespace AllNoThrow
{
  static_assert(std::is_nothrow_constructible<IT, int&>::value, "");
  static_assert(std::is_nothrow_constructible<IT, const int&>::value, "");
  static_assert(std::is_nothrow_constructible<IT, int&&>::value, "");
  static_assert(std::is_nothrow_constructible<IT, const int&&>::value, "");

  static_assert(std::is_nothrow_constructible<IT, IT>::value, "");
  static_assert(std::is_nothrow_constructible<IT, const IT &>::value, "");
  static_assert(std::is_nothrow_constructible<IT, CIT>::value, "");

  static_assert(test_trait::is_nothrow_convertible<int&, IT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<const int&, IT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<int&&, IT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<const int&&, IT>::value, "");

  static_assert(std::is_nothrow_constructible<IIT, int&, int&>::value, "");
  static_assert(std::is_nothrow_constructible<IIT, const int&, const int&>::value, "");
  static_assert(std::is_nothrow_constructible<IIT, int&&, int&&>::value, "");
  static_assert(std::is_nothrow_constructible<IIT, const int&&, const int&&>::value, "");

  static_assert(std::is_nothrow_constructible<IIT, IIT>::value, "");
  static_assert(std::is_nothrow_constructible<IIT, const IIT &>::value, "");
  static_assert(std::is_nothrow_constructible<IIT, IIP>::value, "");
  static_assert(std::is_nothrow_constructible<IIT, const IIP>::value, "");

  static_assert(test_trait::is_nothrow_convertible<IIT, IIT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<const IIT &, IIT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<IIP, IIT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<const IIP, IIT>::value, "");

  static_assert(std::is_nothrow_constructible<IIIT, IIIT&>::value, "");
  static_assert(std::is_nothrow_constructible<IIIT, const IIIT &>::value, "");
  static_assert(std::is_nothrow_constructible<IIIT, IIIT&&>::value, "");
  static_assert(std::is_nothrow_constructible<IIIT, const IIIT&&>::value, "");

  static_assert(test_trait::is_nothrow_convertible<IIIT&, IIIT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<const IIIT &, IIIT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<IIIT&&, IIIT>::value, "");
  static_assert(test_trait::is_nothrow_convertible<const IIIT&&, IIIT>::value, "");

  static_assert(std::is_nothrow_constructible<IIIT, int&, int&, int&>::value, "");
  static_assert(std::is_nothrow_constructible<IIIT, const int&, const int&, int&>::value, "");
  static_assert(std::is_nothrow_constructible<IIIT, int&&, int&&, int&>::value, "");
  static_assert(std::is_nothrow_constructible<IIIT, const int&&, const int&&, int&>::value, "");
}
namespace ThrowCopyNothrowConversion
{
  struct A
  {
    A() noexcept(true)
        {}

    A(const int&) noexcept(true)
      {}

    A(const A&) noexcept(false)
      {  throwing_ctor_called = true; }

  };

  typedef std::tuple<A> AT;
  typedef std::tuple<A,A> AAT;
  typedef std::pair<A,A> AAP;
  typedef std::tuple<int,A> IAT;
  typedef std::pair<int,A> IAP;
  typedef std::tuple<A,A,A> AAAT;
  typedef std::tuple<int,int,A> IIAT;

/* one element tests */
  static_assert(std::is_nothrow_constructible<AT,int>::value,"");
  static_assert(std::is_nothrow_constructible<AT,const int>::value,"");
  static_assert(std::is_nothrow_constructible<AT,int&>::value,"");
  static_assert(std::is_nothrow_constructible<AT,const int &>::value,"");

  static_assert(std::is_nothrow_constructible<AT,IT>::value,"");
  static_assert(std::is_nothrow_constructible<AT,const IT>::value,"");
  static_assert(std::is_nothrow_constructible<AT,IT&>::value,"");
  static_assert(std::is_nothrow_constructible<AT,const IT &>::value,"");
  static_assert(std::is_nothrow_constructible<AT,std::tuple<int&>>::value,"");
  static_assert(std::is_nothrow_constructible<AT,const std::tuple<int&&>>::value,"");

  static_assert(test_trait::is_nothrow_convertible<int,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const int,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<int&,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const int &,AT>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IT,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IT,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IT&,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IT &,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<std::tuple<int&>,AT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const std::tuple<int&&>,AT>::value,"");

  static_assert(!std::is_nothrow_constructible<AT,A>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,const A>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,A&>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,const A &>::value,"");

  static_assert(!std::is_nothrow_constructible<AT,AT>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,const AT>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,AT&>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,const AT &>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,std::tuple<A&>>::value,"");
  static_assert(!std::is_nothrow_constructible<AT,const std::tuple<A&&>>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<A,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const A,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<A&,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const A &,AT>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<AT,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const AT,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<AT&,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const AT &,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<std::tuple<A&>,AT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const std::tuple<A&&>,AT>::value,"");

/* two element tests */

  static_assert(std::is_nothrow_constructible<AAT,IIT>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,const IIT>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,IIT&>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,const IIT &>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,IIP>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,const IIP>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,IIP&>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,const IIP &>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,std::tuple<int&,int&>>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IIT,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IIT,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IIT&,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IIT &,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IIP,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IIP,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IIP&,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IIP &,AAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<std::tuple<int&,int&>,AAT>::value,"");


  static_assert(!std::is_nothrow_constructible<AAT,AAT>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const AAT>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,AAT&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const AAT &>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,AAP>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const AAP>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,AAP&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const AAP &>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,std::tuple<A&,A&>>::value,"");

  static_assert(!std::is_nothrow_constructible<AAT,IAT>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const IAT>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,IAT&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const IAT &>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,IAP>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const IAP>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,IAP&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const IAP &>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,std::tuple<A&,int&>>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<AAT,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const AAT,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<AAT&,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const AAT &,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<AAP,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const AAP,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<AAP&,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const AAP &,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<std::tuple<A&,A&>,AAT>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<IAT,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IAT,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IAT&,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IAT &,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IAP,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IAP,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IAP&,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IAP &,AAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<std::tuple<A&,int&>,AAT>::value,"");


  static_assert(std::is_nothrow_constructible<AAT,int&,const int&>::value,"");
  static_assert(std::is_nothrow_constructible<AAT,int&&,const int&&>::value,"");

  static_assert(!std::is_nothrow_constructible<AAT,A&,const A&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,A&&,const A&&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAT,const A &&,const int&&>::value,"");

/* three element tests */
  static_assert(std::is_nothrow_constructible<AAAT,IIIT>::value,"");
  static_assert(std::is_nothrow_constructible<AAAT,const IIIT>::value,"");
  static_assert(std::is_nothrow_constructible<AAAT,IIIT&>::value,"");
  static_assert(std::is_nothrow_constructible<AAAT,const IIIT &>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IIIT,AAAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IIIT,AAAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IIIT&,AAAT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const IIIT &,AAAT>::value,"");

  static_assert(!std::is_nothrow_constructible<AAAT,IIAT>::value,"");
  static_assert(!std::is_nothrow_constructible<AAAT,const IIAT>::value,"");
  static_assert(!std::is_nothrow_constructible<AAAT,IIAT&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAAT,const IIAT &>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<IIAT,AAAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIAT,AAAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IIAT&,AAAT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIAT &,AAAT>::value,"");

  static_assert(std::is_nothrow_constructible<AAAT,int&,const int&,int&&>::value,"");
  static_assert(!std::is_nothrow_constructible<AAAT,int &,const A &&,const int&&>::value,"");

  void Run()
  {
    VERIFY( (checkCopyThrowConstruct<AT,int>()) );
    VERIFY( (checkMoveThrowConstruct<AT,int>()) );
    VERIFY( (checkCopyThrowConstruct<AT,A>()) );
    VERIFY( (checkMoveThrowConstruct<AT,A>()) );
    VERIFY( (checkCopyThrowConstruct<AT,AT>()) );
    VERIFY( (checkMoveThrowConstruct<AT,AT>()) );
    VERIFY( (checkCopyThrowConstruct<AT,IT>()) );
    VERIFY( (checkMoveThrowConstruct<AT,IT>()) );
    VERIFY( (checkMoveThrowConstruct<AT,CIT>()) );
    VERIFY( (checkCopyThrowConstruct<AAT,AAT>()) );
    VERIFY( (checkMoveThrowConstruct<AAT,AAT>()) );
    VERIFY( (checkCopyThrowConstruct<AAT,IIT>()) );
    VERIFY( (checkMoveThrowConstruct<AAT,IIT>()) );
    VERIFY( (checkCopyThrowConstruct<AAT,IIP>()) );
    VERIFY( (checkMoveThrowConstruct<AAT,IIP>()) );

    VERIFY( (checkCopyThrowConstruct<AAT,std::tuple<int,A>>()) );
    VERIFY( (checkMoveThrowConstruct<AAT,std::tuple<int,A>>()) );
    VERIFY( (checkCopyThrowConstruct<AAT,std::pair<int,A>>()) );
    VERIFY( (checkMoveThrowConstruct<AAT,std::pair<int,A>>()) );

    VERIFY( (checkCopyThrowConstruct<AAAT,AAAT>()) );
    VERIFY( (checkMoveThrowConstruct<AAAT,AAAT>()) );
    VERIFY( (checkCopyThrowConstruct<AAAT,IIAT>()) );
    VERIFY( (checkMoveThrowConstruct<AAAT,IIAT>()) );
  }
}
namespace NothrowCopyThrowMoveThrowCopyConversionNothrowMoveConversion
{
  struct B
  {
    B() noexcept(true)
        {}

    B(const int&) noexcept(false)
      {  throwing_ctor_called = true; }

    B(int&&) noexcept(true)
      {}

    B(const B&) noexcept(true)
      {}

    B(B&&) noexcept(false)
      {  throwing_ctor_called = true; }
  };


  struct D
  {
    D() noexcept(true)
        {}

    explicit
    D(const int&) noexcept(false)
    {  throwing_ctor_called = true; }

    explicit
    D(int&&) noexcept(true)
        {}

    explicit
    D(const D&) noexcept(true)
      {}

    explicit
    D(D&&) noexcept(false)
    {  throwing_ctor_called = true; }

  };

  typedef std::tuple<B> BT;
  typedef std::tuple<B,B> BBT;
  typedef std::pair<B,B> BBP;
  typedef std::tuple<D> DT;
  typedef std::tuple<D,D> DDT;
  typedef std::pair<D,D> DDP;
  typedef std::tuple<int,D> IDT;
  typedef std::pair<int,D> IDP;
  typedef std::tuple<int,B> IBT;
  typedef std::pair<int,B> IBP;
  typedef std::tuple<D,B> DBT;
  typedef std::pair<D,B> DBP;
  typedef std::tuple<B,B,B> BBBT;
  typedef std::tuple<D,D,D> DDDT;
  typedef std::tuple<int,D,int> IDIT;
  typedef std::tuple<int,B,int> IBIT;
  typedef std::tuple<int,D,B> IDBT;

/* one element tests */
  static_assert(std::is_nothrow_constructible<BT, int>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, const int>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, int&>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, const int &>::value, "");

  static_assert(std::is_nothrow_constructible<BT, IT>::value, "");
  static_assert(!std::is_nothrow_constructible<BT,const IT>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, IT&>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, const IT &>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, std::tuple<int&>>::value, "");
  static_assert(!std::is_nothrow_constructible<BT, const std::tuple<int&&>>::value, "");

  static_assert(test_trait::is_nothrow_convertible<int,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const int,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<int&,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const int &,BT>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IT,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IT,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IT&,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IT &,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<std::tuple<int&>,BT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const std::tuple<int&&>,BT>::value,"");


  static_assert(!std::is_nothrow_constructible<BT, B>::value, "");
  static_assert(std::is_nothrow_constructible<BT,const B>::value, "");
  static_assert(std::is_nothrow_constructible<BT, B&>::value, "");
  static_assert(std::is_nothrow_constructible<BT, const B &>::value, "");

  static_assert(!std::is_nothrow_constructible<BT, BT>::value, "");
  static_assert(std::is_nothrow_constructible<BT,const BT>::value, "");
  static_assert(std::is_nothrow_constructible<BT, BT&>::value, "");
  static_assert(std::is_nothrow_constructible<BT, const BT &>::value, "");
  static_assert(std::is_nothrow_constructible<BT, std::tuple<B&>>::value, "");
  static_assert(std::is_nothrow_constructible<BT, const std::tuple<B&&>>::value, "");

  static_assert(!test_trait::is_nothrow_convertible<B,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const B,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<B&,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const B &,BT>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<BT,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BT,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<BT&,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BT &,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<std::tuple<B&>,BT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const std::tuple<B&&>,BT>::value,"");

/* explicit */
  static_assert(std::is_nothrow_constructible<DT, int>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, const int>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, int&>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, const int &>::value, "");

  static_assert(std::is_nothrow_constructible<DT, IT>::value, "");
  static_assert(!std::is_nothrow_constructible<DT,const IT>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, IT&>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, const IT &>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, std::tuple<int&>>::value, "");
  static_assert(!std::is_nothrow_constructible<DT, const std::tuple<int&&>>::value, "");

  static_assert(!std::is_nothrow_constructible<DT, D>::value, "");
  static_assert(std::is_nothrow_constructible<DT,const D>::value, "");
  static_assert(std::is_nothrow_constructible<DT, D&>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const D &>::value, "");

  static_assert(!std::is_nothrow_constructible<DT, DT>::value, "");
  static_assert(std::is_nothrow_constructible<DT,const DT>::value, "");
  static_assert(std::is_nothrow_constructible<DT, DT&>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const DT &>::value, "");
  static_assert(std::is_nothrow_constructible<DT, std::tuple<D&>>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const std::tuple<D&&>>::value, "");

  static_assert(!test_trait::is_nothrow_convertible<DT,DT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const DT,DT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<DT&,DT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const DT &,DT>::value,"");

/* two elements tests */
  static_assert(std::is_nothrow_constructible<BBT,IIT>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IIT>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,IIT&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IIT &>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,IIP>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IIP>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,IIP&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IIP &>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,std::tuple<int&,int&>>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IIT,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIT,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IIT&,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIT &,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IIP,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIP,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IIP&,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIP &,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<std::tuple<int&,int&>,BBT>::value,"");

  static_assert(!std::is_nothrow_constructible<BBT,BBT>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,const BBT>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,BBT&>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,const BBT &>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,BBP>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,const BBP>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,BBP&>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,const BBP &>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,std::tuple<B&,B&>>::value,"");

  static_assert(!std::is_nothrow_constructible<BBT,IBT>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IBT>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,IBT&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IBT &>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,IBP>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IBP>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,IBP&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const IBP &>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,std::tuple<const B&,int&&>>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<BBT,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BBT,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<BBT&,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BBT &,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<BBP,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BBP,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<BBP&,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BBP &,BBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<std::tuple<B&,int&&>,BBT>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<IBT,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IBT,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IBT&,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IBT &,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IBP,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IBP,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IBP&,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IBP &,BBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<std::tuple<B&,int&>,BBT>::value,"");


  static_assert(!std::is_nothrow_constructible<BBT,int&,const int&>::value,"");
  static_assert(std::is_nothrow_constructible<BBT,int&&,const B&>::value,"");

  static_assert(std::is_nothrow_constructible<BBT,B&,const B&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,B&&,const B&&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBT,const B &&,const int&&>::value,"");

/* explicit */
  static_assert(std::is_nothrow_constructible<DDT,IIT>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IIT>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,IIT&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IIT &>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,IIP>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IIP>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,IIP&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IIP &>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,std::tuple<int&,int&>>::value,"");

  static_assert(!std::is_nothrow_constructible<DDT,DDT>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const DDT>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,DDT&>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const DDT &>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,DDP>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const DDP>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,DDP&>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const DDP &>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,std::tuple<D&,D&>>::value,"");

  static_assert(!std::is_nothrow_constructible<DDT,IDT>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IDT>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,IDT&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IDT &>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,IDP>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IDP>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,IDP&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const IDP &>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,std::tuple<const D&,int&&>>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<DDT,DDT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const DDT,DDT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<DDT&,DDT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const DDT &,DDT>::value,"");

  static_assert(!std::is_nothrow_constructible<DDT,int&,const int&>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,int&&,const D&>::value,"");

  static_assert(std::is_nothrow_constructible<DDT,D&,const D&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,D&&,const D&&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,const D &&,const int&&>::value,"");

/* three elements tests */
  static_assert(std::is_nothrow_constructible<BBBT,IIIT>::value,"");
  static_assert(!std::is_nothrow_constructible<BBBT,const IIIT>::value,"");
  static_assert(!std::is_nothrow_constructible<BBBT,IIIT&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBBT,const IIIT &>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IIIT,BBBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIIT,BBBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<IIIT&,BBBT>::value,"");
  static_assert(!test_trait::is_nothrow_convertible<const IIIT &,BBBT>::value,"");

  static_assert(!std::is_nothrow_constructible<BBBT,BBBT>::value,"");
  static_assert(std::is_nothrow_constructible<BBBT,const BBBT>::value,"");
  static_assert(std::is_nothrow_constructible<BBBT,BBBT&>::value,"");
  static_assert(std::is_nothrow_constructible<BBBT,const BBBT &>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<BBBT,BBBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BBBT,BBBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<BBBT&,BBBT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const BBBT &,BBBT>::value,"");

  static_assert(!std::is_nothrow_constructible<BBBT,int&,const int&,int&&>::value,"");
  static_assert(!std::is_nothrow_constructible<BBBT,int&, B&&,const int&&>::value,"");
  static_assert(std::is_nothrow_constructible<BBBT,int&&, B&,const B&&>::value,"");

  static_assert(!std::is_nothrow_constructible<BBBT,std::tuple<int&,const int&,int&&>>::value,"");
  static_assert(!std::is_nothrow_constructible<BBBT,std::tuple<int &, B&&,const int&&>>::value,"");
  static_assert(std::is_nothrow_constructible<BBBT,std::tuple<int &&, B&,const B&&>>::value,"");

/* explicit */
  static_assert(std::is_nothrow_constructible<DDDT,IIIT>::value,"");
  static_assert(!std::is_nothrow_constructible<DDDT,const IIIT>::value,"");
  static_assert(!std::is_nothrow_constructible<DDDT,IIIT&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDDT,const IIIT &>::value,"");

  static_assert(!std::is_nothrow_constructible<DDDT,DDDT>::value,"");
  static_assert(std::is_nothrow_constructible<DDDT,const DDDT>::value,"");
  static_assert(std::is_nothrow_constructible<DDDT,DDDT&>::value,"");
  static_assert(std::is_nothrow_constructible<DDDT,const DDDT &>::value,"");

  static_assert(!test_trait::is_nothrow_convertible<DDDT,DDDT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const DDDT,DDDT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<DDDT&,DDDT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<const DDDT &,DDDT>::value,"");

  static_assert(!std::is_nothrow_constructible<DDDT,int&,const int&,int&&>::value,"");
  static_assert(!std::is_nothrow_constructible<DDDT,int &, D&&,const int&&>::value,"");
  static_assert(std::is_nothrow_constructible<DDDT,int &&, D&,const D&&>::value,"");

  static_assert(!std::is_nothrow_constructible<DDDT,std::tuple<int&,const int&,int&&>>::value,"");
  static_assert(!std::is_nothrow_constructible<DDDT,std::tuple<int &, D&&,const int&&>>::value,"");
  static_assert(std::is_nothrow_constructible<DDDT,std::tuple<int &&, D&,const D&&>>::value,"");


  void Run()
  {
    VERIFY( (checkCopyThrowConstruct<BT, int>()) );
    VERIFY( (checkMoveThrowConstruct<BT, int>()) );
    VERIFY( (checkCopyThrowConstruct<BT, B>()) );
    VERIFY( (checkMoveThrowConstruct<BT, B>()) );
    VERIFY( (checkCopyThrowConstruct<BT, IT>()) );
    VERIFY( (checkMoveThrowConstruct<BT, IT>()) );
    VERIFY( (checkMoveThrowConstruct<BT, CIT>()) );
    VERIFY( (checkCopyThrowConstruct<BBT, IIT>()) );
    VERIFY( (checkMoveThrowConstruct<BBT, IIT>()) );
    VERIFY( (checkCopyThrowConstruct<BBT, IIP>()) );
    VERIFY( (checkMoveThrowConstruct<BBT, IIP>()) );
    VERIFY( (checkCopyThrowConstruct<BBT, BBP>()) );
    VERIFY( (checkMoveThrowConstruct<BBT, BBP>()) );
    VERIFY( (checkCopyThrowConstruct<BBT, std::tuple<B, int>>()) );
    VERIFY( (checkMoveThrowConstruct<BBT, std::tuple<B, int>>()) );

    VERIFY( (checkCopyThrowConstruct<DT, int>()) );
    VERIFY( (checkMoveThrowConstruct<DT, int>()) );
    VERIFY( (checkCopyThrowConstruct<DT, D>()) );
    VERIFY( (checkMoveThrowConstruct<DT, D>()) );
    VERIFY( (checkCopyThrowConstruct<DT, IT>()) );
    VERIFY( (checkMoveThrowConstruct<DT, IT>()) );
    VERIFY( (checkMoveThrowConstruct<DT, CIT>()) );
    VERIFY( (checkCopyThrowConstruct<DDT, IIT>()) );
    VERIFY( (checkMoveThrowConstruct<DDT, IIT>()) );
    VERIFY( (checkCopyThrowConstruct<DDT, IIP>()) );
    VERIFY( (checkMoveThrowConstruct<DDT, IIP>()) );
    VERIFY( (checkCopyThrowConstruct<DDT, DDP>()) );
    VERIFY( (checkMoveThrowConstruct<DDT, DDP>()) );
    VERIFY( (checkCopyThrowConstruct<DDT, std::tuple<D, int>>()) );
    VERIFY( (checkMoveThrowConstruct<DDT, std::tuple<D, int>>()) );

    VERIFY( (checkCopyThrowConstruct<DBT, IIT>()) );
    VERIFY( (checkMoveThrowConstruct<DBT, IIT>()) );
    VERIFY( (checkCopyThrowConstruct<DBT, DBT>()) );
    VERIFY( (checkMoveThrowConstruct<DBT, DBT>()) );
    VERIFY( (checkCopyThrowConstruct<DBT, DBP>()) );
    VERIFY( (checkMoveThrowConstruct<DBT, DBP>()) );
    VERIFY( (checkCopyThrowConstruct<DBT, IIP>()) );
    VERIFY( (checkMoveThrowConstruct<DBT, IIP>()) );


    VERIFY( (checkCopyThrowConstruct<IDIT, IIIT>()) );
    VERIFY( (checkMoveThrowConstruct<IDIT, IIIT>()) );
    VERIFY( (checkCopyThrowConstruct<IDIT, IDIT>()) );
    VERIFY( (checkMoveThrowConstruct<IDIT, IDIT>()) );
    VERIFY( (checkCopyThrowConstruct<IBIT, IIIT>()) );
    VERIFY( (checkMoveThrowConstruct<IBIT, IIIT>()) );
    VERIFY( (checkCopyThrowConstruct<IBIT, IBIT>()) );
    VERIFY( (checkMoveThrowConstruct<IBIT, IBIT>()) );
    VERIFY( (checkCopyThrowConstruct<IDBT, IIIT>()) );
    VERIFY( (checkMoveThrowConstruct<IDBT, IIIT>()) );
    VERIFY( (checkCopyThrowConstruct<IDBT, IDBT>()) );
    VERIFY( (checkMoveThrowConstruct<IDBT, IDBT>()) );

  }
}
namespace ThrowCopy
{
  struct C
  {
    C() noexcept(true)
       {}

    explicit
    C(const C&) noexcept(true) {}

  };

  typedef std::tuple<C> CT;
  typedef std::tuple<C,C> CCT;
  typedef std::pair<C,C> CCP;
  typedef std::tuple<int,int,C> IICT;

  static_assert(std::is_nothrow_constructible<CT, C&>::value, "");
  static_assert(std::is_nothrow_constructible<CT, const C&>::value, "");
  static_assert(std::is_nothrow_constructible<CT, C&&>::value, "");
  static_assert(std::is_nothrow_constructible<CT, const C &&>::value, "");


  static_assert(std::is_nothrow_constructible<CCT, C&, C&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, const C&, const C&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, C&&, C&&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, const C &&, const C &&>::value, "");

  static_assert(std::is_nothrow_constructible<IICT, int, int, C&>::value, "");
  static_assert(std::is_nothrow_constructible<IICT, int, int, const C&>::value, "");
  static_assert(std::is_nothrow_constructible<IICT, int, int, C&&>::value, "");
  static_assert(std::is_nothrow_constructible<IICT, int, int, const C &&>::value, "");

  static_assert(std::is_nothrow_constructible<CT, CT&>::value, "");
  static_assert(std::is_nothrow_constructible<CT, const CT&>::value, "");
  static_assert(std::is_nothrow_constructible<CT, CT&&>::value, "");
  static_assert(std::is_nothrow_constructible<CT, const CT &&>::value, "");

  static_assert(std::is_nothrow_constructible<CCT, CCT&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, const CCT&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, CCT&&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, const CCT &&>::value, "");

  static_assert(std::is_nothrow_constructible<CCT, CCP&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, const CCP&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, CCP&&>::value, "");
  static_assert(std::is_nothrow_constructible<CCT, const CCP &&>::value, "");

  static_assert(std::is_nothrow_constructible<IICT, IICT&>::value, "");
  static_assert(std::is_nothrow_constructible<IICT, const IICT&>::value, "");
  static_assert(std::is_nothrow_constructible<IICT, IICT&&>::value, "");
  static_assert(std::is_nothrow_constructible<IICT, const IICT &&>::value, "");

}
namespace ThrowMoveNothrowConversion
{
  struct D
  {
  D() noexcept(true)
  {}

  explicit
  D(const int&) noexcept(true)
  {}

  explicit
  D(int&&) noexcept(false)
  {  throwing_ctor_called = true; }

  };

  typedef std::tuple<D> DT;
  typedef std::tuple<D,D> DDT;
  typedef std::pair<D,D> DDP;
  typedef std::tuple<int,D,int> IDIT;


  static_assert(!std::is_nothrow_constructible<DT, int>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const int>::value, "");
  static_assert(std::is_nothrow_constructible<DT, int&>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const int &>::value, "");

  static_assert(!std::is_nothrow_constructible<DT, IT>::value, "");
  static_assert(std::is_nothrow_constructible<DT,const IT>::value, "");
  static_assert(std::is_nothrow_constructible<DT, IT&>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const IT &>::value, "");
  static_assert(std::is_nothrow_constructible<DT, std::tuple<int&>>::value, "");
  static_assert(std::is_nothrow_constructible<DT, const std::tuple<int&&>>::value, "");

  static_assert(test_trait::is_nothrow_convertible<DT,DT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<D,DT>::value,"");

/* two elements tests */
  static_assert(!std::is_nothrow_constructible<DDT,IIT>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const IIT>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,IIT&>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const IIT &>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,IIP>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const IIP>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,IIP&>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,const IIP &>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,std::tuple<int&,int&>>::value,"");

  static_assert(std::is_nothrow_constructible<DDT,DDT>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,DDP>::value,"");
  static_assert(std::is_nothrow_constructible<DDT,std::tuple<int&,D>>::value,"");
  static_assert(!std::is_nothrow_constructible<DDT,std::pair<D,int&&>>::value,"");

  static_assert(std::is_convertible<DDT,DDT>::value,"");

/* three elements tests */
  static_assert(!std::is_nothrow_constructible<IDIT,IIIT>::value,"");
  static_assert(std::is_nothrow_constructible<IDIT,const IIIT>::value,"");
  static_assert(std::is_nothrow_constructible<IDIT,IIIT&>::value,"");
  static_assert(std::is_nothrow_constructible<IDIT,const IIIT &>::value,"");

  static_assert(test_trait::is_nothrow_convertible<IDIT,IDIT>::value,"");
  static_assert(test_trait::is_nothrow_convertible<IDIT,IDIT>::value,"");

  static_assert(std::is_nothrow_constructible<IDIT,int&,const int&,int&&>::value,"");
  static_assert(std::is_nothrow_constructible<IDIT,int &, D&&,const int&&>::value,"");
  static_assert(std::is_nothrow_constructible<IDIT,std::tuple<int&,const int&,int&&>>::value,"");
  static_assert(!std::is_nothrow_constructible<IDIT,std::tuple<int &, int&&,const int&&>>::value,"");

  void Run()
  {
    VERIFY( (checkCopyThrowConstruct<DDT, IIT>()) );
    VERIFY( (checkMoveThrowConstruct<DDT, IIT>()) );
    VERIFY( (checkCopyThrowConstruct<DDT, std::tuple<D, int>>()) );
    VERIFY( (checkMoveThrowConstruct<DDT, std::tuple<D, int>>()) );
  }
}

int main()
{

  DefaultConstructionTests::Run();

  ThrowCopyNothrowConversion::Run();

  NothrowCopyThrowMoveThrowCopyConversionNothrowMoveConversion::Run();

  ThrowMoveNothrowConversion::Run();

}

