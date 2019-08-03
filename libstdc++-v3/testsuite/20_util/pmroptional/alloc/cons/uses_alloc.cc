// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>

#include <vector>
#include <string>
#include "../../../../../include/std/pmroptional"


bool tagged_constructor_called = false;
bool plain_constructor_called = false;

void reset_flags()
{
  tagged_constructor_called = false;
  plain_constructor_called = false;
}

struct aa
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  aa(std::allocator_arg_t,allocator_type)
  { tagged_constructor_called = true;}


  aa(std::allocator_arg_t,allocator_type, int i)
  { tagged_constructor_called = true;}

  aa(std::allocator_arg_t,allocator_type, int i, int j)
  { tagged_constructor_called = true;}

  aa(std::allocator_arg_t,allocator_type, std::initializer_list<int> il)
  { tagged_constructor_called = true;}


  aa(std::allocator_arg_t,allocator_type, aa const& other)
  { tagged_constructor_called = true;}

  aa(){ plain_constructor_called = true;}

  aa(int _i){ plain_constructor_called = true;}

  aa(int _i, int _j){ plain_constructor_called = true;}

  aa(std::initializer_list<int>){ plain_constructor_called = true;}

  aa(aa const& other){ plain_constructor_called = true;}

  aa& operator=(aa const& other) = default;
};

struct aa2
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  aa2(allocator_type)
  { tagged_constructor_called = true;}


  aa2(int i,allocator_type)
  { tagged_constructor_called = true;}

  aa2(int i, int j, allocator_type)
  { tagged_constructor_called = true;}

  aa2(std::initializer_list<int> il, allocator_type)
  { tagged_constructor_called = true;}


  aa2(aa2 const& other, allocator_type)
  { tagged_constructor_called = true;}

  aa2(){ plain_constructor_called = true;}

  aa2(int _i){ plain_constructor_called = true;}

  aa2(int _i, int _j){ plain_constructor_called = true;}

  aa2(std::initializer_list<int>){ plain_constructor_called = true;}

  aa2(aa2 const& other){ plain_constructor_called = true;}

  aa2& operator=(aa2 const& other) = default;
};

/* a type which does not satisfy uses_allocator_v, but has tagged constructors */
struct non_aa
{

  non_aa(std::allocator_arg_t,std::pmr::polymorphic_allocator<void>)
  { tagged_constructor_called = true;}


  non_aa(std::allocator_arg_t,std::pmr::polymorphic_allocator<void>, int i)
  { tagged_constructor_called = true;}

  non_aa(std::allocator_arg_t,std::pmr::polymorphic_allocator<void>, int i, int j)
  { tagged_constructor_called = true;}

  non_aa(std::allocator_arg_t,std::pmr::polymorphic_allocator<void>, std::initializer_list<int> il)
  { tagged_constructor_called = true;}


  non_aa(std::allocator_arg_t,std::pmr::polymorphic_allocator<void>, non_aa const& other)
  { tagged_constructor_called = true;}

  non_aa(){ plain_constructor_called = true;}

  non_aa(int _i){ plain_constructor_called = true;}

  non_aa(int _i, int _j){ plain_constructor_called = true;}

  non_aa(std::initializer_list<int>){ plain_constructor_called = true;}

  non_aa(non_aa const& other){ plain_constructor_called = true;}

  non_aa& operator=(non_aa const& other) = default;
};

/* only tagged constructors are called for an aa type */
void test_aa_type()
{
  reset_flags();
  {
    std::pmr::optional<aa> ov;
    VERIFY( !tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();


    ov.emplace(2);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

    ov.emplace(2,3);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

  }

  {
    auto ov = std::pmr::make_optional<aa>();
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

  }

  {
    auto ov = std::pmr::make_optional<aa>(33);
    reset_flags();

    std::vector<std::pmr::optional<aa>> vov(18,4);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

    auto oa = ov;
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

    auto ob = std::move(ov);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

  }

  {
    std::pmr::optional<aa> ov{std::in_place, {1,2}};
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();
  }
  {
    std::pmr::optional<aa> ov{std::in_place, 1,2};
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();
  }
}
/* only tagged constructors are called for an aa2 type */
void test_aa2_type()
{
  reset_flags();
  {
    std::pmr::optional<aa2> ov;
    VERIFY( !tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();


    ov.emplace(2);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

    ov.emplace(2,3);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

  }

  {
    auto ov = std::pmr::make_optional<aa2>();
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

  }

  {
    auto ov = std::pmr::make_optional<aa2>(33);
    reset_flags();

    std::vector<std::pmr::optional<aa2>> vov(18,4);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

    auto oa = ov;
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

    auto ob = std::move(ov);
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();

  }

  {
    std::pmr::optional<aa2> ov{std::in_place, {1,2}};
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();
  }
  {
    std::pmr::optional<aa2> ov{std::in_place, 1,2};
    VERIFY( tagged_constructor_called);
    VERIFY( !plain_constructor_called);
    reset_flags();
  }
}
/* tagged constructors are not used for an aa type, unless explicitly used */
void test_non_aa_type()
{
  reset_flags();
  {
      std::pmr::optional<non_aa > ov;
      VERIFY( !tagged_constructor_called);
      VERIFY( !plain_constructor_called);
      reset_flags();


      ov.emplace(2);
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();

      ov.emplace(2,3);
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();

    }

    {
      auto ov = std::pmr::make_optional<non_aa >();
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();

    }

    {
      auto ov = std::pmr::make_optional<non_aa >(33);
      reset_flags();

      std::vector<std::pmr::optional<non_aa >> vov(18,4);
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();

      auto oa = ov;
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();

      auto ob = std::move(ov);
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();

    }

    {
      std::pmr::optional<non_aa > ov{std::in_place, {1,2}};
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();
    }
    {
      std::pmr::optional<non_aa > ov{std::in_place, 1,2};
      VERIFY( !tagged_constructor_called);
      VERIFY( plain_constructor_called);
      reset_flags();
    }
    {
      std::pmr::optional<non_aa > ov{std::in_place, std::allocator_arg_t(), nullptr ,2};
      VERIFY( tagged_constructor_called);
      VERIFY( !plain_constructor_called);
      reset_flags();
    }
}

int main()
{

 static_assert(std::uses_allocator_v<std::pmr::optional<aa>,
	       std::pmr::polymorphic_allocator<void>>, "error");
 static_assert(std::uses_allocator_v<std::pmr::optional<std::pmr::optional<aa>>,
	       std::pmr::polymorphic_allocator<void>>, "error");

 static_assert(!std::uses_allocator_v<std::pmr::optional<non_aa>,
	       std::pmr::polymorphic_allocator<void>>, "error");
 static_assert(!std::uses_allocator_v<std::pmr::optional<std::pmr::optional<non_aa>>,
	       std::pmr::polymorphic_allocator<void>>, "error");

 static_assert(!std::uses_allocator_v<std::pmr::optional<int>,
	       std::pmr::polymorphic_allocator<void>>, "error");
 static_assert(!std::uses_allocator_v<std::pmr::optional<int>,
	       std::pmr::polymorphic_allocator<void>>, "error");


 test_aa_type();
 test_aa2_type();
 test_non_aa_type();


}
