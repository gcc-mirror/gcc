// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <type_traits>
#include <memory>

template<typename T, typename Base>
  struct Alloc : Base
  {
    typedef T value_type;
    Alloc();
    template <typename U>
      Alloc(const Alloc<U, Base>&);
    T* allocate(std::size_t);
    void deallocate(T*, std::size_t);
  };

template<bool> struct Empty { };
template<> struct Empty<false> { int x; };

template<bool B>
  struct WithType
  { using is_always_equal = std::integral_constant<bool, B>; };

struct EmptyAndTrue : Empty<true>, WithType<true> { };
struct EmptyButFalse : Empty<true>, WithType<false> { };

struct NotEmptyButTrue : Empty<false>, WithType<true> { };
struct NotEmptyAndFalse : Empty<false>, WithType<false> { };

template<typename Base>
  constexpr bool test()
  {
    using traits = std::allocator_traits<Alloc<int, Base>>;
    using test_type = typename traits::is_always_equal;
    static_assert(std::is_base_of<std::true_type, test_type>::value
                  || std::is_base_of<std::false_type, test_type>::value,
                  "has correct base characteristic");
    return test_type::value;
  }

static_assert( test<Empty<true>>(), "empty type is always equal" );
static_assert( !test<Empty<false>>(), "non-empty type is not always equal" );

static_assert( test<EmptyAndTrue>(), "nested type is used" );
static_assert( !test<EmptyButFalse>(), "nested type is used" );

static_assert( test<NotEmptyButTrue>(), "nested type is used" );
static_assert( !test<NotEmptyAndFalse>(), "nested type is used" );
