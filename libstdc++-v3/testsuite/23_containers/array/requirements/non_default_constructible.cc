// { dg-do compile { target c++11 } }
// { dg-require-effective-target rtti }

// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

// PR libstdc++/53248

#include <array>
#include <typeindex>
#include <typeinfo>

template < typename ...Types >
union super_union;

template < >
union super_union<>
{
  static  auto optioned_types() -> std::array<std::type_index, 0>
  { return std::array<std::type_index, 0>{ {} }; }
};

template < typename Head, typename ...Tail >
union super_union<Head, Tail...>
{
  static
  auto optioned_types() -> std::array<std::type_index, 1 + sizeof...(Tail)>
  {
    using std::type_index;

    return { {type_index(typeid(Head)), type_index(typeid(Tail))...} };
  }

  Head                  data;
  super_union<Tail...>  rest;
};
