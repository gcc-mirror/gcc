// -*- C++ -*-
// Testing utilities for the tr1 testsuite.
//
// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _GLIBCXX_TESTSUITE_TR1_H
#define _GLIBCXX_TESTSUITE_TR1_H

namespace __gnu_test
{  
  // For tr1/type_traits.
  template<template<typename> class Category,
	   typename Type, bool Tv>
    bool
    test_category()
    {
      bool ret = true;
      ret &= Category<Type>::value == Tv;
      ret &= Category<const Type>::value == Tv;
      ret &= Category<volatile Type>::value == Tv;
      ret &= Category<const volatile Type>::value == Tv;
      ret &= Category<Type>::type::value == Tv;
      ret &= Category<const Type>::type::value == Tv;
      ret &= Category<volatile Type>::type::value == Tv;
      ret &= Category<const volatile Type>::type::value == Tv;
      return ret;
    }

  template<template<typename> class Property,
	   typename Type, bool Tv>
    bool
    test_property()
    {
      bool ret = true;
      ret &= Property<Type>::value == Tv;
      ret &= Property<Type>::type::value == Tv;
      return ret;
    }

  template<template<typename, typename> class Relationship,
	   typename Type1, typename Type2, bool Tv>
    bool
    test_relationship()
    {
      bool ret = true;
      ret &= Relationship<Type1, Type2>::value == Tv;
      ret &= Relationship<Type1, Type2>::type::value == Tv;
      return ret;
    }

  // Test types.
  class ClassType { };
  typedef const ClassType           cClassType;
  typedef volatile ClassType        vClassType;
  typedef const volatile ClassType  cvClassType;
}; // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_TR1_H
