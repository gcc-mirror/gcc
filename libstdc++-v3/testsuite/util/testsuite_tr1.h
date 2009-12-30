// -*- C++ -*-
// Testing utilities for the tr1 testsuite.
//
// Copyright (C) 2004, 2005, 2006, 2007, 2009 Free Software Foundation, Inc.
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
//

#ifndef _GLIBCXX_TESTSUITE_TR1_H
#define _GLIBCXX_TESTSUITE_TR1_H

#include <ext/type_traits.h>

namespace __gnu_test
{
  // For tr1/type_traits.
  template<template<typename> class Category, typename Type>
    bool
    test_category(bool value)
    {
      bool ret = true;
      ret &= Category<Type>::value == value;
      ret &= Category<const Type>::value == value;
      ret &= Category<volatile Type>::value == value;
      ret &= Category<const volatile Type>::value == value;
      ret &= Category<Type>::type::value == value;
      ret &= Category<const Type>::type::value == value;
      ret &= Category<volatile Type>::type::value == value;
      ret &= Category<const volatile Type>::type::value == value;
      return ret;
    }

  template<template<typename> class Property, typename Type>
    bool
    test_property(typename Property<Type>::value_type value)
    {
      bool ret = true;
      ret &= Property<Type>::value == value;
      ret &= Property<Type>::type::value == value;
      return ret;
    }

  // For testing tr1/type_traits/extent, which has a second template
  // parameter.
  template<template<typename, unsigned> class Property,
           typename Type,
	   unsigned Uint>
    bool
    test_property(typename Property<Type, Uint>::value_type value)
    {
      bool ret = true;
      ret &= Property<Type, Uint>::value == value;
      ret &= Property<Type, Uint>::type::value == value;
      return ret;
    }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  template<template<typename...> class Relationship,
           typename... Types>
    bool
    test_relationship(bool value)
    {
      bool ret = true;
      ret &= Relationship<Types...>::value == value;
      ret &= Relationship<Types...>::type::value == value;
      return ret;
    }
#else
  template<template<typename, typename> class Relationship,
           typename Type1, typename Type2>
    bool
    test_relationship(bool value)
    {
      bool ret = true;
      ret &= Relationship<Type1, Type2>::value == value;
      ret &= Relationship<Type1, Type2>::type::value == value;
      return ret;
    }
#endif

  // Test types.
  class ClassType { };
  typedef const ClassType           cClassType;
  typedef volatile ClassType        vClassType;
  typedef const volatile ClassType  cvClassType;

  class DerivedType : public ClassType { };

  enum EnumType { e0 };

  struct ConvType
  { operator int() const; };

  class AbstractClass
  {
    virtual void rotate(int) = 0;
  };

  class PolymorphicClass
  {
    virtual void rotate(int);
  };

  class DerivedPolymorphic : public PolymorphicClass { };

  class VirtualDestructorClass
  {
    virtual ~VirtualDestructorClass();
  };

  union UnionType { };

  class IncompleteClass;

  struct ExplicitClass
  {
    ExplicitClass(double&);
    explicit ExplicitClass(int&);
  };

  int truncate_float(float x) { return (int)x; }
  long truncate_double(double x) { return (long)x; }

  struct do_truncate_float_t
  {
    do_truncate_float_t()
    {
      ++live_objects;
    }

    do_truncate_float_t(const do_truncate_float_t&)
    {
      ++live_objects;
    }

    ~do_truncate_float_t()
    {
      --live_objects;
    }

    int operator()(float x) { return (int)x; }

    static int live_objects;
  };

  int do_truncate_float_t::live_objects = 0;

  struct do_truncate_double_t
  {
    do_truncate_double_t()
    {
     ++live_objects;
    }

    do_truncate_double_t(const do_truncate_double_t&)
    {
      ++live_objects;
    }

    ~do_truncate_double_t()
    {
      --live_objects;
    }

    long operator()(double x) { return (long)x; }

    static int live_objects;
  };

  int do_truncate_double_t::live_objects = 0;

  struct X
  {
    int bar;

    int foo()                   { return 1; }
    int foo_c() const           { return 2; }
    int foo_v()  volatile       { return 3; }
    int foo_cv() const volatile { return 4; }
  };

  // For use in 8_c_compatibility.
  template<typename R, typename T>
    typename __gnu_cxx::__enable_if<std::__are_same<R, T>::__value, 
				    bool>::__type
    check_ret_type(T)
    { return true; }

} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_TR1_H
