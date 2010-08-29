// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <vector>
#include <list>
#include <testsuite_hooks.h>

template<template <typename> class ContTraits>
  void
  debug_check1()
  {
    bool test __attribute__((unused)) = true;

    typedef ContTraits<int> Traits;
    typedef typename Traits::cont_type cont_type;
    typedef typename Traits::val_type val_type;
    typedef std::vector<val_type> vector_type;

    vector_type v;
    for (int i = 0; i != 5; ++i)
      v.push_back(Traits::make_val(i));
    VERIFY(v.size() == 5);

    const val_type* first = &v.front() + 1;
    const val_type* last = first + 2;

    cont_type c1;
    Traits::insert(c1, first, last);
    VERIFY(c1.size() == 2);

    cont_type c2;
    Traits::insert(c2, last, first);  // Expected failure
  }

template<template <typename> class ContTraits>
  void
  check1()
  {
#ifdef _GLIBCXX_DEBUG
    debug_check1<ContTraits>();
#else
    __builtin_abort();
#endif
  }

template<template <typename> class ContTraits>
  void
  debug_check2()
  {
    bool test __attribute__((unused)) = true;

    typedef ContTraits<int> Traits;
    typedef typename Traits::cont_type cont_type;
    typedef typename Traits::val_type val_type;
    typedef std::vector<val_type> vector_type;

    vector_type v;
    for (int i = 0; i != 5; ++i)
      v.push_back(Traits::make_val(i));
    VERIFY(v.size() == 5);

    typename vector_type::iterator first = v.begin() + 1;
    typename vector_type::iterator last = first + 2;
    cont_type c1;
    Traits::insert(c1, first, last);
    VERIFY(c1.size() == 2);

    cont_type c2;
    Traits::insert(c2, last, first); // Expected failure
  }

template<template <typename> class ContTraits>
  void
  check2()
  {
#ifdef _GLIBCXX_DEBUG
    debug_check2<ContTraits>();
#else
    __builtin_abort();
#endif
  }

template<template <typename> class ContTraits>
  void
  debug_check3()
  {
    bool test __attribute__((unused)) = true;

    typedef ContTraits<int> Traits;
    typedef typename Traits::cont_type cont_type;
    typedef typename Traits::val_type val_type;
    typedef std::list<val_type> list_type;

    list_type l;
    for (int i = 0; i != 5; ++i)
      l.push_back(Traits::make_val(i));
    VERIFY(l.size() == 5);

    typename list_type::iterator first = l.begin(); ++first;
    typename list_type::iterator last = first; ++last; ++last;
    cont_type c1;
    Traits::insert(c1, first, last);
    VERIFY(c1.size() == 2);

    cont_type c2;
    Traits::insert(c2, last, first); // Expected failure
  }

template<template <typename> class ContTraits>
  void
  check3()
  {
#ifdef _GLIBCXX_DEBUG
    debug_check3<ContTraits>();
#else
    __builtin_abort();
#endif
  }
