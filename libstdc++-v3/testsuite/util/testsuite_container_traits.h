// -*- C++ -*-

// Copyright (C) 2009-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef _GLIBCXX_TESTSUITE_CONTAINER_TRAITS_H
#define _GLIBCXX_TESTSUITE_CONTAINER_TRAITS_H

#include <bits/stdc++.h>
#include <ext/vstring.h>

namespace __gnu_test
{
  // Container traits.
  // Base class with default false values for all traits.
  struct traits_base
  {
    // Type, nested type, and typedef related traits.
    typedef std::false_type	is_container;
    typedef std::false_type	is_adaptor;
    typedef std::false_type	is_reversible;
    typedef std::false_type	is_allocator_aware;
    typedef std::false_type	is_associative;
    typedef std::false_type	is_unordered;
    typedef std::false_type	is_mapped;

    typedef std::false_type	has_erase;
    typedef std::false_type	has_erase_after;
    typedef std::false_type	has_throwing_erase;
    typedef std::false_type	has_insert;
    typedef std::false_type	has_insert_after;
    typedef std::false_type	has_emplace;
    typedef std::false_type	has_push_pop;
    typedef std::false_type	has_size_type_constructor;
  };

  // Primary template does nothing. Specialize on each type under
  // test, derive off of traits_base and just add the true traits.
  template<typename _Tp>
    struct traits;

  // Specialize for each container.
  template<typename _Tp, size_t _Np>
    struct traits<std::array<_Tp, _Np>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::deque<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_throwing_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_push_pop;
      typedef std::true_type	has_size_type_constructor;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::forward_list<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;

      typedef std::true_type	has_erase_after;
      typedef std::true_type	has_insert_after;
      typedef std::true_type	has_push_pop;
      typedef std::true_type	has_size_type_constructor;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::list<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_push_pop;
      typedef std::true_type	has_size_type_constructor;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::vector<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
      typedef std::true_type    is_allocator_aware;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_throwing_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_size_type_constructor;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3>
    struct traits<std::basic_string<_Tp1, _Tp2, _Tp3>> : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
      typedef std::true_type    is_allocator_aware;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3,
	   template <typename, typename, typename> class _Tp4>
    struct traits<__gnu_cxx::__versa_string<_Tp1, _Tp2, _Tp3, _Tp4>>
    : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
      typedef std::true_type    is_allocator_aware;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
    struct traits<std::map<_Tp1, _Tp2, _Tp3, _Tp4>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_associative;
      typedef std::true_type	is_mapped;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
    struct traits<std::multimap<_Tp1, _Tp2, _Tp3, _Tp4>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_associative;
      typedef std::true_type	is_mapped;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3>
    struct traits<std::set<_Tp1, _Tp2, _Tp3>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_associative;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3>
    struct traits<std::multiset<_Tp1, _Tp2, _Tp3>> : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_associative;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::priority_queue<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type	is_adaptor;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::queue<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type	is_adaptor;
    };

  template<typename _Tp1, typename _Tp2>
    struct traits<std::stack<_Tp1, _Tp2> > : public traits_base
    {
      typedef std::true_type	is_adaptor;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3,
	   typename _Tp4, typename _Tp5>
    struct traits<std::unordered_map<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>>
    : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_unordered;
      typedef std::true_type	is_mapped;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3,
	   typename _Tp4, typename _Tp5>
    struct traits<std::unordered_multimap<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>>
    : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_unordered;
      typedef std::true_type	is_mapped;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
    struct traits<std::unordered_multiset<_Tp1, _Tp2, _Tp3, _Tp4>>
    : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_unordered;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };

  template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
    struct traits<std::unordered_set<_Tp1, _Tp2, _Tp3, _Tp4>>
    : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_unordered;

      typedef std::true_type	has_erase;
      typedef std::true_type	has_insert;
      typedef std::true_type	has_emplace;
    };
} // namespace __gnu_test

#endif
