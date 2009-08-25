// -*- C++ -*-

// Copyright (C) 2009 Free Software Foundation, Inc.
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
  struct traits_base
  {
    // Type, nested type, and typedef related traits.
    typedef std::false_type	is_container;
    typedef std::false_type	is_adaptor;
    typedef std::false_type	is_reversible;
    typedef std::false_type	is_allocator_aware;
    typedef std::false_type	is_pointer_aware;
    typedef std::false_type	is_associative;
    typedef std::false_type	is_unordered;
    typedef std::false_type	is_mapped;
  };

  // Primary template does nothing. Specialize on each type under
  // test, derive off of traits_base and just add the true traits.
  template<typename _Tp>
    struct traits;

  // Specialize for each container.
  template<typename _Tp, size_t _Np>
    struct traits<std::array<_Tp, _Np> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
    };

  template<typename _Tp>
    struct traits<std::deque<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
    };

  template<typename _Tp>
    struct traits<std::forward_list<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
    };

  template<typename _Tp>
    struct traits<std::list<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
    };

  template<typename _Kp, typename _Tp>
    struct traits<std::map<_Kp, _Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_associative;
      typedef std::true_type	is_mapped;
    };

  template<typename _Kp, typename _Tp>
    struct traits<std::multimap<_Kp, _Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_associative;
      typedef std::true_type	is_mapped;
    };

  template<typename _Tp>
    struct traits<std::multiset<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_associative;
    };

  template<typename _Tp>
    struct traits<std::priority_queue<_Tp> > : public traits_base
    {
      typedef std::true_type	is_adaptor;
    };

  template<typename _Tp>
    struct traits<std::queue<_Tp> > : public traits_base
    {
      typedef std::true_type	is_adaptor;
    };

  template<typename _Tp>
    struct traits<std::set<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_reversible;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_associative;
    };

  template<typename _Tp>
    struct traits<std::stack<_Tp> > : public traits_base
    {
      typedef std::true_type	is_adaptor;
    };

  template<typename _Kp, typename _Tp>
    struct traits<std::unordered_map<_Kp, _Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_unordered;
      typedef std::true_type	is_mapped;
    };

  template<typename _Kp, typename _Tp>
    struct traits<std::unordered_multimap<_Kp, _Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_unordered;
      typedef std::true_type	is_mapped;
    };

  template<typename _Tp>
    struct traits<std::unordered_multiset<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_unordered;
    };

  template<typename _Tp>
    struct traits<std::unordered_set<_Tp> > : public traits_base
    {
      typedef std::true_type	is_container;
      typedef std::true_type	is_allocator_aware;
      typedef std::true_type	is_pointer_aware;
      typedef std::true_type	is_unordered;
    };

  template<typename _Tp>
    struct traits<std::vector<_Tp> > : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
      typedef std::true_type    is_allocator_aware;
      typedef std::true_type    is_pointer_aware;
    };

  template<typename _Tp>
    struct traits<std::basic_string<_Tp> > : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
      typedef std::true_type    is_allocator_aware;
      typedef std::true_type    is_pointer_aware;
    };

  template<typename _Tp>
    struct traits<__gnu_cxx::__versa_string<_Tp> > : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
      typedef std::true_type    is_allocator_aware;
      typedef std::true_type    is_pointer_aware;
    };
} // namespace __gnu_test

#endif 
