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

#ifndef _GLIBCXX_TESTSUITE_CONTAINERS_H
#define _GLIBCXX_TESTSUITE_CONTAINERS_H

#include <cassert>
#include <testsuite_container_traits.h>

// Container requirement testing.
namespace __gnu_test
{
  // Compile-time typedef testing.
  template<typename _Tp, bool _Bt = traits<_Tp>::is_container::value>
    struct basic_types
    {
      // Base container requirements (table 80)
      typedef _Tp 					test_type;
      typedef typename test_type::value_type 		value_type;
      typedef typename test_type::reference 		reference;
      typedef typename test_type::const_reference 	const_reference;
      typedef typename test_type::iterator 		iterator;
      typedef typename test_type::const_iterator 	const_iterator;
      typedef typename test_type::size_type 		size_type;
      typedef typename test_type::difference_type 	difference_type;
    };

  // Conditional typedef testing, positive.
  template<typename _Tp, bool _Bt = traits<_Tp>::is_reversible::value>
    struct reversible_types
    {
      // Reversible container requirements (table 81)
      typedef _Tp 					 test_type;
      typedef typename test_type::reverse_iterator 	 reverse_iterator;
      typedef typename test_type::const_reverse_iterator const_reverse_iterator;
    };

  template<typename _Tp, bool _Bt = traits<_Tp>::is_allocator_aware::value>
    struct allocator_aware_types
    {
      // Allocator-aware requirements (table 82)
      typedef _Tp 					 test_type;
      typedef typename test_type::allocator_type      	 allocator_type;
    };

  template<typename _Tp, bool _Bt = traits<_Tp>::is_pointer_aware::value>
    struct pointer_aware_types
    {
      // Allocator-aware requirements (table 82)
      typedef _Tp 					 test_type;
      typedef typename test_type::pointer		 pointer;
      typedef typename test_type::const_pointer		 const_pointer;
    };

  template<typename _Tp, bool _Bt = traits<_Tp>::is_associative::value>
    struct associative_types
    {
      // Associative container requirements (table 85)
      typedef _Tp 					 test_type;
      typedef typename test_type::key_type		 key_type;
      typedef typename test_type::key_compare		 key_compare;
      typedef typename test_type::value_compare		 value_compare;
    };

  template<typename _Tp, bool = traits<_Tp>::is_unordered::value>
    struct unordered_types
    {
      // Unordered associative container requirements (table 87)
      typedef _Tp 					 test_type;
      typedef typename test_type::key_type		 key_type;
      typedef typename test_type::hasher		 hasher;
      typedef typename test_type::key_equal		 key_equal;
      typedef typename test_type::local_iterator	 local_iterator;
      typedef typename test_type::const_local_iterator	 const_local_iterator;
    };

  template<typename _Tp, bool _Bt = traits<_Tp>::is_mapped::value>
    struct mapped_types
    {
      typedef _Tp 					 test_type;
      typedef typename test_type::mapped_type	   	 mapped_type;
    };

  template<typename _Tp, bool = traits<_Tp>::is_adaptor::value>
    struct adaptor_types
    {
      // Container adaptor requirements.
      typedef _Tp 					test_type;
      typedef typename test_type::value_type 		value_type;
      typedef typename test_type::reference 		reference;
      typedef typename test_type::const_reference 	const_reference;
      typedef typename test_type::size_type 		size_type;
      typedef typename test_type::container_type 	container_type;
    };

  // Conditional typedef testing, negative.
  template<typename _Tp>
    struct basic_types<_Tp, false> { };

  template<typename _Tp>
    struct reversible_types<_Tp, false> { };

  template<typename _Tp>
    struct allocator_aware_types<_Tp, false> { };

  template<typename _Tp>
    struct pointer_aware_types<_Tp, false> { };

  template<typename _Tp>
    struct associative_types<_Tp, false> { };

  template<typename _Tp>
    struct unordered_types<_Tp, false> { };

  template<typename _Tp>
    struct mapped_types<_Tp, false> { };

  template<typename _Tp>
    struct adaptor_types<_Tp, false> { };

  // Primary template.
  template<typename _Tp>
    struct types
    : basic_types<_Tp>, adaptor_types<_Tp>, reversible_types<_Tp>,
      allocator_aware_types<_Tp>, pointer_aware_types<_Tp>,
      associative_types<_Tp>, unordered_types<_Tp>, mapped_types<_Tp>
    { };


  // Run-time test for constant_iterator requirements.
  template<typename _Tp, bool = traits<_Tp>::is_allocator_aware::value>
    struct populate
    {
      populate(_Tp& container)
      {
	typename _Tp::value_type v;
	container.insert(container.begin(), v);
	container.insert(container.begin(), v);
      }
  };

  template<typename _Tp>
    struct populate<_Tp, false>
    {
      populate(_Tp& container) { }
    };

  template<typename _Tp, bool = traits<_Tp>::is_reversible::value>
    struct reverse_members
    {
      reverse_members(_Tp& container)
      {
	assert( container.crbegin() == container.rbegin() );
	assert( container.crend() == container.rend() );
	assert( container.crbegin() != container.crend() );
      }
    };

  template<typename _Tp>
    struct reverse_members<_Tp, false>
    {
      reverse_members(_Tp& container) { }
    };

  // DR 691.
  template<typename _Tp, bool = traits<_Tp>::is_unordered::value>
    struct forward_members_unordered
    {
      forward_members_unordered(typename _Tp::value_type& v)
      {
	typedef _Tp					test_type;
	test_type container;
	container.insert(v);
	assert( container.cbegin(0) == container.begin(0) );
	assert( container.cend(0) == container.end(0) );
	const typename test_type::size_type bn = container.bucket(1);
	assert( container.cbegin(bn) != container.cend(bn) );
      }
    };

  template<typename _Tp>
    struct forward_members_unordered<_Tp, false>
    {
      forward_members_unordered(_Tp& container) { }
    };

  template<typename _Tp>
    struct citerator
    {
      typedef _Tp 					test_type;
      typedef traits<test_type>				traits_type;
      typedef typename test_type::value_type 		value_type;

      static test_type _S_container;

      // Unconditional.
      struct forward_members
      {
	forward_members()
	{
	  assert( _S_container.cbegin() == _S_container.begin() );
	  assert( _S_container.cend() == _S_container.end() );
	  assert( _S_container.cbegin() != _S_container.cend() );
	}
      };

      // Run test.
      citerator()
      {
	populate<test_type> p(_S_container);
	forward_members m1;
	reverse_members<test_type> m2(_S_container);
      }
  };

  template<typename _Tp>
  _Tp citerator<_Tp>::_S_container;


} // namespace __gnu_test

#endif
