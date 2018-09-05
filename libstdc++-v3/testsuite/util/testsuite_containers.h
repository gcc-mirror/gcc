// -*- C++ -*-

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

#include <bits/boost_concept_check.h>
#include <cassert>
#include <testsuite_container_traits.h>
#include <utility> // for rel_ops.

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
      typedef typename test_type::pointer 		pointer;
      typedef typename test_type::const_pointer 	const_pointer;
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
      // _Alloc-aware requirements (table 82)
      typedef _Tp 					 test_type;
      typedef typename test_type::allocator_type      	 allocator_type;
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
    struct adaptor_types<_Tp, false> { };

  template<typename _Tp>
    struct reversible_types<_Tp, false> { };

  template<typename _Tp>
    struct allocator_aware_types<_Tp, false> { };

  template<typename _Tp>
    struct associative_types<_Tp, false> { };

  template<typename _Tp>
    struct unordered_types<_Tp, false> { };

  template<typename _Tp>
    struct mapped_types<_Tp, false> { };

  // Primary template.
  template<typename _Tp>
    struct types
    : basic_types<_Tp>, adaptor_types<_Tp>, reversible_types<_Tp>,
      allocator_aware_types<_Tp>, associative_types<_Tp>,
      unordered_types<_Tp>, mapped_types<_Tp>
    { };


  // Run-time test for constant_iterator requirements.
  template<typename _Tp, bool = traits<_Tp>::is_allocator_aware::value>
    struct populate
    {
      populate(_Tp& container)
      {
	// Avoid uninitialized warnings, requires DefaultContructible.
	typedef typename _Tp::value_type value_type;
	container.insert(container.begin(), value_type());
	container.insert(container.begin(), value_type());
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

  template<typename _Iterator,
	   bool _Mutable,
	   typename = typename std::iterator_traits<_Iterator>::iterator_category>
    struct iterator_concept_checks;

  // DR 691.
  template<typename _Tp>
    struct forward_members_unordered
    {
      forward_members_unordered(typename _Tp::value_type& v)
      {
	// Make sure that even if rel_ops is injected there is no ambiguity
	// when comparing iterators.
	using namespace std::rel_ops;

	typedef _Tp					test_type;
	test_type container;
	container.insert(v);

	iterator_concept_checks<typename _Tp::local_iterator, false> cc;
	iterator_concept_checks<typename _Tp::const_local_iterator,
				false> ccc;

	assert( container.cbegin(0) == container.begin(0) );
	assert( container.cend(0) == container.end(0) );
	const typename test_type::size_type bn = container.bucket(1);
	assert( container.cbegin(bn) != container.cend(bn) );
	assert( container.cbegin(bn) != container.end(bn) );
	assert( container.begin(bn) != container.cend(bn) );
      }
    };

  template<typename _Iterator>
    struct iterator_concept_checks<_Iterator, false,
				   std::forward_iterator_tag>
    {
      iterator_concept_checks()
      {
	using namespace __gnu_cxx;
	__function_requires<_ForwardIteratorConcept<_Iterator>>();
      }
    };

  template<typename _Iterator>
    struct iterator_concept_checks<_Iterator, true,
				   std::forward_iterator_tag>
    {
      iterator_concept_checks()
      {
	using namespace __gnu_cxx;
	__function_requires<_Mutable_ForwardIteratorConcept<_Iterator>>();
      }
    };

  template<typename _Iterator>
    struct iterator_concept_checks<_Iterator, false,
				   std::bidirectional_iterator_tag>
    {
      iterator_concept_checks()
      {
	using namespace __gnu_cxx;
	__function_requires<_BidirectionalIteratorConcept<_Iterator>>();
      }
    };

  template<typename _Iterator>
    struct iterator_concept_checks<_Iterator, true,
				   std::bidirectional_iterator_tag>
    {
      iterator_concept_checks()
      {
	using namespace __gnu_cxx;
	__function_requires<_Mutable_BidirectionalIteratorConcept<_Iterator>>();
      }
    };

  template<typename _Iterator>
    struct iterator_concept_checks<_Iterator, false,
				   std::random_access_iterator_tag>
    {
      iterator_concept_checks()
      {
	using namespace __gnu_cxx;
	__function_requires<_RandomAccessIteratorConcept<_Iterator>>();
      }
    };

  template<typename _Iterator>
    struct iterator_concept_checks<_Iterator, true,
				   std::random_access_iterator_tag>
    {
      iterator_concept_checks()
      {
	using namespace __gnu_cxx;
	__function_requires<_Mutable_RandomAccessIteratorConcept<_Iterator>>();
      }
    };

  template<typename _Tp>
    struct forward_members
    {
      forward_members(_Tp& container)
      {
	// Make sure that even if rel_ops is injected there is no ambiguity
	// when comparing iterators.
	using namespace std::rel_ops;

	typedef traits<_Tp> traits_type;
	iterator_concept_checks<typename _Tp::iterator,
				!(traits_type::is_associative::value
				  || traits_type::is_unordered::value)> cc;
	iterator_concept_checks<typename _Tp::const_iterator, false> ccc;

	assert( container.cbegin() == container.begin() );
	assert( container.end() == container.cend() );
	assert( container.cbegin() != container.cend() );
	assert( container.cbegin() != container.end() );
	assert( container.begin() != container.cend() );
      }
  };

  template<typename _Tp,
	   typename
    = typename std::iterator_traits<typename _Tp::iterator>::iterator_category>
    struct category_members : forward_members<_Tp>
    {
      category_members(_Tp& container)
	: forward_members<_Tp>(container)
      { };
    };

  template<typename _Tp>
    struct category_members<_Tp, std::random_access_iterator_tag>
    : forward_members<_Tp>
    {
      category_members(_Tp& container)
	: forward_members<_Tp>(container)
      {
	// Make sure that even if rel_ops is injected there is no ambiguity
	// when comparing iterators.
	using namespace std::rel_ops;

	assert( !(container.begin() < container.begin()) );
	assert( !(container.cbegin() < container.cbegin()) );
	assert( !(container.cbegin() < container.begin()) );
	assert( !(container.begin() < container.cbegin()) );
	assert( container.begin() <= container.begin() );
	assert( container.cbegin() <= container.cbegin() );
	assert( container.cbegin() <= container.begin() );
	assert( container.begin() <= container.cbegin() );

	assert( !(container.begin() > container.begin()) );
	assert( !(container.cbegin() > container.cbegin()) );
	assert( !(container.cbegin() > container.begin()) );
	assert( !(container.begin() > container.cbegin()) );
	assert( container.begin() >= container.begin() );
	assert( container.cbegin() >= container.cbegin() );
	assert( container.cbegin() >= container.begin() );
	assert( container.begin() >= container.cbegin() );

	assert( container.begin() - container.begin() == 0 );
	assert( container.cbegin() - container.cbegin() == 0 );
	assert( container.cbegin() - container.begin() == 0 );
	assert( container.begin() - container.cbegin() == 0 );

	assert( container.begin() + 0 == container.begin() );
	assert( container.cbegin() + 0 == container.cbegin() );
	assert( 0 + container.begin() == container.begin() );
	assert( 0 + container.cbegin() == container.cbegin() );
	assert( container.begin() - 0 == container.begin() );
	assert( container.cbegin() - 0 == container.cbegin() );
      }
  };

  template<typename _Tp>
    struct citerator
    {
      typedef _Tp 					test_type;
      typedef traits<test_type>				traits_type;
      typedef typename test_type::value_type 		value_type;

      static test_type _S_container;

      // Unconditional.
      struct members : category_members<_Tp>
      {
	members() : category_members<_Tp>(_S_container)
	{ }
      };

      // Run test.
      citerator()
      {
	populate<test_type> p(_S_container);
	members m1;
	reverse_members<test_type> m2(_S_container);
      }
  };

  template<typename _Tp>
  _Tp citerator<_Tp>::_S_container;

  // DR 130 vs. C++98 vs. C++11.
  // Defined in testsuite_shared.cc.
  void
  erase_external(std::set<int>& s);

  void
  erase_external(std::multiset<int>& s);

  void
  erase_external(std::map<int, int>& s);

  void
  erase_external(std::multimap<int, int>& s);

  void
  erase_external_iterators(std::set<int>& s);

  void
  erase_external_iterators(std::multiset<int>& s);

  void
  erase_external_iterators(std::map<int, int>& s);

  void
  erase_external_iterators(std::multimap<int, int>& s);

// NB: "must be compiled with C++11"
#if __cplusplus >= 201103L
template<typename _Tp>
  void
  linkage_check_cxx98_cxx11_erase(_Tp& container)
  {
    // Crashing when external reference and internal reference symbols are
    // equivalently mangled but have different size return types in C++98
    // and C++11 signatures.
    erase_external(container); 		// C++98
    container.erase(container.begin());	// C++11
  }

template<typename _Tp>
  void
  linkage_check_cxx98_cxx11_erase_iterators(_Tp& container)
  {
    // Crashing when external reference and internal reference symbols are
    // equivalently mangled but have different size return types in C++98
    // and C++11 signatures.
    erase_external_iterators(container);// C++98

    auto iter = container.begin();
    container.erase(iter, ++iter);	// C++11
  }
#endif

} // namespace __gnu_test

#endif
