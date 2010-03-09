// -*- C++ -*-

// Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_EXCEPTION_SAFETY_H
#define _GLIBCXX_EXCEPTION_SAFETY_H

#include <testsuite_container_traits.h>
#include <ext/throw_allocator.h>

// Container requirement testing.
namespace __gnu_test
{
  // Base class for exception testing, contains utilities.
  struct setup_base
  {
    typedef std::size_t 				size_type;
    typedef std::uniform_int_distribution<size_type> 	distribution_type;
    typedef std::mt19937 				engine_type;

    // Return randomly generated integer on range [0, __max_size].
    static size_type
    generate(size_type __max_size)
    {
      // Make the generator static...
      const engine_type engine;
      const distribution_type distribution;
      static auto generator = std::bind(distribution, engine,
					std::placeholders::_1);

      // ... but set the range for this particular invocation here.
      const typename distribution_type::param_type p(0, __max_size);
      size_type random = generator(p);
      if (random < distribution.min() || random > distribution.max())
	{
	  std::string __s("setup_base::generate");
	  __s += "\n";
	  __s += "random number generated is: ";
	  char buf[40];
	  __builtin_sprintf(buf, "%lu", random);
	  __s += buf;
	  __s += " on range [";
	  __builtin_sprintf(buf, "%lu", distribution.min());
	  __s += buf;
	  __s += ", ";
	  __builtin_sprintf(buf, "%lu", distribution.max());
	  __s += buf;
	  __s += "]\n";
	  std::__throw_out_of_range(__s.c_str());
	}
      return random;
    }

    // Given an instantiating type, return a unique value.
    template<typename _Tp>
      struct generate_unique
      {
	typedef _Tp value_type;

	operator value_type()
	{
	  static value_type __ret;
	  ++__ret;
	  return __ret;
	}
      };

    // Partial specialization for pair.
    template<typename _Tp1, typename _Tp2>
      struct generate_unique<std::pair<const _Tp1, _Tp2>>
      {
	typedef _Tp1 first_type;
	typedef _Tp2 second_type;
	typedef std::pair<const _Tp1, _Tp2> pair_type;

	operator pair_type()
	{
	  static first_type _S_1;
	  static second_type _S_2;
	  ++_S_1;
	  ++_S_2;
	  return pair_type(_S_1, _S_2);
	}
      };

    // Partial specialization for throw_value
    template<typename _Cond>
      struct generate_unique<__gnu_cxx::throw_value_base<_Cond>>
      {
	typedef __gnu_cxx::throw_value_base<_Cond> value_type;

	operator value_type()
	{
	  static size_t _S_i(0);
	  return value_type(_S_i++);
	}
      };


    // Construct container of size n directly. _Tp == container type.
    template<typename _Tp>
      struct make_container_base
      {
	_Tp _M_container;

	make_container_base() = default;
	make_container_base(const size_type n): _M_container(n) { }

	operator _Tp&() { return _M_container; }
      };

    // Construct container of size n, via multiple insertions. For
    // associated and unordered types, unique value_type elements are
    // necessary.
    template<typename _Tp, bool = traits<_Tp>::is_mapped::value>
      struct make_insert_container_base
      : public make_container_base<_Tp>
      {
	using make_container_base<_Tp>::_M_container;
	typedef typename _Tp::value_type value_type;

	make_insert_container_base(const size_type n)
	{
	  for (size_type i = 0; i < n; ++i)
	    {
	      value_type v = generate_unique<value_type>();
	      _M_container.insert(v);
	    }
	  assert(_M_container.size() == n);
	}
      };

    template<typename _Tp>
      struct make_insert_container_base<_Tp, false>
      : public make_container_base<_Tp>
      {
	using make_container_base<_Tp>::_M_container;
	typedef typename _Tp::value_type value_type;

	make_insert_container_base(const size_type n)
	{
	  for (size_type i = 0; i < n; ++i)
	    {
	      value_type v = generate_unique<value_type>();
	      _M_container.insert(_M_container.end(), v);
	    }
	  assert(_M_container.size() == n);
	}
      };

    template<typename _Tp, bool = traits<_Tp>::has_size_type_constructor::value>
      struct make_container_n;

    // Specialization for non-associative types that have a constructor with
    // a size argument.
    template<typename _Tp>
      struct make_container_n<_Tp, true>
      : public make_container_base<_Tp>
      {
	make_container_n(const size_type n) : make_container_base<_Tp>(n) { }
      };

    template<typename _Tp>
      struct make_container_n<_Tp, false>
      : public make_insert_container_base<_Tp>
      {
	make_container_n(const size_type n)
	: make_insert_container_base<_Tp>(n) { }
      };


    // Randomly size and populate a given container reference.
    // NB: Responsibility for turning off exceptions lies with caller.
    template<typename _Tp, bool = traits<_Tp>::is_allocator_aware::value>
      struct populate
      {
	typedef _Tp 					container_type;
	typedef typename container_type::allocator_type	allocator_type;
	typedef typename container_type::value_type    	value_type;

	populate(_Tp& __container)
	{
	  const allocator_type a = __container.get_allocator();

	  // Size test container.
	  const size_type max_elements = 100;
	  size_type n = generate(max_elements);

	  // Construct new container.
	  make_container_n<container_type> made(n);
	  container_type& tmp = made;
	  std::swap(tmp, __container);
	}
      };

    // Partial specialization, empty.
    template<typename _Tp>
      struct populate<_Tp, false>
      {
	populate(_Tp&) { }
      };

    // Compare two containers for equivalence.
    // Right now, that means size.
    // Returns true if equal, throws if not.
    template<typename _Tp>
      static bool
      compare(const _Tp& __control, const _Tp& __test)
      {
	// Make sure test container is in a consistent state, as
	// compared to the control container.
	// NB: Should be equivalent to __test != __control, but
	// computed without equivalence operators
	const size_type szt = std::distance(__test.begin(), __test.end());
	const size_type szc = std::distance(__control.begin(),
					    __control.end());
	bool __equal_size = szt == szc;

	// Should test iterator validity before and after exception.
	bool __equal_it = std::equal(__test.begin(), __test.end(),
				     __control.begin());

	if (!__equal_size || !__equal_it)
	  throw std::logic_error("setup_base::compare containers not equal");

	return true;
      }
  };


  // Containing structure holding functors.
  struct functor_base : public setup_base
  {
    // Abstract the erase function.
    template<typename _Tp>
      struct erase_base
      {
	typedef typename _Tp::iterator 			iterator;

	iterator (_Tp::* _F_erase_point)(iterator);
	iterator (_Tp::* _F_erase_range)(iterator, iterator);

	erase_base()
	: _F_erase_point(&_Tp::erase), _F_erase_range(&_Tp::erase) { }
      };

    // Specialization, as forward_list has erase_after.
    template<typename _Tp1, typename _Tp2>
      struct erase_base<std::forward_list<_Tp1, _Tp2>>
      {
	typedef std::forward_list<_Tp1, _Tp2> 		container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator 	const_iterator;

	void (container_type::* _F_erase_point)(const_iterator);
	void (container_type::* _F_erase_range)(const_iterator, const_iterator);

	erase_base()
	: _F_erase_point(&container_type::erase_after),
	  _F_erase_range(&container_type::erase_after) { }
      };

    // Specializations for the unordered containers.
    template<typename _Tp1, typename _Tp2, typename _Tp3,
	     typename _Tp4, typename _Tp5>
      struct erase_base<std::unordered_map<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>>
      {
	typedef std::unordered_map<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;

	iterator (container_type::* _F_erase_point)(const_iterator);
	iterator (container_type::* _F_erase_range)(const_iterator,
						    const_iterator);

	erase_base()
	: _F_erase_point(&container_type::erase),
	  _F_erase_range(&container_type::erase) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3,
	     typename _Tp4, typename _Tp5>
      struct erase_base<std::unordered_multimap<_Tp1, _Tp2, _Tp3,
						_Tp4, _Tp5>>
      {
	typedef std::unordered_multimap<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;

	iterator (container_type::* _F_erase_point)(const_iterator);
	iterator (container_type::* _F_erase_range)(const_iterator,
						    const_iterator);

	erase_base()
	: _F_erase_point(&container_type::erase),
	  _F_erase_range(&container_type::erase) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
      struct erase_base<std::unordered_set<_Tp1, _Tp2, _Tp3, _Tp4>>
      {
	typedef std::unordered_set<_Tp1, _Tp2, _Tp3, _Tp4>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;

	iterator (container_type::* _F_erase_point)(const_iterator);
	iterator (container_type::* _F_erase_range)(const_iterator,
						    const_iterator);

	erase_base()
	: _F_erase_point(&container_type::erase),
	  _F_erase_range(&container_type::erase) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
      struct erase_base<std::unordered_multiset<_Tp1, _Tp2, _Tp3, _Tp4>>
      {
	typedef std::unordered_multiset<_Tp1, _Tp2, _Tp3, _Tp4>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;

	iterator (container_type::* _F_erase_point)(const_iterator);
	iterator (container_type::* _F_erase_range)(const_iterator,
						    const_iterator);

	erase_base()
	: _F_erase_point(&container_type::erase),
	  _F_erase_range(&container_type::erase) { }
      };

    template<typename _Tp, bool = traits<_Tp>::has_erase::value>
      struct erase_point : public erase_base<_Tp>
      {
	using erase_base<_Tp>::_F_erase_point;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      // NB: Should be equivalent to size() member function, but
	      // computed with begin() and end().
	      const size_type sz = std::distance(__container.begin(),
						 __container.end());

	      // NB: Lowest common denominator: use forward iterator operations.
	      auto i = __container.begin();
	      std::advance(i, generate(sz));

	      // Makes it easier to think of this as __container.erase(i)
	      (__container.*_F_erase_point)(i);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct erase_point<_Tp, false>
      {
	void
	operator()(_Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::has_erase::value>
      struct erase_range : public erase_base<_Tp>
      {
	using erase_base<_Tp>::_F_erase_range;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      const size_type sz = std::distance(__container.begin(),
						 __container.end());
	      size_type s1 = generate(sz);
	      size_type s2 = generate(sz);
	      auto i1 = __container.begin();
	      auto i2 = __container.begin();
	      std::advance(i1, std::min(s1, s2));
	      std::advance(i2, std::max(s1, s2));

	      // Makes it easier to think of this as __container.erase(i1, i2).
	      (__container.*_F_erase_range)(i1, i2);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct erase_range<_Tp, false>
      {
	void
	operator()(_Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::has_push_pop::value>
      struct pop_front
      {
	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      __container.pop_front();
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct pop_front<_Tp, false>
      {
	void
	operator()(_Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::has_push_pop::value
				  && traits<_Tp>::is_reversible::value>
      struct pop_back
      {
	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      __container.pop_back();
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct pop_back<_Tp, false>
      {
	void
	operator()(_Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::has_push_pop::value>
      struct push_front
      {
	typedef _Tp 					container_type;
	typedef typename container_type::value_type    	value_type;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.push_front(cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}

	// Assumes containers start out equivalent.
	void
	operator()(_Tp& __control, _Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.push_front(cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
    };

    // Specialization, empty.
    template<typename _Tp>
      struct push_front<_Tp, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::has_push_pop::value
				  && traits<_Tp>::is_reversible::value>
      struct push_back
      {
	typedef _Tp 					container_type;
	typedef typename container_type::value_type    	value_type;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.push_back(cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}

	// Assumes containers start out equivalent.
	void
	operator()(_Tp& __control, _Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.push_back(cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
    };

    // Specialization, empty.
    template<typename _Tp>
      struct push_back<_Tp, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };


    // Abstract the insert function into two parts:
    // 1, insert_base_functions == holds function pointer
    // 2, insert_base == links function pointer to class insert method
    template<typename _Tp>
      struct insert_base
      {
	typedef typename _Tp::iterator 			iterator;
	typedef typename _Tp::value_type 		value_type;

	iterator (_Tp::* _F_insert_point)(iterator, const value_type&);

	insert_base() : _F_insert_point(&_Tp::insert) { }
      };

    // Specialization, as string insertion has a different signature.
    template<typename _Tp1, typename _Tp2, typename _Tp3>
      struct insert_base<std::basic_string<_Tp1, _Tp2, _Tp3>>
      {
	typedef std::basic_string<_Tp1, _Tp2, _Tp3> 	container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(iterator, value_type);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3,
	     template <typename, typename, typename> class _Tp4>
      struct insert_base<__gnu_cxx::__versa_string<_Tp1, _Tp2, _Tp3, _Tp4>>
      {
	typedef __gnu_cxx::__versa_string<_Tp1, _Tp2, _Tp3, _Tp4>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(iterator, value_type);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    // Specialization, as forward_list insertion has a different signature.
    template<typename _Tp1, typename _Tp2>
      struct insert_base<std::forward_list<_Tp1, _Tp2>>
      {
	typedef std::forward_list<_Tp1, _Tp2> container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(const_iterator,
						     const value_type&);

	insert_base() : _F_insert_point(&container_type::insert_after) { }
      };

    // Likewise for the unordered containers.
    template<typename _Tp1, typename _Tp2, typename _Tp3,
	     typename _Tp4, typename _Tp5>
      struct insert_base<std::unordered_map<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>>
      {
	typedef std::unordered_map<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(const_iterator,
						     const value_type&);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3,
	     typename _Tp4, typename _Tp5>
      struct insert_base<std::unordered_multimap<_Tp1, _Tp2, _Tp3,
						 _Tp4, _Tp5>>
      {
	typedef std::unordered_multimap<_Tp1, _Tp2, _Tp3, _Tp4, _Tp5>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(const_iterator,
						     const value_type&);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
      struct insert_base<std::unordered_set<_Tp1, _Tp2, _Tp3, _Tp4>>
      {
	typedef std::unordered_set<_Tp1, _Tp2, _Tp3, _Tp4>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(const_iterator,
						     const value_type&);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3, typename _Tp4>
      struct insert_base<std::unordered_multiset<_Tp1, _Tp2, _Tp3, _Tp4>>
      {
	typedef std::unordered_multiset<_Tp1, _Tp2, _Tp3, _Tp4>
	                                                container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;
	typedef typename container_type::value_type 	value_type;

	iterator (container_type::* _F_insert_point)(const_iterator,
						     const value_type&);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    template<typename _Tp, bool = traits<_Tp>::has_insert::value>
      struct insert_point : public insert_base<_Tp>
      {
	typedef _Tp 				       	container_type;
	typedef typename container_type::value_type 	value_type;
	using insert_base<_Tp>::_F_insert_point;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      const size_type sz = std::distance(__test.begin(), __test.end());
	      size_type s = generate(sz);
	      auto i = __test.begin();
	      std::advance(i, s);
	      (__test.*_F_insert_point)(i, cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}

	// Assumes containers start out equivalent.
	void
	operator()(_Tp& __control, _Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      const size_type sz = std::distance(__test.begin(), __test.end());
	      size_type s = generate(sz);
	      auto i = __test.begin();
	      std::advance(i, s);
	      (__test.*_F_insert_point)(i, cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
 	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct insert_point<_Tp, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::is_associative::value
				  || traits<_Tp>::is_unordered::value>
      struct clear
      {
	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      __container.clear();
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct clear<_Tp, false>
      {
	void
	operator()(_Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::is_unordered::value>
      struct rehash
      {
	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      size_type s = generate(__test.bucket_count());
	      __test.rehash(s);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}

	void
	operator()(_Tp& __control, _Tp& __test)
	{
	  try
	    {
	      size_type s = generate(__test.bucket_count());
	      __test.rehash(s);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    {
	      // Also check hash status.
	      bool fail(false);
	      if (__control.load_factor() != __test.load_factor())
		fail = true;
	      if (__control.max_load_factor() != __test.max_load_factor())
		fail = true;
	      if (__control.bucket_count() != __test.bucket_count())
		fail = true;
	      if (__control.max_bucket_count() != __test.max_bucket_count())
		fail = true;

	      if (fail)
		{
		  char buf[40];
		  std::string __s("setup_base::rehash "
				  "containers not equal");
		  __s += "\n";
		  __s += "\n";
		  __s += "\t\t\tcontrol : test";
		  __s += "\n";
		  __s += "load_factor\t\t";
		  __builtin_sprintf(buf, "%lu", __control.load_factor());
		  __s += buf;
		  __s += " : ";
		  __builtin_sprintf(buf, "%lu", __test.load_factor());
		  __s += buf;
		  __s += "\n";

		  __s += "max_load_factor\t\t";
		  __builtin_sprintf(buf, "%lu", __control.max_load_factor());
		  __s += buf;
		  __s += " : ";
		  __builtin_sprintf(buf, "%lu", __test.max_load_factor());
		  __s += buf;
		  __s += "\n";

		  __s += "bucket_count\t\t";
		  __builtin_sprintf(buf, "%lu", __control.bucket_count());
		  __s += buf;
		  __s += " : ";
		  __builtin_sprintf(buf, "%lu", __test.bucket_count());
		  __s += buf;
		  __s += "\n";

		  __s += "max_bucket_count\t";
		  __builtin_sprintf(buf, "%lu", __control.max_bucket_count());
		  __s += buf;
		  __s += " : ";
		  __builtin_sprintf(buf, "%lu", __test.max_bucket_count());
		  __s += buf;
		  __s += "\n";

		  std::__throw_logic_error(__s.c_str());
		}
	    }
 	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct rehash<_Tp, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };


    template<typename _Tp>
      struct swap
      {
	_Tp _M_other;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      __container.swap(_M_other);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };


    template<typename _Tp>
      struct iterator_operations
      {
	typedef _Tp 					container_type;
	typedef typename container_type::iterator       iterator;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      // Any will do.
	      iterator i = __container.begin();
	      iterator __attribute__((unused)) icopy(i);
	      iterator __attribute__((unused)) iassign = i;
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };


    template<typename _Tp>
      struct const_iterator_operations
      {
	typedef _Tp 					container_type;
	typedef typename container_type::const_iterator	const_iterator;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      // Any will do.
	      const_iterator i = __container.begin();
	      const_iterator __attribute__((unused)) icopy(i);
	      const_iterator __attribute__((unused)) iassign = i;
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };
  };

  // Base class for exception tests.
  template<typename _Tp>
    struct test_base: public functor_base
    {
      typedef _Tp 					container_type;

      typedef functor_base				base_type;
      typedef populate<container_type> 	       		populate;
      typedef make_container_n<container_type> 	       	make_container_n;

      typedef clear<container_type> 		       	clear;
      typedef erase_point<container_type> 	       	erase_point;
      typedef erase_range<container_type> 	       	erase_range;
      typedef insert_point<container_type> 	       	insert_point;
      typedef pop_front<container_type> 	       	pop_front;
      typedef pop_back<container_type> 			pop_back;
      typedef push_front<container_type> 	       	push_front;
      typedef push_back<container_type> 	       	push_back;
      typedef rehash<container_type> 			rehash;
      typedef swap<container_type> 			swap;
      typedef iterator_operations<container_type>	iterator_ops;
      typedef const_iterator_operations<container_type>	const_iterator_ops;

      using base_type::compare;

      // Functor objects.
      clear			_M_clear;
      erase_point		_M_erasep;
      erase_range		_M_eraser;
      insert_point		_M_insertp;
      pop_front			_M_popf;
      pop_back			_M_popb;
      push_front	       	_M_pushf;
      push_back			_M_pushb;
      rehash			_M_rehash;
      swap			_M_swap;

      iterator_ops	       	_M_iops;
      const_iterator_ops	_M_ciops;
    };


  // Run through all member functions for basic exception safety
  // guarantee: no resource leaks when exceptions are thrown.
  //
  // Types of resources checked: memory.
  //
  // For each member function, use throw_value and throw_allocator as
  // value_type and allocator_type to force potential exception safety
  // errors.
  //
  // NB: Assumes
  // _Tp::value_type is __gnu_cxx::throw_value_*
  // _Tp::allocator_type is __gnu_cxx::throw_allocator_*
  // And that the _Cond template parameter for them both is
  // __gnu_cxx::limit_condition.
  template<typename _Tp>
    struct basic_safety : public test_base<_Tp>
    {
      typedef _Tp 					container_type;
      typedef test_base<container_type>			base_type;
      typedef typename base_type::populate 		populate;
      typedef std::function<void(container_type&)> 	function_type;
      typedef __gnu_cxx::limit_condition		condition_type;

      using base_type::generate;

      container_type 					_M_container;
      std::vector<function_type>			_M_functions;

      basic_safety() { run(); }

      void
      run()
      {
	// Setup.
	condition_type::never_adjustor off;
	
	// Construct containers.
	populate p1(_M_container);
	populate p2(base_type::_M_swap._M_other);
	
	// Construct list of member functions to exercise.
	_M_functions.push_back(function_type(base_type::_M_iops));
	_M_functions.push_back(function_type(base_type::_M_ciops));
	
	_M_functions.push_back(function_type(base_type::_M_erasep));
	_M_functions.push_back(function_type(base_type::_M_eraser));
	_M_functions.push_back(function_type(base_type::_M_insertp));
	_M_functions.push_back(function_type(base_type::_M_popf));
	_M_functions.push_back(function_type(base_type::_M_popb));
	_M_functions.push_back(function_type(base_type::_M_pushf));
	_M_functions.push_back(function_type(base_type::_M_pushb));
	_M_functions.push_back(function_type(base_type::_M_rehash));
	_M_functions.push_back(function_type(base_type::_M_swap));
	
	// Last.
	_M_functions.push_back(function_type(base_type::_M_clear));

	// Run tests.
	auto i = _M_functions.begin();
	for (auto i = _M_functions.begin(); i != _M_functions.end(); ++i)
	  {
	    function_type& f = *i;
	    run_steps_to_limit(f);
	  }
      }

      template<typename _Funct>
	void
	run_steps_to_limit(const _Funct& __f)
	{
	  size_t i(1);
	  bool exit(false);
	  auto a = _M_container.get_allocator();

	  do
	    {
	      // Use the current step as an allocator label.
	      a.set_label(i);

	      try
		{
		  condition_type::limit_adjustor limit(i);
		  __f(_M_container);

		  // If we get here, done.
		  exit = true;
		}
	      catch(const __gnu_cxx::forced_error&)
		{
		  // Check this step for allocations.
		  // NB: Will throw std::logic_error if allocations.
		  a.check_allocated(i);

		  // Check memory allocated with operator new.

		  ++i;
		}
	    }
	  while (!exit);

	  // Log count info.
	  std::cout << __f.target_type().name() << std::endl;
	  std::cout << "end count " << i << std::endl;
	}
  };


  // Run through all member functions with a no throw requirement, sudden death.
  // all: member functions erase, pop_back, pop_front, swap
  //      iterator copy ctor, assignment operator
  // unordered and associative: clear
  // NB: Assumes _Tp::allocator_type is __gnu_cxx::throw_allocator_random.
  template<typename _Tp>
    struct generation_prohibited : public test_base<_Tp>
    {
      typedef _Tp 					container_type;
      typedef test_base<container_type>			base_type;
      typedef typename base_type::populate 		populate;
      typedef __gnu_cxx::random_condition		condition_type;

      container_type 					_M_container;

      generation_prohibited()  { run(); }

      void
      run()
      {
	// Furthermore, assumes that the test functor will throw
	// forced_exception via throw_allocator, that all errors are
	// propagated and in error. Sudden death!

	// Setup.
	{
	  condition_type::never_adjustor off;
	  populate p1(_M_container);
	  populate p2(base_type::_M_swap._M_other);
	}

	// Run tests.
	{
	  condition_type::always_adjustor on;

	  // NB: Vector and deque are special, erase can throw if the copy
	  // constructor or assignment operator of value_type throws.
	  if (!traits<container_type>::has_throwing_erase::value)
	    {
	      _M_erasep(_M_container);
	      _M_eraser(_M_container);
	    }

	  _M_popf(_M_container);
	  _M_popb(_M_container);

	  _M_iops(_M_container);
	  _M_ciops(_M_container);

	  _M_swap(_M_container);

	  // Last.
	  _M_clear(_M_container);
	}
      }
    };


  // Test strong exception guarantee.
  // Run through all member functions with a roll-back, consistent
  // coherent requirement.
  // all: member functions insert of a single element, push_back, push_front
  // unordered: rehash
  template<typename _Tp>
    struct propagation_consistent : public test_base<_Tp>
    {
      typedef _Tp 					container_type;
      typedef test_base<container_type>			base_type;
      typedef typename base_type::populate 		populate;
      typedef std::function<void(container_type&)> 	function_type;
      typedef __gnu_cxx::limit_condition		condition_type;

      using base_type::compare;

      container_type 					_M_container_test;
      container_type 					_M_container_control;
      std::vector<function_type>			_M_functions;

      propagation_consistent() { run(); }

      void
      sync()
      { _M_container_test = _M_container_control; }

      // Run test.
      void
      run()
      {
	// Setup.
	condition_type::never_adjustor off;

	// Construct containers.
	populate p(_M_container_control);
	sync();

	// Construct list of member functions to exercise.
	_M_functions.push_back(function_type(base_type::_M_pushf));
	_M_functions.push_back(function_type(base_type::_M_pushb));
	_M_functions.push_back(function_type(base_type::_M_insertp));
	_M_functions.push_back(function_type(base_type::_M_rehash));

	// Run tests.
	auto i = _M_functions.begin();
	for (auto i = _M_functions.begin(); i != _M_functions.end(); ++i)
	  {
	    function_type& f = *i;
	    run_steps_to_limit(f);
	  }
      }

      template<typename _Funct>
	void
	run_steps_to_limit(const _Funct& __f)
	{
	  size_t i(1);
	  bool exit(false);

	  do
	    {
	      sync();

	      try
		{
		  condition_type::limit_adjustor limit(i);
		  __f(_M_container_test);

		  // If we get here, done.
		  exit = true;
		}
	      catch(const __gnu_cxx::forced_error&)
		{
		  compare(_M_container_control, _M_container_test);
		  ++i;
		}
	    }
	  while (!exit);

	  // Log count info.
	  std::cout << __f.target_type().name() << std::endl;
	  std::cout << "end count " << i << std::endl;
	}
    };

} // namespace __gnu_test

#endif
