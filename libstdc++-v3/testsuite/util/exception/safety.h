// -*- C++ -*-

// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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
#include <cstdlib> // getenv, atoi
#include <cstdio>  // printf, fflush

// Container requirement testing.
namespace __gnu_test
{
  // Base class for exception testing, contains utilities.
  struct setup_base
  {
    typedef std::size_t 				size_type;
    typedef std::uniform_int_distribution<size_type> 	distribution_type;
    typedef std::mt19937 				engine_type;

    static engine_type
    get_engine()
    {
      engine_type engine;
      if (const char* v = std::getenv("GLIBCXX_SEED_TEST_RNG"))
	{
	  // A single seed value is much smaller than the mt19937 state size,
	  // but we're not trying to be cryptographically secure here.
	  int s = std::atoi(v);
	  if (s == 0)
	    s = (int)std::random_device{}();
	  std::printf("Using random seed %d\n", s);
	  std::fflush(stdout);
	  engine.seed((unsigned)s);
	}
      return engine;
    }

    // Return randomly generated integer on range [0, __max_size].
    static size_type
    generate(size_type __max_size)
    {
      using param_type = typename distribution_type::param_type;

      // Make the engine and distribution static...
      static engine_type engine = get_engine();
      static distribution_type distribution;
      return distribution(engine, param_type{0, __max_size});
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
	const size_type szt
	  = std::distance(__test.begin(), __test.end());
	const size_type szc
	  = std::distance(__control.begin(), __control.end());

	if (szt != szc)
	  throw std::logic_error(
		"setup_base::compare containers size not equal");

	// Should test iterator validity before and after exception.
	bool __equal_it = std::equal(__test.begin(), __test.end(),
				     __control.begin());

	if (!__equal_it)
	  throw std::logic_error(
		"setup_base::compare containers iterators not equal");

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
	typedef typename _Tp::const_iterator		const_iterator;

	iterator (_Tp::* _F_erase_point)(const_iterator);
	iterator (_Tp::* _F_erase_range)(const_iterator, const_iterator);

	erase_base()
	: _F_erase_point(&_Tp::erase), _F_erase_range(&_Tp::erase) { }
      };

#if _GLIBCXX_USE_CXX11_ABI == 0 || __cplusplus < 201103L
    // Specialization, old C++03 signature.
    template<typename _Tp1, typename _Tp2, typename _Tp3>
      struct erase_base<std::basic_string<_Tp1, _Tp2, _Tp3>>
      {
	typedef std::basic_string<_Tp1, _Tp2, _Tp3>     container_type;
	typedef typename container_type::iterator 	iterator;

	iterator (container_type::* _F_erase_point)(iterator);
	iterator (container_type::* _F_erase_range)(iterator, iterator);

	erase_base()
	: _F_erase_point(&container_type::erase),
	  _F_erase_range(&container_type::erase) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3>
      struct erase_base<__gnu_debug::basic_string<_Tp1, _Tp2, _Tp3>>
      {
	typedef __gnu_debug::basic_string<_Tp1, _Tp2, _Tp3>     container_type;
	typedef typename container_type::iterator 	iterator;

	iterator (container_type::* _F_erase_point)(iterator);
	iterator (container_type::* _F_erase_range)(iterator, iterator);

	erase_base()
	: _F_erase_point(&container_type::erase),
	  _F_erase_range(&container_type::erase) { }
      };
#endif

    // Specialization, as forward_list has erase_after.
    template<typename _Tp1, typename _Tp2>
      struct erase_base<std::forward_list<_Tp1, _Tp2>>
      {
	typedef std::forward_list<_Tp1, _Tp2> 		container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator const_iterator;

	iterator (container_type::* _F_erase_point)(const_iterator);
	iterator (container_type::* _F_erase_range)(const_iterator,
						    const_iterator);

	erase_base()
	: _F_erase_point(&container_type::erase_after),
	  _F_erase_range(&container_type::erase_after) { }
      };

    template<typename _Tp,
	     bool = traits<_Tp>::has_erase::value,
	     bool = traits<_Tp>::has_erase_after::value>
      struct erase_point;

    // Specialization for most containers.
    template<typename _Tp>
      struct erase_point<_Tp, true, false> : public erase_base<_Tp>
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
	      // Container::erase(pos) requires dereferenceable pos.
	      if (sz == 0)
		throw std::logic_error("erase_point: empty container");

	      // NB: Lowest common denominator: use forward iterator operations.
	      auto i = __container.begin();
	      std::advance(i, generate(sz - 1));

	      // Makes it easier to think of this as __container.erase(i)
	      (__container.*_F_erase_point)(i);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization for forward_list.
    template<typename _Tp>
      struct erase_point<_Tp, false, true> : public erase_base<_Tp>
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
	      // forward_list::erase_after(pos) requires dereferenceable pos.
	      if (sz == 0)
		throw std::logic_error("erase_point: empty container");

	      // NB: Lowest common denominator: use forward iterator operations.
	      auto i = __container.before_begin();
	      std::advance(i, generate(sz - 1));

	      // Makes it easier to think of this as __container.erase_after(i)
	      (__container.*_F_erase_point)(i);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct erase_point<_Tp, false, false>
      {
	void
	operator()(_Tp&) { }
      };


    template<typename _Tp,
	     bool = traits<_Tp>::has_erase::value,
	     bool = traits<_Tp>::has_erase_after::value>
      struct erase_range;

    // Specialization for most containers.
    template<typename _Tp>
      struct erase_range<_Tp, true, false> : public erase_base<_Tp>
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

    // Specialization for forward_list.
    template<typename _Tp>
      struct erase_range<_Tp, false, true> : public erase_base<_Tp>
      {
	using erase_base<_Tp>::_F_erase_range;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      const size_type sz = std::distance(__container.begin(),
						 __container.end());
	      // forward_list::erase_after(pos, last) requires a pos != last
	      if (sz == 0)
		return; // Caller doesn't check for this, not a logic error.

	      size_type s1 = generate(sz - 1);
	      size_type s2 = generate(sz - 1);
	      auto i1 = __container.before_begin();
	      auto i2 = __container.before_begin();
	      std::advance(i1, std::min(s1, s2));
	      std::advance(i2, std::max(s1, s2) + 1);

	      // Makes it easier to think of this as
	      // __container.erase_after(i1, i2).
	      (__container.*_F_erase_range)(i1, i2);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct erase_range<_Tp, false, false>
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

    template<typename _Tp, bool = traits<_Tp>::has_push_pop::value
				  && traits<_Tp>::has_emplace::value>
      struct emplace_front
      {
	typedef _Tp 					container_type;
	typedef typename container_type::value_type    	value_type;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.emplace_front(cv);
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
	      __test.emplace_front(cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
    };

    // Specialization, empty.
    template<typename _Tp>
      struct emplace_front<_Tp, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };


    template<typename _Tp, bool = traits<_Tp>::has_push_pop::value
				  && traits<_Tp>::has_emplace::value
				  && traits<_Tp>::is_reversible::value>
      struct emplace_back
      {
	typedef _Tp 					container_type;
	typedef typename container_type::value_type    	value_type;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.emplace_back(cv);
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
      struct emplace_back<_Tp, false>
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
	typedef typename _Tp::const_iterator    	const_iterator;
	typedef typename _Tp::value_type 		value_type;

	iterator (_Tp::* _F_insert_point)(const_iterator, const value_type&);

	insert_base() : _F_insert_point(&_Tp::insert) { }
      };

    // Specialization, old C++03 signature.
    template<typename _Tp1, typename _Tp2, typename _Tp3>
      struct insert_base<std::basic_string<_Tp1, _Tp2, _Tp3>>
      {
	typedef std::basic_string<_Tp1, _Tp2, _Tp3> 	container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator	const_iterator;
	typedef typename container_type::value_type 	value_type;

#if _GLIBCXX_USE_CXX11_ABI == 0 || __cplusplus < 201103L
	iterator (container_type::* _F_insert_point)(iterator, value_type);
#else
	iterator (container_type::* _F_insert_point)(const_iterator,
						     value_type);
#endif

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    template<typename _Tp1, typename _Tp2, typename _Tp3>
      struct insert_base<__gnu_debug::basic_string<_Tp1, _Tp2, _Tp3>>
      {
	typedef __gnu_debug::basic_string<_Tp1, _Tp2, _Tp3> 	container_type;
	typedef typename container_type::iterator 	iterator;
	typedef typename container_type::const_iterator	const_iterator;
	typedef typename container_type::value_type 	value_type;

#if _GLIBCXX_USE_CXX11_ABI == 0 || __cplusplus < 201103L
	iterator (container_type::* _F_insert_point)(iterator, value_type);
#else
	iterator (container_type::* _F_insert_point)(const_iterator,
						     value_type);
#endif

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    // Specialization, by value.
    template<typename _Tp1, typename _Tp2, typename _Tp3,
	     template <typename, typename, typename> class _Tp4>
      struct insert_base<__gnu_cxx::__versa_string<_Tp1, _Tp2, _Tp3, _Tp4>>
      {
	typedef __gnu_cxx::__versa_string<_Tp1, _Tp2, _Tp3, _Tp4>
                                                        container_type;
	typedef typename container_type::iterator       iterator;
	typedef typename container_type::const_iterator const_iterator;
	typedef typename container_type::value_type     value_type;

	iterator (container_type::* _F_insert_point)(const_iterator,
						     value_type);

	insert_base() : _F_insert_point(&container_type::insert) { }
      };

    // Specialization, as forward_list has insert_after.
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

    template<typename _Tp, bool = traits<_Tp>::has_insert::value,
			   bool = traits<_Tp>::has_insert_after::value>
      struct insert_point;

    // Specialization for most containers.
    template<typename _Tp>
      struct insert_point<_Tp, true, false> : public insert_base<_Tp>
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

    // Specialization for forward_list.
    template<typename _Tp>
      struct insert_point<_Tp, false, true> : public insert_base<_Tp>
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
	      auto i = __test.before_begin();
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
	      auto i = __test.before_begin();
	      std::advance(i, s);
	      (__test.*_F_insert_point)(i, cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
 	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct insert_point<_Tp, false, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };

    template<typename _Tp, bool = traits<_Tp>::has_emplace::value
				  && (traits<_Tp>::is_associative::value
				      || traits<_Tp>::is_unordered::value)>
      struct emplace;

    // Specialization for associative and unordered containers.
    template<typename _Tp>
      struct emplace<_Tp, true>
      {
	typedef _Tp					container_type;
	typedef typename container_type::value_type	value_type;
	typedef typename container_type::size_type	size_type;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      __test.emplace(cv);
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
	      __test.emplace(cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
 	}
      };

    // Specialization, empty.
    template<typename _Tp>
      struct emplace<_Tp, false>
      {
	void
	operator()(_Tp&) { }

	void
	operator()(_Tp&, _Tp&) { }
      };

    template<typename _Tp, bool = traits<_Tp>::has_emplace::value,
			   bool = traits<_Tp>::is_associative::value
				  || traits<_Tp>::is_unordered::value,
			   bool = traits<_Tp>::has_insert_after::value>
      struct emplace_point;

    // Specialization for most containers.
    template<typename _Tp>
      struct emplace_point<_Tp, true, false, false>
      {
	typedef _Tp 				       	container_type;
	typedef typename container_type::value_type 	value_type;

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
	      __test.emplace(i, cv);
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
	      __test.emplace(i, cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
 	}
      };

    // Specialization for associative and unordered containers.
    template<typename _Tp>
      struct emplace_point<_Tp, true, true, false>
      {
	typedef _Tp 				       	container_type;
	typedef typename container_type::value_type 	value_type;

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
	      __test.emplace_hint(i, cv);
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
	      __test.emplace_hint(i, cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
 	}
      };

    // Specialization for forward_list.
    template<typename _Tp>
      struct emplace_point<_Tp, true, false, true>
      {
	typedef _Tp 				       	container_type;
	typedef typename container_type::value_type 	value_type;

	void
	operator()(_Tp& __test)
	{
	  try
	    {
	      const value_type cv = generate_unique<value_type>();
	      const size_type sz = std::distance(__test.begin(), __test.end());
	      size_type s = generate(sz);
	      auto i = __test.before_begin();
	      std::advance(i, s);
	      __test.emplace_after(i, cv);
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
	      auto i = __test.before_begin();
	      std::advance(i, s);
	      __test.emplace_after(i, cv);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
 	}
      };

    // Specialization, empty.
    template<typename _Tp, bool is_associative_or_unordered,
			   bool has_insert_after>
      struct emplace_point<_Tp, false, is_associative_or_unordered,
			   has_insert_after>
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

    template<typename _Tp>
      struct assign_operator
      {
	_Tp _M_other;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      // An exception while assigning might leave the container empty
	      // making future attempts less relevant. So we copy it before to
	      // always assign to a non empty container. It also check for copy
	      // constructor exception safety at the same time.
	      _Tp __clone(__container);
	      __clone = _M_other;
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };


#if __cplusplus >= 201103L
    template<typename _Tp>
      struct move_assign_operator
      {
	_Tp _M_other;

	void
	operator()(_Tp& __container)
	{
	  try
	    {
	      __container = std::move(_M_other);
	    }
	  catch(const __gnu_cxx::forced_error&)
	    { throw; }
	}
      };
#endif
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
      typedef emplace<container_type>			emplace;
      typedef emplace_point<container_type>		emplace_point;
      typedef emplace_front<container_type>		emplace_front;
      typedef emplace_back<container_type>		emplace_back;
      typedef pop_front<container_type> 	       	pop_front;
      typedef pop_back<container_type> 			pop_back;
      typedef push_front<container_type> 	       	push_front;
      typedef push_back<container_type> 	       	push_back;
      typedef rehash<container_type> 			rehash;
      typedef swap<container_type> 			swap;
      typedef iterator_operations<container_type>	iterator_ops;
      typedef const_iterator_operations<container_type>	const_iterator_ops;
      typedef assign_operator<container_type>		assign_operator;
#if __cplusplus >= 201103L
      typedef move_assign_operator<container_type>	move_assign_operator;
#endif

      using base_type::compare;
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

      basic_safety() { run(); }

      void
      run()
      {
	{
	  // Setup.
	  condition_type::never_adjustor off;

	  // Construct containers.
	  container_type container;
	  populate p1(container);

	  // Construct list of member functions to exercise.
	  std::vector<function_type> functions;
	  typename base_type::iterator_ops iops;
	  functions.push_back(function_type(iops));
	  typename base_type::const_iterator_ops ciops;
	  functions.push_back(function_type(ciops));

	  typename base_type::erase_point erasep;
	  functions.push_back(function_type(erasep));
	  typename base_type::erase_range eraser;
	  functions.push_back(function_type(eraser));
	  typename base_type::insert_point insertp;
	  functions.push_back(function_type(insertp));
	  typename base_type::emplace emplace;
	  functions.push_back(function_type(emplace));
	  typename base_type::emplace_point emplacep;
	  functions.push_back(function_type(emplacep));
	  typename base_type::emplace_front emplacef;
	  functions.push_back(function_type(emplacef));
	  typename base_type::emplace_back emplaceb;
	  functions.push_back(function_type(emplaceb));
	  typename base_type::pop_front popf;
	  functions.push_back(function_type(popf));
	  typename base_type::pop_back popb;
	  functions.push_back(function_type(popb));
	  typename base_type::push_front pushf;
	  functions.push_back(function_type(pushf));
	  typename base_type::push_back pushb;
	  functions.push_back(function_type(pushb));
	  typename base_type::rehash rehash;
	  functions.push_back(function_type(rehash));
	  typename base_type::swap swap;
	  populate p2(swap._M_other);
	  functions.push_back(function_type(swap));
	  typename base_type::assign_operator assignop;
	  populate p3(assignop._M_other);
	  functions.push_back(function_type(assignop));
#if __cplusplus >= 201103L
	  typename base_type::move_assign_operator massignop;
	  populate p4(massignop._M_other);
	  functions.push_back(function_type(massignop));
#endif
	  // Last.
	  typename base_type::clear clear;
	  functions.push_back(function_type(clear));

	  // Run tests.
	  size_t i(1);
	  for (auto it = functions.begin(); it != functions.end(); ++it)
	    {
	      function_type& f = *it;
	      i = run_steps_to_limit(i, container, f);
	    }
	}

	// Now that all instances has been destroyed check that there is no
	// allocation remaining.
	std::cout << "Checking remaining stuff" << std::endl;
	__gnu_cxx::annotate_base::check();
      }

      template<typename _Funct>
	size_t
	run_steps_to_limit(size_t __step, container_type& __cont,
			   const _Funct& __f)
	{
	  bool exit(false);
	  auto a = __cont.get_allocator();

	  do
	    {
	      // Use the current step as an allocator label.
	      a.set_label(__step);

	      try
		{
		  condition_type::limit_adjustor limit(__step);
		  __f(__cont);

		  // If we get here, done.
		  exit = true;
		}
	      catch(const __gnu_cxx::forced_error&)
		{
		  // Check this step for allocations.
		  // NB: Will throw std::logic_error if allocations.
		  a.check(__step);

		  // Check memory allocated with operator new.

		}
	      ++__step;
	    }
	  while (!exit);

	  // Log count info.
#if __cpp_rtti
	  std::cout << __f.target_type().name() << std::endl;
#else
	  std::cout << "[no type info - rtti disabled]\n";
#endif
	  std::cout << "end count " << __step << std::endl;
	  return __step;
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

      generation_prohibited()  { run(); }

      void
      run()
      {
	// Furthermore, assumes that the test functor will throw
	// forced_exception via throw_allocator, that all errors are
	// propagated and in error. Sudden death!

	// Setup.
	container_type container;
	typename base_type::swap swap;

	{
	  condition_type::never_adjustor off;
	  populate p1(container);
	  populate p2(swap._M_other);
	}

	// Run tests.
	{
	  condition_type::always_adjustor on;

	  // NB: Vector and deque are special, erase can throw if the copy
	  // constructor or assignment operator of value_type throws.
	  if (!traits<container_type>::has_throwing_erase::value)
	    {
	      if (!container.empty())
		{
		  typename base_type::erase_point erasep;
		  erasep(container);
		}
	      typename base_type::erase_range eraser;
	      eraser(container);
	    }

	  if (!container.empty())
	    {
	      typename base_type::pop_front popf;
	      popf(container);
	    }
	  if (!container.empty())
	    {
	      typename base_type::pop_back popb;
	      popb(container);
	    }

	  typename base_type::iterator_ops iops;
	  iops(container);
	  typename base_type::const_iterator_ops ciops;
	  ciops(container);

	  swap(container);

	  // Last.
	  typename base_type::clear clear;
	  clear(container);
	}
      }
    };


  // Test strong exception guarantee.
  // Run through all member functions with a roll-back, consistent
  // coherent requirement.
  // all: member functions insert and emplace of a single element, push_back,
  // push_front
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

      propagation_consistent() { run(); }

      // Run test.
      void
      run()
      {
	// Setup.
	condition_type::never_adjustor off;

	// Construct containers.
	container_type container_control;

	populate p(container_control);

	// Construct list of member functions to exercise.
	std::vector<function_type> functions;
	typename base_type::emplace emplace;
	functions.push_back(function_type(emplace));
	typename base_type::emplace_point emplacep;
	functions.push_back(function_type(emplacep));
	typename base_type::emplace_front emplacef;
	functions.push_back(function_type(emplacef));
	typename base_type::emplace_back emplaceb;
	functions.push_back(function_type(emplaceb));
	typename base_type::push_front pushf;
	functions.push_back(function_type(pushf));
	typename base_type::push_back pushb;
	functions.push_back(function_type(pushb));
	typename base_type::insert_point insertp;
	functions.push_back(function_type(insertp));
	typename base_type::rehash rehash;
	functions.push_back(function_type(rehash));

	// Run tests.
	for (auto i = functions.begin(); i != functions.end(); ++i)
	  {
	    function_type& f = *i;
	    run_steps_to_limit(container_control, f);
	  }
      }

      template<typename _Funct>
	void
	run_steps_to_limit(container_type& container_control, const _Funct& __f)
	{
	  size_t i(1);
	  bool exit(false);

	  do
	    {
	      container_type container_test(container_control);

	      try
		{
		  condition_type::limit_adjustor limit(i);
		  __f(container_test);

		  // If we get here, done.
		  exit = true;
		}
	      catch(const __gnu_cxx::forced_error&)
		{
		  compare(container_control, container_test);
		  ++i;
		}
	    }
	  while (!exit);

	  // Log count info.
#if __cpp_rtti
	  std::cout << __f.target_type().name() << std::endl;
#else
	  std::cout << "[no type info - rtti disabled]\n";
#endif
	  std::cout << "end count " << i << std::endl;
	}
    };

} // namespace __gnu_test

#endif
