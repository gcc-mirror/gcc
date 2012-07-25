// hashtable.h header -*- C++ -*-

// Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/hashtable.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{unordered_map, unordered_set}
 */

#ifndef _HASHTABLE_H
#define _HASHTABLE_H 1

#pragma GCC system_header

#include <bits/hashtable_policy.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _Tp, typename _Hash>
    using __cache_default =  __not_<__and_<is_integral<_Tp>,
					   is_empty<_Hash>,
				  integral_constant<bool, !__is_final(_Hash)>,
				 __detail::__is_noexcept_hash<_Tp, _Hash> >>;

  /**
   *  Primary class template _Hashtable.
   *
   *  @ingroup hashtable-detail
   *
   *  @tparam _Value  CopyConstructible type.
   *
   *  @tparam _Key    CopyConstructible type.
   *
   *  @tparam _Alloc  An allocator type
   *  ([lib.allocator.requirements]) whose _Alloc::value_type is
   *  _Value.  As a conforming extension, we allow for
   *  _Alloc::value_type != _Value.
   *
   *  @tparam _ExtractKey  Function object that takes an object of type
   *  _Value and returns a value of type _Key.
   *
   *  @tparam _Equal  Function object that takes two objects of type k
   *  and returns a bool-like value that is true if the two objects
   *  are considered equal.
   *
   *  @tparam _H1  The hash function. A unary function object with
   *  argument type _Key and result type size_t. Return values should
   *  be distributed over the entire range [0, numeric_limits<size_t>:::max()].
   *
   *  @tparam _H2  The range-hashing function (in the terminology of
   *  Tavori and Dreizin).  A binary function object whose argument
   *  types and result type are all size_t.  Given arguments r and N,
   *  the return value is in the range [0, N).
   *
   *  @tparam _Hash  The ranged hash function (Tavori and Dreizin). A
   *  binary function whose argument types are _Key and size_t and
   *  whose result type is size_t.  Given arguments k and N, the
   *  return value is in the range [0, N).  Default: hash(k, N) =
   *  h2(h1(k), N).  If _Hash is anything other than the default, _H1
   *  and _H2 are ignored.
   *
   *  @tparam _RehashPolicy  Policy class with three members, all of
   *  which govern the bucket count. _M_next_bkt(n) returns a bucket
   *  count no smaller than n.  _M_bkt_for_elements(n) returns a
   *  bucket count appropriate for an element count of n.
   *  _M_need_rehash(n_bkt, n_elt, n_ins) determines whether, if the
   *  current bucket count is n_bkt and the current element count is
   *  n_elt, we need to increase the bucket count.  If so, returns
   *  make_pair(true, n), where n is the new bucket count.  If not,
   *  returns make_pair(false, <anything>)
   *
   *  @tparam _Traits  Compile-time class with three boolean
   *  std::integral_constant members:  __cache_hash_code, __constant_iterators,
   *   __unique_keys.
   *
   *  Each _Hashtable data structure has:
   *
   *  - _Bucket[]       _M_buckets
   *  - _Hash_node_base _M_before_begin
   *  - size_type       _M_bucket_count
   *  - size_type       _M_element_count
   *
   *  with _Bucket being _Hash_node* and _Hash_node constaining:
   *
   *  - _Hash_node*   _M_next
   *  - Tp            _M_value
   *  - size_t        _M_code if cache_hash_code is true
   *
   *  In terms of Standard containers the hastable is like the aggregation of:
   *
   *  - std::forward_list<_Node> containing the elements
   *  - std::vector<std::forward_list<_Node>::iterator> representing the buckets
   *
   *  The non-empty buckets contain the node before the first bucket
   *  node. This design allow to implement something like a
   *  std::forward_list::insert_after on container insertion and
   *  std::forward_list::erase_after on container erase
   *  calls. _M_before_begin is equivalent to
   *  std::foward_list::before_begin. Empty buckets are containing
   *  nullptr.  Note that one of the non-empty bucket contains
   *  &_M_before_begin which is not a derefenrenceable node so the
   *  node pointers in buckets shall never be derefenrenced, only its
   *  next node can be.
   *
   *  Walk through a bucket nodes require a check on the hash code to
   *  see if the node is still in the bucket. Such a design impose a
   *  quite efficient hash functor and is one of the reasons it is
   *  highly advise to set __cache_hash_code to true.
   *
   *  The container iterators are simply built from nodes. This way
   *  incrementing the iterator is perfectly efficient independent of
   *  how many empty buckets there are in the container.
   *
   *  On insert we compute element hash code and thanks to it find the
   *  bucket index. If the element must be inserted on an empty bucket
   *  we add it at the beginning of the singly linked list and make the
   *  bucket point to _M_before_begin. The bucket that used to point to
   *  _M_before_begin, if any, is updated to point to its new before
   *  begin node.
   *
   *  On erase, the simple iterator design impose to use the hash
   *  functor to get the index of the bucket to update. For this
   *  reason, when __cache_hash_code is set to false, there is a static
   *  assertion that the hash functor cannot throw.
   *
   *  Functionality is implemented by decomposition into base classes,
   *  where the derived _Hashtable class is used in _Map_base,
   *  _Insert, _Rehash_base, and _Equality base classes to access the
   *  "this" pointer. _Hashtable_base is used in the base classes as a
   *  non-recursive, fully-completed-type so that detailed nested type
   *  information, such as iterator type and node type, can be
   *  used. This is similar to the "Curiously Recurring Template
   *  Pattern" (CRTP) technique, but uses a reconstructed, not
   *  explicitly passed, template pattern.
   *
   *  Base class templates are: 
   *    - __detail::_Hashtable_base
   *    - __detail::_Map_base
   *    - __detail::_Insert
   *    - __detail::_Rehash_base
   *    - __detail::_Equality
   */
  template<typename _Key, typename _Value, typename _Alloc,
	   typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash,
	   typename _RehashPolicy, typename _Traits>
    class _Hashtable
    : public __detail::_Hashtable_base<_Key, _Value, _ExtractKey, _Equal,
				       _H1, _H2, _Hash, _Traits>,
      public __detail::_Map_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
				 _H1, _H2, _Hash, _RehashPolicy, _Traits>,
      public __detail::_Insert<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			       _H1, _H2, _Hash, _RehashPolicy, _Traits>,
      public __detail::_Rehash_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
				    _H1, _H2, _Hash, _RehashPolicy, _Traits>,
      public __detail::_Equality<_Key, _Value, _Alloc, _ExtractKey, _Equal,
				 _H1, _H2, _Hash, _RehashPolicy, _Traits>
    {
    public:
      typedef _Key                                    key_type;
      typedef _Value                                  value_type;
      typedef _Alloc                                  allocator_type;
      typedef _Equal                                  key_equal;

      // mapped_type, if present, comes from _Map_base.
      // hasher, if present, comes from _Hash_code_base/_Hashtable_base.
      typedef typename _Alloc::pointer		      pointer;
      typedef typename _Alloc::const_pointer          const_pointer;
      typedef typename _Alloc::reference              reference;
      typedef typename _Alloc::const_reference        const_reference;

    private:
      using __rehash_type = _RehashPolicy;
      using __rehash_state = typename __rehash_type::_State;

      using __traits_type = _Traits;
      using __hash_cached = typename __traits_type::__hash_cached;
      using __constant_iterators = typename __traits_type::__constant_iterators;
      using __unique_keys = typename __traits_type::__unique_keys;

      using __key_extract = typename std::conditional<
					     __constant_iterators::value,
				       	     __detail::_Identity,
					     __detail::_Select1st>::type;

      using __hashtable_base = __detail::
			       _Hashtable_base<_Key, _Value, _ExtractKey,
					      _Equal, _H1, _H2, _Hash, _Traits>;

      using __hash_code_base =  typename __hashtable_base::__hash_code_base;
      using __hash_code =  typename __hashtable_base::__hash_code;
      using __node_type = typename __hashtable_base::__node_type;
      using __node_base = typename __hashtable_base::__node_base;
      using __bucket_type = typename __hashtable_base::__bucket_type;
      using __ireturn_type = typename __hashtable_base::__ireturn_type;
      using __iconv_type = typename __hashtable_base::__iconv_type;

      using __map_base = __detail::_Map_base<_Key, _Value, _Alloc, _ExtractKey,
					     _Equal, _H1, _H2, _Hash,
					     _RehashPolicy, _Traits>;

      using __rehash_base = __detail::_Rehash_base<_Key, _Value, _Alloc,
						   _ExtractKey, _Equal,
						   _H1, _H2, _Hash,
						   _RehashPolicy, _Traits>;

      using __eq_base = __detail::_Equality<_Key, _Value, _Alloc, _ExtractKey,
					    _Equal, _H1, _H2, _Hash,
					    _RehashPolicy, _Traits>;

      // Metaprogramming for picking apart hash caching.
      using __hash_noexcept = __detail::__is_noexcept_hash<_Key, _H1>;

      template<typename _Cond>
	using __if_hash_cached = __or_<__not_<__hash_cached>, _Cond>;

      template<typename _Cond>
	using __if_hash_not_cached = __or_<__hash_cached, _Cond>;

      // Compile-time diagnostics.

      // When hash codes are not cached the hash functor shall not
      // throw because it is used in methods (erase, swap...) that
      // shall not throw.
      static_assert(__if_hash_not_cached<__hash_noexcept>::value,
		    "Cache the hash code"
		    " or qualify your hash functor with noexcept");

      // Following two static assertions are necessary to guarantee
      // that swapping two hashtable instances won't invalidate
      // associated local iterators.

      // When hash codes are cached local iterator only uses H2 which
      // must then be empty.
      static_assert(__if_hash_cached<is_empty<_H2>>::value,
		    "Functor used to map hash code to bucket index"
		    " must be empty");

      // When hash codes are not cached local iterator is going to use
      // __hash_code_base above to compute node bucket index so it has
      // to be empty.
      static_assert(__if_hash_not_cached<is_empty<__hash_code_base>>::value,
		   "Cache the hash code or make functors involved in hash code"
		   " and bucket index computation empty");

    public:
      template<typename _Keya, typename _Valuea, typename _Alloca,
	       typename _ExtractKeya, typename _Equala,
	       typename _H1a, typename _H2a, typename _Hasha,
	       typename _RehashPolicya, typename _Traitsa,
	       bool _Unique_keysa>
	friend struct __detail::_Map_base;

      template<typename _Keya, typename _Valuea, typename _Alloca,
	       typename _ExtractKeya, typename _Equala,
	       typename _H1a, typename _H2a, typename _Hasha,
	       typename _RehashPolicya, typename _Traitsa>
	friend struct __detail::_Insert_base;

      template<typename _Keya, typename _Valuea, typename _Alloca,
	       typename _ExtractKeya, typename _Equala,
	       typename _H1a, typename _H2a, typename _Hasha,
	       typename _RehashPolicya, typename _Traitsa,
	       bool _Constant_iteratorsa, bool _Unique_keysa>
	friend struct __detail::_Insert;

      using size_type = typename __hashtable_base::size_type;
      using difference_type = typename __hashtable_base::difference_type;

      using iterator = typename __hashtable_base::iterator;
      using const_iterator = typename __hashtable_base::const_iterator;

      using local_iterator = typename __hashtable_base::local_iterator;
      using const_local_iterator = typename __hashtable_base::
				   const_local_iterator;

    private:
      typedef typename _Alloc::template rebind<__node_type>::other
							_Node_allocator_type;
      typedef typename _Alloc::template rebind<__bucket_type>::other
							_Bucket_allocator_type;
      typedef typename _Alloc::template rebind<value_type>::other
							_Value_allocator_type;


      _Node_allocator_type	_M_node_allocator;
      __bucket_type*		_M_buckets;
      size_type			_M_bucket_count;
      __node_base	       	_M_before_begin;
      size_type			_M_element_count;
      _RehashPolicy		_M_rehash_policy;

      template<typename... _Args>
	__node_type*
	_M_allocate_node(_Args&&... __args);

      void
      _M_deallocate_node(__node_type* __n);

      // Deallocate the linked list of nodes pointed to by __n
      void
      _M_deallocate_nodes(__node_type* __n);

      __bucket_type*
      _M_allocate_buckets(size_type __n);

      void
      _M_deallocate_buckets(__bucket_type*, size_type __n);

      // Gets bucket begin, deals with the fact that non-empty buckets contain
      // their before begin node.
      __node_type*
      _M_bucket_begin(size_type __bkt) const;

      __node_type*
      _M_begin() const
      { return static_cast<__node_type*>(_M_before_begin._M_nxt); }

    public:
      // Constructor, destructor, assignment, swap
      _Hashtable(size_type __bucket_hint,
		 const _H1&, const _H2&, const _Hash&,
		 const _Equal&, const _ExtractKey&,
		 const allocator_type&);

      template<typename _InputIterator>
	_Hashtable(_InputIterator __first, _InputIterator __last,
		   size_type __bucket_hint,
		   const _H1&, const _H2&, const _Hash&,
		   const _Equal&, const _ExtractKey&,
		   const allocator_type&);

      _Hashtable(const _Hashtable&);

      _Hashtable(_Hashtable&&);

      // Use delegating construtors.
      explicit
      _Hashtable(size_type __n = 10,
		 const _H1& __hf = _H1(),
		 const key_equal& __eql = key_equal(),
		 const allocator_type& __a = allocator_type())
      : _Hashtable(__n, __hf, __detail::_Mod_range_hashing(),
		   __detail::_Default_ranged_hash(), __eql,
		   __key_extract(), __a)
      { }

      template<typename _InputIterator>
	_Hashtable(_InputIterator __f, _InputIterator __l,
		   size_type __n = 0,
		   const _H1& __hf = _H1(),
		   const key_equal& __eql = key_equal(),
		   const allocator_type& __a = allocator_type())
	: _Hashtable(__f, __l, __n, __hf, __detail::_Mod_range_hashing(),
		     __detail::_Default_ranged_hash(), __eql,
		     __key_extract(), __a)
	{ }

      _Hashtable(initializer_list<value_type> __l,
		 size_type __n = 0,
		 const _H1& __hf = _H1(),
		 const key_equal& __eql = key_equal(),
		 const allocator_type& __a = allocator_type())
      : _Hashtable(__l.begin(), __l.end(), __n, __hf,
		   __detail::_Mod_range_hashing(),
		   __detail::_Default_ranged_hash(), __eql,
		   __key_extract(), __a)
      { }

      _Hashtable&
      operator=(const _Hashtable& __ht)
      {
	_Hashtable __tmp(__ht);
	this->swap(__tmp);
	return *this;
      }

      _Hashtable&
      operator=(_Hashtable&& __ht)
      {
	// NB: DR 1204.
	// NB: DR 675.
	this->clear();
	this->swap(__ht);
	return *this;
      }

      _Hashtable&
      operator=(initializer_list<value_type> __l)
      {
	this->clear();
	this->insert(__l.begin(), __l.end());
	return *this;
      }

      ~_Hashtable() noexcept;

      void swap(_Hashtable&);

      // Basic container operations
      iterator
      begin() noexcept
      { return iterator(_M_begin()); }

      const_iterator
      begin() const noexcept
      { return const_iterator(_M_begin()); }

      iterator
      end() noexcept
      { return iterator(nullptr); }

      const_iterator
      end() const noexcept
      { return const_iterator(nullptr); }

      const_iterator
      cbegin() const noexcept
      { return const_iterator(_M_begin()); }

      const_iterator
      cend() const noexcept
      { return const_iterator(nullptr); }

      size_type
      size() const noexcept
      { return _M_element_count; }

      bool
      empty() const noexcept
      { return size() == 0; }

      allocator_type
      get_allocator() const noexcept
      { return allocator_type(_M_node_allocator); }

      size_type
      max_size() const noexcept
      { return _M_node_allocator.max_size(); }

      // Observers
      key_equal
      key_eq() const
      { return this->_M_eq(); }

      // hash_function, if present, comes from _Hash_code_base.

      // Bucket operations
      size_type
      bucket_count() const noexcept
      { return _M_bucket_count; }

      size_type
      max_bucket_count() const noexcept
      { return max_size(); }

      size_type
      bucket_size(size_type __n) const
      { return std::distance(begin(__n), end(__n)); }

      size_type
      bucket(const key_type& __k) const
      { return _M_bucket_index(__k, this->_M_hash_code(__k)); }

      local_iterator
      begin(size_type __n)
      { return local_iterator(_M_bucket_begin(__n), __n, _M_bucket_count); }

      local_iterator
      end(size_type __n)
      { return local_iterator(nullptr, __n, _M_bucket_count); }

      const_local_iterator
      begin(size_type __n) const
      { return const_local_iterator(_M_bucket_begin(__n), __n,
				    _M_bucket_count); }

      const_local_iterator
      end(size_type __n) const
      { return const_local_iterator(nullptr, __n, _M_bucket_count); }

      // DR 691.
      const_local_iterator
      cbegin(size_type __n) const
      { return const_local_iterator(_M_bucket_begin(__n), __n,
				    _M_bucket_count); }

      const_local_iterator
      cend(size_type __n) const
      { return const_local_iterator(nullptr, __n, _M_bucket_count); }

      float
      load_factor() const noexcept
      {
	return static_cast<float>(size()) / static_cast<float>(bucket_count());
      }

      // max_load_factor, if present, comes from _Rehash_base.

      // Generalization of max_load_factor.  Extension, not found in
      // TR1.  Only useful if _RehashPolicy is something other than
      // the default.
      const _RehashPolicy&
      __rehash_policy() const
      { return _M_rehash_policy; }

      void
      __rehash_policy(const _RehashPolicy&);

      // Lookup.
      iterator
      find(const key_type& __k);

      const_iterator
      find(const key_type& __k) const;

      size_type
      count(const key_type& __k) const;

      std::pair<iterator, iterator>
      equal_range(const key_type& __k);

      std::pair<const_iterator, const_iterator>
      equal_range(const key_type& __k) const;

    protected:
      // Bucket index computation helpers.
      size_type
      _M_bucket_index(__node_type* __n) const
      { return __hash_code_base::_M_bucket_index(__n, _M_bucket_count); }

      size_type
      _M_bucket_index(const key_type& __k, __hash_code __c) const
      { return __hash_code_base::_M_bucket_index(__k, __c, _M_bucket_count); }

      // Find and insert helper functions and types
      // Find the node before the one matching the criteria.
      __node_base*
      _M_find_before_node(size_type, const key_type&, __hash_code) const;

      __node_type*
      _M_find_node(size_type __bkt, const key_type& __key,
		   __hash_code __c) const
      {
	__node_base* __before_n = _M_find_before_node(__bkt, __key, __c);
	if (__before_n)
	  return static_cast<__node_type*>(__before_n->_M_nxt);
	return nullptr;
      }

      // Insert a node at the beginning of a bucket.
      void
      _M_insert_bucket_begin(size_type, __node_type*);

      // Remove the bucket first node
      void
      _M_remove_bucket_begin(size_type __bkt, __node_type* __next_n,
			     size_type __next_bkt);

      // Get the node before __n in the bucket __bkt
      __node_base*
      _M_get_previous_node(size_type __bkt, __node_base* __n);

      template<typename _Arg>
	iterator
	_M_insert_bucket(_Arg&&, size_type, __hash_code);


      template<typename... _Args>
	std::pair<iterator, bool>
	_M_emplace(std::true_type, _Args&&... __args);

      template<typename... _Args>
	iterator
	_M_emplace(std::false_type, _Args&&... __args);

      template<typename _Arg>
	std::pair<iterator, bool>
	_M_insert(_Arg&&, std::true_type);

      template<typename _Arg>
	iterator
	_M_insert(_Arg&&, std::false_type);

    public:
      // Emplace
      template<typename... _Args>
	__ireturn_type
	emplace(_Args&&... __args)
	{ return _M_emplace(__unique_keys(), std::forward<_Args>(__args)...); }

      template<typename... _Args>
	iterator
	emplace_hint(const_iterator, _Args&&... __args)
	{ return __iconv_type()(emplace(std::forward<_Args>(__args)...)); }

      // Insert member functions via inheritance.

      // Erase
      iterator
      erase(const_iterator);

      // LWG 2059.
      iterator
      erase(iterator __it)
      { return erase(const_iterator(__it)); }

      size_type
      erase(const key_type&);

      iterator
      erase(const_iterator, const_iterator);

      void
      clear() noexcept;

      // Set number of buckets to be appropriate for container of n element.
      void rehash(size_type __n);

      // DR 1189.
      // reserve, if present, comes from _Rehash_base.

    private:
      // Helper rehash method used when keys are unique.
      void _M_rehash_aux(size_type __n, std::true_type);

      // Helper rehash method used when keys can be non-unique.
      void _M_rehash_aux(size_type __n, std::false_type);

      // Unconditionally change size of bucket array to n, restore
      // hash policy state to __state on exception.
      void _M_rehash(size_type __n, const __rehash_state& __state);
    };


  // Definitions of class template _Hashtable's out-of-line member functions.
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename... _Args>
      typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy, _Traits>::__node_type*
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _M_allocate_node(_Args&&... __args)
      {
	__node_type* __n = _M_node_allocator.allocate(1);
	__try
	  {
	    _M_node_allocator.construct(__n, std::forward<_Args>(__args)...);
	    return __n;
	  }
	__catch(...)
	  {
	    _M_node_allocator.deallocate(__n, 1);
	    __throw_exception_again;
	  }
      }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_deallocate_node(__node_type* __n)
    {
      _M_node_allocator.destroy(__n);
      _M_node_allocator.deallocate(__n, 1);
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_deallocate_nodes(__node_type* __n)
    {
      while (__n)
	{
	  __node_type* __tmp = __n;
	  __n = __n->_M_next();
	  _M_deallocate_node(__tmp);
	}
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy, _Traits>::__bucket_type*
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_allocate_buckets(size_type __n)
    {
      _Bucket_allocator_type __alloc(_M_node_allocator);

      __bucket_type* __p = __alloc.allocate(__n);
      __builtin_memset(__p, 0, __n * sizeof(__bucket_type));
      return __p;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_deallocate_buckets(__bucket_type* __p, size_type __n)
    {
      _Bucket_allocator_type __alloc(_M_node_allocator);
      __alloc.deallocate(__p, __n);
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			_Traits>::__node_type*
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_bucket_begin(size_type __bkt) const
    {
      __node_base* __n = _M_buckets[__bkt];
      return __n ? static_cast<__node_type*>(__n->_M_nxt) : nullptr;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _Hashtable(size_type __bucket_hint,
	       const _H1& __h1, const _H2& __h2, const _Hash& __h,
	       const _Equal& __eq, const _ExtractKey& __exk,
	       const allocator_type& __a)
    : __hashtable_base(__exk, __h1, __h2, __h, __eq),
      __map_base(),
      __rehash_base(),
      _M_node_allocator(__a),
      _M_bucket_count(0),
      _M_element_count(0),
      _M_rehash_policy()
    {
      _M_bucket_count = _M_rehash_policy._M_next_bkt(__bucket_hint);

      // We don't want the rehash policy to ask for the hashtable to
      // shrink on the first insertion so we need to reset its
      // previous resize level.
      _M_rehash_policy._M_prev_resize = 0;
      _M_buckets = _M_allocate_buckets(_M_bucket_count);
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename _InputIterator>
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _Hashtable(_InputIterator __f, _InputIterator __l,
		 size_type __bucket_hint,
		 const _H1& __h1, const _H2& __h2, const _Hash& __h,
		 const _Equal& __eq, const _ExtractKey& __exk,
		 const allocator_type& __a)
      : __hashtable_base(__exk, __h1, __h2, __h, __eq),
	__map_base(),
	__rehash_base(),
	_M_node_allocator(__a),
	_M_bucket_count(0),
	_M_element_count(0),
	_M_rehash_policy()
      {
	_M_bucket_count =
	  _M_rehash_policy._M_bkt_for_elements(__detail::__distance_fw(__f,
								       __l));
	if (_M_bucket_count <= __bucket_hint)
	  _M_bucket_count = _M_rehash_policy._M_next_bkt(__bucket_hint);

	// We don't want the rehash policy to ask for the hashtable to
	// shrink on the first insertion so we need to reset its
	// previous resize level.
	_M_rehash_policy._M_prev_resize = 0;
	_M_buckets = _M_allocate_buckets(_M_bucket_count);
	__try
	  {
	    for (; __f != __l; ++__f)
	      this->insert(*__f);
	  }
	__catch(...)
	  {
	    clear();
	    _M_deallocate_buckets(_M_buckets, _M_bucket_count);
	    __throw_exception_again;
	  }
      }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _Hashtable(const _Hashtable& __ht)
    : __hashtable_base(__ht),
      __map_base(__ht),
      __rehash_base(__ht),
      _M_node_allocator(__ht._M_node_allocator),
      _M_bucket_count(__ht._M_bucket_count),
      _M_element_count(__ht._M_element_count),
      _M_rehash_policy(__ht._M_rehash_policy)
    {
      _M_buckets = _M_allocate_buckets(_M_bucket_count);
      __try
	{
	  if (!__ht._M_before_begin._M_nxt)
	    return;

	  // First deal with the special first node pointed to by
	  // _M_before_begin.
	  const __node_type* __ht_n = __ht._M_begin();
	  __node_type* __this_n = _M_allocate_node(__ht_n->_M_v);
	  this->_M_copy_code(__this_n, __ht_n);
	  _M_before_begin._M_nxt = __this_n;
	  _M_buckets[_M_bucket_index(__this_n)] = &_M_before_begin;

	  // Then deal with other nodes.
	  __node_base* __prev_n = __this_n;
	  for (__ht_n = __ht_n->_M_next(); __ht_n; __ht_n = __ht_n->_M_next())
	    {
	      __this_n = _M_allocate_node(__ht_n->_M_v);
	      __prev_n->_M_nxt = __this_n;
	      this->_M_copy_code(__this_n, __ht_n);
	      size_type __bkt = _M_bucket_index(__this_n);
	      if (!_M_buckets[__bkt])
		_M_buckets[__bkt] = __prev_n;
	      __prev_n = __this_n;
	    }
	}
      __catch(...)
	{
	  clear();
	  _M_deallocate_buckets(_M_buckets, _M_bucket_count);
	  __throw_exception_again;
	}
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _Hashtable(_Hashtable&& __ht)
    : __hashtable_base(__ht),
      __map_base(__ht),
      __rehash_base(__ht),
      _M_node_allocator(std::move(__ht._M_node_allocator)),
      _M_buckets(__ht._M_buckets),
      _M_bucket_count(__ht._M_bucket_count),
      _M_before_begin(__ht._M_before_begin._M_nxt),
      _M_element_count(__ht._M_element_count),
      _M_rehash_policy(__ht._M_rehash_policy)
    {
      // Update, if necessary, bucket pointing to before begin that hasn't move.
      if (_M_begin())
	_M_buckets[_M_bucket_index(_M_begin())] = &_M_before_begin;
      __ht._M_rehash_policy = _RehashPolicy();
      __ht._M_bucket_count = __ht._M_rehash_policy._M_next_bkt(0);
      __ht._M_buckets = __ht._M_allocate_buckets(__ht._M_bucket_count);
      __ht._M_before_begin._M_nxt = nullptr;
      __ht._M_element_count = 0;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    ~_Hashtable() noexcept
    {
      clear();
      _M_deallocate_buckets(_M_buckets, _M_bucket_count);
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    swap(_Hashtable& __x)
    {
      // The only base class with member variables is hash_code_base.
      // We define _Hash_code_base::_M_swap because different
      // specializations have different members.
      this->_M_swap(__x);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 431. Swapping containers with unequal allocators.
      std::__alloc_swap<_Node_allocator_type>::_S_do_it(_M_node_allocator,
							__x._M_node_allocator);

      std::swap(_M_rehash_policy, __x._M_rehash_policy);
      std::swap(_M_buckets, __x._M_buckets);
      std::swap(_M_bucket_count, __x._M_bucket_count);
      std::swap(_M_before_begin._M_nxt, __x._M_before_begin._M_nxt);
      std::swap(_M_element_count, __x._M_element_count);

      // Fix buckets containing the _M_before_begin pointers that
      // can't be swapped.
      if (_M_begin())
	_M_buckets[_M_bucket_index(_M_begin())] = &_M_before_begin;
      if (__x._M_begin())
	__x._M_buckets[__x._M_bucket_index(__x._M_begin())]
	  = &(__x._M_before_begin);
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    __rehash_policy(const _RehashPolicy& __pol)
    {
      size_type __n_bkt = __pol._M_bkt_for_elements(_M_element_count);
      if (__n_bkt != _M_bucket_count)
	_M_rehash(__n_bkt, _M_rehash_policy._M_state());
      _M_rehash_policy = __pol;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			_Traits>::iterator
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    find(const key_type& __k)
    {
      __hash_code __code = this->_M_hash_code(__k);
      std::size_t __n = _M_bucket_index(__k, __code);
      __node_type* __p = _M_find_node(__n, __k, __code);
      return __p ? iterator(__p) : this->end();
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			_Traits>::const_iterator
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    find(const key_type& __k) const
    {
      __hash_code __code = this->_M_hash_code(__k);
      std::size_t __n = _M_bucket_index(__k, __code);
      __node_type* __p = _M_find_node(__n, __k, __code);
      return __p ? const_iterator(__p) : this->end();
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			_Traits>::size_type
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    count(const key_type& __k) const
    {
      __hash_code __code = this->_M_hash_code(__k);
      std::size_t __n = _M_bucket_index(__k, __code);
      __node_type* __p = _M_bucket_begin(__n);
      if (!__p)
	return 0;

      std::size_t __result = 0;
      for (;; __p = __p->_M_next())
	{
	  if (this->_M_equals(__k, __code, __p))
	    ++__result;
	  else if (__result)
	    // All equivalent values are next to each other, if we
	    // found a not equivalent value after an equivalent one it
	    // means that we won't find anymore an equivalent value.
	    break;
	  if (!__p->_M_nxt || _M_bucket_index(__p->_M_next()) != __n)
	    break;
	}
      return __result;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    std::pair<typename _Hashtable<_Key, _Value, _Alloc,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  _Traits>::iterator,
	      typename _Hashtable<_Key, _Value, _Alloc,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  _Traits>::iterator>
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    equal_range(const key_type& __k)
    {
      __hash_code __code = this->_M_hash_code(__k);
      std::size_t __n = _M_bucket_index(__k, __code);
      __node_type* __p = _M_find_node(__n, __k, __code);

      if (__p)
	{
	  __node_type* __p1 = __p->_M_next();
	  while (__p1 && _M_bucket_index(__p1) == __n
		 && this->_M_equals(__k, __code, __p1))
	    __p1 = __p1->_M_next();

	  return std::make_pair(iterator(__p), iterator(__p1));
	}
      else
	return std::make_pair(this->end(), this->end());
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    std::pair<typename _Hashtable<_Key, _Value, _Alloc,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  _Traits>::const_iterator,
	      typename _Hashtable<_Key, _Value, _Alloc,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  _Traits>::const_iterator>
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    equal_range(const key_type& __k) const
    {
      __hash_code __code = this->_M_hash_code(__k);
      std::size_t __n = _M_bucket_index(__k, __code);
      __node_type* __p = _M_find_node(__n, __k, __code);

      if (__p)
	{
	  __node_type* __p1 = __p->_M_next();
	  while (__p1 && _M_bucket_index(__p1) == __n
		 && this->_M_equals(__k, __code, __p1))
	    __p1 = __p1->_M_next();

	  return std::make_pair(const_iterator(__p), const_iterator(__p1));
	}
      else
	return std::make_pair(this->end(), this->end());
    }

  // Find the node whose key compares equal to k in the bucket n.
  // Return nullptr if no node is found.
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			_Traits>::__node_base*
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_find_before_node(size_type __n, const key_type& __k,
			__hash_code __code) const
    {
      __node_base* __prev_p = _M_buckets[__n];
      if (!__prev_p)
	return nullptr;
      __node_type* __p = static_cast<__node_type*>(__prev_p->_M_nxt);
      for (;; __p = __p->_M_next())
	{
	  if (this->_M_equals(__k, __code, __p))
	    return __prev_p;
	  if (!(__p->_M_nxt) || _M_bucket_index(__p->_M_next()) != __n)
	    break;
	  __prev_p = __p;
	}
      return nullptr;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_insert_bucket_begin(size_type __bkt, __node_type* __node)
    {
      if (_M_buckets[__bkt])
	{
	  // Bucket is not empty, we just need to insert the new node
	  // after the bucket before begin.
	  __node->_M_nxt = _M_buckets[__bkt]->_M_nxt;
	  _M_buckets[__bkt]->_M_nxt = __node;
	}
      else
	{
	  // The bucket is empty, the new node is inserted at the
	  // beginning of the singly linked list and the bucket will
	  // contain _M_before_begin pointer.
	  __node->_M_nxt = _M_before_begin._M_nxt;
	  _M_before_begin._M_nxt = __node;
	  if (__node->_M_nxt)
	    // We must update former begin bucket that is pointing to
	    // _M_before_begin.
	    _M_buckets[_M_bucket_index(__node->_M_next())] = __node;
	  _M_buckets[__bkt] = &_M_before_begin;
	}
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_remove_bucket_begin(size_type __bkt, __node_type* __next,
			   size_type __next_bkt)
    {
      if (!__next || __next_bkt != __bkt)
	{
	  // Bucket is now empty
	  // First update next bucket if any
	  if (__next)
	    _M_buckets[__next_bkt] = _M_buckets[__bkt];

	  // Second update before begin node if necessary
	  if (&_M_before_begin == _M_buckets[__bkt])
	    _M_before_begin._M_nxt = __next;
	  _M_buckets[__bkt] = nullptr;
	}
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			_Traits>::__node_base*
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_get_previous_node(size_type __bkt, __node_base* __n)
    {
      __node_base* __prev_n = _M_buckets[__bkt];
      while (__prev_n->_M_nxt != __n)
	__prev_n = __prev_n->_M_nxt;
      return __prev_n;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename... _Args>
      std::pair<typename _Hashtable<_Key, _Value, _Alloc,
				    _ExtractKey, _Equal, _H1,
				    _H2, _Hash, _RehashPolicy,
				    _Traits>::iterator, bool>
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _M_emplace(std::true_type, _Args&&... __args)
      {
	// First build the node to get access to the hash code
	__node_type* __node = _M_allocate_node(std::forward<_Args>(__args)...);
	__try
	  {
	    const key_type& __k = this->_M_extract()(__node->_M_v);
	    __hash_code __code = this->_M_hash_code(__k);
	    size_type __bkt = _M_bucket_index(__k, __code);

	    if (__node_type* __p = _M_find_node(__bkt, __k, __code))
	      {
		// There is already an equivalent node, no insertion
		_M_deallocate_node(__node);
		return std::make_pair(iterator(__p), false);
	      }

	    // We are going to insert this node
	    this->_M_store_code(__node, __code);
	    const __rehash_state& __saved_state
	      = _M_rehash_policy._M_state();
	    std::pair<bool, std::size_t> __do_rehash
	      = _M_rehash_policy._M_need_rehash(_M_bucket_count,
						_M_element_count, 1);

	    if (__do_rehash.first)
	      {
		_M_rehash(__do_rehash.second, __saved_state);
		__bkt = _M_bucket_index(__k, __code);
	      }

	    _M_insert_bucket_begin(__bkt, __node);
	    ++_M_element_count;
	    return std::make_pair(iterator(__node), true);
	  }
	__catch(...)
	  {
	    _M_deallocate_node(__node);
	    __throw_exception_again;
	  }
      }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename... _Args>
      typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  _Traits>::iterator
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _M_emplace(std::false_type, _Args&&... __args)
      {
	const __rehash_state& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, 1);

	// First build the node to get its hash code.
	__node_type* __node = _M_allocate_node(std::forward<_Args>(__args)...);
	__try
	  {
	    const key_type& __k = this->_M_extract()(__node->_M_v);
	    __hash_code __code = this->_M_hash_code(__k);
	    this->_M_store_code(__node, __code);

	    // Second, do rehash if necessary.
	    if (__do_rehash.first)
		_M_rehash(__do_rehash.second, __saved_state);

	    // Third, find the node before an equivalent one.
	    size_type __bkt = _M_bucket_index(__k, __code);
	    __node_base* __prev = _M_find_before_node(__bkt, __k, __code);

	    if (__prev)
	      {
		// Insert after the node before the equivalent one.
		__node->_M_nxt = __prev->_M_nxt;
		__prev->_M_nxt = __node;
	      }
	    else
	      // The inserted node has no equivalent in the
	      // hashtable. We must insert the new node at the
	      // beginning of the bucket to preserve equivalent
	      // elements relative positions.
	      _M_insert_bucket_begin(__bkt, __node);
	    ++_M_element_count;
	    return iterator(__node);
	  }
	__catch(...)
	  {
	    _M_deallocate_node(__node);
	    __throw_exception_again;
	  }
      }

  // Insert v in bucket n (assumes no element with its key already present).
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename _Arg>
      typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  _Traits>::iterator
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _M_insert_bucket(_Arg&& __v, size_type __n, __hash_code __code)
      {
	const __rehash_state& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, 1);

	if (__do_rehash.first)
	  {
	    const key_type& __k = this->_M_extract()(__v);
	    __n = __hash_code_base::_M_bucket_index(__k, __code,
						    __do_rehash.second);
	  }

	__node_type* __node = nullptr;
	__try
	  {
	    // Allocate the new node before doing the rehash so that we
	    // don't do a rehash if the allocation throws.
	    __node = _M_allocate_node(std::forward<_Arg>(__v));
	    this->_M_store_code(__node, __code);
	    if (__do_rehash.first)
	      _M_rehash(__do_rehash.second, __saved_state);

	    _M_insert_bucket_begin(__n, __node);
	    ++_M_element_count;
	    return iterator(__node);
	  }
	__catch(...)
	  {
	    if (!__node)
	      _M_rehash_policy._M_reset(__saved_state);
	    else
	      _M_deallocate_node(__node);
	    __throw_exception_again;
	  }
      }

  // Insert v if no element with its key is already present.
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename _Arg>
      std::pair<typename _Hashtable<_Key, _Value, _Alloc,
				    _ExtractKey, _Equal, _H1,
				    _H2, _Hash, _RehashPolicy,
				    _Traits>::iterator, bool>
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _M_insert(_Arg&& __v, std::true_type)
      {
	const key_type& __k = this->_M_extract()(__v);
	__hash_code __code = this->_M_hash_code(__k);
	size_type __n = _M_bucket_index(__k, __code);

	if (__node_type* __p = _M_find_node(__n, __k, __code))
	  return std::make_pair(iterator(__p), false);
	return std::make_pair(_M_insert_bucket(std::forward<_Arg>(__v),
			      __n, __code), true);
      }

  // Insert v unconditionally.
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    template<typename _Arg>
      typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  _Traits>::iterator
      _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, _Traits>::
      _M_insert(_Arg&& __v, std::false_type)
      {
	const __rehash_state& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, 1);

	// First compute the hash code so that we don't do anything if
	// it throws.
	__hash_code __code = this->_M_hash_code(this->_M_extract()(__v));

	__node_type* __node = nullptr;
	__try
	  {
	    // Second allocate new node so that we don't rehash if it throws.
	    __node = _M_allocate_node(std::forward<_Arg>(__v));
	    this->_M_store_code(__node, __code);
	    if (__do_rehash.first)
		_M_rehash(__do_rehash.second, __saved_state);

	    // Third, find the node before an equivalent one.
	    size_type __bkt = _M_bucket_index(__node);
	    __node_base* __prev
	      = _M_find_before_node(__bkt, this->_M_extract()(__node->_M_v),
				    __code);
	    if (__prev)
	      {
		// Insert after the node before the equivalent one.
		__node->_M_nxt = __prev->_M_nxt;
		__prev->_M_nxt = __node;
	      }
	    else
	      // The inserted node has no equivalent in the
	      // hashtable. We must insert the new node at the
	      // beginning of the bucket to preserve equivalent
	      // elements relative positions.
	      _M_insert_bucket_begin(__bkt, __node);
	    ++_M_element_count;
	    return iterator(__node);
	  }
	__catch(...)
	  {
	    if (!__node)
	      _M_rehash_policy._M_reset(__saved_state);
	    else
	      _M_deallocate_node(__node);
	    __throw_exception_again;
	  }
      }


  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			_Traits>::iterator
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    erase(const_iterator __it)
    {
      __node_type* __n = __it._M_cur;
      std::size_t __bkt = _M_bucket_index(__n);

      // Look for previous node to unlink it from the erased one, this
      // is why we need buckets to contain the before begin to make
      // this research fast.
      __node_base* __prev_n = _M_get_previous_node(__bkt, __n);
      if (__n == _M_bucket_begin(__bkt))
	_M_remove_bucket_begin(__bkt, __n->_M_next(),
	   __n->_M_nxt ? _M_bucket_index(__n->_M_next()) : 0);
      else if (__n->_M_nxt)
	{
	  size_type __next_bkt = _M_bucket_index(__n->_M_next());
	  if (__next_bkt != __bkt)
	    _M_buckets[__next_bkt] = __prev_n;
	}

      __prev_n->_M_nxt = __n->_M_nxt;
      iterator __result(__n->_M_next());
      _M_deallocate_node(__n);
      --_M_element_count;

      return __result;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			_Traits>::size_type
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    erase(const key_type& __k)
    {
      __hash_code __code = this->_M_hash_code(__k);
      std::size_t __bkt = _M_bucket_index(__k, __code);

      // Look for the node before the first matching node.
      __node_base* __prev_n = _M_find_before_node(__bkt, __k, __code);
      if (!__prev_n)
	return 0;
      __node_type* __n = static_cast<__node_type*>(__prev_n->_M_nxt);
      bool __is_bucket_begin = _M_buckets[__bkt] == __prev_n;

      // We found a matching node, start deallocation loop from it
      std::size_t __next_bkt = __bkt;
      __node_type* __next_n = __n;
      size_type __result = 0;
      __node_type* __saved_n = nullptr;
      do
	{
	  __node_type* __p = __next_n;
	  __next_n = __p->_M_next();

	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 526. Is it undefined if a function in the standard changes
	  // in parameters?
	  if (std::__addressof(this->_M_extract()(__p->_M_v))
	      != std::__addressof(__k))
	    _M_deallocate_node(__p);
	  else
	    __saved_n = __p;
	  --_M_element_count;
	  ++__result;
	  if (!__next_n)
	    break;
	  __next_bkt = _M_bucket_index(__next_n);
	}
      while (__next_bkt == __bkt && this->_M_equals(__k, __code, __next_n));

      if (__saved_n)
	_M_deallocate_node(__saved_n);
      if (__is_bucket_begin)
	_M_remove_bucket_begin(__bkt, __next_n, __next_bkt);
      else if (__next_n && __next_bkt != __bkt)
	_M_buckets[__next_bkt] = __prev_n;
      if (__prev_n)
	__prev_n->_M_nxt = __next_n;
      return __result;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    typename _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			_Traits>::iterator
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    erase(const_iterator __first, const_iterator __last)
    {
      __node_type* __n = __first._M_cur;
      __node_type* __last_n = __last._M_cur;
      if (__n == __last_n)
	return iterator(__n);

      std::size_t __bkt = _M_bucket_index(__n);

      __node_base* __prev_n = _M_get_previous_node(__bkt, __n);
      bool __is_bucket_begin = __n == _M_bucket_begin(__bkt);
      std::size_t __n_bkt = __bkt;
      for (;;)
	{
	  do
	    {
	      __node_type* __tmp = __n;
	      __n = __n->_M_next();
	      _M_deallocate_node(__tmp);
	      --_M_element_count;
	      if (!__n)
		break;
	      __n_bkt = _M_bucket_index(__n);
	    }
	  while (__n != __last_n && __n_bkt == __bkt);
	  if (__is_bucket_begin)
	    _M_remove_bucket_begin(__bkt, __n, __n_bkt);
	  if (__n == __last_n)
	    break;
	  __is_bucket_begin = true;
	  __bkt = __n_bkt;
	}

      if (__n && (__n_bkt != __bkt || __is_bucket_begin))
	_M_buckets[__n_bkt] = __prev_n;
      __prev_n->_M_nxt = __n;
      return iterator(__n);
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    clear() noexcept
    {
      _M_deallocate_nodes(_M_begin());
      __builtin_memset(_M_buckets, 0, _M_bucket_count * sizeof(__bucket_type));
      _M_element_count = 0;
      _M_before_begin._M_nxt = nullptr;
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    rehash(size_type __n)
    {
      const __rehash_state& __saved_state = _M_rehash_policy._M_state();
      std::size_t __buckets
	= _M_rehash_policy._M_bkt_for_elements(_M_element_count + 1);
      if (__buckets <= __n)
	__buckets = _M_rehash_policy._M_next_bkt(__n);

      if (__buckets != _M_bucket_count)
	{
	  _M_rehash(__buckets, __saved_state);

	  // We don't want the rehash policy to ask for the hashtable to shrink
	  // on the next insertion so we need to reset its previous resize
	  // level.
	  _M_rehash_policy._M_prev_resize = 0;
	}
    }

  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_rehash(size_type __n, const __rehash_state& __state)
    {
      __try
	{
	  _M_rehash_aux(__n, __unique_keys());
	}
      __catch(...)
	{
	  // A failure here means that buckets allocation failed.  We only
	  // have to restore hash policy previous state.
	  _M_rehash_policy._M_reset(__state);
	  __throw_exception_again;
	}
    }

  // Rehash when there is no equivalent elements.
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_rehash_aux(size_type __n, std::true_type)
    {
      __bucket_type* __new_buckets = _M_allocate_buckets(__n);
      __node_type* __p = _M_begin();
      _M_before_begin._M_nxt = nullptr;
      std::size_t __bbegin_bkt;
      while (__p)
	{
	  __node_type* __next = __p->_M_next();
	  std::size_t __bkt = __hash_code_base::_M_bucket_index(__p, __n);
	  if (!__new_buckets[__bkt])
	    {
	      __p->_M_nxt = _M_before_begin._M_nxt;
	      _M_before_begin._M_nxt = __p;
	      __new_buckets[__bkt] = &_M_before_begin;
	      if (__p->_M_nxt)
		__new_buckets[__bbegin_bkt] = __p;
	      __bbegin_bkt = __bkt;
	    }
	  else
	    {
	      __p->_M_nxt = __new_buckets[__bkt]->_M_nxt;
	      __new_buckets[__bkt]->_M_nxt = __p;
	    }
	  __p = __next;
	}
      _M_deallocate_buckets(_M_buckets, _M_bucket_count);
      _M_bucket_count = __n;
      _M_buckets = __new_buckets;
    }

  // Rehash when there can be equivalent elements, preserve their relative
  // order.
  template<typename _Key, typename _Value,
	   typename _Alloc, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   typename _Traits>
    void
    _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, _Traits>::
    _M_rehash_aux(size_type __n, std::false_type)
    {
      __bucket_type* __new_buckets = _M_allocate_buckets(__n);

      __node_type* __p = _M_begin();
      _M_before_begin._M_nxt = nullptr;
      std::size_t __bbegin_bkt;
      std::size_t __prev_bkt;
      __node_type* __prev_p = nullptr;
      bool __check_bucket = false;

      while (__p)
	{
	  __node_type* __next = __p->_M_next();
	  std::size_t __bkt = __hash_code_base::_M_bucket_index(__p, __n);

	  if (__prev_p && __prev_bkt == __bkt)
	    {
	      // Previous insert was already in this bucket, we insert after
	      // the previously inserted one to preserve equivalent elements
	      // relative order.
	      __p->_M_nxt = __prev_p->_M_nxt;
	      __prev_p->_M_nxt = __p;

	      // Inserting after a node in a bucket require to check that we
	      // haven't change the bucket last node, in this case next
	      // bucket containing its before begin node must be updated. We
	      // schedule a check as soon as we move out of the sequence of
	      // equivalent nodes to limit the number of checks.
	      __check_bucket = true;
	    }
	  else
	    {
	      if (__check_bucket)
		{
		  // Check if we shall update the next bucket because of insertions
		  // into __prev_bkt bucket.
		  if (__prev_p->_M_nxt)
		    {
		      std::size_t __next_bkt
			= __hash_code_base::_M_bucket_index(__prev_p->_M_next(),
							    __n);
		      if (__next_bkt != __prev_bkt)
			__new_buckets[__next_bkt] = __prev_p;
		    }
		  __check_bucket = false;
		}

	      if (!__new_buckets[__bkt])
		{
		  __p->_M_nxt = _M_before_begin._M_nxt;
		  _M_before_begin._M_nxt = __p;
		  __new_buckets[__bkt] = &_M_before_begin;
		  if (__p->_M_nxt)
		    __new_buckets[__bbegin_bkt] = __p;
		  __bbegin_bkt = __bkt;
		}
	      else
		{
		  __p->_M_nxt = __new_buckets[__bkt]->_M_nxt;
		  __new_buckets[__bkt]->_M_nxt = __p;
		}
	    }
	  __prev_p = __p;
	  __prev_bkt = __bkt;
	  __p = __next;
	}

      if (__check_bucket && __prev_p->_M_nxt)
	{
	  std::size_t __next_bkt
	    = __hash_code_base::_M_bucket_index(__prev_p->_M_next(), __n);
	  if (__next_bkt != __prev_bkt)
	    __new_buckets[__next_bkt] = __prev_p;
	}

      _M_deallocate_buckets(_M_buckets, _M_bucket_count);
      _M_bucket_count = __n;
      _M_buckets = __new_buckets;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _HASHTABLE_H
