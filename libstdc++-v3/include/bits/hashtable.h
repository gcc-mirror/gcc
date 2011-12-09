// hashtable.h header -*- C++ -*-

// Copyright (C) 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

  // Class template _Hashtable, class definition.

  // Meaning of class template _Hashtable's template parameters

  // _Key and _Value: arbitrary CopyConstructible types.

  // _Allocator: an allocator type ([lib.allocator.requirements]) whose
  // value type is Value.  As a conforming extension, we allow for
  // value type != Value.

  // _ExtractKey: function object that takes an object of type Value
  // and returns a value of type _Key.

  // _Equal: function object that takes two objects of type k and returns
  // a bool-like value that is true if the two objects are considered equal.

  // _H1: the hash function.  A unary function object with argument type
  // Key and result type size_t.  Return values should be distributed
  // over the entire range [0, numeric_limits<size_t>:::max()].

  // _H2: the range-hashing function (in the terminology of Tavori and
  // Dreizin).  A binary function object whose argument types and result
  // type are all size_t.  Given arguments r and N, the return value is
  // in the range [0, N).

  // _Hash: the ranged hash function (Tavori and Dreizin). A binary function
  // whose argument types are _Key and size_t and whose result type is
  // size_t.  Given arguments k and N, the return value is in the range
  // [0, N).  Default: hash(k, N) = h2(h1(k), N).  If _Hash is anything other
  // than the default, _H1 and _H2 are ignored.

  // _RehashPolicy: Policy class with three members, all of which govern
  // the bucket count. _M_next_bkt(n) returns a bucket count no smaller
  // than n.  _M_bkt_for_elements(n) returns a bucket count appropriate
  // for an element count of n.  _M_need_rehash(n_bkt, n_elt, n_ins)
  // determines whether, if the current bucket count is n_bkt and the
  // current element count is n_elt, we need to increase the bucket
  // count.  If so, returns make_pair(true, n), where n is the new
  // bucket count.  If not, returns make_pair(false, <anything>).

  // __cache_hash_code: bool.  true if we store the value of the hash
  // function along with the value.  This is a time-space tradeoff.
  // Storing it may improve lookup speed by reducing the number of times
  // we need to call the Equal function.

  // __constant_iterators: bool.  true if iterator and const_iterator are
  // both constant iterator types.  This is true for unordered_set and
  // unordered_multiset, false for unordered_map and unordered_multimap.

  // __unique_keys: bool.  true if the return value of _Hashtable::count(k)
  // is always at most one, false if it may be an arbitrary number.  This
  // true for unordered_set and unordered_map, false for unordered_multiset
  // and unordered_multimap.
  /**
   * Here's _Hashtable data structure, each _Hashtable has:
   * - _Bucket[]     _M_buckets
   * - size_type     _M_bucket_count
   * - size_type     _M_begin_bucket_index
   * - size_type     _M_element_count
   *
   * with _Bucket being _Node* and _Node:
   * - _Node*        _M_next
   * - Tp            _M_value
   * - size_t        _M_code if cache_hash_code is true
   *
   * In terms of Standard containers the hastable is like the aggregation of:
   * - std::forward_list<_Node> containing the elements
   * - std::vector<std::forward_list<_Node>::iterator> representing the buckets
   *
   * The first non-empty bucket with index _M_begin_bucket_index contains the
   * first container node which is also the first bucket node whereas other
   * non-empty buckets contain the node before the first bucket node. This is so
   * to implement something like a std::forward_list::erase_after on container
   * erase calls.
   * 
   * Access to the bucket last element require a check on the hash code to see
   * if the node is still in the bucket. Such a design impose a quite efficient
   * hash functor and is one of the reasons it is highly advise to set
   * __cache_hash_code to true.
   *
   * The container iterators are simply built from nodes. This way incrementing
   * the iterator is perfectly efficient no matter how many empty buckets there
   * are in the container.
   *
   * On insert we compute element hash code and thanks to it find the bucket
   * index. If the element is the first one in the bucket we must find the
   * previous non-empty bucket where the previous node rely. To keep this loop
   * minimal it is important that the number of bucket is not too high compared
   * to the number of elements. So the hash policy must be carefully design so
   * that it computes a bucket count large enough to respect the user defined
   * load factor but also not too large to limit impact on the insert operation.
   *
   * On erase, the simple iterator design impose to use the hash functor to get
   * the index of the bucket to update. For this reason, when __cache_hash_code
   * is set to false, there is a static assertion that the hash functor cannot
   * throw.
   *
   * _M_begin_bucket_index is used to offer contant time access to the container
   * begin iterator.
   */

  template<typename _Key, typename _Value, typename _Allocator,
	   typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash,
	   typename _RehashPolicy,
	   bool __cache_hash_code,
	   bool __constant_iterators,
	   bool __unique_keys>
    class _Hashtable
    : public __detail::_Rehash_base<_RehashPolicy,
				    _Hashtable<_Key, _Value, _Allocator,
					       _ExtractKey,
					       _Equal, _H1, _H2, _Hash,
					       _RehashPolicy,
					       __cache_hash_code,
					       __constant_iterators,
					       __unique_keys> >,
      public __detail::_Hash_code_base<_Key, _Value, _ExtractKey, _Equal,
				       _H1, _H2, _Hash, __cache_hash_code>,
      public __detail::_Map_base<_Key, _Value, _ExtractKey, __unique_keys,
				 _Hashtable<_Key, _Value, _Allocator,
					    _ExtractKey,
					    _Equal, _H1, _H2, _Hash,
					    _RehashPolicy,
					    __cache_hash_code,
					    __constant_iterators,
					    __unique_keys> >,
      public __detail::_Equality_base<_ExtractKey, __unique_keys,
				      _Hashtable<_Key, _Value, _Allocator,
						 _ExtractKey,
						 _Equal, _H1, _H2, _Hash,
						 _RehashPolicy,
						 __cache_hash_code,
						 __constant_iterators,
						 __unique_keys> >
    {
      static_assert(__or_<integral_constant<bool, __cache_hash_code>,
			  __detail::__is_noexcept_hash<_Key, _H1>>::value,
      	    "Cache the hash code or qualify your hash functor with noexcept");
    public:
      typedef _Allocator                                  allocator_type;
      typedef _Value                                      value_type;
      typedef _Key                                        key_type;
      typedef _Equal                                      key_equal;
      // mapped_type, if present, comes from _Map_base.
      // hasher, if present, comes from _Hash_code_base.
      typedef typename _Allocator::pointer                pointer;
      typedef typename _Allocator::const_pointer          const_pointer;
      typedef typename _Allocator::reference              reference;
      typedef typename _Allocator::const_reference        const_reference;

      typedef std::size_t                                 size_type;
      typedef std::ptrdiff_t                              difference_type;
      typedef __detail::_Node_iterator<value_type, __constant_iterators,
				       __cache_hash_code>
							  local_iterator;
      typedef __detail::_Node_const_iterator<value_type,
					     __constant_iterators,
					     __cache_hash_code>
							  const_local_iterator;

      typedef local_iterator iterator;
      typedef const_local_iterator const_iterator;

      template<typename _Key2, typename _Value2, typename _Ex2, bool __unique2,
	       typename _Hashtable2>
	friend struct __detail::_Map_base;

    private:
      typedef typename _RehashPolicy::_State _RehashPolicyState;
      typedef __detail::_Hash_node<_Value, __cache_hash_code> _Node;
      typedef typename _Allocator::template rebind<_Node>::other
							_Node_allocator_type;
      typedef _Node* _Bucket;
      //typedef __detail::_Bucket<_Value, __cache_hash_code> _Bucket;
      typedef typename _Allocator::template rebind<_Bucket>::other
							_Bucket_allocator_type;

      typedef typename _Allocator::template rebind<_Value>::other
							_Value_allocator_type;

      _Node_allocator_type   _M_node_allocator;
      _Bucket*               _M_buckets;
      size_type              _M_bucket_count;
      size_type              _M_begin_bucket_index; // First non-empty bucket.
      size_type              _M_element_count;
      _RehashPolicy          _M_rehash_policy;

      template<typename... _Args>
	_Node*
	_M_allocate_node(_Args&&... __args);

      void
      _M_deallocate_node(_Node* __n);

      // Deallocate the linked list of nodes pointed to by __n
      void
      _M_deallocate_nodes(_Node* __n);

      _Bucket*
      _M_allocate_buckets(size_type __n);

      void
      _M_deallocate_buckets(_Bucket*, size_type __n);

      // Gets bucket begin dealing with the difference between first non-empty
      // bucket containing the first container node and the other non-empty
      // buckets containing the node before the one belonging to the bucket.
      _Node*
      _M_bucket_begin(size_type __bkt) const;

      // Gets the bucket last node if any
      _Node*
      _M_bucket_end(size_type __bkt) const;

      // Gets the bucket node after the last if any
      _Node*
      _M_bucket_past_the_end(size_type __bkt) const
        {
	  _Node* __end = _M_bucket_end(__bkt);
	  return __end ? __end->_M_next : nullptr;
	}

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

      ~_Hashtable() noexcept;

      void swap(_Hashtable&);

      // Basic container operations
      iterator
      begin() noexcept
      { return iterator(_M_buckets[_M_begin_bucket_index]); }

      const_iterator
      begin() const noexcept
      { return const_iterator(_M_buckets[_M_begin_bucket_index]); }

      iterator
      end() noexcept
      { return iterator(nullptr); }

      const_iterator
      end() const noexcept
      { return const_iterator(nullptr); }

      const_iterator
      cbegin() const noexcept
      { return const_iterator(_M_buckets[_M_begin_bucket_index]); }

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
      { return this->_M_eq; }

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
      {
	return this->_M_bucket_index(__k, this->_M_hash_code(__k),
				     bucket_count());
      }

      local_iterator
      begin(size_type __n)
      { return local_iterator(_M_bucket_begin(__n)); }

      local_iterator
      end(size_type __n)
      { return local_iterator(_M_bucket_past_the_end(__n)); }

      const_local_iterator
      begin(size_type __n) const
      { return const_local_iterator(_M_bucket_begin(__n)); }

      const_local_iterator
      end(size_type __n) const
      { return const_local_iterator(_M_bucket_past_the_end(__n)); }

      // DR 691.
      const_local_iterator
      cbegin(size_type __n) const
      { return const_local_iterator(_M_bucket_begin(__n)); }

      const_local_iterator
      cend(size_type __n) const
      { return const_local_iterator(_M_bucket_past_the_end(__n)); }

      float
      load_factor() const noexcept
      {
	return static_cast<float>(size()) / static_cast<float>(bucket_count());
      }

      // max_load_factor, if present, comes from _Rehash_base.

      // Generalization of max_load_factor.  Extension, not found in TR1.  Only
      // useful if _RehashPolicy is something other than the default.
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

    private:
      // Find and insert helper functions and types
      _Node*
      _M_find_node(size_type, const key_type&,
		   typename _Hashtable::_Hash_code_type) const;

      // Insert a node in an empty bucket
      void
      _M_insert_bucket_begin(size_type, _Node*);

      // Insert a node after an other one in a non-empty bucket
      void
      _M_insert_after(size_type, _Node*, _Node*);

      // Remove the bucket first node
      void
      _M_remove_bucket_begin(size_type __bkt, _Node* __next_n,
			     size_type __next_bkt);

      // Get the node before __n in the bucket __bkt
      _Node*
      _M_get_previous_node(size_type __bkt, _Node* __n);

      template<typename _Arg>
	iterator
	_M_insert_bucket(_Arg&&, size_type,
			 typename _Hashtable::_Hash_code_type);

      typedef typename std::conditional<__unique_keys,
					std::pair<iterator, bool>,
					iterator>::type
	_Insert_Return_Type;

      typedef typename std::conditional<__unique_keys,
					std::_Select1st<_Insert_Return_Type>,
					std::_Identity<_Insert_Return_Type>
				   >::type
	_Insert_Conv_Type;

    protected:
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
      // Emplace, insert and erase
      template<typename... _Args>
	_Insert_Return_Type
	emplace(_Args&&... __args)
	{ return _M_emplace(integral_constant<bool, __unique_keys>(),
			    std::forward<_Args>(__args)...); }

      template<typename... _Args>
	iterator
	emplace_hint(const_iterator, _Args&&... __args)
	{ return _Insert_Conv_Type()(emplace(std::forward<_Args>(__args)...)); }

      _Insert_Return_Type
      insert(const value_type& __v)
      { return _M_insert(__v, integral_constant<bool, __unique_keys>()); }

      iterator
      insert(const_iterator, const value_type& __v)
      { return _Insert_Conv_Type()(insert(__v)); }

      template<typename _Pair, typename = typename
	std::enable_if<__and_<integral_constant<bool, !__constant_iterators>,
			      std::is_convertible<_Pair,
						  value_type>>::value>::type>
	_Insert_Return_Type
	insert(_Pair&& __v)
	{ return _M_insert(std::forward<_Pair>(__v),
			   integral_constant<bool, __unique_keys>()); }

      template<typename _Pair, typename = typename
        std::enable_if<__and_<integral_constant<bool, !__constant_iterators>,
			      std::is_convertible<_Pair,
						  value_type>>::value>::type>
	iterator
	insert(const_iterator, _Pair&& __v)
	{ return _Insert_Conv_Type()(insert(std::forward<_Pair>(__v))); }

      template<typename _InputIterator>
	void
	insert(_InputIterator __first, _InputIterator __last);

      void
      insert(initializer_list<value_type> __l)
      { this->insert(__l.begin(), __l.end()); }

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
      // Unconditionally change size of bucket array to n, restore hash policy
      // state to __state on exception.
      void _M_rehash(size_type __n, const _RehashPolicyState& __state);
    };


  // Definitions of class template _Hashtable's out-of-line member functions.
  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename... _Args>
      typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  __chc, __cit, __uk>::_Node*
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _M_allocate_node(_Args&&... __args)
      {
	_Node* __n = _M_node_allocator.allocate(1);
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
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_deallocate_node(_Node* __n)
    {
      _M_node_allocator.destroy(__n);
      _M_node_allocator.deallocate(__n, 1);
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_deallocate_nodes(_Node* __n)
    {
      while (__n)
	{
	  _Node* __tmp = __n;
	  __n = __n->_M_next;
	  _M_deallocate_node(__tmp);
	}
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::_Bucket*
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_allocate_buckets(size_type __n)
    {
      _Bucket_allocator_type __alloc(_M_node_allocator);

      // We allocate one extra bucket to have _M_begin_bucket_index
      // point to it as long as container is empty
      _Bucket* __p = __alloc.allocate(__n + 1);
      __builtin_memset(__p, 0, (__n + 1) * sizeof(_Bucket));
      return __p;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_deallocate_buckets(_Bucket* __p, size_type __n)
    {
      _Bucket_allocator_type __alloc(_M_node_allocator);
      __alloc.deallocate(__p, __n + 1);
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::_Node*
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_bucket_begin(size_type __bkt) const
    {
      if (__bkt == _M_begin_bucket_index)
	return _M_buckets[__bkt];
      _Node* __n = _M_buckets[__bkt];
      return __n ? __n->_M_next : nullptr;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::_Node*
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_bucket_end(size_type __bkt) const
    {
      _Node* __n = _M_bucket_begin(__bkt);
      if (__n)
	for (;; __n = __n->_M_next)
	  if (!__n->_M_next 
	      || this->_M_bucket_index(__n->_M_next, _M_bucket_count)
		 != __bkt)
	    break;
      return __n;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _Hashtable(size_type __bucket_hint,
	       const _H1& __h1, const _H2& __h2, const _Hash& __h,
	       const _Equal& __eq, const _ExtractKey& __exk,
	       const allocator_type& __a)
    : __detail::_Rehash_base<_RehashPolicy, _Hashtable>(),
      __detail::_Hash_code_base<_Key, _Value, _ExtractKey, _Equal,
				_H1, _H2, _Hash, __chc>(__exk, __eq,
							__h1, __h2, __h),
      __detail::_Map_base<_Key, _Value, _ExtractKey, __uk, _Hashtable>(),
      _M_node_allocator(__a),
      _M_bucket_count(0),
      _M_element_count(0),
      _M_rehash_policy()
    {
      _M_bucket_count = _M_rehash_policy._M_next_bkt(__bucket_hint);
      // We don't want the rehash policy to ask for the hashtable to shrink
      // on the first insertion so we need to reset its previous resize level.
      _M_rehash_policy._M_prev_resize = 0;
      _M_buckets = _M_allocate_buckets(_M_bucket_count);
      _M_begin_bucket_index = _M_bucket_count;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename _InputIterator>
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _Hashtable(_InputIterator __f, _InputIterator __l,
		 size_type __bucket_hint,
		 const _H1& __h1, const _H2& __h2, const _Hash& __h,
		 const _Equal& __eq, const _ExtractKey& __exk,
		 const allocator_type& __a)
      : __detail::_Rehash_base<_RehashPolicy, _Hashtable>(),
	__detail::_Hash_code_base<_Key, _Value, _ExtractKey, _Equal,
				  _H1, _H2, _Hash, __chc>(__exk, __eq,
							  __h1, __h2, __h),
	__detail::_Map_base<_Key, _Value, _ExtractKey, __uk, _Hashtable>(),
	_M_node_allocator(__a),
	_M_bucket_count(0),
	_M_element_count(0),
	_M_rehash_policy()
      {
	_M_bucket_count = std::max(_M_rehash_policy._M_next_bkt(__bucket_hint),
				   _M_rehash_policy.
				   _M_bkt_for_elements(__detail::
						       __distance_fw(__f,
								     __l)));
        // We don't want the rehash policy to ask for the hashtable to shrink
        // on the first insertion so we need to reset its previous resize
	// level.
	_M_rehash_policy._M_prev_resize = 0;
	_M_buckets = _M_allocate_buckets(_M_bucket_count);
	_M_begin_bucket_index = _M_bucket_count;
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
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _Hashtable(const _Hashtable& __ht)
    : __detail::_Rehash_base<_RehashPolicy, _Hashtable>(__ht),
      __detail::_Hash_code_base<_Key, _Value, _ExtractKey, _Equal,
				_H1, _H2, _Hash, __chc>(__ht),
      __detail::_Map_base<_Key, _Value, _ExtractKey, __uk, _Hashtable>(__ht),
      _M_node_allocator(__ht._M_node_allocator),
      _M_bucket_count(__ht._M_bucket_count),
      _M_begin_bucket_index(__ht._M_begin_bucket_index),
      _M_element_count(__ht._M_element_count),
      _M_rehash_policy(__ht._M_rehash_policy)
    {
      _M_buckets = _M_allocate_buckets(_M_bucket_count);
      __try
	{
	  const _Node* __ht_n = __ht._M_buckets[__ht._M_begin_bucket_index];
	  if (!__ht_n)
	    return;

	  // Note that the copy constructor do not rely on hash code usage.
	  // First deal with the special first node that is directly store in
	  // the first non-empty bucket
	  _Node* __this_n = _M_allocate_node(__ht_n->_M_v);
	  this->_M_copy_code(__this_n, __ht_n);
	  _M_buckets[_M_begin_bucket_index] = __this_n;
	  __ht_n = __ht_n->_M_next;
	  // Second deal with following non-empty buckets containing previous
	  // nodes node.
	  for (size_type __i = __ht._M_begin_bucket_index + 1;
	       __i != __ht._M_bucket_count; ++__i)
	    {
	      if (!__ht._M_buckets[__i])
		continue;

	      for (; __ht_n != __ht._M_buckets[__i]->_M_next;
		   __ht_n = __ht_n->_M_next)
		{
		  __this_n->_M_next = _M_allocate_node(__ht_n->_M_v);
		  this->_M_copy_code(__this_n->_M_next, __ht_n);
		  __this_n = __this_n->_M_next;
		}

	      _M_buckets[__i] = __this_n;
	    }
	  // Last finalize copy of the nodes of the last non-empty bucket
	  for (; __ht_n; __ht_n = __ht_n->_M_next)
	    {
	      __this_n->_M_next = _M_allocate_node(__ht_n->_M_v);
	      this->_M_copy_code(__this_n->_M_next, __ht_n);
	      __this_n = __this_n->_M_next;
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
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _Hashtable(_Hashtable&& __ht)
    : __detail::_Rehash_base<_RehashPolicy, _Hashtable>(__ht),
      __detail::_Hash_code_base<_Key, _Value, _ExtractKey, _Equal,
				_H1, _H2, _Hash, __chc>(__ht),
      __detail::_Map_base<_Key, _Value, _ExtractKey, __uk, _Hashtable>(__ht),
      _M_node_allocator(std::move(__ht._M_node_allocator)),
      _M_buckets(__ht._M_buckets),
      _M_bucket_count(__ht._M_bucket_count),
      _M_begin_bucket_index(__ht._M_begin_bucket_index),
      _M_element_count(__ht._M_element_count),
      _M_rehash_policy(__ht._M_rehash_policy)
    {
      __ht._M_rehash_policy = _RehashPolicy();
      __ht._M_bucket_count = __ht._M_rehash_policy._M_next_bkt(0);
      __ht._M_buckets = __ht._M_allocate_buckets(__ht._M_bucket_count);
      __ht._M_begin_bucket_index = __ht._M_bucket_count;
      __ht._M_element_count = 0;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    ~_Hashtable() noexcept
    {
      clear();
      _M_deallocate_buckets(_M_buckets, _M_bucket_count);
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    swap(_Hashtable& __x)
    {
      // The only base class with member variables is hash_code_base.  We
      // define _Hash_code_base::_M_swap because different specializations
      // have different members.
      __detail::_Hash_code_base<_Key, _Value, _ExtractKey, _Equal,
	_H1, _H2, _Hash, __chc>::_M_swap(__x);

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 431. Swapping containers with unequal allocators.
      std::__alloc_swap<_Node_allocator_type>::_S_do_it(_M_node_allocator,
							__x._M_node_allocator);

      std::swap(_M_rehash_policy, __x._M_rehash_policy);
      std::swap(_M_buckets, __x._M_buckets);
      std::swap(_M_bucket_count, __x._M_bucket_count);
      std::swap(_M_begin_bucket_index, __x._M_begin_bucket_index);
      std::swap(_M_element_count, __x._M_element_count);
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    __rehash_policy(const _RehashPolicy& __pol)
    {
      size_type __n_bkt = __pol._M_bkt_for_elements(_M_element_count);
      if (__n_bkt != _M_bucket_count)
	_M_rehash(__n_bkt, _M_rehash_policy._M_state());
      _M_rehash_policy = __pol;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::iterator
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    find(const key_type& __k)
    {
      typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
      std::size_t __n = this->_M_bucket_index(__k, __code, _M_bucket_count);
      _Node* __p = _M_find_node(__n, __k, __code);
      return __p ? iterator(__p) : this->end();
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::const_iterator
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    find(const key_type& __k) const
    {
      typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
      std::size_t __n = this->_M_bucket_index(__k, __code, _M_bucket_count);
      _Node* __p = _M_find_node(__n, __k, __code);
      return __p ? const_iterator(__p) : this->end();
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::size_type
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    count(const key_type& __k) const
    {
      typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
      std::size_t __n = this->_M_bucket_index(__k, __code, _M_bucket_count);
      _Node* __p = _M_bucket_begin(__n);
      if (!__p)
	return 0;

      std::size_t __result = 0;
      for (;; __p = __p->_M_next)
	{
	  if (this->_M_compare(__k, __code, __p))
	    ++__result;
	  else if (__result)
	    // All equivalent values are next to each other, if we found a not
	    // equivalent value after an equivalent one it means that we won't
	    // find anymore an equivalent value.
	    break;
	  if (!__p->_M_next
	      || this->_M_bucket_index(__p->_M_next, _M_bucket_count)
		 != __n)
	    break;
	}
      return __result;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    std::pair<typename _Hashtable<_Key, _Value, _Allocator,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  __chc, __cit, __uk>::iterator,
	      typename _Hashtable<_Key, _Value, _Allocator,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  __chc, __cit, __uk>::iterator>
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    equal_range(const key_type& __k)
    {
      typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
      std::size_t __n = this->_M_bucket_index(__k, __code, _M_bucket_count);
      _Node* __p = _M_find_node(__n, __k, __code);

      if (__p)
	{
	  _Node* __p1 = __p->_M_next;
	  while (__p1
		 && this->_M_bucket_index(__p1, _M_bucket_count) == __n
		 && this->_M_compare(__k, __code, __p1))
	    __p1 = __p1->_M_next;

	  return std::make_pair(iterator(__p), iterator(__p1));
	}
      else
	return std::make_pair(this->end(), this->end());
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    std::pair<typename _Hashtable<_Key, _Value, _Allocator,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  __chc, __cit, __uk>::const_iterator,
	      typename _Hashtable<_Key, _Value, _Allocator,
				  _ExtractKey, _Equal, _H1,
				  _H2, _Hash, _RehashPolicy,
				  __chc, __cit, __uk>::const_iterator>
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    equal_range(const key_type& __k) const
    {
      typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
      std::size_t __n = this->_M_bucket_index(__k, __code, _M_bucket_count);
      _Node* __p = _M_find_node(__n, __k, __code);

      if (__p)
	{
	  _Node* __p1 = __p->_M_next;
	  while (__p1
		 && this->_M_bucket_index(__p1, _M_bucket_count) == __n
		 && this->_M_compare(__k, __code, __p1))
	    __p1 = __p1->_M_next;

	  return std::make_pair(const_iterator(__p), const_iterator(__p1));
	}
      else
	return std::make_pair(this->end(), this->end());
    }

  // Find the node whose key compares equal to k in the bucket n. Return nullptr
  // if no node is found.
  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::_Node*
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_find_node(size_type __n, const key_type& __k,
		typename _Hashtable::_Hash_code_type __code) const
    {
      _Node* __p = _M_bucket_begin(__n);
      if (!__p)
	return nullptr;
      for (;; __p = __p->_M_next)
	{
	  if (this->_M_compare(__k, __code, __p))
	    return __p;
	  if (!(__p->_M_next)
	      || this->_M_bucket_index(__p->_M_next, _M_bucket_count) != __n)
	    break;
	}
      return nullptr;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_insert_bucket_begin(size_type __bkt, _Node* __new_node)
    {
      _Node* __prev_n;
      if (__bkt < _M_begin_bucket_index)
	{
	  if (_M_begin_bucket_index != _M_bucket_count)
	    {
	      __new_node->_M_next = _M_buckets[_M_begin_bucket_index];
	      _M_buckets[_M_begin_bucket_index] = __new_node;
	    }
	  __prev_n = __new_node;
	  _M_begin_bucket_index = __bkt;
	}
      else
	{
	  // We need to find previous non-empty bucket to link the new node.
	  // There are several ways to find this previous bucket:
	  // 1. Move backward until we find it (the current method)
	  // 2. Start from the begin bucket index and move forward until we
	  // cross __n position.
	  // 3. Move forward until we find a non-empty bucket that will
	  // contain the previous node.
	  size_type __prev_bkt;
	  for (__prev_bkt = __bkt; __prev_bkt-- != 0;)
	    if (_M_buckets[__prev_bkt])
	      break;
	  __prev_n = _M_bucket_end(__prev_bkt);
	  _M_insert_after(__prev_bkt, __prev_n, __new_node);
	}
      _M_buckets[__bkt] = __prev_n;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_insert_after(size_type __bkt, _Node* __prev_n, _Node* __new_n)
    {
      if (__prev_n->_M_next)
	{
	  size_type __next_bkt =
	    this->_M_bucket_index(__prev_n->_M_next, _M_bucket_count);
	  if (__next_bkt != __bkt)
	    _M_buckets[__next_bkt] = __new_n;
	}
      __new_n->_M_next = __prev_n->_M_next;
      __prev_n->_M_next = __new_n;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_remove_bucket_begin(size_type __bkt, _Node* __next, size_type __next_bkt)
    {
      if (!__next || __next_bkt != __bkt)
	{
	  // Bucket is now empty
	  if (__next && __next_bkt != __bkt)
	    // Update next non-empty bucket before begin node
	    _M_buckets[__next_bkt] = _M_buckets[__bkt];
	  _M_buckets[__bkt] = nullptr;
	  if (__bkt == _M_begin_bucket_index)
	    // We need to update begin bucket index
	    if (__next)
	      {
		_M_begin_bucket_index = __next_bkt;
		_M_buckets[_M_begin_bucket_index] = __next;
	      }
	    else
	      _M_begin_bucket_index = _M_bucket_count;
	}
      else if (__bkt == _M_begin_bucket_index)
	_M_buckets[__bkt] = __next;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey,
			_Equal, _H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::_Node*
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_get_previous_node(size_type __bkt, _Node* __n)
    {
      _Node* __prev_n = nullptr;
      if (__bkt != _M_begin_bucket_index || __n != _M_buckets[__bkt])
	{
	  __prev_n = _M_buckets[__bkt];
	  while (__prev_n->_M_next != __n)
	    __prev_n = __prev_n->_M_next;
	}
      return __prev_n;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename... _Args>
      std::pair<typename _Hashtable<_Key, _Value, _Allocator,
				    _ExtractKey, _Equal, _H1,
				    _H2, _Hash, _RehashPolicy,
				    __chc, __cit, __uk>::iterator, bool>
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _M_emplace(std::true_type, _Args&&... __args)
      {
	// First build the node to get access to the hash code
	_Node* __new_node = _M_allocate_node(std::forward<_Args>(__args)...);
	__try
	  {
	    const key_type& __k = this->_M_extract(__new_node->_M_v);
	    typename _Hashtable::_Hash_code_type __code
	      = this->_M_hash_code(__k);
	    size_type __bkt
	      = this->_M_bucket_index(__k, __code, _M_bucket_count);

	    if (_Node* __p = _M_find_node(__bkt, __k, __code))
	      {
		// There is already an equivalent node, no insertion
		_M_deallocate_node(__new_node);
		return std::make_pair(iterator(__p), false);
	      }

	    // We are going to insert this node
	    this->_M_store_code(__new_node, __code);
	    const _RehashPolicyState& __saved_state
	      = _M_rehash_policy._M_state();
	    std::pair<bool, std::size_t> __do_rehash
	      = _M_rehash_policy._M_need_rehash(_M_bucket_count,
						_M_element_count, 1);

	    if (__do_rehash.first)
	      {
		_M_rehash(__do_rehash.second, __saved_state);
		__bkt = this->_M_bucket_index(__k, __code, _M_bucket_count);
	      }

	    if (_M_buckets[__bkt])
	      _M_insert_after(__bkt, _M_buckets[__bkt], __new_node);
	    else 
	      _M_insert_bucket_begin(__bkt, __new_node);
	    ++_M_element_count;
	    return std::make_pair(iterator(__new_node), true);
	  }
	__catch(...)
	  {
	    _M_deallocate_node(__new_node);
	    __throw_exception_again;
	  }
      }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename... _Args>
      typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  __chc, __cit, __uk>::iterator
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _M_emplace(std::false_type, _Args&&... __args)
      {
	const _RehashPolicyState& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, 1);

	// First build the node to get its hash code
	_Node* __new_node = _M_allocate_node(std::forward<_Args>(__args)...);
	__try
	  {
	    const key_type& __k = this->_M_extract(__new_node->_M_v);
	    typename _Hashtable::_Hash_code_type __code
	      = this->_M_hash_code(__k);
	    this->_M_store_code(__new_node, __code);
	    size_type __bkt
	      = this->_M_bucket_index(__k, __code, _M_bucket_count);

	    // Second find the node, avoid rehash if compare throws.
	    _Node* __prev = _M_find_node(__bkt, __k, __code);
	    
	    if (__do_rehash.first)
	      {
		_M_rehash(__do_rehash.second, __saved_state);
		__bkt = this->_M_bucket_index(__k, __code, _M_bucket_count);
		// __prev is still valid because rehash do not invalidate nodes
	      }

	    if (__prev)
	      // Insert after the previous equivalent node
	      _M_insert_after(__bkt, __prev, __new_node);
	    else if (_M_buckets[__bkt])
	      // Bucket is not empty and the inserted node has no equivalent in
	      // the hashtable. We must insert the new node at the beginning or
	      // end of the bucket to preserve equivalent elements relative
	      // positions.
	      if (__bkt != _M_begin_bucket_index)
		// We insert the new node at the beginning
		_M_insert_after(__bkt, _M_buckets[__bkt], __new_node);
	      else
		// We insert the new node at the end
		_M_insert_after(__bkt, _M_bucket_end(__bkt), __new_node);
	    else
	      _M_insert_bucket_begin(__bkt, __new_node);
	    ++_M_element_count;
	    return iterator(__new_node);
	  }
	__catch(...)
	  {
	    _M_deallocate_node(__new_node);
	    __throw_exception_again;
	  }
      }

  // Insert v in bucket n (assumes no element with its key already present).
  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename _Arg>
      typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  __chc, __cit, __uk>::iterator
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _M_insert_bucket(_Arg&& __v, size_type __n,
		       typename _Hashtable::_Hash_code_type __code)
      {
	const _RehashPolicyState& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, 1);

	if (__do_rehash.first)
	  {
	    const key_type& __k = this->_M_extract(__v);
	    __n = this->_M_bucket_index(__k, __code, __do_rehash.second);
	  }

	_Node* __new_node = nullptr;
	__try
	  {
	    // Allocate the new node before doing the rehash so that we
	    // don't do a rehash if the allocation throws.
	    __new_node = _M_allocate_node(std::forward<_Arg>(__v));
	    this->_M_store_code(__new_node, __code);
	    if (__do_rehash.first)
	      _M_rehash(__do_rehash.second, __saved_state);

	    if (_M_buckets[__n])
	      _M_insert_after(__n, _M_buckets[__n], __new_node);
	    else 
	      _M_insert_bucket_begin(__n, __new_node);
	    ++_M_element_count;
	    return iterator(__new_node);
	  }
	__catch(...)
	  {
	    if (!__new_node)
	      _M_rehash_policy._M_reset(__saved_state);
	    else
	      _M_deallocate_node(__new_node);
	    __throw_exception_again;
	  }
      }

  // Insert v if no element with its key is already present.
  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename _Arg>
      std::pair<typename _Hashtable<_Key, _Value, _Allocator,
				    _ExtractKey, _Equal, _H1,
				    _H2, _Hash, _RehashPolicy,
				    __chc, __cit, __uk>::iterator, bool>
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _M_insert(_Arg&& __v, std::true_type)
      {
	const key_type& __k = this->_M_extract(__v);
	typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
	size_type __n = this->_M_bucket_index(__k, __code, _M_bucket_count);

	if (_Node* __p = _M_find_node(__n, __k, __code))
	  return std::make_pair(iterator(__p), false);
	return std::make_pair(_M_insert_bucket(std::forward<_Arg>(__v),
			      __n, __code), true);
      }

  // Insert v unconditionally.
  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename _Arg>
      typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			  _H1, _H2, _Hash, _RehashPolicy,
			  __chc, __cit, __uk>::iterator
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      _M_insert(_Arg&& __v, std::false_type)
      {
	const _RehashPolicyState& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, 1);

	const key_type& __k = this->_M_extract(__v);
	typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
	size_type __n = this->_M_bucket_index(__k, __code, _M_bucket_count);

	// First find the node, avoid leaking new_node if compare throws.
	_Node* __prev = _M_find_node(__n, __k, __code);
	_Node* __new_node = nullptr;
	__try
	  {
	    // Second allocate new node so that we don't rehash if it throws
	    __new_node = _M_allocate_node(std::forward<_Arg>(__v));
	    this->_M_store_code(__new_node, __code);
	    if (__do_rehash.first)
	      {
		_M_rehash(__do_rehash.second, __saved_state);
		__n = this->_M_bucket_index(__k, __code, _M_bucket_count);
		// __prev is still valid because rehash do not invalidate nodes
	      }

	    if (__prev)
	      // Insert after the previous equivalent node
	      _M_insert_after(__n, __prev, __new_node);
	    else if (_M_buckets[__n])
	      // Bucket is not empty and the inserted node has no equivalent in
	      // the hashtable. We must insert the new node at the beginning or
	      // end of the bucket to preserve equivalent elements relative
	      // positions.
	      if (__n != _M_begin_bucket_index)
		// We insert the new node at the beginning
		_M_insert_after(__n, _M_buckets[__n], __new_node);
	      else
		// We insert the new node at the end
		_M_insert_after(__n, _M_bucket_end(__n), __new_node);
	    else
	      _M_insert_bucket_begin(__n, __new_node);
	    ++_M_element_count;
	    return iterator(__new_node);
	  }
	__catch(...)
	  {
	    if (!__new_node)
	      _M_rehash_policy._M_reset(__saved_state);
	    else
	      _M_deallocate_node(__new_node);
	    __throw_exception_again;
	  }
      }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    template<typename _InputIterator>
      void
      _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
		 _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
      insert(_InputIterator __first, _InputIterator __last)
      {
	size_type __n_elt = __detail::__distance_fw(__first, __last);
	const _RehashPolicyState& __saved_state = _M_rehash_policy._M_state();
	std::pair<bool, std::size_t> __do_rehash
	  = _M_rehash_policy._M_need_rehash(_M_bucket_count,
					    _M_element_count, __n_elt);
	if (__do_rehash.first)
	  _M_rehash(__do_rehash.second, __saved_state);

	for (; __first != __last; ++__first)
	  this->insert(*__first);
      }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::iterator
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    erase(const_iterator __it)
    {
      _Node* __n = __it._M_cur;
      std::size_t __bkt = this->_M_bucket_index(__n, _M_bucket_count);

      // Look for previous node to unlink it from the erased one, this is why
      // we need buckets to contain the before begin node of the bucket to make
      // this research fast.
      _Node* __prev_n = _M_get_previous_node(__bkt, __n);
      if (__n == _M_bucket_begin(__bkt))
	_M_remove_bucket_begin(__bkt, __n->_M_next,
	   __n->_M_next ? this->_M_bucket_index(__n->_M_next, _M_bucket_count)
			: 0);
      else if (__n->_M_next)
	{
	  size_type __next_bkt =
	    this->_M_bucket_index(__n->_M_next, _M_bucket_count);
	  if (__next_bkt != __bkt)
	    _M_buckets[__next_bkt] = __prev_n;
	}

      if (__prev_n)
	__prev_n->_M_next = __n->_M_next;
      iterator __result(__n->_M_next);
      _M_deallocate_node(__n);
      --_M_element_count;

      return __result;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::size_type
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    erase(const key_type& __k)
    {
      typename _Hashtable::_Hash_code_type __code = this->_M_hash_code(__k);
      std::size_t __bkt = this->_M_bucket_index(__k, __code, _M_bucket_count);
      // Look for the first matching node with its previous node at the same
      // time
      _Node* __n = _M_buckets[__bkt];
      if (!__n)
	return 0;
      _Node* __prev_n = nullptr;
      if (__bkt != _M_begin_bucket_index)
	{
	  __prev_n = __n;
	  __n = __n->_M_next;
	}
      bool __is_bucket_begin = true;
      for (;; __prev_n = __n, __n = __n->_M_next)
	{
	  if (this->_M_compare(__k, __code, __n))
	    break;
	  if (!(__n->_M_next)
	      || this->_M_bucket_index(__n->_M_next, _M_bucket_count) != __bkt)
	    return 0;
	  __is_bucket_begin = false;
	}

      // We found a matching node, start deallocation loop from it
      std::size_t __next_bkt = __bkt;
      _Node* __next_n = __n;
      size_type __result = 0;
      _Node* __saved_n = nullptr;
      do
	{
	  _Node* __p = __next_n;
	  __next_n = __p->_M_next;
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 526. Is it undefined if a function in the standard changes
	  // in parameters?
	  if (std::__addressof(this->_M_extract(__p->_M_v))
	      != std::__addressof(__k))
	    _M_deallocate_node(__p);
	  else
	    __saved_n = __p;
	  --_M_element_count;
	  ++__result;
	  if (!__next_n)
	    break;
	  __next_bkt = this->_M_bucket_index(__next_n, _M_bucket_count);
	}
      while (__next_bkt == __bkt && this->_M_compare(__k, __code, __next_n));

      if (__saved_n)
	_M_deallocate_node(__saved_n);
      if (__is_bucket_begin)
	_M_remove_bucket_begin(__bkt, __next_n, __next_bkt);
      else if (__next_n && __next_bkt != __bkt)
	_M_buckets[__next_bkt] = __prev_n;
      if (__prev_n)
	__prev_n->_M_next = __next_n;
      return __result;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    typename _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
			_H1, _H2, _Hash, _RehashPolicy,
			__chc, __cit, __uk>::iterator
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    erase(const_iterator __first, const_iterator __last)
    {
      _Node* __n = __first._M_cur;
      _Node* __last_n = __last._M_cur;
      if (__n == __last_n)
	return iterator(__n);

      std::size_t __bkt = this->_M_bucket_index(__n, _M_bucket_count);

      _Node* __prev_n = _M_get_previous_node(__bkt, __n);
      bool __is_bucket_begin = __n == _M_bucket_begin(__bkt);
      std::size_t __n_bkt = __bkt;
      for (;;)
	{
	  do
	    {
	      _Node* __tmp = __n;
	      __n = __n->_M_next;
	      _M_deallocate_node(__tmp);
	      --_M_element_count;
	      if (!__n)
		break;
	      __n_bkt = this->_M_bucket_index(__n, _M_bucket_count);
	    }
	  while (__n != __last_n && __n_bkt == __bkt);
	  if (__is_bucket_begin)
	    _M_remove_bucket_begin(__bkt, __n, __n_bkt);
	  if (__n == __last_n)
	    break;
	  __is_bucket_begin = true;
	  __bkt = __n_bkt;
	}

      if (__n && __n_bkt != __bkt)
	_M_buckets[__n_bkt] = __prev_n;
      if (__prev_n)
	__prev_n->_M_next = __n;
      return iterator(__n);
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    clear() noexcept
    {
      _M_deallocate_nodes(_M_buckets[_M_begin_bucket_index]);
      __builtin_memset(_M_buckets, 0, _M_bucket_count * sizeof(_Bucket));
      _M_element_count = 0;
      _M_begin_bucket_index = _M_bucket_count;
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    rehash(size_type __n)
    {
      const _RehashPolicyState& __saved_state = _M_rehash_policy._M_state();
      _M_rehash(std::max(_M_rehash_policy._M_next_bkt(__n),
			 _M_rehash_policy._M_bkt_for_elements(_M_element_count
							      + 1)),
		__saved_state);
    }

  template<typename _Key, typename _Value,
	   typename _Allocator, typename _ExtractKey, typename _Equal,
	   typename _H1, typename _H2, typename _Hash, typename _RehashPolicy,
	   bool __chc, bool __cit, bool __uk>
    void
    _Hashtable<_Key, _Value, _Allocator, _ExtractKey, _Equal,
	       _H1, _H2, _Hash, _RehashPolicy, __chc, __cit, __uk>::
    _M_rehash(size_type __n, const _RehashPolicyState& __state)
    {
      __try
	{
	  _Bucket* __new_buckets = _M_allocate_buckets(__n);
	  _Node* __p = _M_buckets[_M_begin_bucket_index];
	  // First loop to store each node in its new bucket
	  while (__p)
	    {
	      _Node* __next = __p->_M_next;
	      std::size_t __new_index = this->_M_bucket_index(__p, __n);
	      if (!__new_buckets[__new_index])
		// Store temporarily bucket end node in _M_buckets if possible.
		// This will boost second loop where we need to access bucket
		// end node quickly.
		if (__new_index < _M_bucket_count)
		  _M_buckets[__new_index] = __p;
	      __p->_M_next = __new_buckets[__new_index];
	      __new_buckets[__new_index] = __p;
	      __p = __next;
	    }
	  _M_begin_bucket_index = __n;
	  _Node* __prev_node = nullptr;
	  // Second loop to link all nodes together and to fix bucket values so
	  // that they contain the before begin node of the bucket.
	  for (size_type __i = 0; __i != __n; ++__i)
	    if (__new_buckets[__i])
	      {
		if (__prev_node)
		  {
		    __prev_node->_M_next = __new_buckets[__i];
		    __new_buckets[__i] = __prev_node;
		  }
		else
		  _M_begin_bucket_index = __i;
		if (__i < _M_bucket_count)
		  __prev_node = _M_buckets[__i];
		else
		  {
		    __prev_node = __new_buckets[__i];
		    while (__prev_node->_M_next)
		      __prev_node = __prev_node->_M_next;
		  }
	      }
	  _M_deallocate_buckets(_M_buckets, _M_bucket_count);
	  _M_bucket_count = __n;
	  _M_buckets = __new_buckets;
	}
      __catch(...)
	{
	  // A failure here means that buckets allocation failed.  We only
	  // have to restore hash policy previous state.
	  _M_rehash_policy._M_reset(__state);
	  __throw_exception_again;
	}
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _HASHTABLE_H
