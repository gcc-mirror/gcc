// Profiling map implementation -*- C++ -*-

// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

/** @file profile/map.h
 *  This file is a GNU profile extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_PROFILE_MAP_H
#define _GLIBCXX_PROFILE_MAP_H 1

#include <profile/base.h>
#include <profile/ordered_base.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __profile
{
  /// Class std::map wrapper with performance instrumentation.
  template<typename _Key, typename _Tp, typename _Compare = std::less<_Key>,
	   typename _Allocator = std::allocator<std::pair<const _Key, _Tp> > >
    class map
    : public _GLIBCXX_STD_C::map<_Key, _Tp, _Compare, _Allocator>,
      public _Ordered_profile<map<_Key, _Tp, _Compare, _Allocator> >
    {
      typedef _GLIBCXX_STD_C::map<_Key, _Tp, _Compare, _Allocator> _Base;

      typedef typename _Base::iterator			_Base_iterator;
      typedef typename _Base::const_iterator		_Base_const_iterator;

    public:
      // types:
      typedef _Key					key_type;
      typedef _Tp					mapped_type;
      typedef typename _Base::value_type		value_type;
      typedef _Compare					key_compare;
      typedef typename _Base::reference			reference;
      typedef typename _Base::const_reference		const_reference;

      typedef __iterator_tracker<_Base_iterator, map>	iterator;
      typedef __iterator_tracker<_Base_const_iterator,
				 map>			const_iterator;
      typedef std::reverse_iterator<iterator>		reverse_iterator;
      typedef std::reverse_iterator<const_iterator>	const_reverse_iterator;

      typedef typename _Base::size_type			size_type;
      typedef typename _Base::difference_type		difference_type;

      // 23.3.1.1 construct/copy/destroy:

#if __cplusplus < 201103L
      map()
      : _Base() { }
      map(const map& __x)
	: _Base(__x) { }
      ~map()
      { }
#else
      map() = default;
      map(const map&) = default;
      map(map&&) = default;
      ~map() = default;
#endif

      explicit
      map(const _Compare& __comp,
	  const _Allocator& __a = _Allocator())
      : _Base(__comp, __a) { }

#if __cplusplus >= 201103L
      template<typename _InputIterator,
	       typename = std::_RequireInputIter<_InputIterator>>
#else
      template<typename _InputIterator>
#endif
	map(_InputIterator __first, _InputIterator __last,
	    const _Compare& __comp = _Compare(),
	    const _Allocator& __a = _Allocator())
	: _Base(__first, __last, __comp, __a) { }

      map(const _Base& __x)
      : _Base(__x) { }

#if __cplusplus >= 201103L
      map(initializer_list<value_type> __l,
	  const _Compare& __c = _Compare(),
	  const _Allocator& __a = _Allocator())
      : _Base(__l, __c, __a) { }

      explicit
      map(const _Allocator& __a)
      : _Base(__a) { }

      map(const map& __x, const _Allocator& __a)
      : _Base(__x, __a) { }

      map(map&& __x, const _Allocator& __a)
      noexcept( noexcept(_Base(std::move(__x), __a)) )
      : _Base(std::move(__x), __a) { }

      map(initializer_list<value_type> __l, const _Allocator& __a)
      : _Base(__l, __a) { }

      template<typename _InputIterator>
	map(_InputIterator __first, _InputIterator __last,
	    const _Allocator& __a)
	: _Base(__first, __last, __a) { }
#endif

#if __cplusplus < 201103L
      map&
      operator=(const map& __x)
      {
	this->_M_profile_destruct();
	_M_base() = __x;
	this->_M_profile_construct();
	return *this;
      }
#else
      map&
      operator=(const map&) = default;

      map&
      operator=(map&&) = default;

      map&
      operator=(initializer_list<value_type> __l)
      {
	this->_M_profile_destruct();
	_M_base() = __l;
	this->_M_profile_construct();
	return *this;
      }
#endif

      // iterators
      iterator
      begin() _GLIBCXX_NOEXCEPT
      { return iterator(_Base::begin(), this); }

      const_iterator
      begin() const _GLIBCXX_NOEXCEPT
      { return const_iterator(_Base::begin(), this); }

      iterator
      end() _GLIBCXX_NOEXCEPT
      { return iterator(_Base::end(), this); }

      const_iterator
      end() const _GLIBCXX_NOEXCEPT
      { return const_iterator(_Base::end(), this); }

#if __cplusplus >= 201103L
      const_iterator
      cbegin() const noexcept
      { return const_iterator(_Base::cbegin(), this); }

      const_iterator
      cend() const noexcept
      { return const_iterator(_Base::cend(), this); }
#endif

      reverse_iterator
      rbegin() _GLIBCXX_NOEXCEPT
      {
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return reverse_iterator(end());
      }

      const_reverse_iterator
      rbegin() const _GLIBCXX_NOEXCEPT
      {
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return const_reverse_iterator(end());
      }

      reverse_iterator
      rend() _GLIBCXX_NOEXCEPT
      {
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return reverse_iterator(begin());
      }

      const_reverse_iterator
      rend() const _GLIBCXX_NOEXCEPT
      {
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return const_reverse_iterator(begin());
      }

#if __cplusplus >= 201103L
      const_reverse_iterator
      crbegin() const noexcept
      {
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return const_reverse_iterator(cend());
      }

      const_reverse_iterator
      crend() const noexcept
      {
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return const_reverse_iterator(cbegin());
      }
#endif

      // 23.3.1.2 element access:
      mapped_type&
      operator[](const key_type& __k)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return _Base::operator[](__k);
      }

#if __cplusplus >= 201103L
      mapped_type&
      operator[](key_type&& __k)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return _Base::operator[](std::move(__k));
      }
#endif

      mapped_type&
      at(const key_type& __k)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return _Base::at(__k);
      }

      const mapped_type&
      at(const key_type& __k) const
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return _Base::at(__k);
      }

      // modifiers:
#if __cplusplus >= 201103L
      template<typename... _Args>
	std::pair<iterator, bool>
	emplace(_Args&&... __args)
	{
	  // The cost is the same whether or not the element is inserted so we
	  // always report insertion of 1 element.
	  __profcxx_map2umap_insert(this->_M_map2umap_info, this->size(), 1);
	  auto __base_ret = _Base::emplace(std::forward<_Args>(__args)...);
	  return std::make_pair(iterator(__base_ret.first, this),
				__base_ret.second);
	}

      template<typename... _Args>
	iterator
	emplace_hint(const_iterator __pos, _Args&&... __args)
	{
	  auto size_before = this->size();
	  auto __res
	    = _Base::emplace_hint(__pos.base(), std::forward<_Args>(__args)...);
	  __profcxx_map2umap_insert(this->_M_map2umap_info,
		size_before, _M_hint_used(__pos.base(), __res) ? 0 : 1);
	  return iterator(__res, this);
	}
#endif

      std::pair<iterator, bool>
      insert(const value_type& __x)
      {
	__profcxx_map2umap_insert(this->_M_map2umap_info, this->size(), 1);
	std::pair<_Base_iterator, bool> __base_ret = _Base::insert(__x);
	return std::make_pair(iterator(__base_ret.first, this),
			      __base_ret.second);
      }

#if __cplusplus >= 201103L
      template<typename _Pair, typename = typename
	       std::enable_if<std::is_constructible<value_type,
						    _Pair&&>::value>::type>
	std::pair<iterator, bool>
	insert(_Pair&& __x)
	{
	  __profcxx_map2umap_insert(this->_M_map2umap_info, this->size(), 1);
	  auto __base_ret= _Base::insert(std::forward<_Pair>(__x));
	  return std::make_pair(iterator(__base_ret.first, this),
				__base_ret.second);
	}
#endif

#if __cplusplus >= 201103L
      void
      insert(std::initializer_list<value_type> __list)
      { insert(__list.begin(), __list.end()); }
#endif

      iterator
#if __cplusplus >= 201103L
      insert(const_iterator __pos, const value_type& __x)
#else
      insert(iterator __pos, const value_type& __x)
#endif
      {
	size_type size_before = this->size();
	_Base_iterator __res = _Base::insert(__pos.base(), __x);
	
	__profcxx_map2umap_insert(this->_M_map2umap_info,
		size_before, _M_hint_used(__pos.base(), __res) ? 0 : 1);
	return iterator(__res, this);
      }

#if __cplusplus >= 201103L
      template<typename _Pair, typename = typename
	       std::enable_if<std::is_constructible<value_type,
						    _Pair&&>::value>::type>
	iterator
	insert(const_iterator __pos, _Pair&& __x)
	{
	  size_type size_before = this->size();
	  auto __res = _Base::insert(__pos.base(), std::forward<_Pair>(__x));
	
	  __profcxx_map2umap_insert(this->_M_map2umap_info,
		size_before, _M_hint_used(__pos.base(), __res) ? 0 : 1);
	  return iterator(__res, this);
      }
#endif

      template<typename _InputIterator>
	void
	insert(_InputIterator __first, _InputIterator __last)
	{
	  for (; __first != __last; ++__first)
	    insert(*__first);
	}

#if __cplusplus >= 201103L
      iterator
      erase(const_iterator __pos)
      {
	__profcxx_map2umap_erase(this->_M_map2umap_info, this->size(), 1);
	return iterator(_Base::erase(__pos.base()), this);
      }

      iterator
      erase(iterator __pos)
      {
	__profcxx_map2umap_erase(this->_M_map2umap_info, this->size(), 1);
	return iterator(_Base::erase(__pos.base()), this);
      }
#else
      void
      erase(iterator __pos)
      {
	__profcxx_map2umap_erase(this->_M_map2umap_info, this->size(), 1);
	_Base::erase(__pos.base());
      }
#endif

      size_type
      erase(const key_type& __x)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	__profcxx_map2umap_erase(this->_M_map2umap_info, this->size(), 1);
	return _Base::erase(__x);
      }

#if __cplusplus >= 201103L
      iterator
      erase(const_iterator __first, const_iterator __last)
      {
	if (__first != __last)
	  {
	    iterator __ret;
	    for (; __first != __last;)
	      __ret = erase(__first++);
	    return __ret;
	  }
	else
	  return iterator(_Base::erase(__first.base(), __last.base()), this);
      }
#else
      void
      erase(iterator __first, iterator __last)
      {
	for (; __first != __last;)
	  erase(__first++);
      }
#endif

      void
      swap(map& __x)
      _GLIBCXX_NOEXCEPT_IF( noexcept(declval<_Base&>().swap(__x)) )
      {
	_Base::swap(__x);
	this->_M_swap(__x);
      }

      void
      clear() _GLIBCXX_NOEXCEPT
      {
	this->_M_profile_destruct();
	_Base::clear();
	this->_M_profile_construct();
      }

      // 23.3.1.3 map operations:
      iterator
      find(const key_type& __x)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return iterator(_Base::find(__x), this);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	iterator
	find(const _Kt& __x)
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  return { _Base::find(__x), this };
	}
#endif

      const_iterator
      find(const key_type& __x) const
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return const_iterator(_Base::find(__x), this);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	const_iterator
	find(const _Kt& __x) const
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  return { _Base::find(__x), this };
	}
#endif

      size_type
      count(const key_type& __x) const
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	return _Base::count(__x);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	size_type
	count(const _Kt& __x) const
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  return _Base::count(__x);
	}
#endif

      iterator
      lower_bound(const key_type& __x)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return iterator(_Base::lower_bound(__x), this);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	iterator
	lower_bound(const _Kt& __x)
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  __profcxx_map2umap_invalidate(this->_M_map2umap_info);
	  return { _Base::lower_bound(__x), this };
	}
#endif

      const_iterator
      lower_bound(const key_type& __x) const
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return const_iterator(_Base::lower_bound(__x), this);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	const_iterator
	lower_bound(const _Kt& __x) const
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  __profcxx_map2umap_invalidate(this->_M_map2umap_info);
	  return { _Base::lower_bound(__x), this };
	}
#endif

      iterator
      upper_bound(const key_type& __x)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return iterator(_Base::upper_bound(__x), this);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	iterator
	upper_bound(const _Kt& __x)
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  __profcxx_map2umap_invalidate(this->_M_map2umap_info);
	  return { _Base::upper_bound(__x), this };
	}
#endif

      const_iterator
      upper_bound(const key_type& __x) const
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	__profcxx_map2umap_invalidate(this->_M_map2umap_info);
	return const_iterator(_Base::upper_bound(__x), this);
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	const_iterator
	upper_bound(const _Kt& __x) const
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  __profcxx_map2umap_invalidate(this->_M_map2umap_info);
	  return { _Base::upper_bound(__x), this };
	}
#endif

      std::pair<iterator,iterator>
      equal_range(const key_type& __x)
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	std::pair<_Base_iterator, _Base_iterator> __base_ret
	  = _Base::equal_range(__x);
	return std::make_pair(iterator(__base_ret.first, this),
			      iterator(__base_ret.second, this));
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	std::pair<iterator, iterator>
	equal_range(const _Kt& __x)
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  auto __res = _Base::equal_range(__x);
	  return { { __res.first, this }, { __res.second, this } };
	}
#endif

      std::pair<const_iterator,const_iterator>
      equal_range(const key_type& __x) const
      {
	__profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	std::pair<_Base_const_iterator, _Base_const_iterator> __base_ret
	  = _Base::equal_range(__x);
	return std::make_pair(const_iterator(__base_ret.first, this),
			      const_iterator(__base_ret.second, this));
      }

#if __cplusplus > 201103L
      template<typename _Kt,
	       typename _Req =
		 typename __has_is_transparent<_Compare, _Kt>::type>
	std::pair<const_iterator, const_iterator>
	equal_range(const _Kt& __x) const
	{
	  __profcxx_map2umap_find(this->_M_map2umap_info, this->size());
	  auto __res = _Base::equal_range(__x);
	  return { { __res.first, this }, { __res.second, this } };
	}
#endif

      _Base&
      _M_base() _GLIBCXX_NOEXCEPT	{ return *this; }

      const _Base&
      _M_base() const _GLIBCXX_NOEXCEPT	{ return *this; }

    private:
      /** If hint is used we consider that the map and unordered_map
       * operations have equivalent insertion cost so we do not update metrics
       * about it.
       * Note that to find out if hint has been used is libstdc++
       * implementation dependent.
       */
      bool
      _M_hint_used(_Base_const_iterator __hint, _Base_iterator __res)
      {
	return (__hint == __res
		|| (__hint == _M_base().end() && ++__res == _M_base().end())
		|| (__hint != _M_base().end() && (++__hint == __res
						  || ++__res == --__hint)));
      }


      template<typename _K1, typename _T1, typename _C1, typename _A1>
        friend bool
        operator==(const map<_K1, _T1, _C1, _A1>&,
		   const map<_K1, _T1, _C1, _A1>&);

      template<typename _K1, typename _T1, typename _C1, typename _A1>
        friend bool
        operator<(const map<_K1, _T1, _C1, _A1>&,
		  const map<_K1, _T1, _C1, _A1>&);      
    };

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator==(const map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    {
      __profcxx_map2umap_invalidate(__lhs._M_map2umap_info);
      __profcxx_map2umap_invalidate(__rhs._M_map2umap_info);
      return __lhs._M_base() == __rhs._M_base();
    }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator<(const map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	      const map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    {
      __profcxx_map2umap_invalidate(__lhs._M_map2umap_info);
      __profcxx_map2umap_invalidate(__rhs._M_map2umap_info);
      return __lhs._M_base() < __rhs._M_base();
    }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator!=(const map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return !(__lhs == __rhs); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator<=(const map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return !(__rhs < __lhs); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator>=(const map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return !(__lhs < __rhs); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator>(const map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	      const map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __rhs < __lhs; }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline void
    swap(map<_Key, _Tp, _Compare, _Allocator>& __lhs,
	 map<_Key, _Tp, _Compare, _Allocator>& __rhs)
    _GLIBCXX_NOEXCEPT_IF(noexcept(__lhs.swap(__rhs)))
    { __lhs.swap(__rhs); }

} // namespace __profile
} // namespace std

#endif
