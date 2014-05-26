// Profiling multimap implementation -*- C++ -*-

// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

/** @file profile/multimap.h
 *  This file is a GNU profile extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_PROFILE_MULTIMAP_H
#define _GLIBCXX_PROFILE_MULTIMAP_H 1

#include <profile/base.h>
#include <profile/ordered_base.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __profile
{
  /// Class std::multimap wrapper with performance instrumentation.
  template<typename _Key, typename _Tp, typename _Compare = std::less<_Key>,
	   typename _Allocator = std::allocator<std::pair<const _Key, _Tp> > >
    class multimap
    : public _GLIBCXX_STD_C::multimap<_Key, _Tp, _Compare, _Allocator>,
      public _Ordered_profile<map<_Key, _Tp, _Compare, _Allocator> >
    {
      typedef _GLIBCXX_STD_C::multimap<_Key, _Tp, _Compare, _Allocator> _Base;

    public:
      // types:
      typedef _Key					key_type;
      typedef _Tp					mapped_type;
      typedef std::pair<const _Key, _Tp>		value_type;
      typedef _Compare					key_compare;
      typedef typename _Base::reference			reference;
      typedef typename _Base::const_reference		const_reference;

      typedef typename _Base::iterator			iterator;
      typedef typename _Base::const_iterator		const_iterator;
      typedef typename _Base::reverse_iterator		reverse_iterator;
      typedef typename _Base::const_reverse_iterator	const_reverse_iterator;

      typedef typename _Base::size_type			size_type;
      typedef typename _Base::difference_type		difference_type;
      typedef typename _Base::pointer			pointer;
      typedef typename _Base::const_pointer		const_pointer;

      // 23.3.1.1 construct/copy/destroy:

#if __cplusplus < 201103L
      multimap()
      : _Base() { }
      multimap(const multimap& __x)
      : _Base(__x) { }
      ~multimap() { }
#else
      multimap() = default;
      multimap(const multimap&) = default;
      multimap(multimap&&) = default;
      ~multimap() = default;
#endif

      explicit multimap(const _Compare& __comp,
			const _Allocator& __a = _Allocator())
      : _Base(__comp, __a) { }

#if __cplusplus >= 201103L
      template<typename _InputIterator,
	       typename = std::_RequireInputIter<_InputIterator>>
#else
      template<typename _InputIterator>
#endif
	multimap(_InputIterator __first, _InputIterator __last,
		 const _Compare& __comp = _Compare(),
		 const _Allocator& __a = _Allocator())
	: _Base(__first, __last, __comp, __a) { }

#if __cplusplus >= 201103L
      multimap(initializer_list<value_type> __l,
	       const _Compare& __c = _Compare(),
	       const _Allocator& __a = _Allocator())
      : _Base(__l, __c, __a) { }

      explicit
      multimap(const _Allocator& __a)
      : _Base(__a) { }

      multimap(const multimap& __x, const _Allocator& __a)
      : _Base(__x, __a) { }

      multimap(multimap&& __x, const _Allocator& __a)
      noexcept( noexcept(_Base(std::move(__x), __a)) )
      : _Base(std::move(__x), __a) { }

      multimap(initializer_list<value_type> __l, const _Allocator& __a)
      : _Base(__l, __a) { }

      template<typename _InputIterator>
	multimap(_InputIterator __first, _InputIterator __last,
		 const _Allocator& __a)
	: _Base(__first, __last, __a) { }
#endif

      multimap(const _Base& __x)
      : _Base(__x) { }

#if __cplusplus < 201103L
      multimap&
      operator=(const multimap& __x)
      {
	_M_base() = __x;
	return *this;
      }
#else
      multimap&
      operator=(const multimap&) = default;

      multimap&
      operator=(multimap&&) = default;

      multimap&
      operator=(initializer_list<value_type> __l)
      {
	_M_base() = __l;
	return *this;
      }
#endif

      reverse_iterator
      rbegin() _GLIBCXX_NOEXCEPT
      {
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::rbegin();
      }

      const_reverse_iterator
      rbegin() const _GLIBCXX_NOEXCEPT
      {
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::rbegin();
      }

      reverse_iterator
      rend() _GLIBCXX_NOEXCEPT
      {
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::rend();
      }

      const_reverse_iterator
      rend() const _GLIBCXX_NOEXCEPT
      {
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::rend();
      }

#if __cplusplus >= 201103L
      const_reverse_iterator
      crbegin() const noexcept
      {
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::crbegin();
      }

      const_reverse_iterator
      crend() const noexcept
      {
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::crend();
      }
#endif

      // modifiers:
#if __cplusplus >= 201103L
      template<typename... _Args>
	iterator
	emplace(_Args&&... __args)
	{
	  __profcxx_map_to_unordered_map_insert(this, this->size(), 1);
	  return _Base::emplace(std::forward<_Args>(__args)...);
	}

      template<typename... _Args>
	iterator
	emplace_hint(const_iterator __pos, _Args&&... __args)
	{
	  auto size_before = this->size();
	  auto __res
	    = _Base::emplace_hint(__pos, std::forward<_Args>(__args)...);
	  __profcxx_map_to_unordered_map_insert(this, size_before,
					_M_hint_used(__pos, __res) ? 0 : 1);
	  return __res;
	}
#endif

      iterator
      insert(const value_type& __x)
      {
	__profcxx_map_to_unordered_map_insert(this, this->size(), 1);
	return _Base::insert(__x);
      }

#if __cplusplus >= 201103L
      template<typename _Pair, typename = typename
	       std::enable_if<std::is_constructible<value_type,
						    _Pair&&>::value>::type>
	iterator
	insert(_Pair&& __x)
	{
	  __profcxx_map_to_unordered_map_insert(this, this->size(), 1);
	  return _Base::insert(std::forward<_Pair>(__x));
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
	iterator __res = _Base::insert(__pos, __x);
	__profcxx_map_to_unordered_map_insert(this, size_before,
					_M_hint_used(__pos, __res) ? 0 : 1);
	return __res;
      }

#if __cplusplus >= 201103L
      template<typename _Pair, typename = typename
	       std::enable_if<std::is_constructible<value_type,
						    _Pair&&>::value>::type>
	iterator
	insert(const_iterator __pos, _Pair&& __x)
	{
	  size_type size_before = this->size();
	  auto __res = _Base::insert(__pos, std::forward<_Pair>(__x));
	  __profcxx_map_to_unordered_map_insert(this, size_before,
					_M_hint_used(__pos, __res) ? 0 : 1);
	  return __res;
	}
#endif

#if __cplusplus >= 201103L
      template<typename _InputIterator,
	       typename = std::_RequireInputIter<_InputIterator>>
#else
      template<typename _InputIterator>
#endif
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
	__profcxx_map_to_unordered_map_erase(this, this->size(), 1);
	return _Base::erase(__pos);
      }

      iterator
      erase(iterator __pos)
      {
	__profcxx_map_to_unordered_map_erase(this, this->size(), 1);
	return _Base::erase(__pos);
      }
#else
      void
      erase(iterator __pos)
      {
	__profcxx_map_to_unordered_map_erase(this, this->size(), 1);
	_Base::erase(__pos);
      }
#endif

      size_type
      erase(const key_type& __x)
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	__profcxx_map_to_unordered_map_erase(this, this->size(), 1);
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
	  return _Base::erase(__first, __last);
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
      swap(multimap& __x)
#if __cplusplus >= 201103L
	noexcept( noexcept(declval<_Base>().swap(__x)) )
#endif
      { _Base::swap(__x); }

      // 23.3.1.3 multimap operations:
      iterator
      find(const key_type& __x)
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	return _Base::find(__x);
      }

      const_iterator
      find(const key_type& __x) const
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	return _Base::find(__x);
      }

      size_type
      count(const key_type& __x) const
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	return _Base::count(__x);
      }

      iterator
      lower_bound(const key_type& __x)
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::lower_bound(__x);
      }

      const_iterator
      lower_bound(const key_type& __x) const
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::lower_bound(__x);
      }

      iterator
      upper_bound(const key_type& __x)
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::upper_bound(__x);
      }

      const_iterator
      upper_bound(const key_type& __x) const
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	__profcxx_map_to_unordered_map_invalidate(this);
	return _Base::upper_bound(__x);
      }

      std::pair<iterator,iterator>
      equal_range(const key_type& __x)
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	return _Base::equal_range(__x);
      }

      std::pair<const_iterator,const_iterator>
      equal_range(const key_type& __x) const
      {
	__profcxx_map_to_unordered_map_find(this, this->size());
	return _Base::equal_range(__x);
      }

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
      _M_hint_used(const_iterator __hint, iterator __res)
      {
	return (__hint == __res
		|| (__hint == this->end() && ++__res == this->end())
		|| (__hint != this->end() && (++__hint == __res
					      || ++__res == --__hint)));
      }
    };

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator==(const multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __lhs._M_base() == __rhs._M_base(); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator!=(const multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __lhs._M_base() != __rhs._M_base(); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator<(const multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	      const multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __lhs._M_base() < __rhs._M_base(); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator<=(const multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __lhs._M_base() <= __rhs._M_base(); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator>=(const multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	       const multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __lhs._M_base() >= __rhs._M_base(); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline bool
    operator>(const multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	      const multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { return __lhs._M_base() > __rhs._M_base(); }

  template<typename _Key, typename _Tp,
	   typename _Compare, typename _Allocator>
    inline void
    swap(multimap<_Key, _Tp, _Compare, _Allocator>& __lhs,
	 multimap<_Key, _Tp, _Compare, _Allocator>& __rhs)
    { __lhs.swap(__rhs); }

} // namespace __profile
} // namespace std

#endif
