// Safe associated container base class implementation  -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file debug/safe_association.h
 *  This file is a GNU debug extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_DEBUG_SAFE_ASSOCIATION_H
#define _GLIBCXX_DEBUG_SAFE_ASSOCIATION_H 1

#include <debug/debug.h>
#include <debug/macros.h>
#include <debug/functions.h>
#include <debug/formatter.h>
#include <debug/safe_sequence.h>

namespace __gnu_debug
{
  /**
   * @brief Base class for constructing a "safe" associated container type.
   *
   * The class template %_Safe_association simplifies the construction of
   * "safe" associated containers.
   */
  template<typename _Base>
    class _Safe_association 
    : public _Base
    {
    public:
      typedef typename _Base::size_type       size_type;
      typedef typename _Base::hasher          hasher;
      typedef typename _Base::key_equal       key_equal;
      typedef typename _Base::allocator_type allocator_type;

      typedef typename _Base::key_type        key_type;
      typedef typename _Base::value_type      value_type;
      typedef typename _Base::difference_type difference_type;
      typedef typename _Base::reference       reference;
      typedef typename _Base::const_reference const_reference;

      typedef __gnu_debug::_Safe_iterator<typename _Base::iterator, 
					  _Safe_association>
                                              iterator;
      typedef __gnu_debug::_Safe_iterator<typename _Base::const_iterator,
					  _Safe_association>
                                              const_iterator;

      _Safe_association() { }

      explicit _Safe_association(size_type __n) : _Base(__n) { }

      _Safe_association(size_type __n, const hasher& __hf) 
      : _Base(__n, __hf) { }

      _Safe_association(size_type __n, const hasher& __hf, 
			const key_equal& __eql,
			const allocator_type& __a = allocator_type())
      : _Base(__n, __hf, __eql, __a) { }

      template<typename _InputIter>
        _Safe_association(_InputIter __f, _InputIter __l)
	: _Base(__gnu_debug::__check_valid_range(__f, __l), __l) { }

      template<typename _InputIter>
        _Safe_association(_InputIter __f, _InputIter __l, size_type __n)
	: _Base(__gnu_debug::__check_valid_range(__f, __l), __l, __n) { }

      template<typename _InputIter>
        _Safe_association(_InputIter __f, _InputIter __l, size_type __n,
		      const hasher& __hf)
	: _Base(__gnu_debug::__check_valid_range(__f, __l), __l, __n, __hf) 
        { }

      template<typename _InputIter>
        _Safe_association(_InputIter __f, _InputIter __l, size_type __n,
			  const hasher& __hf, const key_equal& __eql,
			  const allocator_type& __a = allocator_type())
	: _Base(__gnu_debug::__check_valid_range(__f, __l), 
		__l, __n, __hf, __eql, __a) 
        { }

      _Safe_association(const _Base& __x) : _Base(__x) { }

      _Safe_association(_Safe_association&& __x)
      : _Base(std::forward<_Base>(__x)) { }

      using _Base::size;
      using _Base::max_size;
      using _Base::empty;
      using _Base::get_allocator;
      using _Base::key_eq;

      using _Base::count;
      using _Base::bucket_count;
      using _Base::max_bucket_count;
      using _Base::bucket;
      using _Base::bucket_size;
      using _Base::load_factor;

      const_iterator
      begin() const { return const_iterator(_Base::begin(), this); }

      const_iterator
      end() const   { return const_iterator(_Base::end(), this); }

      std::pair<iterator, bool>
      insert(const value_type& __obj)
      {
	typedef std::pair<typename _Base::iterator, bool> __pair_type;
	__pair_type __res = _Base::insert(__obj);
	return std::make_pair(iterator(__res.first, this), __res.second);
      }

      void
      insert(const value_type* __first, const value_type* __last)
      {
	__glibcxx_check_valid_range(__first, __last);
	_Base::insert(__first, __last);
      }

      template<typename _InputIter>
        void
        insert(_InputIter __first, _InputIter __last)
        {
	  __glibcxx_check_valid_range(__first, __last);
	  _Base::insert(__first.base(), __last.base());
	}

      const_iterator
      find(const key_type& __key) const
      { return const_iterator(_Base::find(__key), this); }

      std::pair<const_iterator, const_iterator>
      equal_range(const key_type& __key) const
      {
	typedef typename _Base::const_iterator _Base_iterator;
	typedef std::pair<_Base_iterator, _Base_iterator> __pair_type;
	__pair_type __res = _Base::equal_range(__key);
	return std::make_pair(const_iterator(__res.first, this),
			      const_iterator(__res.second, this));
      }

      size_type
      erase(const key_type& __key)
      {
	size_type __ret(0);
	iterator __victim(_Base::find(__key), this);
	if (__victim != end())
	  {
	    this->erase(__victim);
	    __ret = 1;
	  }
	return __ret;
      }

      iterator
      erase(iterator __it)
      {
	__glibcxx_check_erase(__it);
	__it._M_invalidate();
	return iterator(_Base::erase(__it.base()));
      }

      iterator
      erase(iterator __first, iterator __last)
      {
	__glibcxx_check_erase_range(__first, __last);
	for (iterator __tmp = __first; __tmp != __last;)
	{
	  iterator __victim = __tmp++;
	  __victim._M_invalidate();
	}
	return iterator(_Base::erase(__first.base(), __last.base()));
      }

      _Base&
      _M_base() { return *this; }

      const _Base&
      _M_base() const { return *this; }
    };
} // namespace __gnu_debug

#endif
