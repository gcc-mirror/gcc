// Safe iterator implementation  -*- C++ -*-

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

/** @file debug/safe_local_iterator.h
 *  This file is a GNU debug extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_DEBUG_SAFE_LOCAL_ITERATOR_H
#define _GLIBCXX_DEBUG_SAFE_LOCAL_ITERATOR_H 1

#include <debug/safe_unordered_base.h>

namespace __gnu_debug
{
  /** \brief Safe iterator wrapper.
   *
   *  The class template %_Safe_local_iterator is a wrapper around an
   *  iterator that tracks the iterator's movement among sequences and
   *  checks that operations performed on the "safe" iterator are
   *  legal. In additional to the basic iterator operations (which are
   *  validated, and then passed to the underlying iterator),
   *  %_Safe_local_iterator has member functions for iterator invalidation,
   *  attaching/detaching the iterator from sequences, and querying
   *  the iterator's state.
   */
  template<typename _Iterator, typename _Sequence>
    class _Safe_local_iterator
    : private _Iterator
    , public _Safe_local_iterator_base
    {
      typedef _Iterator _Iter_base;
      typedef _Safe_local_iterator_base _Safe_base;
      typedef typename _Sequence::const_local_iterator _Const_local_iterator;
      typedef typename _Sequence::size_type size_type;

      typedef std::iterator_traits<_Iterator> _Traits;

      struct _Attach_single
      { };

      _Safe_local_iterator(const _Iterator& __i, _Safe_sequence_base* __cont,
			   _Attach_single) noexcept
      : _Iter_base(__i)
      { _M_attach_single(__cont); }

    public:
      typedef _Iterator					iterator_type;
      typedef typename _Traits::iterator_category	iterator_category;
      typedef typename _Traits::value_type		value_type;
      typedef typename _Traits::difference_type		difference_type;
      typedef typename _Traits::reference		reference;
      typedef typename _Traits::pointer			pointer;

      /// @post the iterator is singular and unattached
      _Safe_local_iterator() noexcept : _Iter_base() { }

      /**
       * @brief Safe iterator construction from an unsafe iterator and
       * its sequence.
       *
       * @pre @p seq is not NULL
       * @post this is not singular
       */
      _Safe_local_iterator(const _Iterator& __i,
			   const _Safe_sequence_base* __cont)
      : _Iter_base(__i), _Safe_base(__cont, _S_constant())
      {
	_GLIBCXX_DEBUG_VERIFY(!this->_M_singular(),
			      _M_message(__msg_init_singular)
			      ._M_iterator(*this, "this"));
      }

      /**
       * @brief Copy construction.
       */
      _Safe_local_iterator(const _Safe_local_iterator& __x) noexcept
      : _Iter_base(__x.base())
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// DR 408. Is vector<reverse_iterator<char*> > forbidden?
	_GLIBCXX_DEBUG_VERIFY(!__x._M_singular()
			      || __x.base() == _Iterator(),
			      _M_message(__msg_init_copy_singular)
			      ._M_iterator(*this, "this")
			      ._M_iterator(__x, "other"));
	_M_attach(__x._M_sequence);
      }

      /**
       * @brief Move construction.
       * @post __x is singular and unattached
       */
      _Safe_local_iterator(_Safe_local_iterator&& __x) noexcept
      : _Iter_base()
      {
	_GLIBCXX_DEBUG_VERIFY(!__x._M_singular()
			      || __x.base() == _Iterator(),
			      _M_message(__msg_init_copy_singular)
			      ._M_iterator(*this, "this")
			      ._M_iterator(__x, "other"));
	auto __cont = __x._M_sequence;
	__x._M_detach();
	std::swap(base(), __x.base());
	_M_attach(__cont);
      }

      /**
       *  @brief Converting constructor from a mutable iterator to a
       *  constant iterator.
      */
      template<typename _MutableIterator>
	_Safe_local_iterator(
	  const _Safe_local_iterator<_MutableIterator,
	  typename __gnu_cxx::__enable_if<std::__are_same<
	      _MutableIterator,
	      typename _Sequence::local_iterator::iterator_type>::__value,
					  _Sequence>::__type>& __x)
	: _Iter_base(__x.base())
	{
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // DR 408. Is vector<reverse_iterator<char*> > forbidden?
	  _GLIBCXX_DEBUG_VERIFY(!__x._M_singular()
				|| __x.base() == _Iterator(),
				_M_message(__msg_init_const_singular)
				._M_iterator(*this, "this")
				._M_iterator(__x, "other"));
	  _M_attach(__x._M_sequence);
	}

      /**
       * @brief Copy assignment.
       */
      _Safe_local_iterator&
      operator=(const _Safe_local_iterator& __x)
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// DR 408. Is vector<reverse_iterator<char*> > forbidden?
	_GLIBCXX_DEBUG_VERIFY(!__x._M_singular()
			      || __x.base() == _Iterator(),
			      _M_message(__msg_copy_singular)
			      ._M_iterator(*this, "this")
			      ._M_iterator(__x, "other"));

	if (this->_M_sequence && this->_M_sequence == __x._M_sequence)
	  {
	    __gnu_cxx::__scoped_lock __l(this->_M_get_mutex());
	    base() = __x.base();
	    _M_version = __x._M_sequence->_M_version;
	  }
	else
	  {
	    _M_detach();
	    base() = __x.base();
	    _M_attach(__x._M_sequence);
	  }

	return *this;
      }

      /**
       * @brief Move assignment.
       * @post __x is singular and unattached
       */
      _Safe_local_iterator&
      operator=(_Safe_local_iterator&& __x) noexcept
      {
	_GLIBCXX_DEBUG_VERIFY(this != &__x,
			      _M_message(__msg_self_move_assign)
			      ._M_iterator(*this, "this"));
	_GLIBCXX_DEBUG_VERIFY(!__x._M_singular()
			      || __x.base() == _Iterator(),
			      _M_message(__msg_copy_singular)
			      ._M_iterator(*this, "this")
			      ._M_iterator(__x, "other"));

	if (this->_M_sequence && this->_M_sequence == __x._M_sequence)
	  {
	    __gnu_cxx::__scoped_lock __l(this->_M_get_mutex());
	    base() = __x.base();
	    _M_version = __x._M_sequence->_M_version;
	  }
	else
	  {
	    _M_detach();
	    base() = __x.base();
	    _M_attach(__x._M_sequence);
	  }

	__x._M_detach();
	__x.base() = _Iterator();
	return *this;
      }

      /**
       *  @brief Iterator dereference.
       *  @pre iterator is dereferenceable
       */
      reference
      operator*() const
      {
	_GLIBCXX_DEBUG_VERIFY(this->_M_dereferenceable(),
			      _M_message(__msg_bad_deref)
			      ._M_iterator(*this, "this"));
	return *base();
      }

      /**
       *  @brief Iterator dereference.
       *  @pre iterator is dereferenceable
       */
      pointer
      operator->() const
      {
	_GLIBCXX_DEBUG_VERIFY(this->_M_dereferenceable(),
			      _M_message(__msg_bad_deref)
			      ._M_iterator(*this, "this"));
	return base().operator->();
      }

      // ------ Input iterator requirements ------
      /**
       *  @brief Iterator preincrement
       *  @pre iterator is incrementable
       */
      _Safe_local_iterator&
      operator++()
      {
	_GLIBCXX_DEBUG_VERIFY(this->_M_incrementable(),
			      _M_message(__msg_bad_inc)
			      ._M_iterator(*this, "this"));
	__gnu_cxx::__scoped_lock __l(this->_M_get_mutex());
	++base();
	return *this;
      }

      /**
       *  @brief Iterator postincrement
       *  @pre iterator is incrementable
       */
      _Safe_local_iterator
      operator++(int)
      {
	_GLIBCXX_DEBUG_VERIFY(this->_M_incrementable(),
			      _M_message(__msg_bad_inc)
			      ._M_iterator(*this, "this"));
	__gnu_cxx::__scoped_lock __l(this->_M_get_mutex());
	return _Safe_local_iterator(base()++, this->_M_sequence,
				    _Attach_single());
      }

      // ------ Utilities ------

      /// Determine if this is a constant iterator.
      static bool
      _S_constant()
      {
	return std::__are_same<_Const_local_iterator,
			       _Safe_local_iterator>::__value;
      }

      /**
       * @brief Return the underlying iterator
       */
      _Iterator&
      base() noexcept { return *this; }

      const _Iterator&
      base() const noexcept { return *this; }

      /**
       * @brief Return the bucket
       */
      size_type
      bucket() const { return base()._M_get_bucket(); }

      /**
       * @brief Conversion to underlying non-debug iterator to allow
       * better interaction with non-debug containers.
       */
      operator _Iterator() const { return *this; }

      /** Attach iterator to the given sequence. */
      void
      _M_attach(_Safe_sequence_base* __seq)
      { _Safe_base::_M_attach(__seq, _S_constant()); }

      /** Likewise, but not thread-safe. */
      void
      _M_attach_single(_Safe_sequence_base* __seq)
      { _Safe_base::_M_attach_single(__seq, _S_constant()); }

      /// Is the iterator dereferenceable?
      bool
      _M_dereferenceable() const
      { return !this->_M_singular() && !_M_is_end(); }

      /// Is the iterator incrementable?
      bool
      _M_incrementable() const
      { return !this->_M_singular() && !_M_is_end(); }

      // Is the iterator range [*this, __rhs) valid?
      bool
      _M_valid_range(const _Safe_local_iterator& __rhs,
		     std::pair<difference_type,
			       _Distance_precision>& __dist_info) const;

      // The sequence this iterator references.
      typename
      __gnu_cxx::__conditional_type<std::__are_same<_Const_local_iterator,
						    _Safe_local_iterator>::__value,
				    const _Sequence*,
				    _Sequence*>::__type
      _M_get_sequence() const
      { return static_cast<_Sequence*>(_M_sequence); }

      /// Is this iterator equal to the sequence's begin(bucket) iterator?
      bool _M_is_begin() const
      { return base() == _M_get_sequence()->_M_base().begin(bucket()); }

      /// Is this iterator equal to the sequence's end(bucket) iterator?
      bool _M_is_end() const
      { return base() == _M_get_sequence()->_M_base().end(bucket()); }

      /// Is this iterator part of the same bucket as the other one?
      template<typename _Other>
	bool
	_M_in_same_bucket(const _Safe_local_iterator<_Other,
						     _Sequence>& __other) const
	{ return bucket() == __other.bucket(); }
    };

  template<typename _IteratorL, typename _IteratorR, typename _Sequence>
    inline bool
    operator==(const _Safe_local_iterator<_IteratorL, _Sequence>& __lhs,
	       const _Safe_local_iterator<_IteratorR, _Sequence>& __rhs)
    {
      _GLIBCXX_DEBUG_VERIFY(!__lhs._M_singular() && !__rhs._M_singular(),
			    _M_message(__msg_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_can_compare(__rhs),
			    _M_message(__msg_compare_different)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_in_same_bucket(__rhs),
			    _M_message(__msg_local_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      return __lhs.base() == __rhs.base();
    }

  template<typename _Iterator, typename _Sequence>
    inline bool
    operator==(const _Safe_local_iterator<_Iterator, _Sequence>& __lhs,
	       const _Safe_local_iterator<_Iterator, _Sequence>& __rhs)
    {
      _GLIBCXX_DEBUG_VERIFY(!__lhs._M_singular() && !__rhs._M_singular(),
			    _M_message(__msg_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_can_compare(__rhs),
			    _M_message(__msg_compare_different)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_in_same_bucket(__rhs),
			    _M_message(__msg_local_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      return __lhs.base() == __rhs.base();
    }

  template<typename _IteratorL, typename _IteratorR, typename _Sequence>
    inline bool
    operator!=(const _Safe_local_iterator<_IteratorL, _Sequence>& __lhs,
	       const _Safe_local_iterator<_IteratorR, _Sequence>& __rhs)
    {
      _GLIBCXX_DEBUG_VERIFY(! __lhs._M_singular() && ! __rhs._M_singular(),
			    _M_message(__msg_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_can_compare(__rhs),
			    _M_message(__msg_compare_different)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_in_same_bucket(__rhs),
			    _M_message(__msg_local_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      return __lhs.base() != __rhs.base();
    }

  template<typename _Iterator, typename _Sequence>
    inline bool
    operator!=(const _Safe_local_iterator<_Iterator, _Sequence>& __lhs,
	       const _Safe_local_iterator<_Iterator, _Sequence>& __rhs)
    {
      _GLIBCXX_DEBUG_VERIFY(!__lhs._M_singular() && !__rhs._M_singular(),
			    _M_message(__msg_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_can_compare(__rhs),
			    _M_message(__msg_compare_different)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      _GLIBCXX_DEBUG_VERIFY(__lhs._M_in_same_bucket(__rhs),
			    _M_message(__msg_local_iter_compare_bad)
			    ._M_iterator(__lhs, "lhs")
			    ._M_iterator(__rhs, "rhs"));
      return __lhs.base() != __rhs.base();
    }

  /** Safe local iterators know if they are dereferenceable. */
  template<typename _Iterator, typename _Sequence>
    inline bool
    __check_dereferenceable(const _Safe_local_iterator<_Iterator,
						       _Sequence>& __x)
    { return __x._M_dereferenceable(); }

  /** Safe local iterators know how to check if they form a valid range. */
  template<typename _Iterator, typename _Sequence>
    inline bool
    __valid_range(const _Safe_local_iterator<_Iterator, _Sequence>& __first,
		  const _Safe_local_iterator<_Iterator, _Sequence>& __last,
		  typename _Distance_traits<_Iterator>::__type& __dist_info)
    { return __first._M_valid_range(__last, __dist_info); }

  /** Safe local iterators need a special method to get distance between each
      other. */
  template<typename _Iterator, typename _Sequence>
    inline std::pair<typename std::iterator_traits<_Iterator>::difference_type,
		     _Distance_precision>
    __get_distance(const _Safe_local_iterator<_Iterator, _Sequence>& __first,
		   const _Safe_local_iterator<_Iterator, _Sequence>& __last,
		   std::input_iterator_tag)
    {
      if (__first.base() == __last.base())
	return { 0, __dp_exact };

      if (__first._M_is_begin())
	{
	  if (__last._M_is_end())
	    return
	      {
		__first._M_get_sequence()->bucket_size(__first.bucket()),
		__dp_exact
	      };

	  return { 1, __dp_sign };
	}

      if (__first._M_is_end())
	{
	  if (__last._M_is_begin())
	    return
	      {
		-__first._M_get_sequence()->bucket_size(__first.bucket()),
		__dp_exact
	      };

	  return { -1, __dp_sign };
	}

      if (__last._M_is_begin())
	return { -1, __dp_sign };

      if (__last._M_is_end())
	return { 1, __dp_sign };

      return { 1, __dp_equality };
    }

#if __cplusplus < 201103L
  template<typename _Iterator, typename _Sequence>
    struct _Unsafe_type<_Safe_local_iterator<_Iterator, _Sequence> >
    { typedef _Iterator _Type; };
#endif

  template<typename _Iterator, typename _Sequence>
    inline _Iterator
    __unsafe(const _Safe_local_iterator<_Iterator, _Sequence>& __it)
    { return __it.base(); }

} // namespace __gnu_debug

#include <debug/safe_local_iterator.tcc>

#endif
