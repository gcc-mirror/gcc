// -*- C++ -*-

// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file parallel/losertree.h
*  @brief Many generic loser tree variants.
*  This file is a GNU parallel extension to the Standard C++ Library.
*/

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_LOSERTREE_H
#define _GLIBCXX_PARALLEL_LOSERTREE_H 1

#include <functional>

#include <bits/stl_algobase.h>
#include <parallel/features.h>
#include <parallel/base.h>

namespace __gnu_parallel
{

/**
 * @brief Guarded loser/tournament tree.
 *
 * The smallest element is at the top.
 *
 * Guarding is done explicitly through one flag _M_sup per element,
 * inf is not needed due to a better initialization routine.  This
 * is a well-performing variant.
 *
 * @param _Tp the element type
 * @param _Compare the comparator to use, defaults to std::less<_Tp>
 */
template<typename _Tp, typename _Compare>
class LoserTreeBase
{
protected:
  /** @brief Internal representation of a LoserTree element. */
  struct _Loser
  {
    /** @brief flag, true iff this is a "maximum" __sentinel. */
    bool _M_sup;
    /** @brief __index of the _M_source __sequence. */
    int _M_source;
    /** @brief _M_key of the element in the LoserTree. */
    _Tp _M_key;
  };

  unsigned int __ik, __k, __offset;

  /** log_2{__k} */
  unsigned int _M_log_k;

  /** @brief LoserTree __elements. */
  _Loser* __losers;

  /** @brief _Compare to use. */
  _Compare __comp;

  /**
   * @brief State flag that determines whether the LoserTree is empty.
   *
   * Only used for building the LoserTree.
   */
  bool __first_insert;

public:
  /**
   * @brief The constructor.
   *
   * @param _k The number of sequences to merge.
   * @param _comp The comparator to use.
   */
  LoserTreeBase(unsigned int _k, _Compare _comp)
  : __comp(_comp)
  {
    __ik = _k;

    // Compute log_2{__k} for the _Loser Tree
    _M_log_k = __log2(__ik - 1) + 1;

    // Next greater power of 2.
    __k = 1 << _M_log_k;
    __offset = __k;

    // Avoid default-constructing __losers[]._M_key
    __losers = static_cast<_Loser*>(::operator new(2 * __k * sizeof(_Loser)));
    for (unsigned int __i = __ik - 1; __i < __k; ++__i)
      __losers[__i + __k]._M_sup = true;

    __first_insert = true;
  }

  /**
   * @brief The destructor.
   */
  ~LoserTreeBase()
  { ::operator delete(__losers); }

  /**
   * @brief Initializes the sequence "_M_source" with the element "_M_key".
   *
   * @param _M_key the element to insert
   * @param _M_source __index of the _M_source __sequence
   * @param _M_sup flag that determines whether the value to insert is an
   *   explicit __supremum.
   */
  inline void
  __insert_start(const _Tp& _M_key, int _M_source, bool _M_sup)
  {
    unsigned int __pos = __k + _M_source;

    if(__first_insert)
      {
        // Construct all keys, so we can easily deconstruct them.
        for (unsigned int __i = 0; __i < (2 * __k); ++__i)
          new(&(__losers[__i]._M_key)) _Tp(_M_key);
        __first_insert = false;
      }
    else
      new(&(__losers[__pos]._M_key)) _Tp(_M_key);

    __losers[__pos]._M_sup = _M_sup;
    __losers[__pos]._M_source = _M_source;
  }

  /**
   * @return the index of the sequence with the smallest element.
   */
  int __get_min_source()
  { return __losers[0]._M_source; }
};

/**
 * @brief Stable LoserTree variant.
 *
 * Provides the stable implementations of insert_start, __init_winner,
 * __init and __delete_min_insert.
 *
 * Unstable variant is done using partial specialisation below.
 */
template<bool __stable/* default == true */, typename _Tp, typename _Compare>
class LoserTree : public LoserTreeBase<_Tp, _Compare>
{
  typedef LoserTreeBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;
  using Base::__first_insert;

public:
  LoserTree(unsigned int _k, _Compare _comp)
  : Base::LoserTreeBase(_k, _comp)
  {}

  unsigned int
  __init_winner(unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);
        if (__losers[__right]._M_sup
            || (!__losers[__left]._M_sup
              && !__comp(__losers[__right]._M_key, __losers[__left]._M_key)))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  void __init()
  { __losers[0] = __losers[__init_winner(1)]; }

  /**
   * @brief Delete the smallest element and insert a new element from
   *   the previously smallest element's sequence.
   *
   * This implementation is stable.
   */
  // Do not pass a const reference since _M_key will be used as local variable.
  void __delete_min_insert(_Tp _M_key, bool _M_sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by _M_source.
        if ((_M_sup && (!__losers[__pos]._M_sup || __losers[__pos]._M_source < _M_source))
              || (!_M_sup && !__losers[__pos]._M_sup
                && ((__comp(__losers[__pos]._M_key, _M_key))
                  || (!__comp(_M_key, __losers[__pos]._M_key)
                    && __losers[__pos]._M_source < _M_source))))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_sup, _M_sup);
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_key, _M_key);
          }
      }

    __losers[0]._M_sup = _M_sup;
    __losers[0]._M_source = _M_source;
    __losers[0]._M_key = _M_key;
  }
};

/**
 * @brief Unstable LoserTree variant.
 *
 * Stability (non-stable here) is selected with partial specialization.
 */
template<typename _Tp, typename _Compare>
class LoserTree</* __stable == */false, _Tp, _Compare> :
    public LoserTreeBase<_Tp, _Compare>
{
  typedef LoserTreeBase<_Tp, _Compare> Base;
  using Base::_M_log_k;
  using Base::__k;
  using Base::__losers;
  using Base::__first_insert;

public:
  LoserTree(unsigned int _k, _Compare _comp)
  : Base::LoserTreeBase(_k, _comp)
  {}

  /**
   * Computes the winner of the competition at __position "__root".
   *
   * Called recursively (starting at 0) to build the initial tree.
   *
   * @param __root __index of the "game" to start.
   */
  unsigned int
  __init_winner (unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);
        if (__losers[__right]._M_sup ||
            (!__losers[__left]._M_sup
              && !__comp(__losers[__right]._M_key, __losers[__left]._M_key)))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  inline void
  __init()
  { __losers[0] = __losers[__init_winner(1)]; }

  /**
   * Delete the _M_key smallest element and insert the element _M_key instead.
   *
   * @param _M_key the _M_key to insert
   * @param _M_sup true iff _M_key is an explicitly marked supremum
   */
  // Do not pass a const reference since _M_key will be used as local variable.
  inline void
  __delete_min_insert(_Tp _M_key, bool _M_sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
    {
        // The smaller one gets promoted.
      if (_M_sup || (!__losers[__pos]._M_sup && __comp(__losers[__pos]._M_key, _M_key)))
      {
            // The other one is smaller.
        std::swap(__losers[__pos]._M_sup, _M_sup);
        std::swap(__losers[__pos]._M_source, _M_source);
        std::swap(__losers[__pos]._M_key, _M_key);
      }
    }

    __losers[0]._M_sup = _M_sup;
    __losers[0]._M_source = _M_source;
    __losers[0]._M_key = _M_key;
  }
};


/**
 * @brief Base class of _Loser Tree implementation using pointers.
 */
template<typename _Tp, typename _Compare>
class _LoserTreePointerBase
{
protected:
  /** @brief Internal representation of LoserTree __elements. */
  struct _Loser
  {
    bool _M_sup;
    int _M_source;
    const _Tp* _M_keyp;
  };

  unsigned int __ik, __k, __offset;
  _Loser* __losers;
  _Compare __comp;

public:
  _LoserTreePointerBase(unsigned int _k, _Compare _comp = std::less<_Tp>())
    : __comp(_comp)
  {
    __ik = _k;

    // Next greater power of 2.
    __k = 1 << (__log2(__ik - 1) + 1);
    __offset = __k;
    __losers = new _Loser[__k * 2];
    for (unsigned int __i = __ik - 1; __i < __k; __i++)
      __losers[__i + __k]._M_sup = true;
  }

  ~_LoserTreePointerBase()
  { ::operator delete[](__losers); }

  int __get_min_source()
  { return __losers[0]._M_source; }

  void __insert_start(const _Tp& _M_key, int _M_source, bool _M_sup)
  {
    unsigned int __pos = __k + _M_source;

    __losers[__pos]._M_sup = _M_sup;
    __losers[__pos]._M_source = _M_source;
    __losers[__pos]._M_keyp = &_M_key;
  }
};

/**
 * @brief Stable LoserTree implementation.
 *
 * The unstable variant is implemented using partial instantiation below.
 */
template<bool __stable/* default == true */, typename _Tp, typename _Compare>
class _LoserTreePointer : public _LoserTreePointerBase<_Tp, _Compare>
{
  typedef _LoserTreePointerBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;

public:
  _LoserTreePointer(unsigned int _k, _Compare _comp = std::less<_Tp>())
    : Base::_LoserTreePointerBase(_k, _comp)
  {}

  unsigned int
  __init_winner(unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);
        if (__losers[__right]._M_sup
            || (!__losers[__left]._M_sup && !__comp(*__losers[__right]._M_keyp,
                                          *__losers[__left]._M_keyp)))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  void __init()
  { __losers[0] = __losers[__init_winner(1)]; }

  void __delete_min_insert(const _Tp& _M_key, bool _M_sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    const _Tp* _M_keyp = &_M_key;
    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by _M_source.
        if ((_M_sup && (!__losers[__pos]._M_sup || __losers[__pos]._M_source < _M_source)) ||
              (!_M_sup && !__losers[__pos]._M_sup &&
              ((__comp(*__losers[__pos]._M_keyp, *_M_keyp)) ||
                (!__comp(*_M_keyp, *__losers[__pos]._M_keyp)
                && __losers[__pos]._M_source < _M_source))))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_sup, _M_sup);
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_keyp, _M_keyp);
          }
      }

    __losers[0]._M_sup = _M_sup;
    __losers[0]._M_source = _M_source;
    __losers[0]._M_keyp = _M_keyp;
  }
};

/**
 * @brief Unstable LoserTree implementation.
 *
 * The stable variant is above.
 */
template<typename _Tp, typename _Compare>
class _LoserTreePointer</* __stable == */false, _Tp, _Compare> :
    public _LoserTreePointerBase<_Tp, _Compare>
{
  typedef _LoserTreePointerBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;

public:
  _LoserTreePointer(unsigned int _k, _Compare _comp = std::less<_Tp>())
    : Base::_LoserTreePointerBase(_k, _comp)
  {}

  unsigned int
  __init_winner(unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);
        if (__losers[__right]._M_sup
              || (!__losers[__left]._M_sup
                && !__comp(*__losers[__right]._M_keyp, *__losers[__left]._M_keyp)))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  void __init()
  { __losers[0] = __losers[__init_winner(1)]; }

  void __delete_min_insert(const _Tp& _M_key, bool _M_sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    const _Tp* _M_keyp = &_M_key;
    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted.
        if (_M_sup || (!__losers[__pos]._M_sup && __comp(*__losers[__pos]._M_keyp, *_M_keyp)))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_sup, _M_sup);
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_keyp, _M_keyp);
          }
      }

    __losers[0]._M_sup = _M_sup;
    __losers[0]._M_source = _M_source;
    __losers[0]._M_keyp = _M_keyp;
  }
};

/** @brief Base class for unguarded LoserTree implementation.
 * 
 * The whole element is copied into the tree structure.
 *
 * No guarding is done, therefore not a single input sequence must
 * run empty.  Unused __sequence heads are marked with a sentinel which
 * is &gt; all elements that are to be merged.
 *
 * This is a very fast variant.
 */
template<typename _Tp, typename _Compare>
class _LoserTreeUnguardedBase
{
protected:
  struct _Loser
  {
    int _M_source;
    _Tp _M_key;
  };

  unsigned int __ik, __k, __offset;
  _Loser* __losers;
  _Compare __comp;

public:
  inline
  _LoserTreeUnguardedBase(unsigned int _k, const _Tp _sentinel,
                         _Compare _comp = std::less<_Tp>())
    : __comp(_comp)
  {
    __ik = _k;

    // Next greater power of 2.
    __k = 1 << (__log2(__ik - 1) + 1);
    __offset = __k;
    // Avoid default-constructing __losers[]._M_key
    __losers = static_cast<_Loser*>(::operator new(2 * __k * sizeof(_Loser)));

    for (unsigned int __i = __k + __ik - 1; __i < (2 * __k); ++__i)
      {
        __losers[__i]._M_key = _sentinel;
        __losers[__i]._M_source = -1;
      }
  }

  inline ~_LoserTreeUnguardedBase()
  { ::operator delete(__losers); }

  inline int
  __get_min_source()
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif
    return __losers[0]._M_source;
  }

  inline void
  __insert_start(const _Tp& _M_key, int _M_source, bool)
  {
    unsigned int __pos = __k + _M_source;

    new(&(__losers[__pos]._M_key)) _Tp(_M_key);
    __losers[__pos]._M_source = _M_source;
  }
};

/**
 * @brief Stable implementation of unguarded LoserTree.
 *
 * Unstable variant is selected below with partial specialization.
 */
template<bool __stable/* default == true */, typename _Tp, typename _Compare>
class _LoserTreeUnguarded : public _LoserTreeUnguardedBase<_Tp, _Compare>
{
  typedef _LoserTreeUnguardedBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;

public:
  _LoserTreeUnguarded(unsigned int _k, const _Tp _sentinel,
                     _Compare _comp = std::less<_Tp>())
    : Base::_LoserTreeUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  __init_winner(unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);
        if (!__comp(__losers[__right]._M_key, __losers[__left]._M_key))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  inline void
  __init()
  {
    __losers[0] = __losers[__init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif
  }

  // Do not pass a const reference since _M_key will be used as local variable.
  inline void
  __delete_min_insert(_Tp _M_key, bool)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by _M_source.
        if (__comp(__losers[__pos]._M_key, _M_key)
              || (!__comp(_M_key, __losers[__pos]._M_key) && __losers[__pos]._M_source < _M_source))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_key, _M_key);
          }
      }

    __losers[0]._M_source = _M_source;
    __losers[0]._M_key = _M_key;
  }
};

/**
 * @brief Non-Stable implementation of unguarded LoserTree.
 *
 * Stable implementation is above.
 */
template<typename _Tp, typename _Compare>
class _LoserTreeUnguarded</* __stable == */false, _Tp, _Compare> :
    public _LoserTreeUnguardedBase<_Tp, _Compare>
{
  typedef _LoserTreeUnguardedBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;

public:
  _LoserTreeUnguarded(unsigned int _k, const _Tp _sentinel,
                     _Compare _comp = std::less<_Tp>())
    : Base::_LoserTreeUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  __init_winner (unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);

#if _GLIBCXX_ASSERTIONS
        // If __left one is sentinel then __right one must be, too.
        if (__losers[__left]._M_source == -1)
          _GLIBCXX_PARALLEL_ASSERT(__losers[__right]._M_source == -1);
#endif

        if (!__comp(__losers[__right]._M_key, __losers[__left]._M_key))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  inline void
  __init()
  {
    __losers[0] = __losers[__init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif
  }

  // Do not pass a const reference since _M_key will be used as local variable.
  inline void
  __delete_min_insert(_Tp _M_key, bool)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted.
        if (__comp(__losers[__pos]._M_key, _M_key))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_key, _M_key);
          }
      }

    __losers[0]._M_source = _M_source;
    __losers[0]._M_key = _M_key;
  }
};

/** @brief Unguarded loser tree, keeping only pointers to the
* __elements in the tree structure.
*
*  No guarding is done, therefore not a single input sequence must
*  run empty.  This is a very fast variant.
*/
template<typename _Tp, typename _Compare>
class LoserTreePointerUnguardedBase
{
protected:
  struct _Loser
  {
    int _M_source;
    const _Tp* _M_keyp;
  };

  unsigned int __ik, __k, __offset;
  _Loser* __losers;
  _Compare __comp;

public:

  inline
  LoserTreePointerUnguardedBase(unsigned int _k, const _Tp& _sentinel,
      _Compare _comp = std::less<_Tp>())
    : __comp(_comp)
  {
    __ik = _k;

    // Next greater power of 2.
    __k = 1 << (__log2(__ik - 1) + 1);
    __offset = __k;
    // Avoid default-constructing __losers[]._M_key
    __losers = new _Loser[2 * __k];

    for (unsigned int __i = __k + __ik - 1; __i < (2 * __k); ++__i)
      {
        __losers[__i]._M_keyp = &_sentinel;
        __losers[__i]._M_source = -1;
      }
  }

  inline ~LoserTreePointerUnguardedBase()
  { delete[] __losers; }

  inline int
  __get_min_source()
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif
    return __losers[0]._M_source;
  }

  inline void
  __insert_start(const _Tp& _M_key, int _M_source, bool)
  {
    unsigned int __pos = __k + _M_source;

    __losers[__pos]._M_keyp = &_M_key;
    __losers[__pos]._M_source = _M_source;
  }
};

/**
 * @brief Stable unguarded LoserTree variant storing pointers.
 *
 * Unstable variant is implemented below using partial specialization.
 */
template<bool __stable/* default == true */, typename _Tp, typename _Compare>
class LoserTreePointerUnguarded :
    public LoserTreePointerUnguardedBase<_Tp, _Compare>
{
  typedef LoserTreePointerUnguardedBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;

public:
  LoserTreePointerUnguarded(unsigned int _k, const _Tp& _sentinel,
      _Compare _comp = std::less<_Tp>())
    : Base::LoserTreePointerUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  __init_winner(unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);
        if (!__comp(*__losers[__right]._M_keyp, *__losers[__left]._M_keyp))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  inline void
  __init()
  {
    __losers[0] = __losers[__init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif
  }

  inline void
  __delete_min_insert(const _Tp& _M_key, bool _M_sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    const _Tp* _M_keyp = &_M_key;
    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by _M_source.
        if (__comp(*__losers[__pos]._M_keyp, *_M_keyp)
          || (!__comp(*_M_keyp, *__losers[__pos]._M_keyp) && __losers[__pos]._M_source < _M_source))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_keyp, _M_keyp);
          }
      }

    __losers[0]._M_source = _M_source;
    __losers[0]._M_keyp = _M_keyp;
  }
};

/**
 * @brief Unstable unguarded LoserTree variant storing pointers.
 *
 * Stable variant is above.
 */
template<typename _Tp, typename _Compare>
class LoserTreePointerUnguarded</* __stable == */false, _Tp, _Compare> :
    public LoserTreePointerUnguardedBase<_Tp, _Compare>
{
  typedef LoserTreePointerUnguardedBase<_Tp, _Compare> Base;
  using Base::__k;
  using Base::__losers;

public:
  LoserTreePointerUnguarded(unsigned int _k, const _Tp& _sentinel,
      _Compare _comp = std::less<_Tp>())
    : Base::LoserTreePointerUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  __init_winner(unsigned int __root)
  {
    if (__root >= __k)
      {
        return __root;
      }
    else
      {
        unsigned int __left = __init_winner (2 * __root);
        unsigned int __right = __init_winner (2 * __root + 1);

#if _GLIBCXX_ASSERTIONS
        // If __left one is sentinel then __right one must be, too.
        if (__losers[__left]._M_source == -1)
          _GLIBCXX_PARALLEL_ASSERT(__losers[__right]._M_source == -1);
#endif

        if (!__comp(*__losers[__right]._M_keyp, *__losers[__left]._M_keyp))
          {
            // Left one is less or equal.
            __losers[__root] = __losers[__right];
            return __left;
          }
        else
          {
            // Right one is less.
            __losers[__root] = __losers[__left];
            return __right;
          }
      }
  }

  inline void
  __init()
  {
    __losers[0] = __losers[__init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif
  }

  inline void
  __delete_min_insert(const _Tp& _M_key, bool _M_sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(__losers[0]._M_source != -1);
#endif

    const _Tp* _M_keyp = &_M_key;
    int _M_source = __losers[0]._M_source;
    for (unsigned int __pos = (__k + _M_source) / 2; __pos > 0; __pos /= 2)
      {
        // The smaller one gets promoted.
        if (__comp(*(__losers[__pos]._M_keyp), *_M_keyp))
          {
            // The other one is smaller.
            std::swap(__losers[__pos]._M_source, _M_source);
            std::swap(__losers[__pos]._M_keyp, _M_keyp);
          }
      }

    __losers[0]._M_source = _M_source;
    __losers[0]._M_keyp = _M_keyp;
  }
};

} // namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_LOSERTREE_H */
