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
 * Guarding is done explicitly through one flag sup per element,
 * inf is not needed due to a better initialization routine.  This
 * is a well-performing variant.
 *
 * @param T the element type
 * @param Comparator the comparator to use, defaults to std::less<T>
 */
template<typename T, typename Comparator>
class LoserTreeBase
{
protected:
  /** @brief Internal representation of a LoserTree element. */
  struct Loser
  {
    /** @brief flag, true iff this is a "maximum" sentinel. */
    bool sup;
    /** @brief index of the source sequence. */
    int source;
    /** @brief key of the element in the LoserTree. */
    T key;
  };

  unsigned int ik, k, offset;

  /** log_2{k} */
  unsigned int _M_log_k;

  /** @brief LoserTree elements. */
  Loser* losers;

  /** @brief Comparator to use. */
  Comparator comp;

  /**
   * @brief State flag that determines whether the LoserTree is empty.
   *
   * Only used for building the LoserTree.
   */
  bool first_insert;

public:
  /**
   * @brief The constructor.
   *
   * @param _k The number of sequences to merge.
   * @param _comp The comparator to use.
   */
  LoserTreeBase(unsigned int _k, Comparator _comp)
  : comp(_comp)
  {
    ik = _k;

    // Compute log_2{k} for the Loser Tree
    _M_log_k = __log2(ik - 1) + 1;

    // Next greater power of 2.
    k = 1 << _M_log_k;
    offset = k;

    // Avoid default-constructing losers[].key
    losers = static_cast<Loser*>(::operator new(2 * k * sizeof(Loser)));
    for (unsigned int i = ik - 1; i < k; ++i)
      losers[i + k].sup = true;

    first_insert = true;
  }

  /**
   * @brief The destructor.
   */
  ~LoserTreeBase()
  { ::operator delete(losers); }

  /**
   * @brief Initializes the sequence "source" with the element "key".
   *
   * @param key the element to insert
   * @param source index of the source sequence
   * @param sup flag that determines whether the value to insert is an
   *   explicit supremum.
   */
  inline void
  insert_start(const T& key, int source, bool sup)
  {
    unsigned int pos = k + source;

    if(first_insert)
      {
        // Construct all keys, so we can easily deconstruct them.
        for (unsigned int i = 0; i < (2 * k); ++i)
          new(&(losers[i].key)) T(key);
        first_insert = false;
      }
    else
      new(&(losers[pos].key)) T(key);

    losers[pos].sup = sup;
    losers[pos].source = source;
  }

  /**
   * @return the index of the sequence with the smallest element.
   */
  int get_min_source()
  { return losers[0].source; }
};

/**
 * @brief Stable LoserTree variant.
 *
 * Provides the stable implementations of insert_start, init_winner,
 * init and delete_min_insert.
 *
 * Unstable variant is done using partial specialisation below.
 */
template<bool stable/* default == true */, typename T, typename Comparator>
class LoserTree : public LoserTreeBase<T, Comparator>
{
  typedef LoserTreeBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;
  using Base::first_insert;

public:
  LoserTree(unsigned int _k, Comparator _comp)
  : Base::LoserTreeBase(_k, _comp)
  {}

  unsigned int
  init_winner(unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);
        if (losers[right].sup
            || (!losers[left].sup
              && !comp(losers[right].key, losers[left].key)))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  void init()
  { losers[0] = losers[init_winner(1)]; }

  /**
   * @brief Delete the smallest element and insert a new element from
   *   the previously smallest element's sequence.
   *
   * This implementation is stable.
   */
  // Do not pass a const reference since key will be used as local variable.
  void delete_min_insert(T key, bool sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by source.
        if ((sup && (!losers[pos].sup || losers[pos].source < source))
              || (!sup && !losers[pos].sup
                && ((comp(losers[pos].key, key))
                  || (!comp(key, losers[pos].key)
                    && losers[pos].source < source))))
          {
            // The other one is smaller.
            std::swap(losers[pos].sup, sup);
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].key, key);
          }
      }

    losers[0].sup = sup;
    losers[0].source = source;
    losers[0].key = key;
  }
};

/**
 * @brief Unstable LoserTree variant.
 *
 * Stability (non-stable here) is selected with partial specialization.
 */
template<typename T, typename Comparator>
class LoserTree</* stable == */false, T, Comparator> :
    public LoserTreeBase<T, Comparator>
{
  typedef LoserTreeBase<T, Comparator> Base;
  using Base::_M_log_k;
  using Base::k;
  using Base::losers;
  using Base::first_insert;

public:
  LoserTree(unsigned int _k, Comparator _comp)
  : Base::LoserTreeBase(_k, _comp)
  {}

  /**
   * Computes the winner of the competition at position "root".
   *
   * Called recursively (starting at 0) to build the initial tree.
   *
   * @param root index of the "game" to start.
   */
  unsigned int
  init_winner (unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);
        if (losers[right].sup ||
            (!losers[left].sup
              && !comp(losers[right].key, losers[left].key)))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  inline void
  init()
  { losers[0] = losers[init_winner(1)]; }

  /**
   * Delete the key smallest element and insert the element key instead.
   *
   * @param key the key to insert
   * @param sup true iff key is an explicitly marked supremum
   */
  // Do not pass a const reference since key will be used as local variable.
  inline void
  delete_min_insert(T key, bool sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
    {
        // The smaller one gets promoted.
      if (sup || (!losers[pos].sup && comp(losers[pos].key, key)))
      {
            // The other one is smaller.
        std::swap(losers[pos].sup, sup);
        std::swap(losers[pos].source, source);
        std::swap(losers[pos].key, key);
      }
    }

    losers[0].sup = sup;
    losers[0].source = source;
    losers[0].key = key;
  }
};


/**
 * @brief Base class of Loser Tree implementation using pointers.
 */
template<typename T, typename Comparator>
class LoserTreePointerBase
{
protected:
  /** @brief Internal representation of LoserTree elements. */
  struct Loser
  {
    bool sup;
    int source;
    const T* keyp;
  };

  unsigned int ik, k, offset;
  Loser* losers;
  Comparator comp;

public:
  LoserTreePointerBase(unsigned int _k, Comparator _comp = std::less<T>())
    : comp(_comp)
  {
    ik = _k;

    // Next greater power of 2.
    k = 1 << (__log2(ik - 1) + 1);
    offset = k;
    losers = new Loser[k * 2];
    for (unsigned int i = ik - 1; i < k; i++)
      losers[i + k].sup = true;
  }

  ~LoserTreePointerBase()
  { ::operator delete[](losers); }

  int get_min_source()
  { return losers[0].source; }

  void insert_start(const T& key, int source, bool sup)
  {
    unsigned int pos = k + source;

    losers[pos].sup = sup;
    losers[pos].source = source;
    losers[pos].keyp = &key;
  }
};

/**
 * @brief Stable LoserTree implementation.
 *
 * The unstable variant is implemented using partial instantiation below.
 */
template<bool stable/* default == true */, typename T, typename Comparator>
class LoserTreePointer : public LoserTreePointerBase<T, Comparator>
{
  typedef LoserTreePointerBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;

public:
  LoserTreePointer(unsigned int _k, Comparator _comp = std::less<T>())
    : Base::LoserTreePointerBase(_k, _comp)
  {}

  unsigned int
  init_winner(unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);
        if (losers[right].sup
            || (!losers[left].sup && !comp(*losers[right].keyp,
                                          *losers[left].keyp)))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  void init()
  { losers[0] = losers[init_winner(1)]; }

  void delete_min_insert(const T& key, bool sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    const T* keyp = &key;
    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by source.
        if ((sup && (!losers[pos].sup || losers[pos].source < source)) ||
              (!sup && !losers[pos].sup &&
              ((comp(*losers[pos].keyp, *keyp)) ||
                (!comp(*keyp, *losers[pos].keyp)
                && losers[pos].source < source))))
          {
            // The other one is smaller.
            std::swap(losers[pos].sup, sup);
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].keyp, keyp);
          }
      }

    losers[0].sup = sup;
    losers[0].source = source;
    losers[0].keyp = keyp;
  }
};

/**
 * @brief Unstable LoserTree implementation.
 *
 * The stable variant is above.
 */
template<typename T, typename Comparator>
class LoserTreePointer</* stable == */false, T, Comparator> :
    public LoserTreePointerBase<T, Comparator>
{
  typedef LoserTreePointerBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;

public:
  LoserTreePointer(unsigned int _k, Comparator _comp = std::less<T>())
    : Base::LoserTreePointerBase(_k, _comp)
  {}

  unsigned int
  init_winner(unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);
        if (losers[right].sup
              || (!losers[left].sup
                && !comp(*losers[right].keyp, *losers[left].keyp)))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  void init()
  { losers[0] = losers[init_winner(1)]; }

  void delete_min_insert(const T& key, bool sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    const T* keyp = &key;
    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted.
        if (sup || (!losers[pos].sup && comp(*losers[pos].keyp, *keyp)))
          {
            // The other one is smaller.
            std::swap(losers[pos].sup, sup);
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].keyp, keyp);
          }
      }

    losers[0].sup = sup;
    losers[0].source = source;
    losers[0].keyp = keyp;
  }
};

/** @brief Base class for unguarded LoserTree implementation.
 * 
 * The whole element is copied into the tree structure.
 *
 * No guarding is done, therefore not a single input sequence must
 * run empty.  Unused sequence heads are marked with a sentinel which
 * is &gt; all elements that are to be merged.
 *
 * This is a very fast variant.
 */
template<typename T, typename Comparator>
class LoserTreeUnguardedBase
{
protected:
  struct Loser
  {
    int source;
    T key;
  };

  unsigned int ik, k, offset;
  Loser* losers;
  Comparator comp;

public:
  inline
  LoserTreeUnguardedBase(unsigned int _k, const T _sentinel,
                         Comparator _comp = std::less<T>())
    : comp(_comp)
  {
    ik = _k;

    // Next greater power of 2.
    k = 1 << (__log2(ik - 1) + 1);
    offset = k;
    // Avoid default-constructing losers[].key
    losers = static_cast<Loser*>(::operator new(2 * k * sizeof(Loser)));

    for (unsigned int i = k + ik - 1; i < (2 * k); ++i)
      {
        losers[i].key = _sentinel;
        losers[i].source = -1;
      }
  }

  inline ~LoserTreeUnguardedBase()
  { ::operator delete(losers); }

  inline int
  get_min_source()
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif
    return losers[0].source;
  }

  inline void
  insert_start(const T& key, int source, bool)
  {
    unsigned int pos = k + source;

    new(&(losers[pos].key)) T(key);
    losers[pos].source = source;
  }
};

/**
 * @brief Stable implementation of unguarded LoserTree.
 *
 * Unstable variant is selected below with partial specialization.
 */
template<bool stable/* default == true */, typename T, typename Comparator>
class LoserTreeUnguarded : public LoserTreeUnguardedBase<T, Comparator>
{
  typedef LoserTreeUnguardedBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;

public:
  LoserTreeUnguarded(unsigned int _k, const T _sentinel,
                     Comparator _comp = std::less<T>())
    : Base::LoserTreeUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  init_winner(unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);
        if (!comp(losers[right].key, losers[left].key))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  inline void
  init()
  {
    losers[0] = losers[init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif
  }

  // Do not pass a const reference since key will be used as local variable.
  inline void
  delete_min_insert(T key, bool)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by source.
        if (comp(losers[pos].key, key)
              || (!comp(key, losers[pos].key) && losers[pos].source < source))
          {
            // The other one is smaller.
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].key, key);
          }
      }

    losers[0].source = source;
    losers[0].key = key;
  }
};

/**
 * @brief Non-Stable implementation of unguarded LoserTree.
 *
 * Stable implementation is above.
 */
template<typename T, typename Comparator>
class LoserTreeUnguarded</* stable == */false, T, Comparator> :
    public LoserTreeUnguardedBase<T, Comparator>
{
  typedef LoserTreeUnguardedBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;

public:
  LoserTreeUnguarded(unsigned int _k, const T _sentinel,
                     Comparator _comp = std::less<T>())
    : Base::LoserTreeUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  init_winner (unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);

#if _GLIBCXX_ASSERTIONS
        // If left one is sentinel then right one must be, too.
        if (losers[left].source == -1)
          _GLIBCXX_PARALLEL_ASSERT(losers[right].source == -1);
#endif

        if (!comp(losers[right].key, losers[left].key))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  inline void
  init()
  {
    losers[0] = losers[init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif
  }

  // Do not pass a const reference since key will be used as local variable.
  inline void
  delete_min_insert(T key, bool)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted.
        if (comp(losers[pos].key, key))
          {
            // The other one is smaller.
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].key, key);
          }
      }

    losers[0].source = source;
    losers[0].key = key;
  }
};

/** @brief Unguarded loser tree, keeping only pointers to the
* elements in the tree structure.
*
*  No guarding is done, therefore not a single input sequence must
*  run empty.  This is a very fast variant.
*/
template<typename T, typename Comparator>
class LoserTreePointerUnguardedBase
{
protected:
  struct Loser
  {
    int source;
    const T* keyp;
  };

  unsigned int ik, k, offset;
  Loser* losers;
  Comparator comp;

public:

  inline
  LoserTreePointerUnguardedBase(unsigned int _k, const T& _sentinel,
      Comparator _comp = std::less<T>())
    : comp(_comp)
  {
    ik = _k;

    // Next greater power of 2.
    k = 1 << (__log2(ik - 1) + 1);
    offset = k;
    // Avoid default-constructing losers[].key
    losers = new Loser[2 * k];

    for (unsigned int i = k + ik - 1; i < (2 * k); ++i)
      {
        losers[i].keyp = &_sentinel;
        losers[i].source = -1;
      }
  }

  inline ~LoserTreePointerUnguardedBase()
  { delete[] losers; }

  inline int
  get_min_source()
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif
    return losers[0].source;
  }

  inline void
  insert_start(const T& key, int source, bool)
  {
    unsigned int pos = k + source;

    losers[pos].keyp = &key;
    losers[pos].source = source;
  }
};

/**
 * @brief Stable unguarded LoserTree variant storing pointers.
 *
 * Unstable variant is implemented below using partial specialization.
 */
template<bool stable/* default == true */, typename T, typename Comparator>
class LoserTreePointerUnguarded :
    public LoserTreePointerUnguardedBase<T, Comparator>
{
  typedef LoserTreePointerUnguardedBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;

public:
  LoserTreePointerUnguarded(unsigned int _k, const T& _sentinel,
      Comparator _comp = std::less<T>())
    : Base::LoserTreePointerUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  init_winner(unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);
        if (!comp(*losers[right].keyp, *losers[left].keyp))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  inline void
  init()
  {
    losers[0] = losers[init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif
  }

  inline void
  delete_min_insert(const T& key, bool sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    const T* keyp = &key;
    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted, ties are broken by source.
        if (comp(*losers[pos].keyp, *keyp)
          || (!comp(*keyp, *losers[pos].keyp) && losers[pos].source < source))
          {
            // The other one is smaller.
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].keyp, keyp);
          }
      }

    losers[0].source = source;
    losers[0].keyp = keyp;
  }
};

/**
 * @brief Unstable unguarded LoserTree variant storing pointers.
 *
 * Stable variant is above.
 */
template<typename T, typename Comparator>
class LoserTreePointerUnguarded</* stable == */false, T, Comparator> :
    public LoserTreePointerUnguardedBase<T, Comparator>
{
  typedef LoserTreePointerUnguardedBase<T, Comparator> Base;
  using Base::k;
  using Base::losers;

public:
  LoserTreePointerUnguarded(unsigned int _k, const T& _sentinel,
      Comparator _comp = std::less<T>())
    : Base::LoserTreePointerUnguardedBase(_k, _sentinel, _comp)
  {}

  unsigned int
  init_winner(unsigned int root)
  {
    if (root >= k)
      {
        return root;
      }
    else
      {
        unsigned int left = init_winner (2 * root);
        unsigned int right = init_winner (2 * root + 1);

#if _GLIBCXX_ASSERTIONS
        // If left one is sentinel then right one must be, too.
        if (losers[left].source == -1)
          _GLIBCXX_PARALLEL_ASSERT(losers[right].source == -1);
#endif

        if (!comp(*losers[right].keyp, *losers[left].keyp))
          {
            // Left one is less or equal.
            losers[root] = losers[right];
            return left;
          }
        else
          {
            // Right one is less.
            losers[root] = losers[left];
            return right;
          }
      }
  }

  inline void
  init()
  {
    losers[0] = losers[init_winner(1)];

#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top at the beginning (0 sequences!)
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif
  }

  inline void
  delete_min_insert(const T& key, bool sup)
  {
#if _GLIBCXX_ASSERTIONS
    // no dummy sequence can ever be at the top!
    _GLIBCXX_PARALLEL_ASSERT(losers[0].source != -1);
#endif

    const T* keyp = &key;
    int source = losers[0].source;
    for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
      {
        // The smaller one gets promoted.
        if (comp(*(losers[pos].keyp), *keyp))
          {
            // The other one is smaller.
            std::swap(losers[pos].source, source);
            std::swap(losers[pos].keyp, keyp);
          }
      }

    losers[0].source = source;
    losers[0].keyp = keyp;
  }
};

} // namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_LOSERTREE_H */
