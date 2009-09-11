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

/** @file parallel/multiway_merge.h
*  @brief Implementation of sequential and parallel multiway merge.
*
*  Explanations on the high-speed merging routines in the appendix of
*
*  P. Sanders.
*  Fast priority queues for cached memory.
*  ACM Journal of Experimental Algorithmics, 5, 2000.
*
*  This file is a GNU parallel extension to the Standard C++ Library.
*/

// Written by Johannes Singler and Manuel Holtgrewe.

#ifndef _GLIBCXX_PARALLEL_MULTIWAY_MERGE_H
#define _GLIBCXX_PARALLEL_MULTIWAY_MERGE_H

#include <vector>

#include <bits/stl_algo.h>
#include <parallel/features.h>
#include <parallel/parallel.h>
#include <parallel/losertree.h>
#if _GLIBCXX_ASSERTIONS
#include <parallel/checkers.h>
#endif

/** @brief Length of a sequence described by a pair of iterators. */
#define _GLIBCXX_PARALLEL_LENGTH(s) ((s).second - (s).first)

namespace __gnu_parallel
{

// Announce guarded and unguarded iterator.

template<typename RandomAccessIterator, typename Comparator>
  class guarded_iterator;

// Making the arguments const references seems to dangerous,
// the user-defined comparator might not be const.
template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<(guarded_iterator<RandomAccessIterator, Comparator>& bi1,
             guarded_iterator<RandomAccessIterator, Comparator>& bi2);

template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<=(guarded_iterator<RandomAccessIterator, Comparator>& bi1,
              guarded_iterator<RandomAccessIterator, Comparator>& bi2);

/** @brief Iterator wrapper supporting an implicit supremum at the end
 *         of the sequence, dominating all comparisons.
 *
 * The implicit supremum comes with a performance cost.
 *
 * Deriving from RandomAccessIterator is not possible since
 * RandomAccessIterator need not be a class.
 */
template<typename RandomAccessIterator, typename Comparator>
  class guarded_iterator
  {
  private:
    /** @brief Current iterator position. */
    RandomAccessIterator current;

    /** @brief End iterator of the sequence. */
    RandomAccessIterator end;

    /** @brief Comparator. */
    Comparator& comp;

  public:
    /** @brief Constructor. Sets iterator to beginning of sequence.
    *  @param begin Begin iterator of sequence.
    *  @param end End iterator of sequence.
    *  @param comp Comparator provided for associated overloaded
    *  compare operators. */
    guarded_iterator(RandomAccessIterator begin,
                     RandomAccessIterator end, Comparator& comp)
    : current(begin), end(end), comp(comp)
    { }

    /** @brief Pre-increment operator.
    *  @return This. */
    guarded_iterator<RandomAccessIterator, Comparator>&
    operator++()
    {
      ++current;
      return *this;
    }

    /** @brief Dereference operator.
    *  @return Referenced element. */
    typename std::iterator_traits<RandomAccessIterator>::value_type&
    operator*()
    { return *current; }

    /** @brief Convert to wrapped iterator.
    *  @return Wrapped iterator. */
    operator RandomAccessIterator()
    { return current; }

    friend bool
    operator< <RandomAccessIterator, Comparator>(
      guarded_iterator<RandomAccessIterator, Comparator>& bi1,
      guarded_iterator<RandomAccessIterator, Comparator>& bi2);

    friend bool
    operator<= <RandomAccessIterator, Comparator>(
      guarded_iterator<RandomAccessIterator, Comparator>& bi1,
      guarded_iterator<RandomAccessIterator, Comparator>& bi2);
  };

/** @brief Compare two elements referenced by guarded iterators.
 *  @param bi1 First iterator.
 *  @param bi2 Second iterator.
 *  @return @c True if less. */
template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<(guarded_iterator<RandomAccessIterator, Comparator>& bi1,
            guarded_iterator<RandomAccessIterator, Comparator>& bi2)
  {
    if (bi1.current == bi1.end)	//bi1 is sup
      return bi2.current == bi2.end;	//bi2 is not sup
    if (bi2.current == bi2.end)	//bi2 is sup
      return true;
    return (bi1.comp)(*bi1, *bi2);	//normal compare
  }

/** @brief Compare two elements referenced by guarded iterators.
 *  @param bi1 First iterator.
 *  @param bi2 Second iterator.
 *  @return @c True if less equal. */
template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<=(guarded_iterator<RandomAccessIterator, Comparator>& bi1,
               guarded_iterator<RandomAccessIterator, Comparator>& bi2)
  {
    if (bi2.current == bi2.end)	//bi1 is sup
      return bi1.current != bi1.end;	//bi2 is not sup
    if (bi1.current == bi1.end)	//bi2 is sup
      return false;
    return !(bi1.comp)(*bi2, *bi1);	//normal compare
  }

template<typename RandomAccessIterator, typename Comparator>
  class unguarded_iterator;

template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<(unguarded_iterator<RandomAccessIterator, Comparator>& bi1,
            unguarded_iterator<RandomAccessIterator, Comparator>& bi2);

template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<=(unguarded_iterator<RandomAccessIterator, Comparator>& bi1,
             unguarded_iterator<RandomAccessIterator, Comparator>& bi2);

template<typename RandomAccessIterator, typename Comparator>
  class unguarded_iterator
  {
  private:
    /** @brief Current iterator position. */
    RandomAccessIterator current;
    /** @brief Comparator. */
    mutable Comparator& comp;

  public:
    /** @brief Constructor. Sets iterator to beginning of sequence.
    *  @param begin Begin iterator of sequence.
    *  @param end Unused, only for compatibility.
    *  @param comp Unused, only for compatibility. */
    unguarded_iterator(RandomAccessIterator begin,
                       RandomAccessIterator end, Comparator& comp)
    : current(begin), comp(comp)
    { }

    /** @brief Pre-increment operator.
    *  @return This. */
    unguarded_iterator<RandomAccessIterator, Comparator>&
    operator++()
    {
      ++current;
      return *this;
    }

    /** @brief Dereference operator.
    *  @return Referenced element. */
    typename std::iterator_traits<RandomAccessIterator>::value_type&
    operator*()
    { return *current; }

    /** @brief Convert to wrapped iterator.
    *  @return Wrapped iterator. */
    operator RandomAccessIterator()
    { return current; }

    friend bool
    operator< <RandomAccessIterator, Comparator>(
      unguarded_iterator<RandomAccessIterator, Comparator>& bi1,
      unguarded_iterator<RandomAccessIterator, Comparator>& bi2);

    friend bool
    operator<= <RandomAccessIterator, Comparator>(
      unguarded_iterator<RandomAccessIterator, Comparator>& bi1,
      unguarded_iterator<RandomAccessIterator, Comparator>& bi2);
  };

/** @brief Compare two elements referenced by unguarded iterators.
 *  @param bi1 First iterator.
 *  @param bi2 Second iterator.
 *  @return @c True if less. */
template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<(unguarded_iterator<RandomAccessIterator, Comparator>& bi1,
            unguarded_iterator<RandomAccessIterator, Comparator>& bi2)
  {
    // Normal compare.
    return (bi1.comp)(*bi1, *bi2);
  }

/** @brief Compare two elements referenced by unguarded iterators.
 *  @param bi1 First iterator.
 *  @param bi2 Second iterator.
 *  @return @c True if less equal. */
template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<=(unguarded_iterator<RandomAccessIterator, Comparator>& bi1,
            unguarded_iterator<RandomAccessIterator, Comparator>& bi2)
  {
    // Normal compare.
    return !(bi1.comp)(*bi2, *bi1);
  }

/** @brief Highly efficient 3-way merging procedure.
 *
 * Merging is done with the algorithm implementation described by Peter
 * Sanders.  Basically, the idea is to minimize the number of necessary
 * comparison after merging out an element.  The implementation trick
 * that makes this fast is that the order of the sequences is stored
 * in the instruction pointer (translated into labels in C++).
 *
 * This works well for merging up to 4 sequences.
 *
 * Note that making the merging stable does <em>not</em> come at a
 * performance hit.
 *
 * Whether the merging is done guarded or unguarded is selected by the
 * used iterator class.
 *
 * @param seqs_begin Begin iterator of iterator pair input sequence.
 * @param seqs_end End iterator of iterator pair input sequence.
 * @param target Begin iterator out output sequence.
 * @param comp Comparator.
 * @param length Maximum length to merge, less equal than the
 * total number of elements available.
 *
 * @return End iterator of output sequence.
 */
template<template<typename RAI, typename C> class iterator,
	 typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_3_variant(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      _DifferenceTp length, Comparator comp)
  {
    _GLIBCXX_CALL(length);

    typedef _DifferenceTp difference_type;

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    if (length == 0)
      return target;

#if _GLIBCXX_ASSERTIONS
    _DifferenceTp orig_length = length;
#endif

    iterator<RandomAccessIterator1, Comparator>
      seq0(seqs_begin[0].first, seqs_begin[0].second, comp),
      seq1(seqs_begin[1].first, seqs_begin[1].second, comp),
      seq2(seqs_begin[2].first, seqs_begin[2].second, comp);

    if (seq0 <= seq1)
      {
        if (seq1 <= seq2)
          goto s012;
        else
          if (seq2 <  seq0)
            goto s201;
          else
            goto s021;
      }
    else
      {
        if (seq1 <= seq2)
          {
            if (seq0 <= seq2)
              goto s102;
            else
              goto s120;
          }
        else
          goto s210;
      }
#define _GLIBCXX_PARALLEL_MERGE_3_CASE(a,b,c,c0,c1)     \
    s ## a ## b ## c :                                  \
      *target = *seq ## a;                              \
      ++target;                                         \
      --length;                                         \
      ++seq ## a;                                       \
      if (length == 0) goto finish;                     \
      if (seq ## a c0 seq ## b) goto s ## a ## b ## c;  \
      if (seq ## a c1 seq ## c) goto s ## b ## a ## c;  \
      goto s ## b ## c ## a;

    _GLIBCXX_PARALLEL_MERGE_3_CASE(0, 1, 2, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_3_CASE(1, 2, 0, <=, < );
    _GLIBCXX_PARALLEL_MERGE_3_CASE(2, 0, 1, < , < );
    _GLIBCXX_PARALLEL_MERGE_3_CASE(1, 0, 2, < , <=);
    _GLIBCXX_PARALLEL_MERGE_3_CASE(0, 2, 1, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_3_CASE(2, 1, 0, < , < );

#undef _GLIBCXX_PARALLEL_MERGE_3_CASE

  finish:
    ;

#if _GLIBCXX_ASSERTIONS
  _GLIBCXX_PARALLEL_ASSERT(
      ((RandomAccessIterator1)seq0 - seqs_begin[0].first) +
      ((RandomAccessIterator1)seq1 - seqs_begin[1].first) +
      ((RandomAccessIterator1)seq2 - seqs_begin[2].first)
      == orig_length);
#endif

    seqs_begin[0].first = seq0;
    seqs_begin[1].first = seq1;
    seqs_begin[2].first = seq2;

    return target;
  }

/**
 * @brief Highly efficient 4-way merging procedure.
 *
 * Merging is done with the algorithm implementation described by Peter
 * Sanders. Basically, the idea is to minimize the number of necessary
 * comparison after merging out an element.  The implementation trick
 * that makes this fast is that the order of the sequences is stored
 * in the instruction pointer (translated into goto labels in C++).
 *
 * This works well for merging up to 4 sequences.
 *
 * Note that making the merging stable does <em>not</em> come at a
 * performance hit.
 *
 * Whether the merging is done guarded or unguarded is selected by the
 * used iterator class.
 *
 * @param seqs_begin Begin iterator of iterator pair input sequence.
 * @param seqs_end End iterator of iterator pair input sequence.
 * @param target Begin iterator out output sequence.
 * @param comp Comparator.
 * @param length Maximum length to merge, less equal than the
 * total number of elements available.
 *
 * @return End iterator of output sequence.
 */
template<template<typename RAI, typename C> class iterator,
	 typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_4_variant(RandomAccessIteratorIterator seqs_begin,
                           RandomAccessIteratorIterator seqs_end,
                           RandomAccessIterator3 target,
                           _DifferenceTp length, Comparator comp)
  {
    _GLIBCXX_CALL(length);
    typedef _DifferenceTp difference_type;

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    iterator<RandomAccessIterator1, Comparator>
      seq0(seqs_begin[0].first, seqs_begin[0].second, comp),
      seq1(seqs_begin[1].first, seqs_begin[1].second, comp),
      seq2(seqs_begin[2].first, seqs_begin[2].second, comp),
      seq3(seqs_begin[3].first, seqs_begin[3].second, comp);

#define _GLIBCXX_PARALLEL_DECISION(a,b,c,d) {                   \
      if (seq ## d < seq ## a) goto s ## d ## a ## b ## c;	\
      if (seq ## d < seq ## b) goto s ## a ## d ## b ## c;	\
      if (seq ## d < seq ## c) goto s ## a ## b ## d ## c;	\
      goto s ## a ## b ## c ## d;  }

    if (seq0 <= seq1)
      {
        if (seq1 <= seq2)
          _GLIBCXX_PARALLEL_DECISION(0,1,2,3)
          else
            if (seq2 < seq0)
              _GLIBCXX_PARALLEL_DECISION(2,0,1,3)
              else
                _GLIBCXX_PARALLEL_DECISION(0,2,1,3)
                  }
    else
      {
        if (seq1 <= seq2)
          {
            if (seq0 <= seq2)
              _GLIBCXX_PARALLEL_DECISION(1,0,2,3)
              else
                _GLIBCXX_PARALLEL_DECISION(1,2,0,3)
                  }
        else
          _GLIBCXX_PARALLEL_DECISION(2,1,0,3)
            }

#define _GLIBCXX_PARALLEL_MERGE_4_CASE(a,b,c,d,c0,c1,c2)        \
    s ## a ## b ## c ## d:                                      \
      if (length == 0) goto finish;                             \
    *target = *seq ## a;                                        \
    ++target;                                                   \
    --length;                                                   \
    ++seq ## a;                                                 \
    if (seq ## a c0 seq ## b) goto s ## a ## b ## c ## d;       \
    if (seq ## a c1 seq ## c) goto s ## b ## a ## c ## d;       \
    if (seq ## a c2 seq ## d) goto s ## b ## c ## a ## d;       \
    goto s ## b ## c ## d ## a;

    _GLIBCXX_PARALLEL_MERGE_4_CASE(0, 1, 2, 3, <=, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(0, 1, 3, 2, <=, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(0, 2, 1, 3, <=, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(0, 2, 3, 1, <=, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(0, 3, 1, 2, <=, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(0, 3, 2, 1, <=, <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(1, 0, 2, 3, < , <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(1, 0, 3, 2, < , <=, <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(1, 2, 0, 3, <=, < , <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(1, 2, 3, 0, <=, <=, < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(1, 3, 0, 2, <=, < , <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(1, 3, 2, 0, <=, <=, < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(2, 0, 1, 3, < , < , <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(2, 0, 3, 1, < , <=, < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(2, 1, 0, 3, < , < , <=);
    _GLIBCXX_PARALLEL_MERGE_4_CASE(2, 1, 3, 0, < , <=, < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(2, 3, 0, 1, <=, < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(2, 3, 1, 0, <=, < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(3, 0, 1, 2, < , < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(3, 0, 2, 1, < , < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(3, 1, 0, 2, < , < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(3, 1, 2, 0, < , < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(3, 2, 0, 1, < , < , < );
    _GLIBCXX_PARALLEL_MERGE_4_CASE(3, 2, 1, 0, < , < , < );

#undef _GLIBCXX_PARALLEL_MERGE_4_CASE
#undef _GLIBCXX_PARALLEL_DECISION

  finish:
    ;

    seqs_begin[0].first = seq0;
    seqs_begin[1].first = seq1;
    seqs_begin[2].first = seq2;
    seqs_begin[3].first = seq3;

    return target;
  }

/** @brief Multi-way merging procedure for a high branching factor,
 *         guarded case.
 *
 * This merging variant uses a LoserTree class as selected by <tt>LT</tt>.
 *
 * Stability is selected through the used LoserTree class <tt>LT</tt>.
 *
 * At least one non-empty sequence is required.
 *
 * @param seqs_begin Begin iterator of iterator pair input sequence.
 * @param seqs_end End iterator of iterator pair input sequence.
 * @param target Begin iterator out output sequence.
 * @param comp Comparator.
 * @param length Maximum length to merge, less equal than the
 * total number of elements available.
 *
 * @return End iterator of output sequence.
 */
template<typename LT,
	 typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_loser_tree(RandomAccessIteratorIterator seqs_begin,
                            RandomAccessIteratorIterator seqs_end,
                            RandomAccessIterator3 target,
                            _DifferenceTp length, Comparator comp)
  {
    _GLIBCXX_CALL(length)

    typedef _DifferenceTp difference_type;
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int k = static_cast<int>(seqs_end - seqs_begin);

    LT lt(k, comp);

    // Default value for potentially non-default-constructible types.
    value_type* arbitrary_element = NULL;

    for (int t = 0; t < k; ++t)
      {
        if(arbitrary_element == NULL
            && _GLIBCXX_PARALLEL_LENGTH(seqs_begin[t]) > 0)
          arbitrary_element = &(*seqs_begin[t].first);
      }

    for (int t = 0; t < k; ++t)
      {
        if (seqs_begin[t].first == seqs_begin[t].second)
          lt.insert_start(*arbitrary_element, t, true);
        else
          lt.insert_start(*seqs_begin[t].first, t, false);
      }

    lt.init();

    int source;

    for (difference_type i = 0; i < length; ++i)
      {
        //take out
        source = lt.get_min_source();

        *(target++) = *(seqs_begin[source].first++);

        // Feed.
        if (seqs_begin[source].first == seqs_begin[source].second)
          lt.delete_min_insert(*arbitrary_element, true);
        else
          // Replace from same source.
          lt.delete_min_insert(*seqs_begin[source].first, false);
      }

    return target;
  }

/** @brief Multi-way merging procedure for a high branching factor,
 *         unguarded case.
 *
 * Merging is done using the LoserTree class <tt>LT</tt>.
 *
 * Stability is selected by the used LoserTrees.
 *
 * @pre No input will run out of elements during the merge.
 *
 * @param seqs_begin Begin iterator of iterator pair input sequence.
 * @param seqs_end End iterator of iterator pair input sequence.
 * @param target Begin iterator out output sequence.
 * @param comp Comparator.
 * @param length Maximum length to merge, less equal than the
 * total number of elements available.
 *
 * @return End iterator of output sequence.
 */
template<typename LT,
    typename RandomAccessIteratorIterator,
    typename RandomAccessIterator3,
    typename _DifferenceTp, typename Comparator>
  RandomAccessIterator3
  multiway_merge_loser_tree_unguarded(
    RandomAccessIteratorIterator seqs_begin,
    RandomAccessIteratorIterator seqs_end,
    RandomAccessIterator3 target,
    const typename std::iterator_traits<typename std::iterator_traits<
      RandomAccessIteratorIterator>::value_type::first_type>::value_type&
        sentinel,
    _DifferenceTp length,
    Comparator comp)
  {
    _GLIBCXX_CALL(length)
    typedef _DifferenceTp difference_type;

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int k = seqs_end - seqs_begin;

    LT lt(k, sentinel, comp);

    for (int t = 0; t < k; ++t)
      {
#if _GLIBCXX_ASSERTIONS
        _GLIBCXX_PARALLEL_ASSERT(seqs_begin[t].first != seqs_begin[t].second);
#endif
        lt.insert_start(*seqs_begin[t].first, t, false);
      }

    lt.init();

    int source;

#if _GLIBCXX_ASSERTIONS
    difference_type i = 0;
#endif

    RandomAccessIterator3 target_end = target + length;
    while (target < target_end)
      {
        // Take out.
        source = lt.get_min_source();

#if _GLIBCXX_ASSERTIONS
        _GLIBCXX_PARALLEL_ASSERT(0 <= source && source < k);
        _GLIBCXX_PARALLEL_ASSERT(i == 0
            || !comp(*(seqs_begin[source].first), *(target - 1)));
#endif

        // Feed.
        *(target++) = *(seqs_begin[source].first++);

#if _GLIBCXX_ASSERTIONS
        ++i;
#endif
        // Replace from same source.
        lt.delete_min_insert(*seqs_begin[source].first, false);
      }

    return target;
  }


/** @brief Multi-way merging procedure for a high branching factor,
 *         requiring sentinels to exist.
 *
 * @param stable The value must the same as for the used LoserTrees.
 * @param UnguardedLoserTree Loser Tree variant to use for the unguarded
 *   merging.
 * @param GuardedLoserTree Loser Tree variant to use for the guarded
 *   merging.
 *
 * @param seqs_begin Begin iterator of iterator pair input sequence.
 * @param seqs_end End iterator of iterator pair input sequence.
 * @param target Begin iterator out output sequence.
 * @param comp Comparator.
 * @param length Maximum length to merge, less equal than the
 * total number of elements available.
 *
 * @return End iterator of output sequence.
 */
template<
    typename UnguardedLoserTree,
    typename RandomAccessIteratorIterator,
    typename RandomAccessIterator3,
    typename _DifferenceTp,
    typename Comparator>
  RandomAccessIterator3
  multiway_merge_loser_tree_sentinel(
    RandomAccessIteratorIterator seqs_begin,
    RandomAccessIteratorIterator seqs_end,
    RandomAccessIterator3 target,
    const typename std::iterator_traits<typename std::iterator_traits<
      RandomAccessIteratorIterator>::value_type::first_type>::value_type&
        sentinel,
    _DifferenceTp length,
    Comparator comp)
  {
    _GLIBCXX_CALL(length)

    typedef _DifferenceTp difference_type;
    typedef std::iterator_traits<RandomAccessIteratorIterator> traits_type;
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    RandomAccessIterator3 target_end;

    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      // Move the sequends end behind the sentinel spots.  This has the
      // effect that the sentinel appears to be within the sequence. Then,
      // we can use the unguarded variant if we merge out as many
      // non-sentinel elements as we have.
      ++((*s).second);

    target_end = multiway_merge_loser_tree_unguarded
        <UnguardedLoserTree>
      (seqs_begin, seqs_end, target, sentinel, length, comp);

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    // Restore the sequence ends so the sentinels are not contained in the
    // sequence any more (see comment in loop above).
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      --((*s).second);

    return target_end;
  }

/**
 * @brief Traits for determining whether the loser tree should
 *   use pointers or copies.
 *
 * The field "use_pointer" is used to determine whether to use pointers in
 * the loser trees or whether to copy the values into the loser tree.
 *
 * The default behavior is to use pointers if the data type is 4 times as
 * big as the pointer to it.
 *
 * Specialize for your data type to customize the behavior.
 *
 * Example:
 *
 *   template<>
 *   struct loser_tree_traits<int>
 *   { static const bool use_pointer = false; };
 *
 *   template<>
 *   struct loser_tree_traits<heavyweight_type>
 *   { static const bool use_pointer = true; };
 *
 * @param T type to give the loser tree traits for.
 */
template <typename T>
struct loser_tree_traits
{
  /**
   * @brief True iff to use pointers instead of values in loser trees.
   *
   * The default behavior is to use pointers if the data type is four
   * times as big as the pointer to it.
   */
  static const bool use_pointer = (sizeof(T) > 4 * sizeof(T*));
};

/**
 * @brief Switch for 3-way merging with sentinels turned off.
 *
 * Note that 3-way merging is always stable!
 */
template<
  bool sentinels /*default == false*/,
  typename RandomAccessIteratorIterator,
  typename RandomAccessIterator3,
  typename _DifferenceTp,
  typename Comparator>
struct multiway_merge_3_variant_sentinel_switch
{
  RandomAccessIterator3 operator()(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      _DifferenceTp length, Comparator comp)
  {
    return multiway_merge_3_variant<guarded_iterator>(
        seqs_begin, seqs_end, target, length, comp);
  }
};

/**
 * @brief Switch for 3-way merging with sentinels turned on.
 *
 * Note that 3-way merging is always stable!
 */
template<
  typename RandomAccessIteratorIterator,
  typename RandomAccessIterator3,
  typename _DifferenceTp,
  typename Comparator>
struct multiway_merge_3_variant_sentinel_switch
    <true, RandomAccessIteratorIterator, RandomAccessIterator3,
     _DifferenceTp, Comparator>
{
  RandomAccessIterator3 operator()(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      _DifferenceTp length, Comparator comp)
  {
    return multiway_merge_3_variant<unguarded_iterator>(
        seqs_begin, seqs_end, target, length, comp);
  }
};

/**
 * @brief Switch for 4-way merging with sentinels turned off.
 *
 * Note that 4-way merging is always stable!
 */
template<
  bool sentinels /*default == false*/,
  typename RandomAccessIteratorIterator,
  typename RandomAccessIterator3,
  typename _DifferenceTp,
  typename Comparator>
struct multiway_merge_4_variant_sentinel_switch
{
  RandomAccessIterator3 operator()(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      _DifferenceTp length, Comparator comp)
  {
    return multiway_merge_4_variant<guarded_iterator>(
        seqs_begin, seqs_end, target, length, comp);
  }
};

/**
 * @brief Switch for 4-way merging with sentinels turned on.
 *
 * Note that 4-way merging is always stable!
 */
template<
  typename RandomAccessIteratorIterator,
  typename RandomAccessIterator3,
  typename _DifferenceTp,
  typename Comparator>
struct multiway_merge_4_variant_sentinel_switch
    <true, RandomAccessIteratorIterator, RandomAccessIterator3,
     _DifferenceTp, Comparator>
{
  RandomAccessIterator3 operator()(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      _DifferenceTp length, Comparator comp)
  {
    return multiway_merge_4_variant<unguarded_iterator>(
        seqs_begin, seqs_end, target, length, comp);
  }
};

/**
 * @brief Switch for k-way merging with sentinels turned on.
 */
template<
  bool sentinels,
  bool stable,
  typename RandomAccessIteratorIterator,
  typename RandomAccessIterator3,
  typename _DifferenceTp,
  typename Comparator>
struct multiway_merge_k_variant_sentinel_switch
{
  RandomAccessIterator3 operator()(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      const typename std::iterator_traits<typename std::iterator_traits<
        RandomAccessIteratorIterator>::value_type::first_type>::value_type&
          sentinel,
      _DifferenceTp length, Comparator comp)
  {
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    return multiway_merge_loser_tree_sentinel<
        typename __gnu_cxx::__conditional_type<
            loser_tree_traits<value_type>::use_pointer
          , LoserTreePointerUnguarded<stable, value_type, Comparator>
          , LoserTreeUnguarded<stable, value_type, Comparator>
        >::__type>(seqs_begin, seqs_end, target, sentinel, length, comp);
  }
};

/**
 * @brief Switch for k-way merging with sentinels turned off.
 */
template<
  bool stable,
  typename RandomAccessIteratorIterator,
  typename RandomAccessIterator3,
  typename _DifferenceTp,
  typename Comparator>
struct multiway_merge_k_variant_sentinel_switch
    <false, stable, RandomAccessIteratorIterator, RandomAccessIterator3,
     _DifferenceTp, Comparator>
{
  RandomAccessIterator3 operator()(
      RandomAccessIteratorIterator seqs_begin,
      RandomAccessIteratorIterator seqs_end,
      RandomAccessIterator3 target,
      const typename std::iterator_traits<typename std::iterator_traits<
        RandomAccessIteratorIterator>::value_type::first_type>::value_type&
          sentinel,
      _DifferenceTp length, Comparator comp)
  {
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    return multiway_merge_loser_tree<
        typename __gnu_cxx::__conditional_type<
            loser_tree_traits<value_type>::use_pointer
          , LoserTreePointer<stable, value_type, Comparator>
          , LoserTree<stable, value_type, Comparator>
        >::__type >(seqs_begin, seqs_end, target, length, comp);
  }
};

/** @brief Sequential multi-way merging switch.
 *
 *  The _GLIBCXX_PARALLEL_DECISION is based on the branching factor and
 *  runtime settings.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge, possibly larger than the
 *  number of elements available.
 *  @param stable Stable merging incurs a performance penalty.
 *  @param sentinel The sequences have a sentinel element.
 *  @return End iterator of output sequence. */
template<
    bool stable,
    bool sentinels,
    typename RandomAccessIteratorIterator,
    typename RandomAccessIterator3,
    typename _DifferenceTp,
    typename Comparator>
  RandomAccessIterator3
  sequential_multiway_merge(
    RandomAccessIteratorIterator seqs_begin,
    RandomAccessIteratorIterator seqs_end,
    RandomAccessIterator3 target,
    const typename std::iterator_traits<typename std::iterator_traits<
      RandomAccessIteratorIterator>::value_type::first_type>::value_type&
        sentinel,
    _DifferenceTp length, Comparator comp)
  {
    _GLIBCXX_CALL(length)

    typedef _DifferenceTp difference_type;
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

#if _GLIBCXX_ASSERTIONS
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      {
        _GLIBCXX_PARALLEL_ASSERT(is_sorted((*s).first, (*s).second, comp));
      }
#endif

    _DifferenceTp total_length = 0;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      total_length += _GLIBCXX_PARALLEL_LENGTH(*s);

    length = std::min<_DifferenceTp>(length, total_length);

    if(length == 0)
      return target;

    RandomAccessIterator3 return_target = target;
    int k = static_cast<int>(seqs_end - seqs_begin);

    switch (k)
      {
      case 0:
        break;
      case 1:
        return_target = std::copy(seqs_begin[0].first,
                                  seqs_begin[0].first + length,
                                  target);
        seqs_begin[0].first += length;
        break;
      case 2:
        return_target = merge_advance(seqs_begin[0].first,
                                      seqs_begin[0].second,
                                      seqs_begin[1].first,
                                      seqs_begin[1].second,
                                      target, length, comp);
        break;
      case 3:
        return_target = multiway_merge_3_variant_sentinel_switch<
            sentinels
          , RandomAccessIteratorIterator
          , RandomAccessIterator3
          , _DifferenceTp
          , Comparator>()(seqs_begin, seqs_end, target, length, comp);
        break;
      case 4:
        return_target = multiway_merge_4_variant_sentinel_switch<
            sentinels
          , RandomAccessIteratorIterator
          , RandomAccessIterator3
          , _DifferenceTp
          , Comparator>()(seqs_begin, seqs_end, target, length, comp);
        break;
      default:
          return_target = multiway_merge_k_variant_sentinel_switch<
              sentinels
            , stable
            , RandomAccessIteratorIterator
            , RandomAccessIterator3
            , _DifferenceTp
            , Comparator>()(seqs_begin, seqs_end, target, sentinel, length, comp);
          break;
      }
#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target + length, comp));
#endif

    return return_target;
  }

/**
 * @brief Stable sorting functor.
 *
 * Used to reduce code instanciation in multiway_merge_sampling_splitting.
 */
template<bool stable, class RandomAccessIterator, class StrictWeakOrdering>
struct sampling_sorter
{
  void operator()(RandomAccessIterator first, RandomAccessIterator last,
                  StrictWeakOrdering comp)
  { __gnu_sequential::stable_sort(first, last, comp); }
};

/**
 * @brief Non-stable sorting functor.
 *
 * Used to reduce code instantiation in multiway_merge_sampling_splitting.
 */
template<class RandomAccessIterator, class StrictWeakOrdering>
struct sampling_sorter<false, RandomAccessIterator, StrictWeakOrdering>
{
  void operator()(RandomAccessIterator first, RandomAccessIterator last,
                  StrictWeakOrdering comp)
  { __gnu_sequential::sort(first, last, comp); }
};

/**
 * @brief Sampling based splitting for parallel multiway-merge routine.
 */
template<
    bool stable
  , typename RandomAccessIteratorIterator
  , typename Comparator
  , typename difference_type>
void multiway_merge_sampling_splitting(
    RandomAccessIteratorIterator seqs_begin,
    RandomAccessIteratorIterator seqs_end,
    difference_type length, difference_type total_length, Comparator comp,
    std::vector<std::pair<difference_type, difference_type> > *pieces)
{
  typedef typename std::iterator_traits<RandomAccessIteratorIterator>
    ::value_type::first_type
    RandomAccessIterator1;
  typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
    value_type;

  // k sequences.
  int k = static_cast<int>(seqs_end - seqs_begin);

  int num_threads = omp_get_num_threads();

  difference_type num_samples =
      __gnu_parallel::_Settings::get().merge_oversampling * num_threads;

  value_type* samples = static_cast<value_type*>(
    ::operator new(sizeof(value_type) * k * num_samples));
  // Sample.
  for (int s = 0; s < k; ++s)
    for (difference_type i = 0; i < num_samples; ++i)
      {
        difference_type sample_index =
            static_cast<difference_type>(
                _GLIBCXX_PARALLEL_LENGTH(seqs_begin[s])
                    * (double(i + 1) / (num_samples + 1))
                    * (double(length) / total_length));
        new(&(samples[s * num_samples + i]))
            value_type(seqs_begin[s].first[sample_index]);
      }

  // Sort stable or non-stable, depending on value of template parameter
  // "stable".
  sampling_sorter<stable, value_type*, Comparator>()(
      samples, samples + (num_samples * k), comp);

  for (int slab = 0; slab < num_threads; ++slab)
    // For each slab / processor.
    for (int seq = 0; seq < k; ++seq)
      {
        // For each sequence.
        if (slab > 0)
          pieces[slab][seq].first =
              std::upper_bound(
                seqs_begin[seq].first,
                seqs_begin[seq].second,
                samples[num_samples * k * slab / num_threads],
                  comp)
              - seqs_begin[seq].first;
        else
          // Absolute beginning.
          pieces[slab][seq].first = 0;
        if ((slab + 1) < num_threads)
          pieces[slab][seq].second =
              std::upper_bound(
                  seqs_begin[seq].first,
                  seqs_begin[seq].second,
                  samples[num_samples * k * (slab + 1) /
                      num_threads], comp)
              - seqs_begin[seq].first;
        else
            // Absolute end.
          pieces[slab][seq].second = _GLIBCXX_PARALLEL_LENGTH(seqs_begin[seq]);
      }
    ::operator delete(samples);
}

/**
 * @brief Exact splitting for parallel multiway-merge routine.
 *
 * None of the passed sequences may be empty.
 */
template<
    bool stable
  , typename RandomAccessIteratorIterator
  , typename Comparator
  , typename difference_type>
void multiway_merge_exact_splitting(
    RandomAccessIteratorIterator seqs_begin,
    RandomAccessIteratorIterator seqs_end,
    difference_type length, difference_type total_length, Comparator comp,
    std::vector<std::pair<difference_type, difference_type> > *pieces)
{
  typedef typename std::iterator_traits<RandomAccessIteratorIterator>
    ::value_type::first_type
    RandomAccessIterator1;

  const bool tight = (total_length == length);

  // k sequences.
  const int k = static_cast<int>(seqs_end - seqs_begin);

  const int num_threads = omp_get_num_threads();

  // (Settings::multiway_merge_splitting == __gnu_parallel::_Settings::EXACT).
  std::vector<RandomAccessIterator1>* offsets =
      new std::vector<RandomAccessIterator1>[num_threads];
  std::vector<
      std::pair<RandomAccessIterator1, RandomAccessIterator1>
      > se(k);

  copy(seqs_begin, seqs_end, se.begin());

  difference_type* borders =
      new difference_type[num_threads + 1];
  equally_split(length, num_threads, borders);

  for (int s = 0; s < (num_threads - 1); ++s)
    {
      offsets[s].resize(k);
      multiseq_partition(
          se.begin(), se.end(), borders[s + 1],
          offsets[s].begin(), comp);

      // Last one also needed and available.
      if (!tight)
        {
          offsets[num_threads - 1].resize(k);
          multiseq_partition(se.begin(), se.end(),
                difference_type(length),
                offsets[num_threads - 1].begin(),  comp);
        }
    }
  delete[] borders;

  for (int slab = 0; slab < num_threads; ++slab)
    {
      // For each slab / processor.
      for (int seq = 0; seq < k; ++seq)
        {
          // For each sequence.
          if (slab == 0)
            {
              // Absolute beginning.
              pieces[slab][seq].first = 0;
            }
          else
            pieces[slab][seq].first =
                pieces[slab - 1][seq].second;
          if (!tight || slab < (num_threads - 1))
            pieces[slab][seq].second =
                offsets[slab][seq] - seqs_begin[seq].first;
          else
            {
              // slab == num_threads - 1
              pieces[slab][seq].second =
                  _GLIBCXX_PARALLEL_LENGTH(seqs_begin[seq]);
            }
        }
    }
  delete[] offsets;
}

/** @brief Parallel multi-way merge routine.
 *
 * The _GLIBCXX_PARALLEL_DECISION is based on the branching factor
 * and runtime settings.
 *
 * Must not be called if the number of sequences is 1.
 *
 * @param Splitter functor to split input (either exact or sampling based)
 *
 * @param seqs_begin Begin iterator of iterator pair input sequence.
 * @param seqs_end End iterator of iterator pair input sequence.
 * @param target Begin iterator out output sequence.
 * @param comp Comparator.
 * @param length Maximum length to merge, possibly larger than the
 * number of elements available.
 * @param stable Stable merging incurs a performance penalty.
 * @param sentinel Ignored.
 * @return End iterator of output sequence.
 */
template<
    bool stable,
    bool sentinels,
    typename RandomAccessIteratorIterator,
    typename RandomAccessIterator3,
    typename _DifferenceTp,
    typename Splitter,
    typename Comparator
    >
  RandomAccessIterator3
  parallel_multiway_merge(RandomAccessIteratorIterator seqs_begin,
                          RandomAccessIteratorIterator seqs_end,
                          RandomAccessIterator3 target,
                          Splitter splitter,
                          _DifferenceTp length,
                          Comparator comp,
                          thread_index_t num_threads)
    {
#if _GLIBCXX_ASSERTIONS
      _GLIBCXX_PARALLEL_ASSERT(seqs_end - seqs_begin > 1);
#endif

      _GLIBCXX_CALL(length)

      typedef _DifferenceTp difference_type;
      typedef typename std::iterator_traits<RandomAccessIteratorIterator>
        ::value_type::first_type
        RandomAccessIterator1;
      typedef typename
        std::iterator_traits<RandomAccessIterator1>::value_type value_type;

      // Leave only non-empty sequences.
      typedef std::pair<RandomAccessIterator1, RandomAccessIterator1> seq_type;
      seq_type* ne_seqs = new seq_type[seqs_end - seqs_begin];
      int k = 0;
      difference_type total_length = 0;
      for (RandomAccessIteratorIterator raii = seqs_begin;
           raii != seqs_end; ++raii)
        {
          _DifferenceTp seq_length = _GLIBCXX_PARALLEL_LENGTH(*raii);
          if(seq_length > 0)
            {
              total_length += seq_length;
              ne_seqs[k++] = *raii;
            }
        }

      _GLIBCXX_CALL(total_length)

      length = std::min<_DifferenceTp>(length, total_length);

      if (total_length == 0 || k == 0)
      {
        delete[] ne_seqs;
        return target;
      }

      std::vector<std::pair<difference_type, difference_type> >* pieces;

      num_threads = static_cast<thread_index_t>
        (std::min<difference_type>(num_threads, total_length));

#     pragma omp parallel num_threads (num_threads)
        {
#         pragma omp single
            {
              num_threads = omp_get_num_threads();
              // Thread t will have to merge pieces[iam][0..k - 1]
              pieces = new std::vector<
                  std::pair<difference_type, difference_type> >[num_threads];
              for (int s = 0; s < num_threads; ++s)
                pieces[s].resize(k);

              difference_type num_samples =
                  __gnu_parallel::_Settings::get().merge_oversampling *
                    num_threads;

              splitter(ne_seqs, ne_seqs + k, length, total_length,
                       comp, pieces);
            } //single

          thread_index_t iam = omp_get_thread_num();

          difference_type target_position = 0;

          for (int c = 0; c < k; ++c)
            target_position += pieces[iam][c].first;

          seq_type* chunks = new seq_type[k];

          for (int s = 0; s < k; ++s)
            {
              chunks[s] = std::make_pair(
                ne_seqs[s].first + pieces[iam][s].first,
                ne_seqs[s].first + pieces[iam][s].second);
            }

          if(length > target_position)
            sequential_multiway_merge<stable, sentinels>(
              chunks, chunks + k, target + target_position,
               *(seqs_begin->second), length - target_position, comp);

          delete[] chunks;
        } // parallel

#if _GLIBCXX_ASSERTIONS
      _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target + length, comp));
#endif

      k = 0;
      // Update ends of sequences.
      for (RandomAccessIteratorIterator raii = seqs_begin;
           raii != seqs_end; ++raii)
        {
          _DifferenceTp length = _GLIBCXX_PARALLEL_LENGTH(*raii);
          if(length > 0)
            (*raii).first += pieces[num_threads - 1][k++].second;
        }

      delete[] pieces;
      delete[] ne_seqs;

      return target + length;
    }

/**
 * @brief Multiway Merge Frontend.
 *
 * Merge the sequences specified by seqs_begin and seqs_end into
 * target.  seqs_begin and seqs_end must point to a sequence of
 * pairs.  These pairs must contain an iterator to the beginning
 * of a sequence in their first entry and an iterator the end of
 * the same sequence in their second entry.
 *
 * Ties are broken arbitrarily.  See stable_multiway_merge for a variant
 * that breaks ties by sequence number but is slower.
 *
 * The first entries of the pairs (i.e. the begin iterators) will be moved
 * forward.
 *
 * The output sequence has to provide enough space for all elements
 * that are written to it.
 *
 * This function will merge the input sequences:
 *
 * - not stable
 * - parallel, depending on the input size and Settings
 * - using sampling for splitting
 * - not using sentinels
 *
 * Example:
 *
 * <pre>
 *   int sequences[10][10];
 *   for (int i = 0; i < 10; ++i)
 *     for (int j = 0; i < 10; ++j)
 *       sequences[i][j] = j;
 *
 *   int out[33];
 *   std::vector<std::pair<int*> > seqs;
 *   for (int i = 0; i < 10; ++i)
 *     { seqs.push(std::make_pair<int*>(sequences[i], sequences[i] + 10)) }
 *
 *   multiway_merge(seqs.begin(), seqs.end(), target, std::less<int>(), 33);
 * </pre>
 *
 * @see stable_multiway_merge
 *
 * @pre All input sequences must be sorted.
 * @pre Target must provide enough space to merge out length elements or
 *    the number of elements in all sequences, whichever is smaller.
 *
 * @post [target, return value) contains merged elements from the
 *    input sequences.
 * @post return value - target = min(length, number of elements in all
 *    sequences).
 *
 * @param RandomAccessIteratorPairIterator iterator over sequence
 *    of pairs of iterators
 * @param RandomAccessIteratorOut iterator over target sequence
 * @param _DifferenceTp difference type for the sequence
 * @param Comparator strict weak ordering type to compare elements
 *    in sequences
 *
 * @param seqs_begin  begin of sequence sequence
 * @param seqs_end    end of sequence sequence
 * @param target      target sequence to merge to.
 * @param comp        strict weak ordering to use for element comparison.
 * @param length Maximum length to merge, possibly larger than the
 * number of elements available.
 *
 * @return end iterator of output sequence
 */
// multiway_merge
// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::sequential_tag)
{
  typedef _DifferenceTp difference_type;
  _GLIBCXX_CALL(seqs_end - seqs_begin)

  // catch special case: no sequences
  if (seqs_begin == seqs_end)
    return target;

  // Execute multiway merge *sequentially*.
  return sequential_multiway_merge
    </* stable = */ false, /* sentinels = */ false>
      (seqs_begin, seqs_end, target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::exact_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
             __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
                    </* stable = */ false, /* sentinels = */ false>(
          seqs_begin, seqs_end, target,
          multiway_merge_exact_splitting</* stable = */ false,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
                      </* stable = */ false, /* sentinels = */ false>(
          seqs_begin, seqs_end, target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::sampling_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
             __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
                    </* stable = */ false, /* sentinels = */ false>(
          seqs_begin, seqs_end,
          target,
          multiway_merge_exact_splitting</* stable = */ false,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
                      </* stable = */ false, /* sentinels = */ false>(
          seqs_begin, seqs_end,
          target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , parallel_tag tag = parallel_tag(0))
{
  return multiway_merge(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , default_parallel_tag tag)
{
  return multiway_merge(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

// stable_multiway_merge
// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::sequential_tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute multiway merge *sequentially*.
    return sequential_multiway_merge
      </* stable = */ true, /* sentinels = */ false>
        (seqs_begin, seqs_end, target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::exact_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
        </* stable = */ true, /* sentinels = */ false>(
          seqs_begin, seqs_end,
          target,
          multiway_merge_exact_splitting</* stable = */ true,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge</* stable = */ true,
        /* sentinels = */ false>(
          seqs_begin, seqs_end,
          target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , sampling_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
        </* stable = */ true, /* sentinels = */ false>(
          seqs_begin, seqs_end,
          target,
          multiway_merge_sampling_splitting</* stable = */ true,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
        </* stable = */ true, /* sentinels = */ false>(
          seqs_begin, seqs_end,
          target, *(seqs_begin->second), length, comp);
}


// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , parallel_tag tag = parallel_tag(0))
{
  return stable_multiway_merge(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , default_parallel_tag tag)
{
  return stable_multiway_merge(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

/**
 * @brief Multiway Merge Frontend.
 *
 * Merge the sequences specified by seqs_begin and seqs_end into
 * target.  seqs_begin and seqs_end must point to a sequence of
 * pairs.  These pairs must contain an iterator to the beginning
 * of a sequence in their first entry and an iterator the end of
 * the same sequence in their second entry.
 *
 * Ties are broken arbitrarily.  See stable_multiway_merge for a variant
 * that breaks ties by sequence number but is slower.
 *
 * The first entries of the pairs (i.e. the begin iterators) will be moved
 * forward accordingly.
 *
 * The output sequence has to provide enough space for all elements
 * that are written to it.
 *
 * This function will merge the input sequences:
 *
 * - not stable
 * - parallel, depending on the input size and Settings
 * - using sampling for splitting
 * - using sentinels
 *
 * You have to take care that the element the end iterator points to is
 * readable and contains a value that is greater than any other non-sentinel
 * value in all sequences.
 *
 * Example:
 *
 * <pre>
 *   int sequences[10][11];
 *   for (int i = 0; i < 10; ++i)
 *     for (int j = 0; i < 11; ++j)
 *       sequences[i][j] = j; // last one is sentinel!
 *
 *   int out[33];
 *   std::vector<std::pair<int*> > seqs;
 *   for (int i = 0; i < 10; ++i)
 *     { seqs.push(std::make_pair<int*>(sequences[i], sequences[i] + 10)) }
 *
 *   multiway_merge(seqs.begin(), seqs.end(), target, std::less<int>(), 33);
 * </pre>
 *
 * @pre All input sequences must be sorted.
 * @pre Target must provide enough space to merge out length elements or
 *    the number of elements in all sequences, whichever is smaller.
 * @pre For each @c i, @c seqs_begin[i].second must be the end
 *    marker of the sequence, but also reference the one more sentinel
 *    element.
 *
 * @post [target, return value) contains merged elements from the
 *    input sequences.
 * @post return value - target = min(length, number of elements in all
 *    sequences).
 *
 * @see stable_multiway_merge_sentinels
 *
 * @param RandomAccessIteratorPairIterator iterator over sequence
 *    of pairs of iterators
 * @param RandomAccessIteratorOut iterator over target sequence
 * @param _DifferenceTp difference type for the sequence
 * @param Comparator strict weak ordering type to compare elements
 *    in sequences
 *
 * @param seqs_begin  begin of sequence sequence
 * @param seqs_end    end of sequence sequence
 * @param target      target sequence to merge to.
 * @param comp        strict weak ordering to use for element comparison.
 * @param length Maximum length to merge, possibly larger than the
 * number of elements available.
 *
 * @return end iterator of output sequence
 */
// multiway_merge_sentinels
// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::sequential_tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute multiway merge *sequentially*.
    return sequential_multiway_merge
      </* stable = */ false, /* sentinels = */ true>
        (seqs_begin, seqs_end,
         target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::exact_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
        </* stable = */ false, /* sentinels = */ true>(
          seqs_begin, seqs_end,
          target,
          multiway_merge_exact_splitting</* stable = */ false,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
        </* stable = */ false, /* sentinels = */ true>(
          seqs_begin, seqs_end,
          target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , sampling_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
        </* stable = */ false, /* sentinels = */ true>
          (seqs_begin, seqs_end, target,
          multiway_merge_sampling_splitting</* stable = */ false,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
        </* stable = */false, /* sentinels = */ true>(
          seqs_begin, seqs_end,
          target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , parallel_tag tag = parallel_tag(0))
{
  return multiway_merge_sentinels(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , default_parallel_tag tag)
{
  return multiway_merge_sentinels(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

// stable_multiway_merge_sentinels
// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::sequential_tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute multiway merge *sequentially*.
    return sequential_multiway_merge
      </* stable = */ true, /* sentinels = */ true>
        (seqs_begin, seqs_end, target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , __gnu_parallel::exact_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
          __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
          __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
        </* stable = */ true, /* sentinels = */ true>(
          seqs_begin, seqs_end,
          target,
          multiway_merge_exact_splitting</* stable = */ true,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
        </* stable = */ true, /* sentinels = */ true>(
          seqs_begin, seqs_end, target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , sampling_tag tag)
{
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    // catch special case: no sequences
    if (seqs_begin == seqs_end)
      return target;

    // Execute merge; maybe parallel, depending on the number of merged
    // elements and the number of sequences and global thresholds in
    // Settings.
    if ((seqs_end - seqs_begin > 1) &&
          _GLIBCXX_PARALLEL_CONDITION(
          ((seqs_end - seqs_begin) >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_k)
          && ((sequence_index_t)length >=
            __gnu_parallel::_Settings::get().multiway_merge_minimal_n)))
      return parallel_multiway_merge
        </* stable = */ true, /* sentinels = */ true>(
          seqs_begin, seqs_end,
          target,
          multiway_merge_sampling_splitting</* stable = */ true,
            typename std::iterator_traits<RandomAccessIteratorPairIterator>
              ::value_type*, Comparator, _DifferenceTp>,
          static_cast<difference_type>(length), comp, tag.get_num_threads());
    else
      return sequential_multiway_merge
        </* stable = */ true, /* sentinels = */ true>(
          seqs_begin, seqs_end,
          target, *(seqs_begin->second), length, comp);
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , parallel_tag tag = parallel_tag(0))
{
  return stable_multiway_merge_sentinels(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

// public interface
template<
    typename RandomAccessIteratorPairIterator
  , typename RandomAccessIteratorOut
  , typename _DifferenceTp
  , typename Comparator>
RandomAccessIteratorOut
stable_multiway_merge_sentinels(RandomAccessIteratorPairIterator seqs_begin
    , RandomAccessIteratorPairIterator seqs_end
    , RandomAccessIteratorOut target
    , _DifferenceTp length, Comparator comp
    , default_parallel_tag tag)
{
  return stable_multiway_merge_sentinels(seqs_begin, seqs_end, target, length, comp,
                         exact_tag(tag.get_num_threads()));
}

}; // namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_MULTIWAY_MERGE_H */
