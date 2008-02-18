// -*- C++ -*-

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

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

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_MULTIWAY_MERGE_H
#define _GLIBCXX_PARALLEL_MULTIWAY_MERGE_H

#include <vector>

#include <bits/stl_algo.h>
#include <parallel/features.h>
#include <parallel/parallel.h>
#include <parallel/merge.h>
#include <parallel/losertree.h>
#if _GLIBCXX_ASSERTIONS
#include <parallel/checkers.h>
#endif

/** @brief Length of a sequence described by a pair of iterators. */
#define _GLIBCXX_PARALLEL_LENGTH(s) ((s).second - (s).first)

// XXX need iterator typedefs
namespace __gnu_parallel
{
template<typename RandomAccessIterator, typename Comparator>
  class guarded_iterator;

template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<(guarded_iterator<RandomAccessIterator, Comparator>& bi1,
            guarded_iterator<RandomAccessIterator, Comparator>& bi2);

template<typename RandomAccessIterator, typename Comparator>
  inline bool
  operator<=(guarded_iterator<RandomAccessIterator, Comparator>& bi1,
	     guarded_iterator<RandomAccessIterator, Comparator>& bi2);

  /** @brief Iterator wrapper supporting an implicit supremum at the end
      of the sequence, dominating all comparisons.
      *  Deriving from RandomAccessIterator is not possible since
      *  RandomAccessIterator need not be a class.
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
    typename std::iterator_traits<RandomAccessIterator>::value_type
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
    RandomAccessIterator& current;
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
    typename std::iterator_traits<RandomAccessIterator>::value_type
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

/** Prepare a set of sequences to be merged without a (end) guard
 *  @param seqs_begin
 *  @param seqs_end
 *  @param comp
 *  @param min_sequence
 *  @param stable
 *  @pre (seqs_end - seqs_begin > 0) */
template<typename RandomAccessIteratorIterator, typename Comparator>
  typename std::iterator_traits<
      typename std::iterator_traits<RandomAccessIteratorIterator>::value_type
      ::first_type>::difference_type
  prepare_unguarded(RandomAccessIteratorIterator seqs_begin,
                    RandomAccessIteratorIterator seqs_end, Comparator comp,
                    int& min_sequence, bool stable)
  {
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
        ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;
    typedef typename std::iterator_traits<RandomAccessIterator1>
      ::difference_type
      difference_type;

    if ((*seqs_begin).first == (*seqs_begin).second)
      {
        // Empty sequence found, it's the first one.
        min_sequence = 0;
        return -1;
      }

    // Last element in sequence.
    value_type min = *((*seqs_begin).second - 1);
    min_sequence = 0;
    for (RandomAccessIteratorIterator s = seqs_begin + 1; s != seqs_end; ++s)
      {
        if ((*s).first == (*s).second)
          {
            // Empty sequence found.
            min_sequence = static_cast<int>(s - seqs_begin);
            return -1;
          }

        // Last element in sequence.
        const value_type& v = *((*s).second - 1);
        if (comp(v, min))	//strictly smaller
          {
            min = v;
            min_sequence = static_cast<int>(s - seqs_begin);
          }
      }

    difference_type overhang_size = 0;

    int s = 0;
    for (s = 0; s <= min_sequence; ++s)
      {
        RandomAccessIterator1 split;
        if (stable)
          split = std::upper_bound(seqs_begin[s].first, seqs_begin[s].second,
                                  min, comp);
        else
          split = std::lower_bound(seqs_begin[s].first, seqs_begin[s].second,
                                  min, comp);

        overhang_size += seqs_begin[s].second - split;
      }

    for (; s < (seqs_end - seqs_begin); ++s)
      {
        RandomAccessIterator1 split = std::lower_bound(
            seqs_begin[s].first, seqs_begin[s].second, min, comp);
        overhang_size += seqs_begin[s].second - split;
      }

    // So many elements will be left over afterwards.
    return overhang_size;
  }

/** Prepare a set of sequences to be merged with a (end) guard (sentinel)
 *  @param seqs_begin
 *  @param seqs_end
 *  @param comp */
template<typename RandomAccessIteratorIterator, typename Comparator>
  typename std::iterator_traits<typename std::iterator_traits<
      RandomAccessIteratorIterator>::value_type::first_type>::difference_type
  prepare_unguarded_sentinel(RandomAccessIteratorIterator seqs_begin,
                            RandomAccessIteratorIterator seqs_end,
                            Comparator comp)
  {
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>
      ::value_type
      value_type;
    typedef typename std::iterator_traits<RandomAccessIterator1>
      ::difference_type
      difference_type;

    // Last element in sequence.
    value_type* max = NULL;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      {
        if ((*s).first == (*s).second)
          continue;

        // Last element in sequence.
        value_type& v = *((*s).second - 1);

        // Strictly greater.
        if (!max || comp(*max, v))
          max = &v;
      }

    difference_type overhang_size = 0;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      {
        RandomAccessIterator1 split =
            std::lower_bound((*s).first, (*s).second, *max, comp);
        overhang_size += (*s).second - split;

        // Set sentinel.
        *((*s).second) = *max;
      }

    // So many elements will be left over afterwards.
    return overhang_size;
  }

/** @brief Highly efficient 3-way merging procedure.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Unused, stable anyway.
 *  @return End iterator of output sequence. */
template<template<typename RAI, typename C> class iterator,
	 typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_3_variant(RandomAccessIteratorIterator seqs_begin,
			   RandomAccessIteratorIterator seqs_end,
			   RandomAccessIterator3 target,
			   Comparator comp, _DifferenceTp length,
			   bool stable)
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

#define _GLIBCXX_PARALLEL_MERGE_3_CASE(a,b,c,c0,c1)\
    s ## a ## b ## c :                                  \
      *target = *seq ## a;                              \
    ++target;                                           \
    --length;                                           \
    ++seq ## a;                                         \
    if (length == 0) goto finish;                       \
    if (seq ## a c0 seq ## b) goto s ## a ## b ## c;    \
    if (seq ## a c1 seq ## c) goto s ## b ## a ## c;    \
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

    seqs_begin[0].first = seq0;
    seqs_begin[1].first = seq1;
    seqs_begin[2].first = seq2;

    return target;
  }

template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_3_combined(RandomAccessIteratorIterator seqs_begin,
                            RandomAccessIteratorIterator seqs_end,
                            RandomAccessIterator3 target,
                            Comparator comp,
                            _DifferenceTp length, bool stable)
  {
    _GLIBCXX_CALL(length);

    typedef _DifferenceTp difference_type;
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int min_seq;
    RandomAccessIterator3 target_end;

    // Stable anyway.
    difference_type overhang =
        prepare_unguarded(seqs_begin, seqs_end, comp, min_seq, true);

    difference_type total_length = 0;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      total_length += _GLIBCXX_PARALLEL_LENGTH(*s);

    if (overhang != -1)
      {
        difference_type unguarded_length =
            std::min(length, total_length - overhang);
        target_end = multiway_merge_3_variant<unguarded_iterator>
          (seqs_begin, seqs_end, target, comp, unguarded_length, stable);
        overhang = length - unguarded_length;
      }
    else
      {
        // Empty sequence found.
        overhang = length;
        target_end = target;
      }

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length - overhang);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    switch (min_seq)
      {
      case 0:
        // Iterators will be advanced accordingly.
        target_end = merge_advance(seqs_begin[1].first, seqs_begin[1].second,
                                  seqs_begin[2].first, seqs_begin[2].second,
                                  target_end, overhang, comp);
        break;
      case 1:
        target_end = merge_advance(seqs_begin[0].first, seqs_begin[0].second,
                                  seqs_begin[2].first, seqs_begin[2].second,
                                  target_end, overhang, comp);
        break;
      case 2:
        target_end = merge_advance(seqs_begin[0].first, seqs_begin[0].second,
                                  seqs_begin[1].first, seqs_begin[1].second,
                                  target_end, overhang, comp);
        break;
      default:
        _GLIBCXX_PARALLEL_ASSERT(false);
      }

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    return target_end;
  }

/** @brief Highly efficient 4-way merging procedure.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Unused, stable anyway.
 *  @return End iterator of output sequence. */
template<template<typename RAI, typename C> class iterator,
	 typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_4_variant(RandomAccessIteratorIterator seqs_begin,
                           RandomAccessIteratorIterator seqs_end,
                           RandomAccessIterator3 target,
                           Comparator comp, _DifferenceTp length, bool stable)
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

template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_4_combined(RandomAccessIteratorIterator seqs_begin,
                            RandomAccessIteratorIterator seqs_end,
                            RandomAccessIterator3 target,
                            Comparator comp,
                            _DifferenceTp length, bool stable)
  {
    _GLIBCXX_CALL(length);
    typedef _DifferenceTp difference_type;

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int min_seq;
    RandomAccessIterator3 target_end;

    // Stable anyway.
    difference_type overhang =
        prepare_unguarded(seqs_begin, seqs_end, comp, min_seq, true);

    difference_type total_length = 0;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      total_length += _GLIBCXX_PARALLEL_LENGTH(*s);

    if (overhang != -1)
      {
        difference_type unguarded_length =
            std::min(length, total_length - overhang);
        target_end = multiway_merge_4_variant<unguarded_iterator>
          (seqs_begin, seqs_end, target, comp, unguarded_length, stable);
        overhang = length - unguarded_length;
      }
    else
      {
        // Empty sequence found.
        overhang = length;
        target_end = target;
      }

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length - overhang);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    std::vector<std::pair<RandomAccessIterator1, RandomAccessIterator1> >
        one_missing(seqs_begin, seqs_end);
    one_missing.erase(one_missing.begin() + min_seq);	//remove

    target_end = multiway_merge_3_variant<guarded_iterator>(
        one_missing.begin(), one_missing.end(),
        target_end, comp, overhang, stable);

    // Insert back again.
    one_missing.insert(one_missing.begin() + min_seq, seqs_begin[min_seq]);
    // Write back modified iterators.
    copy(one_missing.begin(), one_missing.end(), seqs_begin);

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    return target_end;
  }

/** @brief Basic multi-way merging procedure.
 *
 *  The head elements are kept in a sorted array, new heads are
 *  inserted linearly.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Stable merging incurs a performance penalty.
 *  @return End iterator of output sequence.
 */
template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_bubble(RandomAccessIteratorIterator seqs_begin,
                        RandomAccessIteratorIterator seqs_end,
                        RandomAccessIterator3 target,
                        Comparator comp, _DifferenceTp length, bool stable)
  {
    _GLIBCXX_CALL(length)

    typedef _DifferenceTp difference_type;
    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int k = static_cast<int>(seqs_end - seqs_begin);
    int nrs;  // Number of remaining sequences.

    // Avoid default constructor.
    value_type* fe = static_cast<value_type*>(
      ::operator new(sizeof(value_type) * k));  // Front elements.
    int* source = new int[k];
    difference_type total_length = 0;

    // Write entries into queue.
    nrs = 0;
    for (int pi = 0; pi < k; ++pi)
      {
        if (seqs_begin[pi].first != seqs_begin[pi].second)
          {
            ::new(&(fe[nrs])) value_type(*(seqs_begin[pi].first));
            source[nrs] = pi;
            ++nrs;
            total_length += _GLIBCXX_PARALLEL_LENGTH(seqs_begin[pi]);
          }
      }

    if (stable)
      {
        // Bubble sort fe and source by fe.
        for (int k = 0; k < nrs - 1; ++k)
          for (int pi = nrs - 1; pi > k; --pi)
            if (comp(fe[pi], fe[pi - 1]) ||
                (!comp(fe[pi - 1], fe[pi]) && source[pi] < source[pi - 1]))
              {
                std::swap(fe[pi - 1], fe[pi]);
                std::swap(source[pi - 1], source[pi]);
              }
      }
    else
      {
        for (int k = 0; k < nrs - 1; ++k)
          for (int pi = nrs - 1; pi > k; --pi)
            if (comp(fe[pi], fe[pi-1]))
              {
                std::swap(fe[pi-1], fe[pi]);
                std::swap(source[pi-1], source[pi]);
              }
      }

    // Iterate.
    if (stable)
      {
        int j;
        while (nrs > 0 && length > 0)
          {
            if (source[0] < source[1])
              {
                // fe[0] <= fe[1]
                while ((nrs == 1 || !comp(fe[1], fe[0])) && length > 0)
                  {
                    *target = fe[0];
                    ++target;
                    ++(seqs_begin[source[0]].first);
                    --length;
                    if (seqs_begin[source[0]].first
			== seqs_begin[source[0]].second)
                      {
                        // Move everything to the left.
                        for (int s = 0; s < nrs - 1; ++s)
                          {
                            fe[s] = fe[s + 1];
                            source[s] = source[s + 1];
                          }
                        fe[nrs - 1].~value_type();  //Destruct explicitly.
                        --nrs;
                        break;
                      }
                    else
                      fe[0] = *(seqs_begin[source[0]].first);
                  }
              }
            else
              {
                // fe[0] < fe[1]
                while ((nrs == 1 || comp(fe[0], fe[1])) && length > 0)
                  {
                    *target = fe[0];
                    ++target;
                    ++(seqs_begin[source[0]].first);
                    --length;
                    if (seqs_begin[source[0]].first
			== seqs_begin[source[0]].second)
                      {
                        for (int s = 0; s < nrs - 1; ++s)
                          {
                            fe[s] = fe[s + 1];
                            source[s] = source[s + 1];
                          }
                        fe[nrs - 1].~value_type();  //Destruct explicitly.
                        --nrs;
                        break;
                      }
                    else
                      fe[0] = *(seqs_begin[source[0]].first);
                  }
              }

            // Sink down.
            j = 1;
            while ((j < nrs) && (comp(fe[j], fe[j - 1])
				 || (!comp(fe[j - 1], fe[j])
				     && (source[j] < source[j - 1]))))
              {
                std::swap(fe[j - 1], fe[j]);
                std::swap(source[j - 1], source[j]);
                ++j;
              }
          }
      }
    else
      {
        int j;
        while (nrs > 0 && length > 0)
          {
            // fe[0] <= fe[1]
            while (nrs == 1 || (!comp(fe[1], fe[0])) && length > 0)
              {
                *target = fe[0];
                ++target;
                ++seqs_begin[source[0]].first;
                --length;
                if (seqs_begin[source[0]].first
		    == seqs_begin[source[0]].second)
                  {
                    for (int s = 0; s < (nrs - 1); ++s)
                      {
                        fe[s] = fe[s + 1];
                        source[s] = source[s + 1];
                      }
                    fe[nrs - 1].~value_type();  //Destruct explicitly.
                    --nrs;
                    break;
                  }
                else
                  fe[0] = *(seqs_begin[source[0]].first);
              }

            // Sink down.
            j = 1;
            while ((j < nrs) && comp(fe[j], fe[j - 1]))
              {
                std::swap(fe[j - 1], fe[j]);
                std::swap(source[j - 1], source[j]);
                ++j;
              }
          }
      }

    ::operator delete(fe);  //Destructors already called.
    delete[] source;

    return target;
  }

/** @brief Multi-way merging procedure for a high branching factor,
 * guarded case.
 *
 *  The head elements are kept in a loser tree.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *   @param stable Stable merging incurs a performance penalty.
 *  @return End iterator of output sequence.
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
                            Comparator comp,
                            _DifferenceTp length, bool stable)
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

    difference_type total_length = 0;

    // Default value for potentially non-default-constructible types.
    value_type* arbitrary_element = NULL;

    for (int t = 0; t < k; ++t)
      {
        if(arbitrary_element == NULL
	   && _GLIBCXX_PARALLEL_LENGTH(seqs_begin[t]) > 0)
          arbitrary_element = &(*seqs_begin[t].first);
        total_length += _GLIBCXX_PARALLEL_LENGTH(seqs_begin[t]);
      }

    if(total_length == 0)
      return target;

    for (int t = 0; t < k; ++t)
      {
        if (stable)
          {
            if (seqs_begin[t].first == seqs_begin[t].second)
              lt.insert_start_stable(*arbitrary_element, t, true);
            else
              lt.insert_start_stable(*seqs_begin[t].first, t, false);
          }
        else
          {
            if (seqs_begin[t].first == seqs_begin[t].second)
              lt.insert_start(*arbitrary_element, t, true);
            else
              lt.insert_start(*seqs_begin[t].first, t, false);
          }
      }

    if (stable)
      lt.init_stable();
    else
      lt.init();

    total_length = std::min(total_length, length);

    int source;

    if (stable)
      {
        for (difference_type i = 0; i < total_length; ++i)
          {
            // Take out.
            source = lt.get_min_source();

            *(target++) = *(seqs_begin[source].first++);

            // Feed.
            if (seqs_begin[source].first == seqs_begin[source].second)
              lt.delete_min_insert_stable(*arbitrary_element, true);
            else
              // Replace from same source.
              lt.delete_min_insert_stable(*seqs_begin[source].first, false);

          }
      }
    else
      {
        for (difference_type i = 0; i < total_length; ++i)
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
      }

    return target;
  }

/** @brief Multi-way merging procedure for a high branching factor,
 * unguarded case.
 *
 *  The head elements are kept in a loser tree.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Stable merging incurs a performance penalty.
 *  @return End iterator of output sequence.
 *  @pre No input will run out of elements during the merge.
 */
template<typename LT,
	 typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp, typename Comparator>
  RandomAccessIterator3
  multiway_merge_loser_tree_unguarded(RandomAccessIteratorIterator seqs_begin,
                                      RandomAccessIteratorIterator seqs_end,
                                      RandomAccessIterator3 target,
                                      Comparator comp,
                                      _DifferenceTp length, bool stable)
  {
    _GLIBCXX_CALL(length)
    typedef _DifferenceTp difference_type;

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int k = seqs_end - seqs_begin;

    LT lt(k, comp);

    difference_type total_length = 0;

    for (int t = 0; t < k; ++t)
      {
#if _GLIBCXX_ASSERTIONS
        _GLIBCXX_PARALLEL_ASSERT(seqs_begin[t].first != seqs_begin[t].second);
#endif
        if (stable)
          lt.insert_start_stable(*seqs_begin[t].first, t, false);
        else
          lt.insert_start(*seqs_begin[t].first, t, false);

        total_length += _GLIBCXX_PARALLEL_LENGTH(seqs_begin[t]);
      }

    if (stable)
      lt.init_stable();
    else
      lt.init();

    // Do not go past end.
    length = std::min(total_length, length);

    int source;

#if _GLIBCXX_ASSERTIONS
    difference_type i = 0;
#endif

    if (stable)
      {
        RandomAccessIterator3 target_end = target + length;
        while (target < target_end)
          {
            // Take out.
            source = lt.get_min_source();

#if _GLIBCXX_ASSERTIONS
            _GLIBCXX_PARALLEL_ASSERT(i == 0
                || !comp(*(seqs_begin[source].first), *(target - 1)));
#endif

            *(target++) = *(seqs_begin[source].first++);

#if _GLIBCXX_ASSERTIONS
            _GLIBCXX_PARALLEL_ASSERT(
                (seqs_begin[source].first != seqs_begin[source].second)
                || (i == length - 1));
            ++i;
#endif
            // Feed.
            // Replace from same source.
            lt.delete_min_insert_stable(*seqs_begin[source].first, false);

          }
      }
    else
      {
        RandomAccessIterator3 target_end = target + length;
        while (target < target_end)
          {
            // Take out.
            source = lt.get_min_source();

#if _GLIBCXX_ASSERTIONS
            if (i > 0 && comp(*(seqs_begin[source].first), *(target - 1)))
              printf("         %i %i %i\n", length, i, source);
            _GLIBCXX_PARALLEL_ASSERT(i == 0
                || !comp(*(seqs_begin[source].first), *(target - 1)));
#endif

            *(target++) = *(seqs_begin[source].first++);

#if _GLIBCXX_ASSERTIONS
            if (!((seqs_begin[source].first != seqs_begin[source].second)
                || (i >= length - 1)))
              printf("         %i %i %i\n", length, i, source);
            _GLIBCXX_PARALLEL_ASSERT(
                (seqs_begin[source].first != seqs_begin[source].second)
                || (i >= length - 1));
            ++i;
#endif
            // Feed.
            // Replace from same source.
            lt.delete_min_insert(*seqs_begin[source].first, false);
          }
      }

    return target;
  }

template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_loser_tree_combined(RandomAccessIteratorIterator seqs_begin,
                                     RandomAccessIteratorIterator seqs_end,
                                     RandomAccessIterator3 target,
                                     Comparator comp,
                                     _DifferenceTp length, bool stable)
  {
    _GLIBCXX_CALL(length)

    typedef _DifferenceTp difference_type;

    typedef typename std::iterator_traits<RandomAccessIteratorIterator>
      ::value_type::first_type
      RandomAccessIterator1;
    typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
      value_type;

    int min_seq;
    RandomAccessIterator3 target_end;
    difference_type overhang = prepare_unguarded(seqs_begin, seqs_end,
                                          comp, min_seq, stable);

    difference_type total_length = 0;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      total_length += _GLIBCXX_PARALLEL_LENGTH(*s);

    if (overhang != -1)
      {
        difference_type unguarded_length =
            std::min(length, total_length - overhang);
        target_end = multiway_merge_loser_tree_unguarded
          <typename loser_tree_unguarded_traits<value_type, Comparator>::LT>
          (seqs_begin, seqs_end, target, comp, unguarded_length, stable);
        overhang = length - unguarded_length;
      }
    else
      {
        // Empty sequence found.
        overhang = length;
        target_end = target;
      }

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length - overhang);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    target_end = multiway_merge_loser_tree
      <typename loser_tree_traits<value_type, Comparator>::LT>
      (seqs_begin, seqs_end, target_end, comp, overhang, stable);

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    return target_end;
  }

template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_loser_tree_sentinel(RandomAccessIteratorIterator seqs_begin,
                                     RandomAccessIteratorIterator seqs_end,
				     RandomAccessIterator3 target,
				     Comparator comp,
				     _DifferenceTp length, bool stable)
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
    difference_type overhang =
        prepare_unguarded_sentinel(seqs_begin, seqs_end, comp);

    difference_type total_length = 0;
    for (RandomAccessIteratorIterator s = seqs_begin; s != seqs_end; ++s)
      {
        total_length += _GLIBCXX_PARALLEL_LENGTH(*s);

        // Sentinel spot.
        ++((*s).second);
      }

    difference_type unguarded_length =
        std::min(length, total_length - overhang);
    target_end = multiway_merge_loser_tree_unguarded
      <typename loser_tree_unguarded_traits<value_type, Comparator>::LT>
      (seqs_begin, seqs_end, target, comp, unguarded_length, stable);
    overhang = length - unguarded_length;

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length - overhang);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    // Copy rest stable.
    for (RandomAccessIteratorIterator s = seqs_begin;
         s != seqs_end && overhang > 0; ++s)
      {
        // Restore.
        --((*s).second);
        difference_type local_length =
            std::min<difference_type>(overhang, _GLIBCXX_PARALLEL_LENGTH(*s));
        target_end = std::copy((*s).first, (*s).first + local_length,
                               target_end);
        (*s).first += local_length;
        overhang -= local_length;
      }

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(overhang == 0);
    _GLIBCXX_PARALLEL_ASSERT(target_end == target + length);
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target_end, comp));
#endif

    return target_end;
  }

/** @brief Sequential multi-way merging switch.
 *
 *  The _GLIBCXX_PARALLEL_DECISION if based on the branching factor and
 *  runtime settings.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Stable merging incurs a performance penalty.
 *  @param sentinel The sequences have a sentinel element.
 *  @return End iterator of output sequence. */
template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge(RandomAccessIteratorIterator seqs_begin,
                 RandomAccessIteratorIterator seqs_end,
                 RandomAccessIterator3 target,
                 Comparator comp, _DifferenceTp length,
                 bool stable, bool sentinel,
                 sequential_tag)
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
      _GLIBCXX_PARALLEL_ASSERT(is_sorted((*s).first, (*s).second, comp));
#endif

    RandomAccessIterator3 return_target = target;
    int k = static_cast<int>(seqs_end - seqs_begin);

    _MultiwayMergeAlgorithm mwma = _Settings::get().multiway_merge_algorithm;

    if (!sentinel && mwma == LOSER_TREE_SENTINEL)
      mwma = LOSER_TREE_COMBINED;

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
        switch (mwma)
          {
          case LOSER_TREE_COMBINED:
            return_target = multiway_merge_3_combined(seqs_begin,
						      seqs_end,
						      target,
						      comp, length,
						      stable);
            break;
          case LOSER_TREE_SENTINEL:
            return_target =
	      multiway_merge_3_variant<unguarded_iterator>(seqs_begin,
							   seqs_end,
							   target,
							   comp, length,
							   stable);
            break;
          default:
            return_target = 
	      multiway_merge_3_variant<guarded_iterator>(seqs_begin,
							 seqs_end,
							 target,
							 comp, length,
							 stable);
            break;
          }
        break;
      case 4:
        switch (mwma)
          {
          case LOSER_TREE_COMBINED:
            return_target = multiway_merge_4_combined(seqs_begin,
						      seqs_end,
						      target,
						      comp, length, stable);
            break;
          case LOSER_TREE_SENTINEL:
            return_target = 
	      multiway_merge_4_variant<unguarded_iterator>(seqs_begin,
							   seqs_end,
							   target,
							   comp, length,
							   stable);
            break;
          default:
            return_target = multiway_merge_4_variant<guarded_iterator>(
	      seqs_begin,
	      seqs_end,
	      target,
	      comp, length, stable);
            break;
          }
        break;
      default:
        {
          switch (mwma)
            {
            case BUBBLE:
              return_target = multiway_merge_bubble(seqs_begin,
						    seqs_end,
						    target,
						    comp, length, stable);
              break;
#if _GLIBCXX_LOSER_TREE_EXPLICIT
            case LOSER_TREE_EXPLICIT:
              return_target = multiway_merge_loser_tree<
	      LoserTreeExplicit<value_type, Comparator> >(seqs_begin,
							  seqs_end,
							  target,
							  comp, length,
							  stable);
              break;
#endif
#if _GLIBCXX_LOSER_TREE
            case LOSER_TREE:
              return_target = multiway_merge_loser_tree<
                    LoserTree<value_type, Comparator> >(seqs_begin,
							seqs_end,
							target,
							comp, length,
							stable);
              break;
#endif
#if _GLIBCXX_LOSER_TREE_COMBINED
            case LOSER_TREE_COMBINED:
              return_target = multiway_merge_loser_tree_combined(seqs_begin,
								 seqs_end,
								 target,
								 comp, length,
								 stable);
              break;
#endif
#if _GLIBCXX_LOSER_TREE_SENTINEL
            case LOSER_TREE_SENTINEL:
              return_target = multiway_merge_loser_tree_sentinel(seqs_begin,
								 seqs_end,
								 target,
								 comp, length,
								 stable);
              break;
#endif
            default:
              // multiway_merge algorithm not implemented.
              _GLIBCXX_PARALLEL_ASSERT(0);
              break;
            }
        }
      }
#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target + length, comp));
#endif

    return return_target;
  }

/** @brief Parallel multi-way merge routine.
 *
 *  The _GLIBCXX_PARALLEL_DECISION if based on the branching factor
 *  and runtime settings.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Stable merging incurs a performance penalty.
 *  @param sentinel Ignored.
 *  @return End iterator of output sequence.
 */
template<typename RandomAccessIteratorIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  parallel_multiway_merge(RandomAccessIteratorIterator seqs_begin,
                          RandomAccessIteratorIterator seqs_end,
                           RandomAccessIterator3 target,
                           Comparator comp,
                           _DifferenceTp length, bool stable, bool sentinel)
    {
      _GLIBCXX_CALL(length)

      typedef _DifferenceTp difference_type;
      typedef typename std::iterator_traits<RandomAccessIteratorIterator>
        ::value_type::first_type
        RandomAccessIterator1;
      typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
        value_type;

      // k sequences.
      int k = static_cast<int>(seqs_end - seqs_begin);

      difference_type total_length = 0;
      for (RandomAccessIteratorIterator raii = seqs_begin;
           raii != seqs_end; ++raii)
        total_length += _GLIBCXX_PARALLEL_LENGTH(*raii);

      _GLIBCXX_CALL(total_length)

      if (total_length == 0 || k == 0)
        return target;

      bool tight = (total_length == length);

      std::vector<std::pair<difference_type, difference_type> >* pieces;

      thread_index_t num_threads = static_cast<thread_index_t>(
	std::min<difference_type>(get_max_threads(), total_length));
      const _Settings& __s = _Settings::get();

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

              difference_type num_samples = __s.merge_oversampling 
					    * num_threads;

              if (__s.multiway_merge_splitting == SAMPLING)
                {
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
                        ::new(&(samples[s * num_samples + i]))
			    value_type(seqs_begin[s].first[sample_index]);
                      }

                  if (stable)
                    __gnu_sequential::stable_sort(samples, samples
						  + (num_samples * k), comp);
                  else
                    __gnu_sequential::sort(samples, samples
					   + (num_samples * k), comp);

                  for (int slab = 0; slab < num_threads; ++slab)
                    // For each slab / processor.
                    for (int seq = 0; seq < k; ++seq)
                      {
                        // For each sequence.
                        if (slab > 0)
                          pieces[slab][seq].first =
                              std::upper_bound(seqs_begin[seq].first,
					       seqs_begin[seq].second,
					       samples[num_samples * k
						       * slab / num_threads],
					       comp)
			    - seqs_begin[seq].first;
                        else
                          {
                            // Absolute beginning.
                            pieces[slab][seq].first = 0;
                          }
                        if ((slab + 1) < num_threads)
                          pieces[slab][seq].second =
			    std::upper_bound(seqs_begin[seq].first,
					     seqs_begin[seq].second,
					     samples[num_samples * k
						     * (slab + 1)
						     / num_threads], comp)
			    - seqs_begin[seq].first;
                        else
			  pieces[slab][seq].second 
			    = _GLIBCXX_PARALLEL_LENGTH(seqs_begin[seq]);
                      }
		  ::operator delete(samples);
                }
              else
                {
                  // (_Settings::multiway_merge_splitting == _Settings::EXACT).
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
					     offsets[num_threads - 1].begin(),
					     comp);
                        }
                    }


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
            } //single

          thread_index_t iam = omp_get_thread_num();

          difference_type target_position = 0;

          for (int c = 0; c < k; ++c)
            target_position += pieces[iam][c].first;

          if (k > 2)
            {
              std::pair<RandomAccessIterator1, RandomAccessIterator1>* chunks
                = new
                  std::pair<RandomAccessIterator1, RandomAccessIterator1>[k];

              difference_type local_length = 0;
              for (int s = 0; s < k; ++s)
                {
                  chunks[s] = std::make_pair(
		    seqs_begin[s].first + pieces[iam][s].first,
		    seqs_begin[s].first + pieces[iam][s].second);
                  local_length += _GLIBCXX_PARALLEL_LENGTH(chunks[s]);
                }

              multiway_merge(
                    chunks, chunks + k, target + target_position, comp,
                    std::min(local_length, length - target_position),
                    stable, false, sequential_tag());

              delete[] chunks;
            }
          else if (k == 2)
            {
              RandomAccessIterator1
                  begin0 = seqs_begin[0].first + pieces[iam][0].first,
                  begin1 = seqs_begin[1].first + pieces[iam][1].first;
              merge_advance(begin0,
			    seqs_begin[0].first + pieces[iam][0].second,
			    begin1,
			    seqs_begin[1].first + pieces[iam][1].second,
			    target + target_position,
			    (pieces[iam][0].second - pieces[iam][0].first) +
			    (pieces[iam][1].second - pieces[iam][1].first),
			    comp);
            }
        } //parallel

#if _GLIBCXX_ASSERTIONS
      _GLIBCXX_PARALLEL_ASSERT(is_sorted(target, target + length, comp));
#endif

      // Update ends of sequences.
      for (int s = 0; s < k; ++s)
        seqs_begin[s].first += pieces[num_threads - 1][s].second;

      delete[] pieces;

      return target + length;
    }

/**
 *  @brief Multi-way merging front-end.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Stable merging incurs a performance penalty.
 *  @return End iterator of output sequence.
 */
template<typename RandomAccessIteratorPairIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge(RandomAccessIteratorPairIterator seqs_begin,
                RandomAccessIteratorPairIterator seqs_end,
                RandomAccessIterator3 target, Comparator comp,
                _DifferenceTp length, bool stable)
  {
    typedef _DifferenceTp difference_type;
    _GLIBCXX_CALL(seqs_end - seqs_begin)

    if (seqs_begin == seqs_end)
      return target;

    const _Settings& __s = _Settings::get();

    RandomAccessIterator3 target_end;
    if (_GLIBCXX_PARALLEL_CONDITION(
        ((seqs_end - seqs_begin) >= __s.multiway_merge_minimal_k)
        && ((sequence_index_t)length >= __s.multiway_merge_minimal_n)))
      target_end = parallel_multiway_merge(seqs_begin, seqs_end,
					   target, comp,
					  static_cast<difference_type>(length),
					   stable, false);
    else
      target_end = multiway_merge(seqs_begin, seqs_end, target, comp, length,
				  stable, false, sequential_tag());

    return target_end;
  }

/** @brief Multi-way merging front-end.
 *  @param seqs_begin Begin iterator of iterator pair input sequence.
 *  @param seqs_end End iterator of iterator pair input sequence.
 *  @param target Begin iterator out output sequence.
 *  @param comp Comparator.
 *  @param length Maximum length to merge.
 *  @param stable Stable merging incurs a performance penalty.
 *  @return End iterator of output sequence.
 *  @pre For each @c i, @c seqs_begin[i].second must be the end
 *  marker of the sequence, but also reference the one more sentinel
 *  element. */
template<typename RandomAccessIteratorPairIterator,
	 typename RandomAccessIterator3,
	 typename _DifferenceTp,
	 typename Comparator>
  RandomAccessIterator3
  multiway_merge_sentinel(RandomAccessIteratorPairIterator seqs_begin,
                          RandomAccessIteratorPairIterator seqs_end,
                          RandomAccessIterator3 target,
                          Comparator comp,
                          _DifferenceTp length,
                          bool stable)
  {
    typedef _DifferenceTp difference_type;

    if (seqs_begin == seqs_end)
      return target;

    _GLIBCXX_CALL(seqs_end - seqs_begin)

    const _Settings& __s = _Settings::get();
    const bool cond1 = seqs_end - seqs_begin >= __s.multiway_merge_minimal_k;
    const bool cond2 = sequence_index_t(length) >= __s.multiway_merge_minimal_n;
    if (_GLIBCXX_PARALLEL_CONDITION(cond1 && cond2))
      return parallel_multiway_merge(seqs_begin, seqs_end, target, comp, 
				     length, stable, true);
    else
      return multiway_merge(seqs_begin, seqs_end, target, comp, length, stable,
			    true, sequential_tag());
  }
}

#endif
