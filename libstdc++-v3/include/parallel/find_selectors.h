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

/** @file parallel/find_selectors.h
 *  @brief Function objects representing different tasks to be plugged
 *  into the parallel find algorithm.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_FIND_SELECTORS_H
#define _GLIBCXX_PARALLEL_FIND_SELECTORS_H 1

#include <parallel/tags.h>
#include <parallel/basic_iterator.h>
#include <bits/stl_pair.h>

namespace __gnu_parallel
{
  /** @brief Base class of all __gnu_parallel::find_template selectors. */
  struct generic_find_selector
  { };

  /** 
   *  @brief Test predicate on a single element, used for std::find()
   *  and std::find_if ().
   */
  struct find_if_selector : public generic_find_selector
  {
    /** @brief Test on one position.
     * @param i1 Iterator on first sequence.
     * @param i2 Iterator on second sequence (unused).
     * @param pred Find predicate.
     */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      bool 
      operator()(RandomAccessIterator1 i1, RandomAccessIterator2 i2, Pred pred)
      { return pred(*i1); }

    /** @brief Corresponding sequential algorithm on a sequence.
     *  @param begin1 Begin iterator of first sequence.
     *  @param end1 End iterator of first sequence.
     *  @param begin2 Begin iterator of second sequence.
     *  @param pred Find predicate.
     */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      std::pair<RandomAccessIterator1, RandomAccessIterator2> 
      sequential_algorithm(RandomAccessIterator1 begin1,
			   RandomAccessIterator1 end1,
			   RandomAccessIterator2 begin2, Pred pred)
      { return std::make_pair(find_if(begin1, end1, pred,
				      sequential_tag()), begin2); }
  };

  /** @brief Test predicate on two adjacent elements. */
  struct adjacent_find_selector : public generic_find_selector
  {
    /** @brief Test on one position.
     *  @param i1 Iterator on first sequence.
     *  @param i2 Iterator on second sequence (unused).
     *  @param pred Find predicate.
     */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      bool 
      operator()(RandomAccessIterator1 i1, RandomAccessIterator2 i2, Pred pred)
      {
	// Passed end iterator is one short.
	return pred(*i1, *(i1 + 1));
      }

    /** @brief Corresponding sequential algorithm on a sequence.
     *  @param begin1 Begin iterator of first sequence.
     *  @param end1 End iterator of first sequence.
     *  @param begin2 Begin iterator of second sequence.
     *  @param pred Find predicate.
     */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      std::pair<RandomAccessIterator1, RandomAccessIterator2>
      sequential_algorithm(RandomAccessIterator1 begin1,
			   RandomAccessIterator1 end1,
			   RandomAccessIterator2 begin2, Pred pred)
      {
	// Passed end iterator is one short.
	RandomAccessIterator1 spot = adjacent_find(begin1, end1 + 1,
						   pred, sequential_tag());
	if (spot == (end1 + 1))
	  spot = end1;
	return std::make_pair(spot, begin2);
      }
  };

  /** @brief Test inverted predicate on a single element. */
  struct mismatch_selector : public generic_find_selector
  {
    /** 
     *  @brief Test on one position.
     *  @param i1 Iterator on first sequence.
     *  @param i2 Iterator on second sequence (unused).
     *  @param pred Find predicate. 
     */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      bool 
      operator()(RandomAccessIterator1 i1, RandomAccessIterator2 i2, Pred pred)
      { return !pred(*i1, *i2); }

    /** 
     *  @brief Corresponding sequential algorithm on a sequence.
     *  @param begin1 Begin iterator of first sequence.
     *  @param end1 End iterator of first sequence.
     *  @param begin2 Begin iterator of second sequence.
     *  @param pred Find predicate. 
     */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      std::pair<RandomAccessIterator1, RandomAccessIterator2>
      sequential_algorithm(RandomAccessIterator1 begin1,
			   RandomAccessIterator1 end1,
			   RandomAccessIterator2 begin2, Pred pred)
      { return mismatch(begin1, end1, begin2, pred, sequential_tag()); }
  };


  /** @brief Test predicate on several elements. */
  template<typename ForwardIterator>
  struct find_first_of_selector : public generic_find_selector
  {
    ForwardIterator begin;
    ForwardIterator end;

    explicit find_first_of_selector(ForwardIterator begin, ForwardIterator end)
    : begin(begin), end(end) { }

    /** @brief Test on one position.
     *  @param i1 Iterator on first sequence.
     *  @param i2 Iterator on second sequence (unused).
     *  @param pred Find predicate. */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      bool 
      operator()(RandomAccessIterator1 i1, RandomAccessIterator2 i2, Pred pred)
      {
	for (ForwardIterator pos_in_candidates = begin;
	     pos_in_candidates != end; ++pos_in_candidates)
	  if (pred(*i1, *pos_in_candidates))
	    return true;
	return false;
      }

    /** @brief Corresponding sequential algorithm on a sequence.
     *  @param begin1 Begin iterator of first sequence.
     *  @param end1 End iterator of first sequence.
     *  @param begin2 Begin iterator of second sequence.
     *  @param pred Find predicate. */
    template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	     typename Pred>
      std::pair<RandomAccessIterator1, RandomAccessIterator2>
      sequential_algorithm(RandomAccessIterator1 begin1,
			   RandomAccessIterator1 end1,
			   RandomAccessIterator2 begin2, Pred pred)
      { return std::make_pair(find_first_of(begin1, end1, begin, end, pred,
					    sequential_tag()), begin2); }
  };
}

#endif /* _GLIBCXX_PARALLEL_FIND_SELECTORS_H */
