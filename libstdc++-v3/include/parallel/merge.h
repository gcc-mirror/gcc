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

/** @file parallel/merge.h
 *  @brief Parallel implementation of std::merge().
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_MERGE_H
#define _GLIBCXX_PARALLEL_MERGE_H 1

#include <parallel/basic_iterator.h>
#include <bits/stl_algo.h>

namespace __gnu_parallel
{
  /** @brief Merge routine being able to merge only the @c max_length
   * smallest elements.
   *
   * The @c begin iterators are advanced accordingly, they might not
   * reach @c end, in contrast to the usual variant.
   * @param begin1 Begin iterator of first sequence.
   * @param end1 End iterator of first sequence.
   * @param begin2 Begin iterator of second sequence.
   * @param end2 End iterator of second sequence.
   * @param target Target begin iterator.
   * @param max_length Maximum number of elements to merge.
   * @param comp Comparator.
   * @return Output end iterator. */
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputIterator, typename _DifferenceTp,
	   typename Comparator>
    OutputIterator
    merge_advance_usual(RandomAccessIterator1& begin1,
			RandomAccessIterator1 end1,
			RandomAccessIterator2& begin2,
			RandomAccessIterator2 end2, OutputIterator target,
			_DifferenceTp max_length, Comparator comp)
    {
      typedef _DifferenceTp difference_type;
      while (begin1 != end1 && begin2 != end2 && max_length > 0)
	{
	  // array1[i1] < array0[i0]
	  if (comp(*begin2, *begin1))
	    *target++ = *begin2++;
	  else
	    *target++ = *begin1++;
	  --max_length;
	}

      if (begin1 != end1)
	{
	  target = std::copy(begin1, begin1 + max_length, target);
	  begin1 += max_length;
	}
      else
	{
	  target = std::copy(begin2, begin2 + max_length, target);
	  begin2 += max_length;
	}
      return target;
    }

  /** @brief Merge routine being able to merge only the @c max_length
   * smallest elements.
   *
   * The @c begin iterators are advanced accordingly, they might not
   * reach @c end, in contrast to the usual variant.
   * Specially designed code should allow the compiler to generate
   * conditional moves instead of branches.
   * @param begin1 Begin iterator of first sequence.
   * @param end1 End iterator of first sequence.
   * @param begin2 Begin iterator of second sequence.
   * @param end2 End iterator of second sequence.
   * @param target Target begin iterator.
   * @param max_length Maximum number of elements to merge.
   * @param comp Comparator.
   * @return Output end iterator. */
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputIterator, typename _DifferenceTp,
	   typename Comparator>
    OutputIterator
    merge_advance_movc(RandomAccessIterator1& begin1,
		       RandomAccessIterator1 end1,
		       RandomAccessIterator2& begin2,
		       RandomAccessIterator2 end2,
		       OutputIterator target,
		       _DifferenceTp max_length, Comparator comp)
    {
      typedef _DifferenceTp difference_type;
      typedef typename std::iterator_traits<RandomAccessIterator1>::value_type
	value_type1;
      typedef typename std::iterator_traits<RandomAccessIterator2>::value_type
	value_type2;

#if _GLIBCXX_ASSERTIONS
      _GLIBCXX_PARALLEL_ASSERT(max_length >= 0);
#endif

      while (begin1 != end1 && begin2 != end2 && max_length > 0)
	{
	  RandomAccessIterator1 next1 = begin1 + 1;
	  RandomAccessIterator2 next2 = begin2 + 1;
	  value_type1 element1 = *begin1;
	  value_type2 element2 = *begin2;

	  if (comp(element2, element1))
	    {
	      element1 = element2;
	      begin2 = next2;
	    }
	  else
	    begin1 = next1;

	  *target = element1;

	  ++target;
	  --max_length;
	}
      if (begin1 != end1)
	{
	  target = std::copy(begin1, begin1 + max_length, target);
	  begin1 += max_length;
	}
      else
	{
	  target = std::copy(begin2, begin2 + max_length, target);
	  begin2 += max_length;
	}
      return target;
    }

  /** @brief Merge routine being able to merge only the @c max_length
   * smallest elements.
   *
   *  The @c begin iterators are advanced accordingly, they might not
   *  reach @c end, in contrast to the usual variant.
   *  Static switch on whether to use the conditional-move variant.
   *  @param begin1 Begin iterator of first sequence.
   *  @param end1 End iterator of first sequence.
   *  @param begin2 Begin iterator of second sequence.
   *  @param end2 End iterator of second sequence.
   *  @param target Target begin iterator.
   *  @param max_length Maximum number of elements to merge.
   *  @param comp Comparator.
   *  @return Output end iterator. */
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputIterator, typename _DifferenceTp,
	   typename Comparator>
    inline OutputIterator
    merge_advance(RandomAccessIterator1& begin1, RandomAccessIterator1 end1,
		  RandomAccessIterator2& begin2, RandomAccessIterator2 end2,
		  OutputIterator target, _DifferenceTp max_length,
		  Comparator comp)
    {
      _GLIBCXX_CALL(max_length)

      return merge_advance_movc(begin1, end1, begin2, end2, target,
				max_length, comp);
    }

  /** @brief Merge routine fallback to sequential in case the
      iterators of the two input sequences are of different type.
      *  @param begin1 Begin iterator of first sequence.
      *  @param end1 End iterator of first sequence.
      *  @param begin2 Begin iterator of second sequence.
      *  @param end2 End iterator of second sequence.
      *  @param target Target begin iterator.
      *  @param max_length Maximum number of elements to merge.
      *  @param comp Comparator.
      *  @return Output end iterator. */
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename RandomAccessIterator3, typename Comparator>
    inline RandomAccessIterator3
    parallel_merge_advance(RandomAccessIterator1& begin1,
			   RandomAccessIterator1 end1,
			   RandomAccessIterator2& begin2,
			   // different iterators, parallel implementation
			   // not available			   
			   RandomAccessIterator2 end2,
			   RandomAccessIterator3 target, typename
			   std::iterator_traits<RandomAccessIterator1>::
			   difference_type max_length, Comparator comp)
    { return merge_advance(begin1, end1, begin2, end2, target,
			   max_length, comp); }

  /** @brief Parallel merge routine being able to merge only the @c
   * max_length smallest elements.
   *
   *  The @c begin iterators are advanced accordingly, they might not
   *  reach @c end, in contrast to the usual variant.
   *  The functionality is projected onto parallel_multiway_merge.
   *  @param begin1 Begin iterator of first sequence.
   *  @param end1 End iterator of first sequence.
   *  @param begin2 Begin iterator of second sequence.
   *  @param end2 End iterator of second sequence.
   *  @param target Target begin iterator.
   *  @param max_length Maximum number of elements to merge.
   *  @param comp Comparator.
   *  @return Output end iterator.
   */
  template<typename RandomAccessIterator1, typename RandomAccessIterator3,
	   typename Comparator>
    inline RandomAccessIterator3
    parallel_merge_advance(RandomAccessIterator1& begin1,
			   RandomAccessIterator1 end1,
			   RandomAccessIterator1& begin2,
			   RandomAccessIterator1 end2,
			   RandomAccessIterator3 target, typename
			   std::iterator_traits<RandomAccessIterator1>::
			   difference_type max_length, Comparator comp)
    {
      typedef typename
          std::iterator_traits<RandomAccessIterator1>::value_type value_type;
      typedef typename std::iterator_traits<RandomAccessIterator1>::
	difference_type difference_type1 /* == difference_type2 */;
      typedef typename std::iterator_traits<RandomAccessIterator3>::
	difference_type difference_type3;
      typedef typename std::pair<RandomAccessIterator1, RandomAccessIterator1>
        iterator_pair;

      std::pair<RandomAccessIterator1, RandomAccessIterator1>
	seqs[2] = { std::make_pair(begin1, end1),
		    std::make_pair(begin2, end2) };
      RandomAccessIterator3
        target_end = parallel_multiway_merge
          < /* stable = */ true, /* sentinels = */ false>(
            seqs, seqs + 2, target, comp,
            multiway_merge_exact_splitting
              < /* stable = */ true, iterator_pair*,
                Comparator, difference_type1>,
            max_length);

      return target_end;
    }
}	//namespace __gnu_parallel

#endif
