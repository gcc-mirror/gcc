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

/** @file parallel/algo.h
 *  @brief Parallel STL function calls corresponding to the stl_algo.h header.
 *
 *  The functions defined here mainly do case switches and
 *  call the actual parallelized versions in other files.
 *  Inlining policy: Functions that basically only contain one function call,
 *  are declared inline.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler and Felix Putze.

#ifndef _GLIBCXX_PARALLEL_ALGO_H
#define _GLIBCXX_PARALLEL_ALGO_H 1

#include <parallel/algorithmfwd.h>
#include <bits/stl_algobase.h>
#include <bits/stl_algo.h>
#include <parallel/iterator.h>
#include <parallel/base.h>
#include <parallel/sort.h>
#include <parallel/workstealing.h>
#include <parallel/par_loop.h>
#include <parallel/omp_loop.h>
#include <parallel/omp_loop_static.h>
#include <parallel/for_each_selectors.h>
#include <parallel/for_each.h>
#include <parallel/find.h>
#include <parallel/find_selectors.h>
#include <parallel/search.h>
#include <parallel/random_shuffle.h>
#include <parallel/partition.h>
#include <parallel/merge.h>
#include <parallel/unique_copy.h>
#include <parallel/set_operations.h>

namespace std
{
namespace __parallel
{
  // Sequential fallback
  template<typename InputIterator, typename Function>
    inline Function
    for_each(InputIterator begin, InputIterator end, Function f, 
	     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::for_each(begin, end, f); }

  // Sequential fallback for input iterator case
  template<typename InputIterator, typename Function, typename IteratorTag>
    inline Function
    for_each_switch(InputIterator begin, InputIterator end, Function f, 
		    IteratorTag)
    { return for_each(begin, end, f, __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename Function>
    Function
    for_each_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		    Function f, random_access_iterator_tag, 
		    __gnu_parallel::_Parallelism parallelism_tag
		    = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().for_each_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  bool dummy;
	  __gnu_parallel::for_each_selector<RandomAccessIterator> functionality;

	  return __gnu_parallel::
	    for_each_template_random_access(begin, end, f, functionality,
					    __gnu_parallel::dummy_reduct(),
					    true, dummy, -1, parallelism_tag);
	}
      else
	return for_each(begin, end, f, __gnu_parallel::sequential_tag());
    }

  // Public interface
  template<typename Iterator, typename Function>
    inline Function
    for_each(Iterator begin, Iterator end, Function f, 
	     __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef std::iterator_traits<Iterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      return for_each_switch(begin, end, f, iterator_category(), 
			     parallelism_tag);
    }

  template<typename Iterator, typename Function>
    inline Function
    for_each(Iterator begin, Iterator end, Function f) 
    {
      typedef std::iterator_traits<Iterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      return for_each_switch(begin, end, f, iterator_category());
    }


  // Sequential fallback
  template<typename InputIterator, typename T>
    inline InputIterator
    find(InputIterator begin, InputIterator end, const T& val, 
	 __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::find(begin, end, val); }

  // Sequential fallback for input iterator case
  template<typename InputIterator, typename T, typename IteratorTag>
    inline InputIterator
    find_switch(InputIterator begin, InputIterator end, const T& val,
		IteratorTag)
    { return _GLIBCXX_STD_P::find(begin, end, val); }

  // Parallel find for random access iterators
  template<typename RandomAccessIterator, typename T>
    RandomAccessIterator
    find_switch(RandomAccessIterator begin, RandomAccessIterator end,
		const T& val, random_access_iterator_tag)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;

      if (_GLIBCXX_PARALLEL_CONDITION(true))
	{
	  binder2nd<__gnu_parallel::equal_to<value_type, T> >
	    comp(__gnu_parallel::equal_to<value_type, T>(), val);
	  return __gnu_parallel::find_template(begin, end, begin, comp,
					       __gnu_parallel::
					       find_if_selector()).first;
	}
      else
	return _GLIBCXX_STD_P::find(begin, end, val);
    }

  // Public interface
  template<typename InputIterator, typename T>
    inline InputIterator
    find(InputIterator begin, InputIterator end, const T& val)
    {
      typedef std::iterator_traits<InputIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      return find_switch(begin, end, val, iterator_category());
    }

  // Sequential fallback
  template<typename InputIterator, typename Predicate>
    inline InputIterator
    find_if(InputIterator begin, InputIterator end, Predicate pred, 
	    __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::find_if(begin, end, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator, typename Predicate, typename IteratorTag>
    inline InputIterator
    find_if_switch(InputIterator begin, InputIterator end, Predicate pred, 
		   IteratorTag)
    { return _GLIBCXX_STD_P::find_if(begin, end, pred); }

  // Parallel find_if for random access iterators
  template<typename RandomAccessIterator, typename Predicate>
    RandomAccessIterator
    find_if_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		   Predicate pred, random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	return __gnu_parallel::find_template(begin, end, begin, pred, 
					     __gnu_parallel::
					     find_if_selector()).first;
      else
	return _GLIBCXX_STD_P::find_if(begin, end, pred);
    }

  // Public interface
  template<typename InputIterator, typename Predicate>
    inline InputIterator
    find_if(InputIterator begin, InputIterator end, Predicate pred)
    {
      typedef std::iterator_traits<InputIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      return find_if_switch(begin, end, pred, iterator_category());
    }

  // Sequential fallback
  template<typename InputIterator, typename ForwardIterator>
    inline InputIterator
    find_first_of(InputIterator begin1, InputIterator end1, 
		  ForwardIterator begin2, ForwardIterator end2, 
		  __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::find_first_of(begin1, end1, begin2, end2); }

  // Sequential fallback
  template<typename InputIterator, typename ForwardIterator,
	   typename BinaryPredicate>
    inline InputIterator
    find_first_of(InputIterator begin1, InputIterator end1,
		  ForwardIterator begin2, ForwardIterator end2,
		  BinaryPredicate comp, __gnu_parallel::sequential_tag)
  { return _GLIBCXX_STD_P::find_first_of(begin1, end1, begin2, end2, comp); }

  // Sequential fallback for input iterator type
  template<typename InputIterator, typename ForwardIterator,
	   typename IteratorTag1, typename IteratorTag2>
    inline InputIterator
    find_first_of_switch(InputIterator begin1, InputIterator end1,
			 ForwardIterator begin2, ForwardIterator end2, 
			 IteratorTag1, IteratorTag2)
    { return find_first_of(begin1, end1, begin2, end2, 
			   __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename ForwardIterator,
	   typename BinaryPredicate, typename IteratorTag>
    inline RandomAccessIterator
    find_first_of_switch(RandomAccessIterator begin1,
			 RandomAccessIterator end1,
			 ForwardIterator begin2, ForwardIterator end2, 
			 BinaryPredicate comp, random_access_iterator_tag, 
			 IteratorTag)
    {
      return __gnu_parallel::
	find_template(begin1, end1, begin1, comp,
		      __gnu_parallel::find_first_of_selector
		      <ForwardIterator>(begin2, end2)).first;
    }

  // Sequential fallback for input iterator type
  template<typename InputIterator, typename ForwardIterator,
	   typename BinaryPredicate, typename IteratorTag1,
	   typename IteratorTag2>
    inline InputIterator
    find_first_of_switch(InputIterator begin1, InputIterator end1,
			 ForwardIterator begin2, ForwardIterator end2, 
			 BinaryPredicate comp, IteratorTag1, IteratorTag2)
    { return find_first_of(begin1, end1, begin2, end2, comp, 
			   __gnu_parallel::sequential_tag()); }

  // Public interface
  template<typename InputIterator, typename ForwardIterator,
	   typename BinaryPredicate>
    inline InputIterator
    find_first_of(InputIterator begin1, InputIterator end1,
		  ForwardIterator begin2, ForwardIterator end2, 
		  BinaryPredicate comp)
    {
      typedef std::iterator_traits<InputIterator> iteratori_traits;
      typedef std::iterator_traits<ForwardIterator> iteratorf_traits;
      typedef typename iteratori_traits::iterator_category iteratori_category;
      typedef typename iteratorf_traits::iterator_category iteratorf_category;

      return find_first_of_switch(begin1, end1, begin2, end2, comp,
				  iteratori_category(), iteratorf_category());
    }

  // Public interface, insert default comparator
  template<typename InputIterator, typename ForwardIterator>
    inline InputIterator
    find_first_of(InputIterator begin1, InputIterator end1, 
		  ForwardIterator begin2, ForwardIterator end2)
    {
      typedef std::iterator_traits<InputIterator> iteratori_traits;
      typedef std::iterator_traits<ForwardIterator> iteratorf_traits;
      typedef typename iteratori_traits::value_type valuei_type;
      typedef typename iteratorf_traits::value_type valuef_type;

      return find_first_of(begin1, end1, begin2, end2, __gnu_parallel::
			   equal_to<valuei_type, valuef_type>());
    }

  // Sequential fallback
  template<typename InputIterator, typename OutputIterator>
    inline OutputIterator
    unique_copy(InputIterator begin1, InputIterator end1, OutputIterator out,
		__gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::unique_copy(begin1, end1, out); }

  // Sequential fallback
  template<typename InputIterator, typename OutputIterator,
	   typename Predicate>
    inline OutputIterator
    unique_copy(InputIterator begin1, InputIterator end1, OutputIterator out,
		Predicate pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::unique_copy(begin1, end1, out, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator, typename OutputIterator,
	   typename Predicate, typename IteratorTag1, typename IteratorTag2>
    inline OutputIterator
    unique_copy_switch(InputIterator begin, InputIterator last, 
		       OutputIterator out, Predicate pred, 
		       IteratorTag1, IteratorTag2)
    { return _GLIBCXX_STD_P::unique_copy(begin, last, out, pred); }

  // Parallel unique_copy for random access iterators
  template<typename RandomAccessIterator, typename RandomAccessOutputIterator,
	   typename Predicate>
    RandomAccessOutputIterator
    unique_copy_switch(RandomAccessIterator begin, RandomAccessIterator last, 
		       RandomAccessOutputIterator out, Predicate pred, 
		       random_access_iterator_tag, random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(last - begin)
	    > __gnu_parallel::_Settings::get().unique_copy_minimal_n))
	return __gnu_parallel::parallel_unique_copy(begin, last, out, pred);
      else
	return _GLIBCXX_STD_P::unique_copy(begin, last, out, pred);
    }

  // Public interface
  template<typename InputIterator, typename OutputIterator>
    inline OutputIterator
    unique_copy(InputIterator begin1, InputIterator end1, OutputIterator out)
    {
      typedef std::iterator_traits<InputIterator> iteratori_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori_traits::iterator_category iteratori_category;
      typedef typename iteratori_traits::value_type value_type;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return unique_copy_switch(begin1, end1, out, equal_to<value_type>(),
				iteratori_category(), iteratoro_category());
    }

  // Public interface
  template<typename InputIterator, typename OutputIterator, typename Predicate>
    inline OutputIterator
    unique_copy(InputIterator begin1, InputIterator end1, OutputIterator out,
		Predicate pred)
    {
      typedef std::iterator_traits<InputIterator> iteratori_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori_traits::iterator_category iteratori_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return unique_copy_switch(begin1, end1, out, pred, iteratori_category(), 
				iteratoro_category());
    }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    set_union(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, InputIterator2 end2,
	      OutputIterator out, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_union(begin1, end1, begin2, end2, out); }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator
    set_union(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, InputIterator2 end2,
	      OutputIterator out, Predicate pred,
	      __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_union(begin1, end1,
				       begin2, end2, out, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate, typename OutputIterator,
	   typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
    inline OutputIterator
    set_union_switch(InputIterator1 begin1, InputIterator1 end1, 
		     InputIterator2 begin2, InputIterator2 end2, 
		     OutputIterator result, Predicate pred, IteratorTag1,
		     IteratorTag2, IteratorTag3)
    { return _GLIBCXX_STD_P::set_union(begin1, end1,
				       begin2, end2, result, pred); }

  // Parallel set_union for random access iterators
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputRandomAccessIterator, typename Predicate>
    OutputRandomAccessIterator
    set_union_switch(RandomAccessIterator1 begin1, RandomAccessIterator1 end1, 
		     RandomAccessIterator2 begin2, RandomAccessIterator2 end2, 
		     OutputRandomAccessIterator result, Predicate pred,
		     random_access_iterator_tag, random_access_iterator_tag, 
		     random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end1 - begin1)
	    >= __gnu_parallel::_Settings::get().set_union_minimal_n
	    || static_cast<__gnu_parallel::sequence_index_t>(end2 - begin2)
	    >= __gnu_parallel::_Settings::get().set_union_minimal_n))
	return __gnu_parallel::parallel_set_union(begin1, end1,
						  begin2, end2, result, pred);
      else
	return _GLIBCXX_STD_P::set_union(begin1, end1,
					 begin2, end2, result, pred);
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator 
    set_union(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, InputIterator2 end2, OutputIterator out)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;
      typedef typename iteratori1_traits::value_type value1_type;
      typedef typename iteratori2_traits::value_type value2_type;

      return set_union_switch(begin1, end1, begin2, end2, out, 
			      __gnu_parallel::less<value1_type, value2_type>(), 
			      iteratori1_category(), iteratori2_category(), 
			      iteratoro_category());
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator 
    set_union(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, InputIterator2 end2,
	      OutputIterator out, Predicate pred)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return set_union_switch(begin1, end1, begin2, end2, out, pred,
			      iteratori1_category(), iteratori2_category(), 
			      iteratoro_category());
    }

  // Sequential fallback.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    set_intersection(InputIterator1 begin1, InputIterator1 end1,
		     InputIterator2 begin2, InputIterator2 end2,
		     OutputIterator out, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_intersection(begin1, end1,
					      begin2, end2, out); }

  // Sequential fallback.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator
    set_intersection(InputIterator1 begin1, InputIterator1 end1,
		     InputIterator2 begin2, InputIterator2 end2,
		     OutputIterator out, Predicate pred, 
		     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_intersection(begin1, end1, begin2, end2, 
					      out, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate, typename OutputIterator,
	   typename IteratorTag1, typename IteratorTag2,
	   typename IteratorTag3>
    inline OutputIterator 
    set_intersection_switch(InputIterator1 begin1, InputIterator1 end1, 
			    InputIterator2 begin2, InputIterator2 end2, 
			    OutputIterator result, Predicate pred, 
			    IteratorTag1, IteratorTag2, IteratorTag3)
    { return _GLIBCXX_STD_P::set_intersection(begin1, end1, begin2, 
					      end2, result, pred); }

  // Parallel set_intersection for random access iterators
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputRandomAccessIterator, typename Predicate>
    OutputRandomAccessIterator
    set_intersection_switch(RandomAccessIterator1 begin1,
			    RandomAccessIterator1 end1,
			    RandomAccessIterator2 begin2,
			    RandomAccessIterator2 end2,
			    OutputRandomAccessIterator result,
			    Predicate pred,
			    random_access_iterator_tag,
			    random_access_iterator_tag,
			    random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end1 - begin1)
	    >= __gnu_parallel::_Settings::get().set_union_minimal_n
	    || static_cast<__gnu_parallel::sequence_index_t>(end2 - begin2)
	    >= __gnu_parallel::_Settings::get().set_union_minimal_n))
	return __gnu_parallel::parallel_set_intersection(begin1, end1, begin2, 
							 end2, result, pred);
      else
	return _GLIBCXX_STD_P::set_intersection(begin1, end1, begin2, 
						end2, result, pred);
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator 
    set_intersection(InputIterator1 begin1, InputIterator1 end1, 
		     InputIterator2 begin2, InputIterator2 end2, 
		     OutputIterator out)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;
      typedef typename iteratori1_traits::value_type value1_type;
      typedef typename iteratori2_traits::value_type value2_type;

      return set_intersection_switch(begin1, end1, begin2, end2, out,
				     __gnu_parallel::
				     less<value1_type, value2_type>(),
				     iteratori1_category(),
				     iteratori2_category(), 
				     iteratoro_category());
    }

  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator 
    set_intersection(InputIterator1 begin1, InputIterator1 end1,
		     InputIterator2 begin2, InputIterator2 end2,
		     OutputIterator out, Predicate pred)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return set_intersection_switch(begin1, end1, begin2, end2, out, pred,
				     iteratori1_category(),
				     iteratori2_category(),
				     iteratoro_category());
    }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    set_symmetric_difference(InputIterator1 begin1, InputIterator1 end1,
			     InputIterator2 begin2, InputIterator2 end2,
			     OutputIterator out,
			     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_symmetric_difference(begin1,end1,
						      begin2, end2, out); }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator
    set_symmetric_difference(InputIterator1 begin1, InputIterator1 end1,
			     InputIterator2 begin2, InputIterator2 end2,
			     OutputIterator out, Predicate pred,
			     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_symmetric_difference(begin1, end1, begin2,
						      end2, out, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate, typename OutputIterator,
	   typename IteratorTag1, typename IteratorTag2,
	   typename IteratorTag3>
    inline OutputIterator 
    set_symmetric_difference_switch(InputIterator1 begin1,
				    InputIterator1 end1,
				    InputIterator2 begin2,
				    InputIterator2 end2,
				    OutputIterator result, Predicate pred,
				    IteratorTag1, IteratorTag2, IteratorTag3)
    { return _GLIBCXX_STD_P::set_symmetric_difference(begin1, end1,
						      begin2, end2,
						      result, pred); }

  // Parallel set_symmetric_difference for random access iterators
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputRandomAccessIterator, typename Predicate>
    OutputRandomAccessIterator
    set_symmetric_difference_switch(RandomAccessIterator1 begin1,
				    RandomAccessIterator1 end1,
				    RandomAccessIterator2 begin2,
				    RandomAccessIterator2 end2,
				    OutputRandomAccessIterator result,
				    Predicate pred,
				    random_access_iterator_tag,
				    random_access_iterator_tag,
				    random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end1 - begin1)
	    >= __gnu_parallel::_Settings::get().set_symmetric_difference_minimal_n
	    || static_cast<__gnu_parallel::sequence_index_t>(end2 - begin2)
	    >= __gnu_parallel::_Settings::get().set_symmetric_difference_minimal_n))
	return __gnu_parallel::parallel_set_symmetric_difference(begin1, end1,
								 begin2, end2,
								 result, pred);
      else
	return _GLIBCXX_STD_P::set_symmetric_difference(begin1, end1,
							begin2, end2,
							result, pred);
    }

  // Public interface.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator 
    set_symmetric_difference(InputIterator1 begin1, InputIterator1 end1,
			     InputIterator2 begin2, InputIterator2 end2,
			     OutputIterator out)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;
      typedef typename iteratori1_traits::value_type value1_type;
      typedef typename iteratori2_traits::value_type value2_type;

      return set_symmetric_difference_switch(begin1, end1, begin2, end2, out,
					     __gnu_parallel::
					     less<value1_type, value2_type>(),
					     iteratori1_category(),
					     iteratori2_category(),
					     iteratoro_category());
    }

  // Public interface.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator 
    set_symmetric_difference(InputIterator1 begin1, InputIterator1 end1,
			     InputIterator2 begin2, InputIterator2 end2,
			     OutputIterator out, Predicate pred)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return set_symmetric_difference_switch(begin1, end1, begin2, end2, out,
					     pred, iteratori1_category(),
					     iteratori2_category(),
					     iteratoro_category());
    }

  // Sequential fallback.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    set_difference(InputIterator1 begin1, InputIterator1 end1, 
		   InputIterator2 begin2, InputIterator2 end2, 
		   OutputIterator out, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_difference(begin1,end1, begin2, end2, out); }

  // Sequential fallback.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator
    set_difference(InputIterator1 begin1, InputIterator1 end1, 
		   InputIterator2 begin2, InputIterator2 end2, 
		   OutputIterator out, Predicate pred, 
		   __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::set_difference(begin1, end1,
					    begin2, end2, out, pred); }

  // Sequential fallback for input iterator case.
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate, typename OutputIterator,
	   typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
    inline OutputIterator
    set_difference_switch(InputIterator1 begin1, InputIterator1 end1, 
			  InputIterator2 begin2, InputIterator2 end2, 
			  OutputIterator result, Predicate pred, 
			  IteratorTag1, IteratorTag2, IteratorTag3)
    { return _GLIBCXX_STD_P::set_difference(begin1, end1,
					    begin2, end2, result, pred); }

  // Parallel set_difference for random access iterators
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename OutputRandomAccessIterator, typename Predicate>
    OutputRandomAccessIterator
    set_difference_switch(RandomAccessIterator1 begin1,
			  RandomAccessIterator1 end1,
			  RandomAccessIterator2 begin2,
			  RandomAccessIterator2 end2,
			  OutputRandomAccessIterator result, Predicate pred,
			  random_access_iterator_tag,
			  random_access_iterator_tag,
			  random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end1 - begin1)
	    >= __gnu_parallel::_Settings::get().set_difference_minimal_n
	    || static_cast<__gnu_parallel::sequence_index_t>(end2 - begin2)
	    >= __gnu_parallel::_Settings::get().set_difference_minimal_n))
	return __gnu_parallel::parallel_set_difference(begin1, end1,
						       begin2, end2,
						       result, pred);
      else
	return _GLIBCXX_STD_P::set_difference(begin1, end1,
					      begin2, end2, result, pred);
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    set_difference(InputIterator1 begin1, InputIterator1 end1, 
		   InputIterator2 begin2, InputIterator2 end2, 
		   OutputIterator out)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;
      typedef typename iteratori1_traits::value_type value1_type;
      typedef typename iteratori2_traits::value_type value2_type;

      return set_difference_switch(begin1, end1, begin2, end2, out,
				   __gnu_parallel::
				   less<value1_type, value2_type>(), 
				   iteratori1_category(),
				   iteratori2_category(), 
				   iteratoro_category());
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Predicate>
    inline OutputIterator
    set_difference(InputIterator1 begin1, InputIterator1 end1, 
		   InputIterator2 begin2, InputIterator2 end2, 
		   OutputIterator out, Predicate pred)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return set_difference_switch(begin1, end1, begin2, end2, out, pred,
				   iteratori1_category(),
				   iteratori2_category(), 
				   iteratoro_category());
    }

  // Sequential fallback
  template<typename ForwardIterator>
    inline ForwardIterator
    adjacent_find(ForwardIterator begin, ForwardIterator end, 
		  __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::adjacent_find(begin, end); }

  // Sequential fallback
  template<typename ForwardIterator, typename BinaryPredicate>
    inline ForwardIterator
    adjacent_find(ForwardIterator begin, ForwardIterator end, 
		  BinaryPredicate binary_pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::adjacent_find(begin, end, binary_pred); }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator>
    RandomAccessIterator
    adjacent_find_switch(RandomAccessIterator begin, RandomAccessIterator end, 
			 random_access_iterator_tag)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;

      if (_GLIBCXX_PARALLEL_CONDITION(true))
	{
	  RandomAccessIterator spot = __gnu_parallel::
	    find_template(begin, end - 1, begin, equal_to<value_type>(),
			  __gnu_parallel::adjacent_find_selector()).first;
	  if (spot == (end - 1))
	    return end;
	  else
	    return spot;
	}
      else
	return adjacent_find(begin, end, __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator, typename IteratorTag>
    inline ForwardIterator
    adjacent_find_switch(ForwardIterator begin, ForwardIterator end,
			 IteratorTag)
    { return adjacent_find(begin, end, __gnu_parallel::sequential_tag()); }

  // Public interface
  template<typename ForwardIterator>
    inline ForwardIterator
    adjacent_find(ForwardIterator begin, ForwardIterator end)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return adjacent_find_switch(begin, end, iterator_category());
    }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator, typename BinaryPredicate,
	   typename IteratorTag>
    inline ForwardIterator
    adjacent_find_switch(ForwardIterator begin, ForwardIterator end, 
			 BinaryPredicate pred, IteratorTag)
    { return adjacent_find(begin, end, pred,
			   __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename BinaryPredicate>
    RandomAccessIterator
    adjacent_find_switch(RandomAccessIterator begin, RandomAccessIterator end, 
			 BinaryPredicate pred, random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	return __gnu_parallel::find_template(begin, end, begin, pred, 
					     __gnu_parallel::
					     adjacent_find_selector()).first;
      else
	return adjacent_find(begin, end, pred,
			     __gnu_parallel::sequential_tag());
    }

  // Public interface
  template<typename ForwardIterator, typename BinaryPredicate>
    inline ForwardIterator
    adjacent_find(ForwardIterator begin, ForwardIterator end, 
		  BinaryPredicate pred)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return adjacent_find_switch(begin, end, pred, iterator_category());
    }

  // Sequential fallback
  template<typename InputIterator, typename T>
    inline typename iterator_traits<InputIterator>::difference_type
    count(InputIterator begin, InputIterator end, const T& value, 
	  __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::count(begin, end, value); }

  // Parallel code for random access iterators
  template<typename RandomAccessIterator, typename T>
    typename iterator_traits<RandomAccessIterator>::difference_type
    count_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		 const T& value, random_access_iterator_tag, 
		 __gnu_parallel::_Parallelism parallelism_tag 
		 = __gnu_parallel::parallel_unbalanced)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;
      typedef __gnu_parallel::sequence_index_t sequence_index_t;

      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().count_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  __gnu_parallel::count_selector<RandomAccessIterator, difference_type>
	    functionality;
	  difference_type res = 0;
	  __gnu_parallel::
	    for_each_template_random_access(begin, end, value,
					    functionality,
					    std::plus<sequence_index_t>(),
					    res, res, -1, parallelism_tag);
	  return res;
	}
      else
	return count(begin, end, value, __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case.
  template<typename InputIterator, typename T, typename IteratorTag>
    inline typename iterator_traits<InputIterator>::difference_type
    count_switch(InputIterator begin, InputIterator end, const T& value, 
		 IteratorTag)
    { return count(begin, end, value, __gnu_parallel::sequential_tag()); }

  // Public interface.
  template<typename InputIterator, typename T>
    inline typename iterator_traits<InputIterator>::difference_type
    count(InputIterator begin, InputIterator end, const T& value, 
	  __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef iterator_traits<InputIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return count_switch(begin, end, value, iterator_category(), 
			  parallelism_tag);
    }

  template<typename InputIterator, typename T>
    inline typename iterator_traits<InputIterator>::difference_type
    count(InputIterator begin, InputIterator end, const T& value)
    {
      typedef iterator_traits<InputIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return count_switch(begin, end, value, iterator_category());
    }


  // Sequential fallback.
  template<typename InputIterator, typename Predicate>
    inline typename iterator_traits<InputIterator>::difference_type
    count_if(InputIterator begin, InputIterator end, Predicate pred, 
	     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::count_if(begin, end, pred); }

  // Parallel count_if for random access iterators
  template<typename RandomAccessIterator, typename Predicate>
    typename iterator_traits<RandomAccessIterator>::difference_type
    count_if_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		    Predicate pred, random_access_iterator_tag, 
		    __gnu_parallel::_Parallelism parallelism_tag
		    = __gnu_parallel::parallel_unbalanced)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;
      typedef __gnu_parallel::sequence_index_t sequence_index_t;

      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().count_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  difference_type res = 0;
	  __gnu_parallel::
	    count_if_selector<RandomAccessIterator, difference_type>
	    functionality;
	  __gnu_parallel::
	    for_each_template_random_access(begin, end, pred,
					    functionality,
					    std::plus<sequence_index_t>(),
					    res, res, -1, parallelism_tag);
	  return res;
	}
      else
	return count_if(begin, end, pred, __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case.
  template<typename InputIterator, typename Predicate, typename IteratorTag>
    inline typename iterator_traits<InputIterator>::difference_type
    count_if_switch(InputIterator begin, InputIterator end, Predicate pred, 
		    IteratorTag)
    { return count_if(begin, end, pred, __gnu_parallel::sequential_tag()); }

  // Public interface.
  template<typename InputIterator, typename Predicate>
    inline typename iterator_traits<InputIterator>::difference_type
    count_if(InputIterator begin, InputIterator end, Predicate pred, 
	     __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef iterator_traits<InputIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return count_if_switch(begin, end, pred, iterator_category(), 
			     parallelism_tag);
    }

  template<typename InputIterator, typename Predicate>
    inline typename iterator_traits<InputIterator>::difference_type
    count_if(InputIterator begin, InputIterator end, Predicate pred)
    {
      typedef iterator_traits<InputIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return count_if_switch(begin, end, pred, iterator_category());
    }


  // Sequential fallback.
  template<typename ForwardIterator1, typename ForwardIterator2>
    inline ForwardIterator1
    search(ForwardIterator1 begin1, ForwardIterator1 end1,
	   ForwardIterator2 begin2, ForwardIterator2 end2,
	   __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::search(begin1, end1, begin2, end2); }

  // Parallel algorithm for random access iterator
  template<typename RandomAccessIterator1, typename RandomAccessIterator2>
    RandomAccessIterator1
    search_switch(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
		  RandomAccessIterator2 begin2, RandomAccessIterator2 end2,
		  random_access_iterator_tag, random_access_iterator_tag)
    {
      typedef std::iterator_traits<RandomAccessIterator1> iterator1_traits;
      typedef typename iterator1_traits::value_type value1_type;
      typedef std::iterator_traits<RandomAccessIterator2> iterator2_traits;
      typedef typename iterator2_traits::value_type value2_type;

      if (_GLIBCXX_PARALLEL_CONDITION(true))
	return __gnu_parallel::
	  search_template(begin1, end1, begin2, end2, __gnu_parallel::
			  equal_to<value1_type, value2_type>());
      else
	return search(begin1, end1, begin2, end2,
		      __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator1, typename ForwardIterator2,
	   typename IteratorTag1, typename IteratorTag2>
    inline ForwardIterator1
    search_switch(ForwardIterator1 begin1, ForwardIterator1 end1,
		  ForwardIterator2 begin2, ForwardIterator2 end2,
		  IteratorTag1, IteratorTag2)
    { return search(begin1, end1, begin2, end2,
		    __gnu_parallel::sequential_tag()); }

  // Public interface.
  template<typename ForwardIterator1, typename ForwardIterator2>
    inline ForwardIterator1
    search(ForwardIterator1 begin1, ForwardIterator1 end1,
	   ForwardIterator2 begin2, ForwardIterator2 end2)
    {
      typedef std::iterator_traits<ForwardIterator1> iterator1_traits;
      typedef typename iterator1_traits::iterator_category iterator1_category;
      typedef std::iterator_traits<ForwardIterator2> iterator2_traits;
      typedef typename iterator2_traits::iterator_category iterator2_category;

      return search_switch(begin1, end1, begin2, end2,
			   iterator1_category(), iterator2_category());
    }

  // Public interface.
  template<typename ForwardIterator1, typename ForwardIterator2,
	   typename BinaryPredicate>
    inline ForwardIterator1
    search(ForwardIterator1 begin1, ForwardIterator1 end1,
	   ForwardIterator2 begin2, ForwardIterator2 end2,
	   BinaryPredicate pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::search(begin1, end1, begin2, end2, pred); }

  // Parallel algorithm for random access iterator.
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename BinaryPredicate>
    RandomAccessIterator1
    search_switch(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
		  RandomAccessIterator2 begin2, RandomAccessIterator2 end2,
		  BinaryPredicate pred,
		  random_access_iterator_tag, random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	return __gnu_parallel::search_template(begin1, end1,
					       begin2, end2, pred);
      else
	return search(begin1, end1, begin2, end2, pred,
		      __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator1, typename ForwardIterator2,
	   typename BinaryPredicate, typename IteratorTag1,
	   typename IteratorTag2>
    inline ForwardIterator1
    search_switch(ForwardIterator1 begin1, ForwardIterator1 end1,
		  ForwardIterator2 begin2, ForwardIterator2 end2,
		  BinaryPredicate pred, IteratorTag1, IteratorTag2)
    { return search(begin1, end1, begin2, end2, pred,
		    __gnu_parallel::sequential_tag()); }

  // Public interface
  template<typename ForwardIterator1, typename ForwardIterator2,
	   typename BinaryPredicate>
    inline ForwardIterator1
    search(ForwardIterator1 begin1, ForwardIterator1 end1,
	   ForwardIterator2 begin2, ForwardIterator2 end2,
	   BinaryPredicate  pred)
    {
      typedef std::iterator_traits<ForwardIterator1> iterator1_traits;
      typedef typename iterator1_traits::iterator_category iterator1_category;
      typedef std::iterator_traits<ForwardIterator2> iterator2_traits;
      typedef typename iterator2_traits::iterator_category iterator2_category;
      return search_switch(begin1, end1, begin2, end2, pred,
			   iterator1_category(), iterator2_category());
    }

  // Sequential fallback
  template<typename ForwardIterator, typename Integer, typename T>
    inline ForwardIterator
    search_n(ForwardIterator begin, ForwardIterator end, Integer count,
	     const T& val, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::search_n(begin, end, count, val); }

  // Sequential fallback
  template<typename ForwardIterator, typename Integer, typename T,
	   typename BinaryPredicate>
    inline ForwardIterator
    search_n(ForwardIterator begin, ForwardIterator end, Integer count,
	     const T& val, BinaryPredicate binary_pred,
	     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::search_n(begin, end, count, val, binary_pred); }

  // Public interface.
  template<typename ForwardIterator, typename Integer, typename T>
    inline ForwardIterator
    search_n(ForwardIterator begin, ForwardIterator end, Integer count,
	     const T& val)
    {
      typedef typename iterator_traits<ForwardIterator>::value_type value_type;
      return search_n(begin, end, count, val,
		      __gnu_parallel::equal_to<value_type, T>());
    }

  // Parallel algorithm for random access iterators.
  template<typename RandomAccessIterator, typename Integer,
	   typename T, typename BinaryPredicate>
    RandomAccessIterator
    search_n_switch(RandomAccessIterator begin, RandomAccessIterator end,
		    Integer count, const T& val, BinaryPredicate binary_pred,
		    random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	{
	  __gnu_parallel::pseudo_sequence<T, Integer> ps(val, count);
	  return __gnu_parallel::search_template(begin, end, ps.begin(),
						 ps.end(), binary_pred);
	}
      else
	return std::__search_n(begin, end, count, val,
			       binary_pred, random_access_iterator_tag());
    }

  // Sequential fallback for input iterator case.
  template<typename ForwardIterator, typename Integer, typename T,
	   typename BinaryPredicate, typename IteratorTag>
    inline ForwardIterator
    search_n_switch(ForwardIterator begin, ForwardIterator end, Integer count,
		    const T& val, BinaryPredicate binary_pred, IteratorTag)
    { return __search_n(begin, end, count, val, binary_pred, IteratorTag()); }

  // Public interface.
  template<typename ForwardIterator, typename Integer, typename T,
	   typename BinaryPredicate>
    inline ForwardIterator
    search_n(ForwardIterator begin, ForwardIterator end, Integer count,
	     const T& val, BinaryPredicate binary_pred)
    {
      return search_n_switch(begin, end, count, val, binary_pred,
			     typename std::iterator_traits<ForwardIterator>::
			     iterator_category());
    }


  // Sequential fallback.
  template<typename InputIterator, typename OutputIterator,
	   typename UnaryOperation>
    inline OutputIterator
    transform(InputIterator begin, InputIterator end, OutputIterator result, 
	      UnaryOperation unary_op, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::transform(begin, end, result, unary_op); }

  // Parallel unary transform for random access iterators.
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename UnaryOperation>
    RandomAccessIterator2
    transform1_switch(RandomAccessIterator1 begin, RandomAccessIterator1 end,
		      RandomAccessIterator2 result, UnaryOperation unary_op,
		      random_access_iterator_tag, random_access_iterator_tag,
		      __gnu_parallel::_Parallelism parallelism_tag
		      = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().transform_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  bool dummy = true;
	  typedef __gnu_parallel::iterator_pair<RandomAccessIterator1,
	    RandomAccessIterator2, random_access_iterator_tag> ip;
	  ip begin_pair(begin, result), end_pair(end, result + (end - begin));
	  __gnu_parallel::transform1_selector<ip> functionality;
	  __gnu_parallel::
	    for_each_template_random_access(begin_pair, end_pair,
					    unary_op, functionality,
					    __gnu_parallel::dummy_reduct(),
					    dummy, dummy, -1, parallelism_tag);
	  return functionality.finish_iterator;
	}
      else
	return transform(begin, end, result, unary_op, 
			 __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case.
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename UnaryOperation, typename IteratorTag1,
	   typename IteratorTag2>
    inline RandomAccessIterator2
    transform1_switch(RandomAccessIterator1 begin, RandomAccessIterator1 end,
		      RandomAccessIterator2 result, UnaryOperation unary_op,
		      IteratorTag1, IteratorTag2)
    { return transform(begin, end, result, unary_op, 
		       __gnu_parallel::sequential_tag()); }

  // Public interface.
  template<typename InputIterator, typename OutputIterator,
	   typename UnaryOperation>
    inline OutputIterator
    transform(InputIterator begin, InputIterator end, OutputIterator result,
	      UnaryOperation unary_op, 
	      __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef std::iterator_traits<InputIterator> iteratori_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori_traits::iterator_category iteratori_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return transform1_switch(begin, end, result, unary_op,
			       iteratori_category(), iteratoro_category(), 
			       parallelism_tag);
    }

  template<typename InputIterator, typename OutputIterator,
	   typename UnaryOperation>
    inline OutputIterator
    transform(InputIterator begin, InputIterator end, OutputIterator result,
	      UnaryOperation unary_op)
    {
      typedef std::iterator_traits<InputIterator> iteratori_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori_traits::iterator_category iteratori_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return transform1_switch(begin, end, result, unary_op,
			       iteratori_category(), iteratoro_category());
    }


  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename BinaryOperation>
    inline OutputIterator
    transform(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, OutputIterator result,
	      BinaryOperation binary_op, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::transform(begin1, end1,
				       begin2, result, binary_op); }

  // Parallel binary transform for random access iterators.
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename RandomAccessIterator3, typename BinaryOperation>
    RandomAccessIterator3
    transform2_switch(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
		      RandomAccessIterator2 begin2,
		      RandomAccessIterator3 result, BinaryOperation binary_op,
		      random_access_iterator_tag, random_access_iterator_tag,
		      random_access_iterator_tag,
		      __gnu_parallel::_Parallelism parallelism_tag 
		      = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
				      (end1 - begin1) >= __gnu_parallel::_Settings::get().transform_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  bool dummy = true;
	  typedef __gnu_parallel::iterator_triple<RandomAccessIterator1,
	    RandomAccessIterator2, RandomAccessIterator3,
	    random_access_iterator_tag> ip;
	  ip begin_triple(begin1, begin2, result),
	    end_triple(end1, begin2 + (end1 - begin1),
		       result + (end1 - begin1));
	  __gnu_parallel::transform2_selector<ip> functionality;
	  __gnu_parallel::
	    for_each_template_random_access(begin_triple, end_triple,
					    binary_op, functionality,
					    __gnu_parallel::dummy_reduct(),
					    dummy, dummy, -1,
					    parallelism_tag);
	  return functionality.finish_iterator;
	}
      else
	return transform(begin1, end1, begin2, result, binary_op, 
			 __gnu_parallel::sequential_tag());
    }

  // Sequential fallback for input iterator case.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename BinaryOperation,
	   typename tag1, typename tag2, typename tag3>
    inline OutputIterator
    transform2_switch(InputIterator1 begin1, InputIterator1 end1, 
		      InputIterator2 begin2, OutputIterator result, 
		      BinaryOperation binary_op, tag1, tag2, tag3)
    { return transform(begin1, end1, begin2, result, binary_op,
		       __gnu_parallel::sequential_tag()); }

  // Public interface.
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename BinaryOperation>
    inline OutputIterator
    transform(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, OutputIterator result,
	      BinaryOperation binary_op, 
	      __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return transform2_switch(begin1, end1, begin2, result, binary_op,
			       iteratori1_category(), iteratori2_category(), 
			       iteratoro_category(), parallelism_tag);
    }

  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename BinaryOperation>
    inline OutputIterator
    transform(InputIterator1 begin1, InputIterator1 end1,
	      InputIterator2 begin2, OutputIterator result,
	      BinaryOperation binary_op)
    {
      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return transform2_switch(begin1, end1, begin2, result, binary_op,
			       iteratori1_category(), iteratori2_category(),
			       iteratoro_category());
    }

  // Sequential fallback
  template<typename ForwardIterator, typename T>
    inline void
    replace(ForwardIterator begin, ForwardIterator end, const T& old_value, 
	    const T& new_value, __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::replace(begin, end, old_value, new_value); }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator, typename T, typename IteratorTag>
    inline void
    replace_switch(ForwardIterator begin, ForwardIterator end, 
		   const T& old_value, const T& new_value, IteratorTag)
    { replace(begin, end, old_value, new_value, 
	      __gnu_parallel::sequential_tag()); }

  // Parallel replace for random access iterators
  template<typename RandomAccessIterator, typename T>
    inline void
    replace_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		   const T& old_value, const T& new_value, 
		   random_access_iterator_tag, 
		   __gnu_parallel::_Parallelism parallelism_tag
		   = __gnu_parallel::parallel_balanced)
    {
      // XXX parallel version is where?
      replace(begin, end, old_value, new_value, 
	      __gnu_parallel::sequential_tag()); 
    }

  // Public interface
  template<typename ForwardIterator, typename T>
    inline void
    replace(ForwardIterator begin, ForwardIterator end, const T& old_value, 
	    const T& new_value, __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      replace_switch(begin, end, old_value, new_value, iterator_category(), 
		     parallelism_tag);
    }

  template<typename ForwardIterator, typename T>
    inline void
    replace(ForwardIterator begin, ForwardIterator end, const T& old_value, 
	    const T& new_value)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      replace_switch(begin, end, old_value, new_value, iterator_category());
    }


  // Sequential fallback
  template<typename ForwardIterator, typename Predicate, typename T>
    inline void
    replace_if(ForwardIterator begin, ForwardIterator end, Predicate pred, 
	       const T& new_value, __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::replace_if(begin, end, pred, new_value); }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator, typename Predicate, typename T,
	   typename IteratorTag>
    inline void
    replace_if_switch(ForwardIterator begin, ForwardIterator end,
		      Predicate pred, const T& new_value, IteratorTag)
    { replace_if(begin, end, pred, new_value,
		 __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators.
  template<typename RandomAccessIterator, typename Predicate, typename T>
    void
    replace_if_switch(RandomAccessIterator begin, RandomAccessIterator end,
		      Predicate pred, const T& new_value,
		      random_access_iterator_tag,
		      __gnu_parallel::_Parallelism parallelism_tag
		      = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().replace_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  bool dummy;
	  __gnu_parallel::
	    replace_if_selector<RandomAccessIterator, Predicate, T>
	    functionality(new_value);
	  __gnu_parallel::
	    for_each_template_random_access(begin, end, pred,
					    functionality,
					    __gnu_parallel::dummy_reduct(),
					    true, dummy, -1, parallelism_tag);
	}
      else
	replace_if(begin, end, pred, new_value, 
		   __gnu_parallel::sequential_tag());
    }

  // Public interface.
  template<typename ForwardIterator, typename Predicate, typename T>
    inline void
    replace_if(ForwardIterator begin, ForwardIterator end,
	       Predicate pred, const T& new_value, 
	       __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef std::iterator_traits<ForwardIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      replace_if_switch(begin, end, pred, new_value, iterator_category(), 
			parallelism_tag);
    }

  template<typename ForwardIterator, typename Predicate, typename T>
    inline void
    replace_if(ForwardIterator begin, ForwardIterator end,
	       Predicate pred, const T& new_value)
    {
      typedef std::iterator_traits<ForwardIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      replace_if_switch(begin, end, pred, new_value, iterator_category());
    }

  // Sequential fallback
  template<typename ForwardIterator, typename Generator>
    inline void
    generate(ForwardIterator begin, ForwardIterator end, Generator gen, 
	     __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::generate(begin, end, gen); }

  // Sequential fallback for input iterator case.
  template<typename ForwardIterator, typename Generator, typename IteratorTag>
    inline void
    generate_switch(ForwardIterator begin, ForwardIterator end, Generator gen, 
		    IteratorTag)
    { generate(begin, end, gen, __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators.
  template<typename RandomAccessIterator, typename Generator>
    void
    generate_switch(RandomAccessIterator begin, RandomAccessIterator end,
		    Generator gen, random_access_iterator_tag, 
		    __gnu_parallel::_Parallelism parallelism_tag
		    = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().generate_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  bool dummy;
	  __gnu_parallel::generate_selector<RandomAccessIterator>
	    functionality;
	  __gnu_parallel::
	    for_each_template_random_access(begin, end, gen, functionality,
					    __gnu_parallel::dummy_reduct(),
					    true, dummy, -1, parallelism_tag);
	}
      else
	generate(begin, end, gen, __gnu_parallel::sequential_tag());
    }

  // Public interface.
  template<typename ForwardIterator, typename Generator>
    inline void
    generate(ForwardIterator begin, ForwardIterator end,
	     Generator gen, __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef std::iterator_traits<ForwardIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      generate_switch(begin, end, gen, iterator_category(), parallelism_tag);
    }

  template<typename ForwardIterator, typename Generator>
    inline void
    generate(ForwardIterator begin, ForwardIterator end, Generator gen)
    {
      typedef std::iterator_traits<ForwardIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      generate_switch(begin, end, gen, iterator_category());
    }


  // Sequential fallback.
  template<typename OutputIterator, typename Size, typename Generator>
    inline OutputIterator
    generate_n(OutputIterator begin, Size n, Generator gen, 
	       __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::generate_n(begin, n, gen); }

  // Sequential fallback for input iterator case.
  template<typename OutputIterator, typename Size, typename Generator,
	   typename IteratorTag>
    inline OutputIterator
    generate_n_switch(OutputIterator begin, Size n, Generator gen, IteratorTag)
    { return generate_n(begin, n, gen, __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators.
  template<typename RandomAccessIterator, typename Size, typename Generator>
    inline RandomAccessIterator
    generate_n_switch(RandomAccessIterator begin, Size n, Generator gen, 
		      random_access_iterator_tag, 
		      __gnu_parallel::_Parallelism parallelism_tag
		      = __gnu_parallel::parallel_balanced)
    {
      // XXX parallel version is where?
      return generate_n(begin, n, gen, __gnu_parallel::sequential_tag()); 
    }

  // Public interface.
  template<typename OutputIterator, typename Size, typename Generator>
    inline OutputIterator
    generate_n(OutputIterator begin, Size n, Generator gen, 
	       __gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef std::iterator_traits<OutputIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      return generate_n_switch(begin, n, gen, iterator_category(), 
			       parallelism_tag); 
    }

  template<typename OutputIterator, typename Size, typename Generator>
    inline OutputIterator
    generate_n(OutputIterator begin, Size n, Generator gen)
    {
      typedef std::iterator_traits<OutputIterator> iterator_traits;
      typedef typename iterator_traits::iterator_category iterator_category;
      return generate_n_switch(begin, n, gen, iterator_category());
    }


  // Sequential fallback.
  template<typename RandomAccessIterator>
    inline void
    random_shuffle(RandomAccessIterator begin, RandomAccessIterator end, 
		   __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::random_shuffle(begin, end); }

  // Sequential fallback.
  template<typename RandomAccessIterator, typename RandomNumberGenerator>
    inline void
    random_shuffle(RandomAccessIterator begin, RandomAccessIterator end, 
		   RandomNumberGenerator& rand, __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::random_shuffle(begin, end, rand); }


  /** @brief Functor wrapper for std::rand(). */
  template<typename must_be_int = int>
    struct c_rand_number
    {
      int
      operator()(int limit)
      { return rand() % limit; }
    };

  // Fill in random number generator.
  template<typename RandomAccessIterator>
    inline void
    random_shuffle(RandomAccessIterator begin, RandomAccessIterator end)
    {
      c_rand_number<> r;
      // Parallelization still possible.
      __gnu_parallel::random_shuffle(begin, end, r);
    }

  // Parallel algorithm for random access iterators.
  template<typename RandomAccessIterator, typename RandomNumberGenerator>
    void
    random_shuffle(RandomAccessIterator begin, RandomAccessIterator end, 
		   RandomNumberGenerator& rand)
    {
      if (begin == end)
	return;
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().random_shuffle_minimal_n))
	__gnu_parallel::parallel_random_shuffle(begin, end, rand);
      else
	__gnu_parallel::sequential_random_shuffle(begin, end, rand);
    }

  // Sequential fallback.
  template<typename ForwardIterator, typename Predicate>
    inline ForwardIterator
    partition(ForwardIterator begin, ForwardIterator end,
	      Predicate pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::partition(begin, end, pred); }

  // Sequential fallback for input iterator case.
  template<typename ForwardIterator, typename Predicate, typename IteratorTag>
    inline ForwardIterator
    partition_switch(ForwardIterator begin, ForwardIterator end,
		     Predicate pred, IteratorTag)
    { return partition(begin, end, pred, __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators.
  template<typename RandomAccessIterator, typename Predicate>
    RandomAccessIterator
    partition_switch(RandomAccessIterator begin, RandomAccessIterator end,
		     Predicate pred, random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().partition_minimal_n))
	{
	  typedef typename std::iterator_traits<RandomAccessIterator>::
	    difference_type difference_type;
	  difference_type middle = __gnu_parallel::
	    parallel_partition(begin, end, pred,
			       __gnu_parallel::get_max_threads());
	  return begin + middle;
	}
      else
	return partition(begin, end, pred, __gnu_parallel::sequential_tag());
    }

  // Public interface.
  template<typename ForwardIterator, typename Predicate>
    inline ForwardIterator
    partition(ForwardIterator begin, ForwardIterator end, Predicate pred)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return partition_switch(begin, end, pred, iterator_category());
    }

  // Sequential fallback
  template<typename RandomAccessIterator>
    inline void
    sort(RandomAccessIterator begin, RandomAccessIterator end, 
	 __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::sort(begin, end); }

  // Sequential fallback
  template<typename RandomAccessIterator, typename Comparator>
    inline void
    sort(RandomAccessIterator begin, RandomAccessIterator end, Comparator comp,
	 __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::sort<RandomAccessIterator, Comparator>(begin, end,
							     comp); }

  // Public interface, insert default comparator
  template<typename RandomAccessIterator>
    inline void
    sort(RandomAccessIterator begin, RandomAccessIterator end)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      sort(begin, end, std::less<value_type>());
    }

  template<typename RandomAccessIterator, typename Comparator>
    void
    sort(RandomAccessIterator begin, RandomAccessIterator end, Comparator comp)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;

      if (begin != end)
	{
	  if (_GLIBCXX_PARALLEL_CONDITION(
		static_cast<__gnu_parallel::sequence_index_t>(end - begin)
		>= __gnu_parallel::_Settings::get().sort_minimal_n))
	    __gnu_parallel::parallel_sort(begin, end, comp, false);
	  else
	    sort(begin, end, comp, __gnu_parallel::sequential_tag());
	}
    }

  // Sequential fallback.
  template<typename RandomAccessIterator>
    inline void
    stable_sort(RandomAccessIterator begin, RandomAccessIterator end, 
		__gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::stable_sort(begin, end); }

  // Sequential fallback.
  template<typename RandomAccessIterator, typename Comparator>
    inline void
    stable_sort(RandomAccessIterator begin, RandomAccessIterator end, 
		Comparator comp, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::stable_sort(begin, end, comp); }

  template<typename RandomAccessIterator>
    inline void
    stable_sort(RandomAccessIterator begin, RandomAccessIterator end)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      stable_sort(begin, end, std::less<value_type>());
    }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename Comparator>
    void
    stable_sort(RandomAccessIterator begin, RandomAccessIterator end, 
		Comparator comp)
    {
      if (begin != end)
	{
	  if (_GLIBCXX_PARALLEL_CONDITION(
		static_cast<__gnu_parallel::sequence_index_t>(end - begin)
		>= __gnu_parallel::_Settings::get().sort_minimal_n))
	    __gnu_parallel::parallel_sort(begin, end, comp, true);
	  else
	    stable_sort(begin, end, comp, __gnu_parallel::sequential_tag());
	}
    }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    merge(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2, 
	  InputIterator2 end2, OutputIterator result,
	  __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::merge(begin1, end1, begin2, end2, result); }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Comparator>
    inline OutputIterator
    merge(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2,
	  InputIterator2 end2, OutputIterator result, Comparator comp,
	  __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::merge(begin1, end1, begin2, end2, result, comp); }

  // Sequential fallback for input iterator case
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Comparator,
	   typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
    inline OutputIterator
    merge_switch(InputIterator1 begin1, InputIterator1 end1,
		 InputIterator2 begin2, InputIterator2 end2,
		 OutputIterator result, Comparator comp,
		 IteratorTag1, IteratorTag2, IteratorTag3)
     { return _GLIBCXX_STD_P::merge(begin1, end1, begin2, end2,
				    result, comp); }

  // Parallel algorithm for random access iterators
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Comparator>
    OutputIterator
    merge_switch(InputIterator1 begin1, InputIterator1 end1, 
		 InputIterator2 begin2, InputIterator2 end2, 
		 OutputIterator result, Comparator comp, 
		 random_access_iterator_tag, random_access_iterator_tag, 
		 random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    (static_cast<__gnu_parallel::sequence_index_t>(end1 - begin1)
	     >= __gnu_parallel::_Settings::get().merge_minimal_n
	     || static_cast<__gnu_parallel::sequence_index_t>(end2 - begin2)
	     >= __gnu_parallel::_Settings::get().merge_minimal_n)))
	return __gnu_parallel::parallel_merge_advance(begin1, end1,
						      begin2, end2,
						      result, (end1 - begin1)
						      + (end2 - begin2), comp);
      else
	return __gnu_parallel::merge_advance(begin1, end1, begin2, end2,
					     result, (end1 - begin1)
					     + (end2 - begin2), comp);
  }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator, typename Comparator>
    inline OutputIterator
    merge(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2, 
	  InputIterator2 end2, OutputIterator result, Comparator comp)
    {
      typedef typename iterator_traits<InputIterator1>::value_type value_type;

      typedef std::iterator_traits<InputIterator1> iteratori1_traits;
      typedef std::iterator_traits<InputIterator2> iteratori2_traits;
      typedef std::iterator_traits<OutputIterator> iteratoro_traits;
      typedef typename iteratori1_traits::iterator_category
	iteratori1_category;
      typedef typename iteratori2_traits::iterator_category
	iteratori2_category;
      typedef typename iteratoro_traits::iterator_category iteratoro_category;

      return merge_switch(begin1, end1, begin2, end2, result, comp, 
			  iteratori1_category(), iteratori2_category(), 
			  iteratoro_category());
  }


  // Public interface, insert default comparator
  template<typename InputIterator1, typename InputIterator2,
	   typename OutputIterator>
    inline OutputIterator
    merge(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2, 
	  InputIterator2 end2, OutputIterator result)
    {
      typedef std::iterator_traits<InputIterator1> iterator1_traits;
      typedef std::iterator_traits<InputIterator2> iterator2_traits;
      typedef typename iterator1_traits::value_type value1_type;
      typedef typename iterator2_traits::value_type value2_type;

      return merge(begin1, end1, begin2, end2, result, 
		   __gnu_parallel::less<value1_type, value2_type>());
    }

  // Sequential fallback
  template<typename RandomAccessIterator>
    inline void
    nth_element(RandomAccessIterator begin, RandomAccessIterator nth, 
		RandomAccessIterator end, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::nth_element(begin, nth, end); }

  // Sequential fallback
  template<typename RandomAccessIterator, typename Comparator>
    inline void
    nth_element(RandomAccessIterator begin, RandomAccessIterator nth, 
		RandomAccessIterator end, Comparator comp, 
	      __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::nth_element(begin, nth, end, comp); }

  // Public interface
  template<typename RandomAccessIterator, typename Comparator>
    inline void
    nth_element(RandomAccessIterator begin, RandomAccessIterator nth, 
		RandomAccessIterator end, Comparator comp)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().nth_element_minimal_n))
	__gnu_parallel::parallel_nth_element(begin, nth, end, comp);
      else
	nth_element(begin, nth, end, comp, __gnu_parallel::sequential_tag());
    }

  // Public interface, insert default comparator
  template<typename RandomAccessIterator>
    inline void
    nth_element(RandomAccessIterator begin, RandomAccessIterator nth, 
		RandomAccessIterator end)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      nth_element(begin, nth, end, std::less<value_type>());
    }

  // Sequential fallback
  template<typename RandomAccessIterator, typename _Compare>
    inline void
    partial_sort(RandomAccessIterator begin, RandomAccessIterator middle, 
		 RandomAccessIterator end, _Compare comp,
		 __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::partial_sort(begin, middle, end, comp); }

  // Sequential fallback
  template<typename RandomAccessIterator>
    inline void
    partial_sort(RandomAccessIterator begin, RandomAccessIterator middle, 
		 RandomAccessIterator end, __gnu_parallel::sequential_tag)
    { _GLIBCXX_STD_P::partial_sort(begin, middle, end); }

  // Public interface, parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename _Compare>
    void
    partial_sort(RandomAccessIterator begin, RandomAccessIterator middle, 
		 RandomAccessIterator end, _Compare comp)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().partial_sort_minimal_n))
	__gnu_parallel::parallel_partial_sort(begin, middle, end, comp);
      else
	partial_sort(begin, middle, end, comp,
		     __gnu_parallel::sequential_tag());
    }

  // Public interface, insert default comparator
  template<typename RandomAccessIterator>
    inline void
    partial_sort(RandomAccessIterator begin, RandomAccessIterator middle, 
		 RandomAccessIterator end)
    {
      typedef iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      partial_sort(begin, middle, end, std::less<value_type>());
    }

  // Sequential fallback
  template<typename ForwardIterator>
    inline ForwardIterator
    max_element(ForwardIterator begin, ForwardIterator end, 
		__gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::max_element(begin, end); }

  // Sequential fallback
  template<typename ForwardIterator, typename Comparator>
    inline ForwardIterator
    max_element(ForwardIterator begin, ForwardIterator end, Comparator comp, 
		__gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::max_element(begin, end, comp); }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator, typename Comparator, typename IteratorTag>
    inline ForwardIterator
    max_element_switch(ForwardIterator begin, ForwardIterator end, 
		       Comparator comp, IteratorTag)
    { return max_element(begin, end, comp, __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename Comparator>
    RandomAccessIterator
    max_element_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		       Comparator comp, random_access_iterator_tag, 
		       __gnu_parallel::_Parallelism parallelism_tag
		       = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().max_element_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  RandomAccessIterator res(begin);
	  __gnu_parallel::identity_selector<RandomAccessIterator>
	    functionality;
	  __gnu_parallel::
	    for_each_template_random_access(begin, end,
					    __gnu_parallel::nothing(),
					    functionality,
					    __gnu_parallel::
					    max_element_reduct<Comparator,
					    RandomAccessIterator>(comp),
					    res, res, -1, parallelism_tag);
	  return res;
	}
      else
	return max_element(begin, end, comp, __gnu_parallel::sequential_tag());
    }

  // Public interface, insert default comparator
  template<typename ForwardIterator>
    inline ForwardIterator
    max_element(ForwardIterator begin, ForwardIterator end, 
		__gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef typename iterator_traits<ForwardIterator>::value_type value_type;
      return max_element(begin, end, std::less<value_type>(), parallelism_tag);
    }

  template<typename ForwardIterator>
    inline ForwardIterator
    max_element(ForwardIterator begin, ForwardIterator end)
    {
      typedef typename iterator_traits<ForwardIterator>::value_type value_type;
      return max_element(begin, end, std::less<value_type>());
    }

  // Public interface
  template<typename ForwardIterator, typename Comparator>
    inline ForwardIterator
    max_element(ForwardIterator begin, ForwardIterator end, Comparator comp,
		__gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return max_element_switch(begin, end, comp, iterator_category(), 
				parallelism_tag);
    }

  template<typename ForwardIterator, typename Comparator>
    inline ForwardIterator
    max_element(ForwardIterator begin, ForwardIterator end, Comparator comp)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return max_element_switch(begin, end, comp, iterator_category());
    }


  // Sequential fallback
  template<typename ForwardIterator>
    inline ForwardIterator
    min_element(ForwardIterator begin, ForwardIterator end, 
		__gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::min_element(begin, end); }

  // Sequential fallback
  template<typename ForwardIterator, typename Comparator>
    inline ForwardIterator
    min_element(ForwardIterator begin, ForwardIterator end, Comparator comp, 
		__gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::min_element(begin, end, comp); }

  // Sequential fallback for input iterator case
  template<typename ForwardIterator, typename Comparator, typename IteratorTag>
    inline ForwardIterator
    min_element_switch(ForwardIterator begin, ForwardIterator end, 
		       Comparator comp, IteratorTag)
    { return min_element(begin, end, comp, __gnu_parallel::sequential_tag()); }

  // Parallel algorithm for random access iterators
  template<typename RandomAccessIterator, typename Comparator>
    RandomAccessIterator
    min_element_switch(RandomAccessIterator begin, RandomAccessIterator end, 
		       Comparator comp, random_access_iterator_tag, 
		       __gnu_parallel::_Parallelism parallelism_tag
		       = __gnu_parallel::parallel_balanced)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(
	    static_cast<__gnu_parallel::sequence_index_t>(end - begin)
	    >= __gnu_parallel::_Settings::get().min_element_minimal_n
	    && __gnu_parallel::is_parallel(parallelism_tag)))
	{
	  RandomAccessIterator res(begin);
	  __gnu_parallel::identity_selector<RandomAccessIterator>
	    functionality;
	  __gnu_parallel::
	    for_each_template_random_access(begin, end,
					    __gnu_parallel::nothing(),
					    functionality,
					    __gnu_parallel::
					    min_element_reduct<Comparator,
					    RandomAccessIterator>(comp),
					    res, res, -1, parallelism_tag);
	  return res;
	}
      else
	return min_element(begin, end, comp, __gnu_parallel::sequential_tag());
    }

  // Public interface, insert default comparator
  template<typename ForwardIterator>
    inline ForwardIterator
    min_element(ForwardIterator begin, ForwardIterator end, 
		__gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef typename iterator_traits<ForwardIterator>::value_type value_type;
      return min_element(begin, end, std::less<value_type>(), parallelism_tag);
    }

  template<typename ForwardIterator>
    inline ForwardIterator
    min_element(ForwardIterator begin, ForwardIterator end)
    {
      typedef typename iterator_traits<ForwardIterator>::value_type value_type;
      return min_element(begin, end, std::less<value_type>());
    }

  // Public interface
  template<typename ForwardIterator, typename Comparator>
    inline ForwardIterator
    min_element(ForwardIterator begin, ForwardIterator end, Comparator comp,
		__gnu_parallel::_Parallelism parallelism_tag)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return min_element_switch(begin, end, comp, iterator_category(), 
				parallelism_tag);
    }

  template<typename ForwardIterator, typename Comparator>
    inline ForwardIterator
    min_element(ForwardIterator begin, ForwardIterator end, Comparator comp)
    {
      typedef iterator_traits<ForwardIterator> traits_type;
      typedef typename traits_type::iterator_category iterator_category;
      return min_element_switch(begin, end, comp, iterator_category());
    }
} // end namespace
} // end namespace

#endif /* _GLIBCXX_ALGORITHM_H */
