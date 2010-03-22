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

/** @file parallel/algobase.h
 *  @brief Parallel STL function calls corresponding to the
 *  stl_algobase.h header.  The functions defined here mainly do case
 *  switches and call the actual parallelized versions in other files.
 *  Inlining policy: Functions that basically only contain one
 *  function call, are declared inline.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler and Felix Putze.

#ifndef _GLIBCXX_PARALLEL_ALGOBASE_H
#define _GLIBCXX_PARALLEL_ALGOBASE_H 1

#include <bits/stl_algobase.h>
#include <parallel/base.h>
#include <parallel/tags.h>
#include <parallel/settings.h>
#include <parallel/find.h>
#include <parallel/find_selectors.h>

namespace std
{
namespace __parallel
{
  // NB: equal and lexicographical_compare require mismatch.

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2>
    inline pair<InputIterator1, InputIterator2>
    mismatch(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2,
	     __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::mismatch(begin1, end1, begin2); }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate>
    inline pair<InputIterator1, InputIterator2>
    mismatch(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2,
	     Predicate pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::mismatch(begin1, end1, begin2, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate, typename IteratorTag1, typename IteratorTag2>
    inline pair<InputIterator1, InputIterator2>
    mismatch_switch(InputIterator1 begin1, InputIterator1 end1, 
		    InputIterator2 begin2, Predicate pred, IteratorTag1, 
		    IteratorTag2)
    { return _GLIBCXX_STD_P::mismatch(begin1, end1, begin2, pred); }

  // Parallel mismatch for random access iterators
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename Predicate>
    pair<RandomAccessIterator1, RandomAccessIterator2>
    mismatch_switch(RandomAccessIterator1 begin1, RandomAccessIterator1 end1,
		    RandomAccessIterator2 begin2, Predicate pred, 
		    random_access_iterator_tag, random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	{
	  RandomAccessIterator1 res =
	    __gnu_parallel::find_template(begin1, end1, begin2, pred,
					  __gnu_parallel::
					  mismatch_selector()).first;
	  return make_pair(res , begin2 + (res - begin1));
	}
      else
	return _GLIBCXX_STD_P::mismatch(begin1, end1, begin2, pred);
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2>
    inline pair<InputIterator1, InputIterator2>
    mismatch(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2)
    {
      typedef std::iterator_traits<InputIterator1> iterator1_traits;
      typedef std::iterator_traits<InputIterator2> iterator2_traits;
      typedef typename iterator1_traits::value_type value1_type;
      typedef typename iterator2_traits::value_type value2_type;
      typedef typename iterator1_traits::iterator_category iterator1_category;
      typedef typename iterator2_traits::iterator_category iterator2_category;

      typedef __gnu_parallel::equal_to<value1_type, value2_type> equal_to_type;

      return mismatch_switch(begin1, end1, begin2, equal_to_type(),
			     iterator1_category(), iterator2_category());
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate>
    inline pair<InputIterator1, InputIterator2>
    mismatch(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2,
	     Predicate pred)
    {
      typedef std::iterator_traits<InputIterator1> iterator1_traits;
      typedef std::iterator_traits<InputIterator2> iterator2_traits;
      typedef typename iterator1_traits::iterator_category iterator1_category;
      typedef typename iterator2_traits::iterator_category iterator2_category;

      return mismatch_switch(begin1, end1, begin2, pred, iterator1_category(), 
			     iterator2_category());
    }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2>
    inline bool
    equal(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2, 
	  __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::equal(begin1, end1, begin2); }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate>
    inline bool
    equal(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2, 
	  Predicate pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::equal(begin1, end1, begin2, pred); }

  // Public interface
  template<typename InputIterator1, typename InputIterator2>
    inline bool
    equal(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2)
    { return _GLIBCXX_STD_P::mismatch(begin1, end1, begin2).first == end1; }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate>
    inline bool
    equal(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2, 
	  Predicate pred)
    {
      return _GLIBCXX_STD_P::mismatch(begin1, end1, begin2, pred).first
                  == end1;
    }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2>
    inline bool
    lexicographical_compare(InputIterator1 begin1, InputIterator1 end1, 
			    InputIterator2 begin2, InputIterator2 end2, 
			    __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::lexicographical_compare(begin1, end1,
						     begin2, end2); }

  // Sequential fallback
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate>
    inline bool
    lexicographical_compare(InputIterator1 begin1, InputIterator1 end1, 
			    InputIterator2 begin2, InputIterator2 end2, 
			    Predicate pred, __gnu_parallel::sequential_tag)
    { return _GLIBCXX_STD_P::lexicographical_compare(begin1, end1, 
						     begin2, end2, pred); }

  // Sequential fallback for input iterator case
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate, typename IteratorTag1, typename IteratorTag2>
    inline bool
    lexicographical_compare_switch(InputIterator1 begin1, InputIterator1 end1, 
				   InputIterator2 begin2, InputIterator2 end2, 
				   Predicate pred, IteratorTag1, IteratorTag2)
    { return _GLIBCXX_STD_P::lexicographical_compare(begin1, end1, 
						     begin2, end2, pred); }

  // Parallel lexicographical_compare for random access iterators
  // Limitation: Both valuetypes must be the same
  template<typename RandomAccessIterator1, typename RandomAccessIterator2,
	   typename Predicate>
    bool
    lexicographical_compare_switch(RandomAccessIterator1 begin1, 
				   RandomAccessIterator1 end1, 
				   RandomAccessIterator2 begin2, 
				   RandomAccessIterator2 end2, Predicate pred, 
				   random_access_iterator_tag, 
				   random_access_iterator_tag)
    {
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	{
	  typedef iterator_traits<RandomAccessIterator1> traits1_type;
	  typedef typename traits1_type::value_type value1_type;

	  typedef iterator_traits<RandomAccessIterator2> traits2_type;
	  typedef typename traits2_type::value_type value2_type;

	  typedef __gnu_parallel::equal_from_less<Predicate, value1_type,
	                                          value2_type> equal_type;

	  // Longer sequence in first place.
	  if ((end1 - begin1) < (end2 - begin2))
	    {
	      typedef pair<RandomAccessIterator1, RandomAccessIterator2>
		pair_type;
	      pair_type mm = mismatch_switch(begin1, end1, begin2, 
					     equal_type(pred), 
					     random_access_iterator_tag(), 
					     random_access_iterator_tag());

	      return (mm.first == end1) || bool(pred(*mm.first, *mm.second));
	    }
	  else
	    {
	      typedef pair<RandomAccessIterator2, RandomAccessIterator1>
		pair_type;
	      pair_type mm = mismatch_switch(begin2, end2, begin1, 
					     equal_type(pred), 
					     random_access_iterator_tag(), 
					     random_access_iterator_tag());

	      return (mm.first != end2) && bool(pred(*mm.second, *mm.first));
	    }
	}
      else
	return _GLIBCXX_STD_P::lexicographical_compare(begin1, end1,
						       begin2, end2, pred);
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2>
    inline bool
    lexicographical_compare(InputIterator1 begin1, InputIterator1 end1,
			    InputIterator2 begin2, InputIterator2 end2)
    {
      typedef iterator_traits<InputIterator1> traits1_type;
      typedef typename traits1_type::value_type value1_type;
      typedef typename traits1_type::iterator_category iterator1_category;

      typedef iterator_traits<InputIterator2> traits2_type;
      typedef typename traits2_type::value_type value2_type;
      typedef typename traits2_type::iterator_category iterator2_category;
      typedef __gnu_parallel::less<value1_type, value2_type> less_type;

      return lexicographical_compare_switch(begin1, end1, begin2, end2, 
					    less_type(), iterator1_category(), 
					    iterator2_category());
    }

  // Public interface
  template<typename InputIterator1, typename InputIterator2,
	   typename Predicate>
    inline bool
    lexicographical_compare(InputIterator1 begin1, InputIterator1 end1,
			    InputIterator2 begin2, InputIterator2 end2,
			    Predicate pred)
    {
      typedef iterator_traits<InputIterator1> traits1_type;
      typedef typename traits1_type::iterator_category iterator1_category;

      typedef iterator_traits<InputIterator2> traits2_type;
      typedef typename traits2_type::iterator_category iterator2_category;

      return lexicographical_compare_switch(begin1, end1, begin2, end2, pred, 
					    iterator1_category(), 
					    iterator2_category());
    }
} // end namespace
} // end namespace

#endif /* _GLIBCXX_PARALLEL_ALGOBASE_H */
