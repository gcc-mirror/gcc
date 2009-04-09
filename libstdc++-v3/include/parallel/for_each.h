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

/** @file parallel/for_each.h
 *  @brief Main interface for embarrassingly parallel functions.
 *
 *  The explicit implementation are in other header files, like
 *  workstealing.h, par_loop.h, omp_loop.h, and omp_loop_static.h.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_FOR_EACH_H
#define _GLIBCXX_PARALLEL_FOR_EACH_H 1

#include <parallel/settings.h>
#include <parallel/par_loop.h>
#include <parallel/omp_loop.h>
#include <parallel/workstealing.h>

namespace __gnu_parallel
{
  /** @brief Chose the desired algorithm by evaluating @c parallelism_tag.
   *  @param begin Begin iterator of input sequence.
   *  @param end End iterator of input sequence.
   *  @param user_op A user-specified functor (comparator, predicate,
   *  associative operator,...)
   *  @param functionality functor to "process" an element with
   *  user_op (depends on desired functionality, e. g. accumulate,
   *  for_each,...
   *  @param reduction Reduction functor.
   *  @param reduction_start Initial value for reduction.
   *  @param output Output iterator.
   *  @param bound Maximum number of elements processed.
   *  @param parallelism_tag Parallelization method */
  template<typename InputIterator, typename UserOp,
	   typename Functionality, typename Red, typename Result>
    UserOp
    for_each_template_random_access(InputIterator begin, InputIterator end,
				    UserOp user_op,
				    Functionality& functionality,
				    Red reduction, Result reduction_start,
				    Result& output, typename
				    std::iterator_traits<InputIterator>::
				    difference_type bound,
				    _Parallelism parallelism_tag)
    {
      if (parallelism_tag == parallel_unbalanced)
	return for_each_template_random_access_ed(begin, end, user_op,
						  functionality, reduction,
						  reduction_start,
						  output, bound);
      else if (parallelism_tag == parallel_omp_loop)
	return for_each_template_random_access_omp_loop(begin, end, user_op,
							functionality,
							reduction,
							reduction_start,
							output, bound);
      else if (parallelism_tag == parallel_omp_loop_static)
	return for_each_template_random_access_omp_loop(begin, end, user_op,
							functionality,
							reduction,
							reduction_start,
							output, bound);
      else	//e. g. parallel_balanced
	return for_each_template_random_access_workstealing(begin, end,
							    user_op,
							    functionality,
							    reduction,
							    reduction_start,
							    output, bound);
  }
}

#endif /* _GLIBCXX_PARALLEL_FOR_EACH_H */
