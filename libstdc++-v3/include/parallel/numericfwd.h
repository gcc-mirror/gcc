// <numeric> parallel extensions -*- C++ -*-

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

/** @file parallel/numericfwd.h
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_PARALLEL_NUMERICFWD_H
#define _GLIBCXX_PARALLEL_NUMERICFWD_H 1

#pragma GCC system_header

#include <parallel/tags.h>
#include <parallel/settings.h>

namespace std
{
namespace __parallel
{
  template<typename _IIter, typename _Tp>
    _Tp
    accumulate(_IIter, _IIter, _Tp);

  template<typename _IIter, typename _Tp>
    _Tp
    accumulate(_IIter, _IIter, _Tp, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _Tp>
    _Tp
    accumulate(_IIter, _IIter, _Tp, __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _Tp, typename _Tag>
    _Tp
    accumulate_switch(_IIter, _IIter, _Tp, _Tag);

  template<typename _IIter, typename _Tp, typename _BinaryOper>
    _Tp
    accumulate(_IIter, _IIter, _Tp, _BinaryOper);

  template<typename _IIter, typename _Tp, typename _BinaryOper>
    _Tp
    accumulate(_IIter, _IIter, _Tp, _BinaryOper,
	       __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _Tp, typename _BinaryOper>
    _Tp
    accumulate(_IIter, _IIter, _Tp, _BinaryOper,
	       __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _Tp, typename _BinaryOper,
	   typename _Tag>
    _Tp
    accumulate_switch(_IIter, _IIter, _Tp, _BinaryOper, _Tag);

  template<typename _RAIter, typename _Tp, typename _BinaryOper>
    _Tp
    accumulate_switch(_RAIter, _RAIter, _Tp, _BinaryOper,
		      random_access_iterator_tag,
		      __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _OIter>
    _OIter
    adjacent_difference(_IIter, _IIter, _OIter);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    adjacent_difference(_IIter, _IIter, _OIter, _BinaryOper);

  template<typename _IIter, typename _OIter>
    _OIter
    adjacent_difference(_IIter, _IIter, _OIter,
			__gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    adjacent_difference(_IIter, _IIter, _OIter, _BinaryOper, 
			__gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter>
    _OIter
    adjacent_difference(_IIter, _IIter, _OIter,
			__gnu_parallel::_Parallelism);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    adjacent_difference(_IIter, _IIter, _OIter, _BinaryOper, 
			__gnu_parallel::_Parallelism);

  template<typename _IIter, typename _OIter, typename _BinaryOper,
	   typename _Tag1, typename _Tag2>
    _OIter
    adjacent_difference_switch(_IIter, _IIter, _OIter, _BinaryOper,
			       _Tag1, _Tag2);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    adjacent_difference_switch(_IIter, _IIter, _OIter, _BinaryOper, 
			       random_access_iterator_tag, 
			       random_access_iterator_tag, 
			       __gnu_parallel::_Parallelism);

  template<typename _IIter1, typename _IIter2, typename _Tp>
    _Tp
    inner_product(_IIter1, _IIter1, _IIter2, _Tp);

  template<typename _IIter1, typename _IIter2, typename _Tp>
    _Tp
    inner_product(_IIter1, _IIter1, _IIter2, _Tp,
		  __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _Tp>
    _Tp
    inner_product(_IIter1, _IIter1, _IIter2, _Tp,
		  __gnu_parallel::_Parallelism);

  template<typename _IIter1, typename _IIter2, typename _Tp,
	   typename _BinaryFunction1, typename _BinaryFunction2>
    _Tp
    inner_product(_IIter1, _IIter1, _IIter2, _Tp,
		  _BinaryFunction1, _BinaryFunction2);

  template<typename _IIter1, typename _IIter2, typename _Tp,
	   typename _BinaryFunction1, typename _BinaryFunction2>
    _Tp
    inner_product(_IIter1, _IIter1, _IIter2, _Tp, _BinaryFunction1,
		  _BinaryFunction2, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _Tp,
	   typename BinaryFunction1, typename BinaryFunction2>
    _Tp
    inner_product(_IIter1, _IIter1, _IIter2, _Tp, BinaryFunction1,
		  BinaryFunction2, __gnu_parallel::_Parallelism);

  template<typename _RAIter1, typename _RAIter2, typename _Tp,
	   typename BinaryFunction1, typename BinaryFunction2>
    _Tp
    inner_product_switch(_RAIter1, _RAIter1, _RAIter2, _Tp, BinaryFunction1, 
			 BinaryFunction2, random_access_iterator_tag, 
			 random_access_iterator_tag, 
			 __gnu_parallel::_Parallelism);

  template<typename _IIter1, typename _IIter2, typename _Tp,
	   typename _BinaryFunction1, typename _BinaryFunction2,
	   typename _Tag1, typename _Tag2>
    _Tp
    inner_product_switch(_IIter1, _IIter1, _IIter2, _Tp, _BinaryFunction1, 
			 _BinaryFunction2, _Tag1, _Tag2);


  template<typename _IIter, typename _OIter>
    _OIter
    partial_sum(_IIter, _IIter, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    partial_sum(_IIter, _IIter, _OIter, _BinaryOper,
		__gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter>
    _OIter
    partial_sum(_IIter, _IIter, _OIter result);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    partial_sum(_IIter, _IIter, _OIter, _BinaryOper);

  template<typename _IIter, typename _OIter, typename _BinaryOper,
	   typename _Tag1, typename _Tag2>
    _OIter
    partial_sum_switch(_IIter, _IIter, _OIter, _BinaryOper, _Tag1, _Tag2);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
    _OIter
    partial_sum_switch(_IIter, _IIter, _OIter, _BinaryOper,
		       random_access_iterator_tag, random_access_iterator_tag);
} // end namespace
} // end namespace

#endif
