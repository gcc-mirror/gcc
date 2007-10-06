// <numeric> parallel extensions -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
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
  template<typename _IIter, typename T>
  inline T
  accumulate(_IIter, _IIter, T);

  template<typename _IIter, typename T>
  inline T
  accumulate(_IIter, _IIter, T, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename T>
  inline T
  accumulate(_IIter, _IIter, T, __gnu_parallel::parallelism parallelism_tag);

  template<typename _IIter, typename T, typename _Tag>
  inline T
  accumulate_switch(_IIter, _IIter, T, _Tag);

  template<typename _IIter, typename T, typename _BinaryOper>
  inline T
  accumulate(_IIter, _IIter, T, _BinaryOper);

  template<typename _IIter, typename T, typename _BinaryOper>
  inline T
  accumulate(_IIter, _IIter, T, _BinaryOper, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename T, typename _BinaryOper>
  inline T
  accumulate(_IIter, _IIter, T, _BinaryOper, 
	     __gnu_parallel::parallelism parallelism_tag);

  template<typename _IIter, typename T, typename _BinaryOper, typename _Tag>
  T
  accumulate_switch(_IIter, _IIter, T, _BinaryOper, _Tag);

  template<typename _RAIter, typename T, typename _BinaryOper>
  T
  accumulate_switch(_RAIter, _RAIter, T, _BinaryOper, 
		    random_access_iterator_tag, __gnu_parallel::parallelism);


 template<typename _IIter, typename _OIter>
  inline _OIter
  adjacent_difference(_IIter, _IIter, _OIter);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  inline _OIter
  adjacent_difference(_IIter, _IIter, _OIter, _BinaryOper);

  template<typename _IIter, typename _OIter>
  inline _OIter
  adjacent_difference(_IIter, _IIter, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  inline _OIter
  adjacent_difference(_IIter, _IIter, _OIter, _BinaryOper, 
		      __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter>
  inline _OIter
  adjacent_difference(_IIter, _IIter, _OIter, __gnu_parallel::parallelism);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  inline _OIter
  adjacent_difference(_IIter, _IIter, _OIter, _BinaryOper, 
		      __gnu_parallel::parallelism);

  template<typename _IIter, typename _OIter, typename _BinaryOper, typename _Tag1, typename _Tag2>
  inline _OIter
  adjacent_difference_switch(_IIter, _IIter, _OIter, _BinaryOper, _Tag1, _Tag2);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  _OIter
  adjacent_difference_switch(_IIter, _IIter, _OIter, _BinaryOper, 
			     random_access_iterator_tag, 
			     random_access_iterator_tag, 
			     __gnu_parallel::parallelism);


  template<typename _IIter1, typename _IIter2, typename T>
  inline T
  inner_product(_IIter1, _IIter1, _IIter2, T);

  template<typename _IIter1, typename _IIter2, typename T>
  inline T
  inner_product(_IIter1, _IIter1, _IIter2, T, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename T>
  inline T
  inner_product(_IIter1, _IIter1, _IIter2, T, __gnu_parallel::parallelism);


  template<typename _IIter1, typename _IIter2, typename T, typename BinaryFunction1, typename BinaryFunction2>
  inline T
  inner_product(_IIter1, _IIter1, _IIter2, T, BinaryFunction1, BinaryFunction2);

  template<typename _IIter1, typename _IIter2, typename T, typename BinaryFunction1, typename BinaryFunction2>
  inline T
  inner_product(_IIter1, _IIter1, _IIter2, T, BinaryFunction1, BinaryFunction2,
		__gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename T, typename BinaryFunction1, typename BinaryFunction2>
  inline T
  inner_product(_IIter1, _IIter1, _IIter2, T, BinaryFunction1, BinaryFunction2,
		__gnu_parallel::parallelism);

  template<typename _RAIter1, typename _RAIter2, typename T, typename BinaryFunction1, typename BinaryFunction2>
  T
  inner_product_switch(_RAIter1, _RAIter1, _RAIter2, T, BinaryFunction1, 
		       BinaryFunction2, random_access_iterator_tag, 
		       random_access_iterator_tag, 
		       __gnu_parallel::parallelism);

  template<typename _IIter1, typename _IIter2, typename T, typename BinaryFunction1, typename BinaryFunction2, typename _Tag1, typename _Tag2>
  inline T
  inner_product_switch(_IIter1, _IIter1, _IIter2, T, BinaryFunction1, 
		       BinaryFunction2, _Tag1, _Tag2);


  template<typename _IIter, typename _OIter>
  inline _OIter
  partial_sum(_IIter, _IIter, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  inline _OIter
  partial_sum(_IIter, _IIter, _OIter, _BinaryOper, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter>
  inline _OIter
  partial_sum(_IIter, _IIter, _OIter result);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  inline _OIter
  partial_sum(_IIter, _IIter, _OIter, _BinaryOper);

  template<typename _IIter, typename _OIter, typename _BinaryOper, typename _Tag1, typename _Tag2>
  inline _OIter
  partial_sum_switch(_IIter, _IIter, _OIter, _BinaryOper, _Tag1, _Tag2);

  template<typename _IIter, typename _OIter, typename _BinaryOper>
  _OIter
  partial_sum_switch(_IIter, _IIter, _OIter, _BinaryOper, random_access_iterator_tag, random_access_iterator_tag);
} // end namespace
} // end namespace

#endif
