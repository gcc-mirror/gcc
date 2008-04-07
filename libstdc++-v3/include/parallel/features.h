// -*- C++ -*-

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

/** @file parallel/features.h
 *  @brief Defines on whether to include algorithm variants.
 *
 *  Less variants reduce executable size and compile time.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_FEATURES_H
#define _GLIBCXX_PARALLEL_FEATURES_H 1

#ifndef _GLIBCXX_MERGESORT
/** @def _GLIBCXX_MERGESORT
 *  @brief Include parallel multi-way mergesort.
 *  @see __gnu_parallel::_Settings::sort_algorithm */
#define _GLIBCXX_MERGESORT 1
#endif

#ifndef _GLIBCXX_QUICKSORT
/** @def _GLIBCXX_QUICKSORT
 *  @brief Include parallel unbalanced quicksort.
 *  @see __gnu_parallel::_Settings::sort_algorithm */
#define _GLIBCXX_QUICKSORT 1
#endif

#ifndef _GLIBCXX_BAL_QUICKSORT
/** @def _GLIBCXX_BAL_QUICKSORT
 *  @brief Include parallel dynamically load-balanced quicksort.
 *  @see __gnu_parallel::_Settings::sort_algorithm */
#define _GLIBCXX_BAL_QUICKSORT 1
#endif

#ifndef _GLIBCXX_FIND_GROWING_BLOCKS
/** @brief Include the growing blocks variant for std::find.
 *  @see __gnu_parallel::_Settings::find_algorithm */
#define _GLIBCXX_FIND_GROWING_BLOCKS 1
#endif

#ifndef _GLIBCXX_FIND_CONSTANT_SIZE_BLOCKS
/** @brief Include the equal-sized blocks variant for std::find.
 *  @see __gnu_parallel::_Settings::find_algorithm */
#define _GLIBCXX_FIND_CONSTANT_SIZE_BLOCKS 1
#endif

#ifndef _GLIBCXX_FIND_EQUAL_SPLIT
/** @def _GLIBCXX_FIND_EQUAL_SPLIT
 *  @brief Include the equal splitting variant for std::find.
 *  @see __gnu_parallel::_Settings::find_algorithm */
#define _GLIBCXX_FIND_EQUAL_SPLIT 1
#endif


#ifndef _GLIBCXX_TREE_INITIAL_SPLITTING
/** @def _GLIBCXX_TREE_INITIAL_SPLITTING
 *  @brief Include the initial splitting variant for
 *  _Rb_tree::insert_unique(InputIterator beg, InputIterator end).
 *  @see __gnu_parallel::_Rb_tree */
#define _GLIBCXX_TREE_INITIAL_SPLITTING 1
#endif

#ifndef _GLIBCXX_TREE_DYNAMIC_BALANCING
/** @def _GLIBCXX_TREE_DYNAMIC_BALANCING
 *  @brief Include the dynamic balancing variant for
 *  _Rb_tree::insert_unique(InputIterator beg, InputIterator end).
 *  @see __gnu_parallel::_Rb_tree */
#define _GLIBCXX_TREE_DYNAMIC_BALANCING 1
#endif

#ifndef _GLIBCXX_TREE_FULL_COPY
/** @def _GLIBCXX_TREE_FULL_COPY
 *  @brief In order to sort the input sequence of
 *  _Rb_tree::insert_unique(InputIterator beg, InputIterator end) a
 *  full copy of the input elements is done.
 *  @see __gnu_parallel::_Rb_tree */
#define _GLIBCXX_TREE_FULL_COPY 1
#endif


#endif
