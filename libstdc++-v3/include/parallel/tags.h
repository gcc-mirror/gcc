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

/**
 * @file parallel/tags.h
 * @brief Tags for compile-time selection.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler and Felix Putze.

#ifndef _GLIBCXX_PARALLEL_TAGS_H
#define _GLIBCXX_PARALLEL_TAGS_H 1

namespace __gnu_parallel
{
  /** @brief Forces sequential execution at compile time. */
  struct sequential_tag { };

  /** @brief Forces exact splitting in multiway merge at compile time. */
  struct exact_tag { };

  /** @brief Recommends parallel execution at compile time. */
  struct parallel_tag { };

  /** @brief Recommends parallel execution using dynamic
      load-balancing at compile time. */
  struct balanced_tag : public parallel_tag { };

  /** @brief Recommends parallel execution using static
      load-balancing at compile time. */
  struct unbalanced_tag : public parallel_tag { };

  /** @brief Recommends parallel execution using OpenMP dynamic
      load-balancing at compile time. */
  struct omp_loop_tag : public parallel_tag { };

  /** @brief Recommends parallel execution using OpenMP static
      load-balancing at compile time. */
  struct omp_loop_static_tag : public parallel_tag { };


  struct find_tag { };

  /** @brief Selects the growing block size variant for std::find().
      @see _GLIBCXX_FIND_GROWING_BLOCKS */
  struct growing_blocks_tag : public find_tag { };

  /** @brief Selects the constant block size variant for std::find().
      @see _GLIBCXX_FIND_CONSTANT_SIZE_BLOCKS */
  struct constant_size_blocks_tag : public find_tag { };

  /** @brief Selects the equal splitting variant for std::find().
      @see _GLIBCXX_FIND_EQUAL_SPLIT */
  struct equal_split_tag : public find_tag { };
}

#endif /* _GLIBCXX_PARALLEL_TAGS_H */
