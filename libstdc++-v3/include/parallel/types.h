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

/** @file parallel/types.h
 *  @brief Basic types and typedefs.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler and Felix Putze.

#ifndef _GLIBCXX_PARALLEL_TYPES_H
#define _GLIBCXX_PARALLEL_TYPES_H 1

#include <cstdlib>
#include <tr1/cstdint>

namespace __gnu_parallel
{
  // Enumerated types.

  /// @brief Run-time equivalents for the compile-time tags.
  enum parallelism
    {
      /// Not parallel.
      sequential,

      /// Parallel unbalanced (equal-sized chunks).
      parallel_unbalanced,

      /// Parallel balanced (work-stealing).
      parallel_balanced,

      /// Parallel with OpenMP dynamic load-balancing.
      parallel_omp_loop,

      /// Parallel with OpenMP static load-balancing.
      parallel_omp_loop_static,

      /// Parallel with OpenMP taskqueue construct.
      parallel_taskqueue
    };

  inline bool 
  is_parallel(const parallelism __p) { return __p != sequential; }

  /// Integer Types.
  using std::tr1::int16_t;
  using std::tr1::uint16_t;

  using std::tr1::int32_t;
  using std::tr1::uint32_t;

  using std::tr1::int64_t;
  using std::tr1::uint64_t;

  /**
   * @brief Unsigned integer to index elements.
   * The total number of elements for each algorithm must fit into this type.
   */
  typedef uint64_t sequence_index_t;

  /**
   * @brief Unsigned integer to index a thread number.
   * The maximum thread number (for each processor) must fit into this type.
   */
  typedef uint16_t thread_index_t;

  // XXX atomics interface?
  /**
   * @brief Longest compare-and-swappable integer type on this platform.
   */
  typedef int64_t lcas_t;

  // XXX numeric_limits::digits?
  /**
   * @brief Number of bits of ::lcas_t.
   */
  static const int lcas_t_bits = sizeof(lcas_t) * 8;

  /**
   * @brief ::lcas_t with the right half of bits set to 1.
   */
  static const lcas_t lcas_t_mask = ((lcas_t(1) << (lcas_t_bits / 2)) - 1);
}

#endif /* _GLIBCXX_TYPES_H */
