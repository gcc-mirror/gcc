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

/** @file parallel/timing.h
 *  @brief Provides a simple tool to do performance debugging, also in
 *  parallel code.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_TIMING_H
#define _GLIBCXX_PARALLEL_TIMING_H 1

#include <omp.h>
#include <cstdio>
#include <cstring>
#include <parallel/tags.h>

namespace __gnu_parallel
{
  // XXX integrate with existing performance testing infrastructure.
  /** @brief Type of of point in time, used for the Timing classes. */
  typedef double point_in_time;

  template<typename tag, typename must_be_int = int>
  class Timing;

  /** @brief A class that provides simple run time measurements, also
      for parallel code.
   *  @param tag If parallel_tag, then the measurements are actually done.
   *  Otherwise, no code at all is emitted by the compiler. */
  template<typename must_be_int>
  class Timing<parallel_tag, must_be_int>
  {
  private:
    static const int max_points_in_time = 100;
    point_in_time points_in_time[max_points_in_time];
    point_in_time active, last_start;
    int pos;
    char* str;
    const char* tags[max_points_in_time];

  public:
    Timing()
    {
      str = NULL;
      pos = 0;
      active = 0.0;
      last_start = -1.0;
    }

    ~Timing()
    {
      delete[] str;
    }

    /** @brief Take a running time measurement.
     *  @param tag Optional description that will be output again with
     *  the timings.
     *  It should describe the operation before the tic(). To time a
     *  series of @c n operations, there should be @c n+1 calls to
     *  tic(), and one call to print(). */
    inline void
    tic(const char* tag = NULL)
    {
      points_in_time[pos] = omp_get_wtime();
      tags[pos] = tag;
      pos++;
    }

    /** @brief Start the running time measurement.
     *
     *  Should be paired with stop(). */
    inline void
    start()
    {
      _GLIBCXX_PARALLEL_ASSERT(last_start == -1.0);
      last_start = omp_get_wtime();
    }

    /** @brief Stop the running time measurement.
     *
     *  Should be paired with start(). */
    inline void
    stop()
    {
      _GLIBCXX_PARALLEL_ASSERT(last_start != -1.0);
      active += (omp_get_wtime() - last_start);
      last_start = -1.0;
    }

    /** @brief Reset running time accumulation. */
    inline void
    reset()
    {
      active = 0.0;
      last_start = -1.0;
    }

    /** @brief Accumulate the time between all pairs of start() and
	stop() so far */
    inline point_in_time
    active_time()
    { return active; }

    /** @brief Total time between first and last tic() */
    inline point_in_time
    total_time()
    { return (points_in_time[pos - 1] - points_in_time[0]) * 1000.0; }

  private:
    /** @brief Construct string to print out, presenting the timings. */
    const char*
    c_str()
    {
      // Avoid stream library here, to avoid cyclic dependencies in
      // header files.
      char tmp[1000];

      if (!str)
	str = new char[pos * 200];
      else
	str[0] = '\0';

      sprintf(str, "t %2d      T[ms]", omp_get_thread_num());
      strcat(str, "\n");

      for (int i = 0; i < pos; )
	{
	  point_in_time last = points_in_time[i];
	  i++;
	  if (i == pos)
	    break;
	  if (tags[i] == NULL)
	    sprintf(tmp, "%2d:     ", i - 1);
	  else
	    sprintf(tmp, "%20s:     ", tags[i]);
	  strcat(str, tmp);

	  sprintf(tmp, "%7.2f     ", (points_in_time[i] - last) * 1000.0);
	  strcat(str, tmp);
	  strcat(str, "\n");
	}

      return str;
    }

  public:
    /** @brief Print the running times between the tic()s. */
    void
    print()
    {
      printf("print\n");
#pragma omp barrier
#pragma omp master
      printf("\n\n");
#pragma omp critical
      printf("%s\n", c_str());
    }
  };

  /** @brief A class that provides simple run time measurements, also
      for parallel code.
   *  @param tag If parallel_tag, then the measurements are actually done,
   *  otherwise, no code at all is emitted by the compiler.
   */
  template<typename must_be_int>
  class Timing<sequential_tag, must_be_int>
  {
  private:
    static const char* empty_string;

  public:
    inline void tic(const char* /*tag*/ = NULL) { }
    inline void start() { }
    inline void stop() { }
    inline void reset() { }
    inline point_in_time active_time() { return -1.0; }
    inline point_in_time total_time() { return -1.0; }
    inline const char* c_str() { return empty_string; }
    inline void print() { }
  };

  template<typename must_be_int>
  const char* Timing<sequential_tag, must_be_int>::empty_string = "";

}

#endif
