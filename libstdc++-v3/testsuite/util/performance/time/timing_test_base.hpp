// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file timing_test_base.hpp
 * Contains a base class for timing tests.
 */

#ifndef PB_DS_TIMING_TEST_BASE_HPP
#define PB_DS_TIMING_TEST_BASE_HPP

#include <performance/time/elapsed_timer.hpp>
#include <statistic/result_recorder.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      class timing_test_base
      {
      protected:
	template<typename Functor>
	double
	operator()(Functor& fn);

	static double
        min_time_res();

      private:
	template<typename Functor>
	std::size_t
	get_min_resolution(Functor&);

	template<typename Functor>
	double
	run_at_resolution(Functor&, std::size_t);
      };

      template<typename Functor>
      double
      timing_test_base::operator()(Functor& fn)
      {
	const std::size_t resolution = get_min_resolution(fn);
	__gnu_pbds::test::detail::result_recorder<double> rec;
	double res;
	do
	  res = run_at_resolution(fn, resolution);
	while (rec.add_result(res) == false);
	res = rec.get_sample_mean() / resolution;
	return res;
      }

      double
      timing_test_base::min_time_res()
      { return 1e-7; }

      template<typename Functor>
      std::size_t
      timing_test_base::get_min_resolution(Functor& fn)
      {
	std::size_t guess = 0;
	const double epsilon_val = min_time_res();
	double res;
	do
	  {
	    guess = guess * 2 + 1;
	    res = run_at_resolution(fn, guess);
	  }
	while (res < epsilon_val);
	return guess;
      }

      template<typename Functor>
      double
      timing_test_base::run_at_resolution(Functor& fn, std::size_t resolution)
      {
	elapsed_timer res;
	fn(resolution);
	return res;
      }

    } // namespace detail
  } // namespace test
} // namespace __gnu_pbds

#endif

