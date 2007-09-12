// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
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

