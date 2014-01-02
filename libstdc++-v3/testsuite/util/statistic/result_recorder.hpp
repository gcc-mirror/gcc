// -*- C++ -*-

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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
 * @file result_recorder.hpp
 * Contains a class for recording results
 */

#ifndef PB_DS_RES_RECORDER_HPP
#define PB_DS_RES_RECORDER_HPP

#include <statistic/sample_mean.hpp>
#include <statistic/sample_variance.hpp>
#include <statistic/sample_mean_confidence_checker.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      /*
       * Records results until the probability that the sample mean is 10% away
       * from the true mean is ~ 0.05.
       */
      template<typename Value_Type>
      class result_recorder
      {
      public:
	typedef Value_Type value_type;

	result_recorder()
        : m_sample_mean(value_type()), m_sample_var(value_type())
	{ }

	bool
        add_result(value_type res);

	inline value_type
        get_sample_mean() const
	{ return m_sample_mean; }

      private:
	typedef std::list<value_type> list_type;

	list_type m_l;
	value_type m_sample_mean;
	value_type m_sample_var;
      };


      template<typename Value_Type>
      bool
      result_recorder<Value_Type>::
      add_result(value_type res)
      {
	m_l.push_back(res);
	m_sample_mean = sample_mean(m_l.begin(), m_l.end());
	m_sample_var = sample_variance(m_l.begin(), m_l.end(), m_sample_mean);

	size_t dist = std::distance(m_l.begin(), m_l.end());
	return sample_mean_confidence_checker(m_sample_mean, m_sample_var,
					      dist, 0.1);
      }
    } // namespace detail
  } // namespace test
} // namespace __gnu_pbds

#endif 

