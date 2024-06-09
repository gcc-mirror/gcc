// -*- C++ -*-

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
 * @file rand_regression_test.hpp
 * Contains a random-operation test.
 */

#ifndef PB_DS_PQ_RAND_REGRESSION_TEST_HPP
#define PB_DS_PQ_RAND_REGRESSION_TEST_HPP

#include <iostream>
#include <vector>
#include <io/verified_cmd_line_input.hpp>
#include <regression/common_type.hpp>
#include <regression/rand/priority_queue/container_rand_regression_test.h>

namespace __gnu_pbds
{
namespace test
{
namespace detail
{
#ifndef PB_DS_REGRESSION
#error "Must define PB_DS_REGRESSION"
#endif

  struct rand_reg_test
  {
  public:
    rand_reg_test(size_t seed, size_t n, size_t m, double tp, double ip,
		  double dp, double ep, double cp, double mp, bool d)
    : m_sd(seed), m_n(n), m_m(m), m_tp(tp), m_ip(ip), m_dp(dp), m_ep(ep),
      m_cp(cp), m_mp(mp), m_disp(d)
    {
      if (m_disp)
	xml_test_rand_regression_formatter(seed, n, m, tp, ip, ep, cp, mp);
    }

    template<typename Cntnr>
    void
    operator()(Cntnr)
    {
      unsigned long ul = static_cast<unsigned long>(m_sd);
      container_rand_regression_test<Cntnr> t(ul, m_n, m_n, m_tp, m_ip, m_dp,
					      m_ep, m_cp, m_mp, m_disp);
      t();
    }

  private:
    const size_t m_sd;
    const size_t m_n;
    const size_t m_m;
    const double m_tp;
    const double m_ip;
    const double m_dp;
    const double m_ep;
    const double m_cp;
    const double m_mp;
    const bool m_disp;
  };

  void
  usage(const std::string& r_name);

  void
  verify_params(size_t&, size_t&, size_t&,
		double&, double&, double&, double&, double&, double&, bool&);
} // namespace detail

  template<typename TL>
  int
  rand_regression_test(size_t iter, size_t keys, const std::string name, TL tl)
  {
    // Sane defaults.
    size_t n = iter;
    size_t m = keys;
    size_t sd = twister_rand_gen::get_time_determined_seed();
    double tp = 0.2;
    double ip = 0.6;
    double dp = 0.1;
    double ep = 0.2;
    double cp = 0.001;
    double mp = 1;
    bool disp = true; // show progress

    try
      {
	detail::verify_params(sd, n, m, tp, ip, dp, ep, cp, mp, disp);
      }
    catch(__gnu_pbds::test::illegal_input_error&)
      {
	detail::usage(name);
	return -1;
      }
    catch(...)
      {
	return -2;
      };

    try
      {
	detail::rand_reg_test tst(sd, n, m, tp, ip, dp, ep, cp, mp, disp);
	__gnu_cxx::typelist::apply(tst, tl);
      }
    catch(...)
      {
	std::cerr << "Test failed with seed " << sd << std::endl;
	throw;
      }

    return 0;
  }

namespace detail
{
  inline void
  usage(const std::string& name)
  {
    using namespace std;
    cerr << "usage: " << name << " <sd> <n> <m> <tp> <ip> <dp> <ep> <cp> <mp> ['t' | 'f']" <<
      endl << endl;

    cerr << "This test performs basic regression tests on various priority queues."
      "For each container, it performs a sequence of operations. At each iteration, it does "
      "the following: " << endl;
    cerr << "*  Performs an operation on the container " << endl;
    cerr << "*  Performs the same operation on an cntnr object" << endl;
    cerr << "*  Possibly compares the container to the cntnr object" << endl;
    cerr << "*  Checks that exceptions (thrown by an allocator) "
      "do not violate exception guarantees";

    cerr << endl << endl;

    cerr << "sd = seed for random-number generator; 0 = "
      "time determined value" << endl;
    cerr << "n = number of iterations" << endl;
    cerr << "m = number of distinct values" << endl;
    cerr << "tp = probability that an exception will be actively thrown" << endl;
    cerr << "ip = probability that an operation will be insert" << endl;
    cerr << "dp = probability that an operation will be modify" << endl;
    cerr << "ep = probability that an operation will be erase" << endl;
    cerr << "cp = probability that an operation will be clear" << endl;
    cerr << "(therefore, 1 - (ip + dp + ep + cp) = probability of any other operation)" << endl;
    cerr << "mp = probability that the container will be compared to the cntnr object" << endl;
    cerr << "'t' or 'f' determine whether progress will be displayed" << endl;
  }

  inline void
  verify_params(size_t& r_seed, size_t& r_n,
		size_t& r_m, double& r_tp, double& r_ip, double& r_dp,
		double& r_ep, double& r_cp, double& r_mp, bool& r_d)
  {
    verify_prob(r_tp);
    verify_prob(r_ip);
    verify_prob(r_dp);
    verify_prob(r_ep);
    verify_prob(r_cp);
    verify_prob(r_mp);
    verify_prob(r_ip + r_dp + r_ep + r_cp);
  }
} // namespace detail
} // namespace test
} // namespace __gnu_pbds

#endif
