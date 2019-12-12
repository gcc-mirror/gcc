// -*- C++ -*-

// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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
 * @file join_test.hpp
 * Contains a join performance test.
 */

#ifndef PB_DS_JOIN_TEST_HPP
#define PB_DS_JOIN_TEST_HPP

#include <performance/time/timing_test_base.hpp>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <performance/io/xml_formatter.hpp>
#include <common_type/priority_queue/string_form.hpp>
#include <iterator>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename It, typename Cntnr>
      class double_push_functor
      {
	const It _M_begin;
	const It _M_end;

      public:
        double_push_functor(It b,  It e) : _M_begin(b), _M_end(e) { }

	void
        operator()(std::size_t resolution)
	{
	  typedef typename Cntnr::const_reference const_reference;
	  for (std::size_t n = 0; n < resolution; ++n)
	    {
	      Cntnr c0;
	      Cntnr c1;
	      for (It it = _M_begin; it != _M_end; ++it)
		{
		  c0.push(const_reference(it->first));
		  c1.push(const_reference(it->first));
		}
	    }
	}
      };

      template<typename It, typename Cntnr>
      class double_push_join_functor
      {
	const It _M_begin;
	const It _M_end;

      public:
        double_push_join_functor(It b,  It e) : _M_begin(b), _M_end(e) { }

	void
        operator()(std::size_t resolution)
	{
	  typedef typename Cntnr::const_reference const_reference;
	  for (std::size_t n = 0; n < resolution; ++n)
	    {
	      Cntnr c0;
	      Cntnr c1;
	      for (It it = _M_begin; it != _M_end; ++it)
		{
		  c0.push(const_reference(it->first));
		  c1.push(const_reference(it->first));
		}
	      c0.join(c1);
	    }
	}
      };
    } // namespace detail

    template<typename It>
    class join_test : private __gnu_pbds::test::detail::timing_test_base
    {
    public:
      join_test(It ins_b, size_t ins_vn, size_t ins_vs, size_t ins_vm)
      : m_ins_b(ins_b), m_ins_vn(ins_vn), m_ins_vs(ins_vs), m_ins_vm(ins_vm)
      { }

      template<typename Cntnr>
      void
      operator()(Cntnr)
      {
	using __gnu_pbds::test::detail::double_push_functor;
	using __gnu_pbds::test::detail::double_push_join_functor;
	typedef __gnu_pbds::test::detail::timing_test_base base_type;
	typedef double_push_functor<It, Cntnr> psh_fnct;
	typedef double_push_join_functor<It, Cntnr> psh_jn_fnct;

	typedef xml_result_set_performance_formatter formatter_type;
	formatter_type res(string_form<Cntnr>::name(),
			   string_form<Cntnr>::desc());

	for (size_t n = 0; m_ins_vn + n*  m_ins_vs < m_ins_vm; ++n)
	  {
	    const size_t v = m_ins_vn + n *  m_ins_vs;
	    It b = m_ins_b;
	    It e = m_ins_b;
	    std::advance(e, v);

	    psh_fnct double_push_fn(b, e);
	    const double dbl_psh_res = base_type::operator()(double_push_fn);
	    psh_jn_fnct dbl_psh_jn_fn(b, e);
	    const double dbl_psh_jn_res = base_type::operator()(dbl_psh_jn_fn);
	    const double min_res = double(timing_test_base::min_time_res());
	    const double effective_delta = std::max(dbl_psh_jn_res - dbl_psh_res, min_res);
	    res.add_res(v, effective_delta / v);
	  }
      }

    private:
      join_test(const join_test&);

      template<typename Cntnr>
      void
      join(Cntnr, It b, It e)
      {
	Cntnr cntnr;
	typedef typename Cntnr::const_reference const_reference;
	for (It it = b; it != e; ++it)
	  cntnr.join(const_reference(*it));
      }

      const It 		m_ins_b;
      const size_t 	m_ins_vn;
      const size_t 	m_ins_vs;
      const size_t 	m_ins_vm;
    };
  } // namespace test
} // namespace __gnu_pbds

#endif

