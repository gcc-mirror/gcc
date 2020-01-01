// -*- C++ -*-

// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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
 * @file subscript_find_test.hpp
 * Contains a generic subscript_find_test test.
 */

#ifndef PB_DS_SUBSCRIPT_TEST_HPP
#define PB_DS_SUBSCRIPT_TEST_HPP

#include <performance/time/timing_test_base.hpp>
#include <common_type/assoc/string_form.hpp>
#include <iterator>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename It, class Cntnr>
      class subscript_find_functor
      {
      public:
        subscript_find_functor(Cntnr& container,  It fnd_it_b,  It fnd_it_e)
	: m_r_container(container), m_fnd_it_b(fnd_it_b), m_fnd_it_e(fnd_it_e)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      It fnd_it = m_fnd_it_b;
	      while (fnd_it != m_fnd_it_e)
                ++m_r_container[(fnd_it++)->first];
	      ++fnd_it;
	    }
	}

      private:
	Cntnr& m_r_container;
	const It m_fnd_it_b;
	const It m_fnd_it_e;
      };

    } // namespace detail

    template<typename It>
    class subscript_find_test : private __gnu_pbds::test::detail::timing_test_base
    {
    public:
      subscript_find_test(It ins_b, It b, size_t ins_vn, size_t ins_vs,
			  size_t ins_vm, size_t vn, size_t vs, size_t vm)
      : m_ins_b(ins_b), m_fnd_b(b), m_ins_vn(ins_vn), m_ins_vs(ins_vs),
	m_ins_vm(ins_vm), m_fnd_vn(vn), m_fnd_vs(vs), m_fnd_vm(vm)
      { }

      template<typename Cntnr>
      void
      operator()(Cntnr);

    private:
      subscript_find_test(const subscript_find_test&);

    private:
      const It m_ins_b;
      const It m_fnd_b;
      const size_t m_ins_vn;
      const size_t m_ins_vs;
      const size_t m_ins_vm;
      const size_t m_fnd_vn;
      const size_t m_fnd_vs;
      const size_t m_fnd_vm;
    };

   template<typename It>
   template<typename Cntnr>
    void
    subscript_find_test<It>::
    operator()(Cntnr)
    {
      typedef xml_result_set_performance_formatter formatter_type;
      formatter_type res_set_fmt(string_form<Cntnr>::name(),
				 string_form<Cntnr>::desc());

      for (size_t i = 0; m_ins_vn + i*  m_ins_vs < m_ins_vm; ++i)
	{
	  const size_t v = m_ins_vn + i*  m_ins_vs;
	  const size_t fnd_size = m_fnd_vn + i*  m_fnd_vs;

	  It ins_it_b = m_ins_b;
	  It ins_it_e = m_ins_b;
	  std::advance(ins_it_e, v);

	  Cntnr test_container(ins_it_b, ins_it_e);

	  It fnd_it_b = m_fnd_b;
	  It fnd_it_e = m_fnd_b;
	  std::advance(fnd_it_e, fnd_size);

	  __gnu_pbds::test::detail::subscript_find_functor<It, Cntnr>
            fn(test_container, fnd_it_b, fnd_it_e);

	  const double res =
            __gnu_pbds::test::detail::timing_test_base::operator()(fn);

	  res_set_fmt.add_res(v, res / fnd_size);
	}
    }
  } // namespace test
} // namespace __gnu_pbds

#endif

