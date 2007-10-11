// -*- C++ -*-

// Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
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
 * @file multimap_find_test.hpp
 * Contains a generic multimap_find_test test.
 */

#ifndef PB_DS_MULTIMAP_INSERT_TEST_HPP
#define PB_DS_MULTIMAP_INSERT_TEST_HPP

#include <performance/time/timing_test_base.hpp>
#include <performance/io/xml_formatter.hpp>
#include <common_type/assoc/string_form.hpp>
#include <iterator>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename It, class Cntnr, bool Native>
      class multimap_find_functor
      {
      public:
        multimap_find_functor(const Cntnr& container, It fnd_it_b, It fnd_it_e)
	: m_r_container(container), m_fnd_it_b(fnd_it_b), m_fnd_it_e(fnd_it_e)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  size_t not_found_count = 0;
	  typedef typename Cntnr::const_point_iterator iterator_type;
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      iterator_type end = m_r_container.end();
	      for (It fnd_it = m_fnd_it_b; fnd_it != m_fnd_it_e; ++fnd_it)
		{
		  iterator_type it = m_r_container.find(fnd_it->first);
		  if (it == end ||
		      it->second.find(fnd_it->second) == it->second.end())
                    ++not_found_count;
		}
	    }

	  if (not_found_count != 0)
            std::abort();
	}

      private:
	const Cntnr& m_r_container;
	const It m_fnd_it_b;
	const It m_fnd_it_e;
      };

      template<typename It, class Cntnr>
      class multimap_find_functor<It, Cntnr, true>
      {
      public:
        multimap_find_functor(const Cntnr& container, It fnd_it_b, It fnd_it_e)
	: m_r_container(container), m_fnd_it_b(fnd_it_b), m_fnd_it_e(fnd_it_e)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  typedef typename Cntnr::const_reference const_reference;
	  size_t not_found_count = 0;
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr cntnr;
	      for (It fnd_it = m_fnd_it_b; fnd_it != m_fnd_it_e; ++fnd_it)
                if (m_r_container.find(const_reference(*fnd_it)) 
		    == m_r_container.end())
		  ++not_found_count;
	    }

	  if (not_found_count != 0)
            std::abort();
	}

      private:
	const Cntnr& m_r_container;
	const It m_fnd_it_b;
	const It m_fnd_it_e;
      };
    } // namespace detail


    template<typename It, bool Native>
    class multimap_find_test 
    : private __gnu_pbds::test::detail::timing_test_base
    {
    public:
      multimap_find_test(It ins_b, size_t ins_vn, size_t vs, size_t ins_vm)
      : m_ins_b(ins_b), m_ins_vn(ins_vn), m_ins_vs(vs), m_ins_vm(ins_vm)
      { }

      template<typename Cntnr>
      void
      operator()(Cntnr);

    private:
      multimap_find_test(const multimap_find_test&);

      template<typename Cntnr>
      Cntnr
      init(It ins_b, It ins_e, Cntnr, __gnu_pbds::detail::true_type)
      { return Cntnr(ins_b, ins_e); }

      template<typename Cntnr>
      Cntnr
      init(It ins_b, It ins_e, Cntnr, __gnu_pbds::detail::false_type)
      {
	Cntnr ret;
	for (It it = ins_b; it != ins_e; ++it)
	  ret[it->first].insert(it->second);
	return ret;
      }

      const It m_ins_b;
      const size_t m_ins_vn;
      const size_t m_ins_vs;
      const size_t m_ins_vm;
    };


    template<typename It, bool Native>
    template<typename Cntnr>
    void
    multimap_find_test<It, Native>::
    operator()(Cntnr)
    {
      typedef xml_result_set_performance_formatter formatter_type;
      formatter_type res_set_fmt(string_form<Cntnr>::name(),
				 string_form<Cntnr>::desc());

      for (size_t i = 0; m_ins_vn + i * m_ins_vs < m_ins_vm; ++i)
	{
	  const size_t v = m_ins_vn + i * m_ins_vs;
	  It ins_it_b = m_ins_b;
	  It ins_it_e = m_ins_b;
	  std::advance(ins_it_e, v);

	  Cntnr c = init(ins_it_b, ins_it_e, Cntnr(),
			 __gnu_pbds::detail::integral_constant<int,Native>());

	  __gnu_pbds::test::detail::multimap_find_functor<It, Cntnr, Native>
            fn(c, ins_it_b, ins_it_e);

	  const double res =
            __gnu_pbds::test::detail::timing_test_base::operator()(fn);

	  res_set_fmt.add_res(v, res / v);
	}
    }
  } // namespace test
} // namespace __gnu_pbds

#endif 

