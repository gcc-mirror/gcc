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
 * @file tree_split_join_test.hpp
 * Contains a test for splitting and joining trees
 */

#ifndef PB_DS_TREE_SPLIT_JOIN_TEST_HPP
#define PB_DS_TREE_SPLIT_JOIN_TEST_HPP

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
      template<typename Cntnr, bool Support_Split_Join>
      class split_join_functor
      {
      public:
        split_join_functor(Cntnr& r_container) : m_r_container(r_container)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      typename Cntnr::const_iterator mid_it = m_r_container.begin();
	      std::advance(mid_it, m_r_container.size() / 2);
	      Cntnr other;
	      m_r_container.split(*mid_it, other);
	      m_r_container.join(other);
	    }
	}

      private:
	Cntnr& m_r_container;
      };

      template<typename Cntnr>
      class split_join_functor<Cntnr, false>
      {
      public:
        split_join_functor(Cntnr& r_container) : m_r_container(r_container)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      typename Cntnr::iterator mid_it = m_r_container.begin();
	      std::advance(mid_it, m_r_container.size() / 2);

	      Cntnr other(mid_it, m_r_container.end());
	      m_r_container.erase(mid_it, m_r_container.end());

	      m_r_container.insert(other.begin(), other.end());
	      other.clear();
	    }
	}

      private:
	Cntnr& m_r_container;
      };
    } // namespace detail

    template<bool Support_Split_Join>
    class tree_split_join_test : private __gnu_pbds::test::detail::timing_test_base
    {
    public:
      tree_split_join_test(size_t vn, size_t vs, size_t vm);

      template<typename Cntnr>
      void
      operator()(Cntnr);

    private:
      tree_split_join_test(const tree_split_join_test& );

    private:
      const size_t m_vn;
      const size_t m_vs;
      const size_t m_vm;
    };

    template<bool Support_Split_Join>
    tree_split_join_test<Support_Split_Join>::
    tree_split_join_test(size_t vn, size_t vs, size_t vm) :
      m_vn(vn),
      m_vs(vs),
      m_vm(vm)
    { }

    template<bool Support_Split_Join>
    template<typename Cntnr>
    void
    tree_split_join_test<Support_Split_Join>::
    operator()(Cntnr)
    {
      typedef xml_result_set_performance_formatter formatter_type;
      formatter_type res_set_fmt(string_form<Cntnr>::name(),
				 string_form<Cntnr>::desc());

      for (size_t v = m_vn; v < m_vm; v += m_vs)
	{
	  Cntnr cntnr;
	  for (size_t ins = 0; ins < v; ++ ins)
            cntnr.insert((typename Cntnr::value_type)ins);

	  __gnu_pbds::test::detail::split_join_functor<Cntnr, Support_Split_Join>
            fn(cntnr);

	  const double res =
            __gnu_pbds::test::detail::timing_test_base::operator()(fn);
	  res_set_fmt.add_res(v, res);
	}
    }
  } // namespace test
} // namespace __gnu_pbds

#endif

