// -*- C++ -*-

// Copyright (C) 2005-2013 Free Software Foundation, Inc.
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
 * @file modify_test.hpp
 * Contains a modify performance test.
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
      // Primary templates.
      template<typename It, class Cntnr, class Tag>
      class push_functor
      {
      public:
        push_functor(It ins_it_b,  It ins_it_e) 
	: m_ins_it_b(ins_it_b), m_ins_it_e(ins_it_e)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  typedef typename Cntnr::point_iterator point_iterator;
	  typedef typename Cntnr::const_reference const_reference;
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr c;
	      typedef std::vector<point_iterator> it_vec_t;
	      it_vec_t m_a_its;
	      for (It ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                m_a_its.push_back(c.push(const_reference(ins_it->first)));
	    }
	}

      private:
	const It m_ins_it_b;
	const It m_ins_it_e;
      };

      template<typename It, class Cntnr, class Tag>
      class push_modify_functor
      {
      private:
	typedef typename Cntnr::point_iterator point_iterator;
	typedef typename Cntnr::const_reference const_reference;
	typedef typename Cntnr::value_type value_type;

      public:
        push_modify_functor(It ins_it_b, It ins_it_e, value_type mod_val) 
	: m_ins_it_b(ins_it_b), m_ins_it_e(ins_it_e), m_mod_val(mod_val)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr c;
	      typedef std::vector<typename Cntnr::point_iterator> it_vec_t;
	      it_vec_t m_a_its;
	      for (It ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                m_a_its.push_back(c.push(const_reference(ins_it->first)));

	      typename it_vec_t::iterator mod_it = m_a_its.begin();
	      while (mod_it != m_a_its.end())
                c.modify(*mod_it++, m_mod_val);
	    }
	}

      private:
	const It m_ins_it_b;
	const It m_ins_it_e;
	const value_type m_mod_val;
      };

      // Specializations.
      template<typename It, class Cntnr>
      class push_functor<It, Cntnr, __gnu_pbds::binary_heap_tag>
      {
      public:
        push_functor(It ins_it_b,  It ins_it_e) 
	: m_ins_it_b(ins_it_b), m_ins_it_e(ins_it_e)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  typedef typename Cntnr::const_reference const_reference;
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr c;
	      for (It ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                c.push(const_reference(ins_it->first));
	    }
	}

      private:
	const It m_ins_it_b;
	const It m_ins_it_e;
      };

      template<typename It, class Cntnr>
      class push_functor<It, Cntnr, __gnu_pbds::test::native_pq_tag>
      {
      public:
        push_functor(It ins_it_b,  It ins_it_e) 
	: m_ins_it_b(ins_it_b), m_ins_it_e(ins_it_e)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  typedef typename Cntnr::const_reference const_reference;
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr c;

	      for (It ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                c.push(const_reference(ins_it->first));
	    }
	}

      private:
	const It m_ins_it_b;
	const It m_ins_it_e;
      };


      template<typename It, class Cntnr>
      class push_modify_functor<It, Cntnr, __gnu_pbds::binary_heap_tag>
      {
      private:
	typedef typename Cntnr::iterator iterator;
	typedef typename Cntnr::const_reference const_reference;
	typedef typename Cntnr::value_type value_type;

      public:
        push_modify_functor(It ins_it_b, It ins_it_e, value_type mod_val) 
	: m_ins_it_b(ins_it_b), m_ins_it_e(ins_it_e), m_mod_val(mod_val)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr c;
	      It ins_it;
	      for (ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                c.push(const_reference(ins_it->first));

	      for (ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
		{
		  bool modified = false;
		  for (iterator it = c.begin(); !modified && it != c.end(); ++it)
                    if (*it == ins_it->first)
		      {
                        c.modify(it, m_mod_val);
                        modified = true;
		      }
		}
	    }
	}

      private:
	const It m_ins_it_b;
	const It m_ins_it_e;
	const value_type m_mod_val;
      };

      template<typename It, class Cntnr>
      class push_modify_functor<It, Cntnr, __gnu_pbds::test::native_pq_tag>
      {
      private:
	typedef typename Cntnr::value_type value_type;
	typedef typename Cntnr::const_reference const_reference;

      public:
        push_modify_functor(It ins_it_b,  It ins_it_e, value_type mod_val) 
	: m_ins_it_b(ins_it_b), m_ins_it_e(ins_it_e), m_mod_val(mod_val)
	{ }

	void
        operator()(std::size_t resolution)
	{
	  for (std::size_t i = 0; i < resolution; ++i)
	    {
	      Cntnr c;
	      It ins_it;
	      for (ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                c.push(const_reference(ins_it->first));
	      for (ins_it = m_ins_it_b; ins_it != m_ins_it_e; ++ins_it)
                c.modify(ins_it->first, m_mod_val);
	    }
	}

      private:
	const It m_ins_it_b;
	const It m_ins_it_e;
	const value_type m_mod_val;
      };
    } // namespace detail

    template<typename It>
    class modify_test : private __gnu_pbds::test::detail::timing_test_base
    {
    public:
      modify_test(It b, size_t vn, size_t vs, size_t vm, bool modify_up)
      : m_ins_b(b), m_ins_vn(vn), m_ins_vs(vs), m_ins_vm(vm),
      m_modify_up(modify_up)
      { }

      template<typename Cntnr>
      void
      operator()(Cntnr);

    private:
      modify_test(const modify_test&);

      template<typename Cntnr>
      void
      modify(Cntnr, It ins_it_b, It ins_it_e)
      {
	typedef typename Cntnr::const_reference const_reference;
	Cntnr cntnr;	
	for (It ins_it = ins_it_b; ins_it != ins_it_e; ++ins_it)
	  cntnr.modify(const_reference(*ins_it));
      }

      const It m_ins_b;
      const size_t m_ins_vn;
      const size_t m_ins_vs;
      const size_t m_ins_vm;
      const bool m_modify_up;
    };

    template<typename It>
    template<typename Cntnr>
    void
    modify_test<It>::
    operator()(Cntnr)
    {
      typedef typename Cntnr::value_type value_type;
      typedef typename Cntnr::container_category container_category;
      typedef typename Cntnr::const_reference const_reference;
      typedef detail::timing_test_base timing_test_base;
      typedef detail::push_functor<It, Cntnr, container_category> psh_fnct;
      typedef detail::push_modify_functor<It, Cntnr, container_category> psh_mod_fnct;
      typedef xml_result_set_performance_formatter formatter_type;
      formatter_type res_set_fmt(string_form<Cntnr>::name(),
				 string_form<Cntnr>::desc());

      for (size_t i = 0; m_ins_vn + i * m_ins_vs < m_ins_vm; ++i)
	{
	  const size_t v = m_ins_vn + i * m_ins_vs;
	  It b = m_ins_b;
	  It e = m_ins_b;
	  std::advance(e, v);

	  psh_fnct psh_fn(b, e);
	  const double psh_res = timing_test_base::operator()(psh_fn);

	  value_type val = b->first;
	  {
            Cntnr mod_val_container;
            for (It mod_val_it = b; mod_val_it != e; ++mod_val_it)
	      {
                value_type pot = mod_val_it->first;
                if (m_modify_up == mod_val_container.get_cmp_fn()(val, pot))
		  val = pot;
	      }
	  }

	  psh_mod_fnct psh_mod_fn(b, e, val);
	  const double psh_mod_res = timing_test_base::operator()(psh_mod_fn);

	  const double min_res = double(timing_test_base::min_time_res());
	  const double effective_delta = std::max(psh_mod_res - psh_res,
						  min_res);

	  res_set_fmt.add_res(v, effective_delta / v);
	}
    }
  } // namespace test
} // namespace __gnu_pbds

#endif

