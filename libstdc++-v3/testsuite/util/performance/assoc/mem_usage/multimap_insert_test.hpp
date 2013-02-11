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
 * @file multimap_insert_test.hpp
 * Contains a generic multimap_insert_test test.
 */

#ifndef PB_DS_MULTIMAP_INSERT_TEST_HPP
#define PB_DS_MULTIMAP_INSERT_TEST_HPP

#include <iterator>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <testsuite_allocator.h>
#include <performance/io/xml_formatter.hpp>
#include <common_type/assoc/string_form.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    template<typename It, bool Native>
    class multimap_insert_test
    {
    public:
      multimap_insert_test(It b, size_t ins_vn, size_t ins_vs, size_t ins_vm):
      m_ins_b(b), m_ins_vn(ins_vn), m_ins_vs(ins_vs), m_ins_vm(ins_vm)
      { }

      template<typename Cntnr>
      void
      operator()(Cntnr);

    private:
      multimap_insert_test(const multimap_insert_test&);

      template<typename Cntnr>
      size_t
      insert(Cntnr, It ins_it_b, It ins_it_e, __gnu_pbds::detail::true_type);

      template<typename Cntnr>
      size_t
      insert(Cntnr, It ins_it_b, It ins_it_e, __gnu_pbds::detail::false_type);

      const It m_ins_b;
      const size_t m_ins_vn;
      const size_t m_ins_vs;
      const size_t m_ins_vm;
    };

    template<typename It, bool Native>
    template<typename Cntnr>
    void
    multimap_insert_test<It, Native>::
    operator()(Cntnr)
    {
      typedef xml_result_set_performance_formatter formatter_type;
      formatter_type res_set_fmt(string_form<Cntnr>::name(),
				 string_form<Cntnr>::desc());

      for (size_t i = 0; m_ins_vn + i * m_ins_vs < m_ins_vm; ++i)
	{
	  const size_t ins_size = m_ins_vn + i * m_ins_vs;
	  It ins_it_b = m_ins_b;
	  It ins_it_e = m_ins_b;
	  std::advance(ins_it_e, ins_size);

	  const size_t delta_mem = insert(Cntnr(), ins_it_b, ins_it_e,
					  __gnu_pbds::detail::integral_constant<int,Native>());

	  res_set_fmt.add_res(ins_size, static_cast<double>(delta_mem));
	}
    }

    template<typename It, bool Native>
    template<typename Cntnr>
    size_t
    multimap_insert_test<It, Native>::
    insert(Cntnr, It ins_it_b, It ins_it_e, __gnu_pbds::detail::true_type)
    {
      typedef __gnu_test::tracker_allocator_counter counter_type;
      __gnu_test::tracker_allocator<char> alloc;
      const size_t init_mem = counter_type::get_allocation_count() 
	                      - counter_type::get_deallocation_count();
      Cntnr cntnr;
      for (It ins_it = ins_it_b; ins_it != ins_it_e; ++ins_it)
        cntnr.insert((typename Cntnr::const_reference)(*ins_it));
      const size_t final_mem = counter_type::get_allocation_count() 
	                       - counter_type::get_deallocation_count();
      assert(final_mem > init_mem);
      return (final_mem - init_mem);
    }

    template<typename It, bool Native>
    template<typename Cntnr>
    size_t
    multimap_insert_test<It, Native>::
    insert(Cntnr, It ins_it_b, It ins_it_e, __gnu_pbds::detail::false_type)
    {
      typedef __gnu_test::tracker_allocator_counter counter_type;
      __gnu_test::tracker_allocator<char> alloc;
      const size_t init_mem = counter_type::get_allocation_count() 
	                      - counter_type::get_deallocation_count();
      Cntnr cntnr;
      for (It ins_it = ins_it_b; ins_it != ins_it_e; ++ins_it)
        cntnr[ins_it->first].insert(ins_it->second);
      const size_t final_mem =  counter_type::get_allocation_count() 
	                        - counter_type::get_deallocation_count();
      assert(final_mem > init_mem);
      return (final_mem - init_mem);
    }
  } // namespace test
} // namespace __gnu_pbds

#endif 

