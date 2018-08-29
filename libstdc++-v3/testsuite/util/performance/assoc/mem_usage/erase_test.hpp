// -*- C++ -*-

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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
 * @file erase_test.hpp
 * Contains a generic erase test.
 */

#ifndef PB_DS_ERASE_TEST_HPP
#define PB_DS_ERASE_TEST_HPP

#include <iterator>
#include <testsuite_allocator.h>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <performance/io/xml_formatter.hpp>
#include <common_type/assoc/string_form.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    template<typename It>
    class erase_test
    {
    public:
      erase_test(It ins_b, size_t ins_vn, size_t ins_vs, size_t ins_vm)
      :	m_ins_b(ins_b), m_ins_vn(ins_vn), m_ins_vs(ins_vs), m_ins_vm(ins_vm)
      { }

      template<typename Cntnr>
        void
        operator()(Cntnr);

    private:
      erase_test(const erase_test&);

      const It 		m_ins_b;
      const size_t 	m_ins_vn;
      const size_t 	m_ins_vs;
      const size_t 	m_ins_vm;
    };

    template<typename It>
    template<typename Cntnr>
    void
    erase_test<It>::
    operator()(Cntnr)
    {
      typedef string_form<Cntnr> sform_type;
      typedef xml_result_set_performance_formatter formatter_type;
      formatter_type res_set_fmt(sform_type::name(), sform_type::desc());

      for (size_t i = 0; m_ins_vn + i * m_ins_vs < m_ins_vm; ++i)
	{
	  const size_t ins_size = m_ins_vn + i * m_ins_vs;

	  It ins_it_b = m_ins_b;
	  It ins_it_e = m_ins_b;
	  std::advance(ins_it_e, ins_size);

	  typedef __gnu_test::tracker_allocator_counter counter_type;
	  __gnu_test::tracker_allocator<char> alloc;
	  const size_t init_mem = counter_type::get_allocation_count()
	                          - counter_type::get_deallocation_count();
	  Cntnr cntnr(ins_it_b, ins_it_e);

	  while (cntnr.size() > 1)
            cntnr.erase(*cntnr.begin());

	  const size_t final_mem = counter_type::get_allocation_count()
	                         - counter_type::get_deallocation_count();
	  assert(final_mem > init_mem);
	  const size_t delta_mem = final_mem - init_mem;
	  res_set_fmt.add_res(ins_size, static_cast<double>(delta_mem));
	}
    }
  } // namespace test
} // namespace __gnu_pbds

#endif

