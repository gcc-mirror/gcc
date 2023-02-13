// -*- C++ -*-

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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
 * @file container_rand_regression_test.h
 * Contains a random regression test for a specific container type.
 */

#ifndef PB_DS_CONTAINER_RAND_REGRESSION_TEST_H
#define PB_DS_CONTAINER_RAND_REGRESSION_TEST_H

#include <algorithm>
#include <string>
#include <sstream>
#include <utility>
#include <cassert>
#include <regression/basic_type.hpp>
#include <ext/pb_ds/priority_queue.hpp>
#include <io/prog_bar.hpp>
#include <testsuite_rng.h>
#include <common_type/priority_queue/string_form.hpp>
#include <regression/rand/xml_formatter.hpp>
#include <regression/trait/priority_queue/trait.hpp>

namespace __gnu_pbds
{
namespace test
{
namespace detail
{
  // Rand test specialized for a specific container.
  template<typename Cntnr>
    class container_rand_regression_test
    {
    private:
      typedef Cntnr 						cntnr;
      typedef typename cntnr::allocator_type 			allocator_type;
      typedef typename cntnr::size_type 			size_type;
      typedef twister_rand_gen 					gen;
      typedef basic_type 					value_type;
      typedef native_priority_queue<std::string, true>		native_type;
      typedef regression_test_traits<cntnr> 			test_traits;

      enum op
	{
	  insert_op,
	  modify_op,
	  erase_op,
	  clear_op,
	  other_op
	};

      op
      get_next_op();

      size_t
      get_next_sub_op(size_t max);

      static void
      defs();

      static void
      value_defs();

      static void
      ds_defs();

      static void
      iterator_defs();

      static void
      policy_defs();

      void
      policy_access();

      void
      it_copy();

      void
      it_assign();

      bool
      default_constructor();

      void
      swap();

      bool
      copy_constructor();

      bool
      assignment_operator();

      bool
      it_constructor();

      bool
      push();

      bool
      modify();

      bool
      pop();

      bool
      erase_if();

      bool
      erase_it();

      bool
      clear();

      bool
      split_join();

      void
      cmp(const Cntnr& r_container, const native_type& r_native_c,
	  const std::string& r_call_fn);

      void
      print_container(const native_type& r_cnt,
		      std::ostream& r_os = std::cerr) const;

      void
      print_container(const cntnr& r_cnt,
		      std::ostream& r_os = std::cerr) const;

      struct destructor_printer
      {
	destructor_printer(const std::string& r_msg)
	: m_msg(r_msg), m_print(true) { }

	void
	cancel()
	{ m_print = false; }

	~destructor_printer()
	{
	  if (m_print)
	    {
	      std::cerr << std::endl << "Uncaught exception: " << std::endl
			<< m_msg << std::endl;
	    }
	}

	const std::string 	m_msg;
	bool 			m_print;
      };

      const unsigned long 	m_seed;
      const size_t 		m_n;
      const size_t 		m_m;
      const double 		m_tp;
      const double 		m_ip;
      const double 		m_dp;
      const double 		m_ep;
      const double 		m_cp;
      const double 		m_mp;
      const bool 		m_disp;
      twister_rand_gen 		m_g;
      Cntnr* 			m_p_c;
      native_type 		m_native_c;
      allocator_type 		m_alloc;
      size_t 			m_i;

    public:
      container_rand_regression_test(unsigned long seed, size_t n, size_t m,
				     double tp, double ip, double dp,
				     double ep, double cp, double mp,
				     bool disp);

      virtual
      ~container_rand_regression_test();

      void
      operator()();
  };


#ifdef PB_DS_REGRESSION_TRACE
# define PB_DS_TRACE(X) std::cerr << X << std::endl
#else
# define PB_DS_TRACE(X)
#endif

#define PB_DS_CLASS_T_DEC			\
      template<typename Cntnr>

#define PB_DS_CLASS_C_DEC				\
      container_rand_regression_test<Cntnr>

#define PB_DS_COND_COMPARE(L, R)		\
      if (m_g.get_prob() < m_mp)		\
        cmp(L, R, __FUNCTION__);

#define PB_DS_RUN_MTHD(MTHD)			\
      {						\
        bool done = false;			\
        while (!done)				\
	  done = MTHD();			\
      }

#define _GLIBCXX_THROW_IF_(PRED, MORE, P_C, P_NC, F, L)			\
      if (PRED)								\
	{								\
	  std::cerr << "Failure at " << F << ": " << L << std::endl;	\
	  std::cerr << MORE << std::endl;				\
	  std::cerr << "container:" << std::endl;			\
	  print_container(*(P_C));					\
	  std::cerr << std::endl;					\
	  std::cerr << "native container:" << std::endl;		\
	  print_container(*(P_NC));					\
	  std::cerr << std::endl;					\
	  throw std::logic_error("pbds throw if failed");		\
	}

#define _GLIBCXX_THROW_IF(PRED, MORE, P_C, P_NC)			\
      _GLIBCXX_THROW_IF_(PRED, MORE, P_C, P_NC, __FILE__, __LINE__)

#include <regression/rand/priority_queue/container_rand_regression_test.tcc>

#undef PB_DS_COND_COMPARE
#undef PB_DS_RUN_MTHD
#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC
#undef _GLIBCXX_THROW_IF_
#undef _GLIBCXX_THROW_IF
#undef PB_DS_TRACE

} // namespace detail
} // namespace test
} // namespace __gnu_pbds

#endif
