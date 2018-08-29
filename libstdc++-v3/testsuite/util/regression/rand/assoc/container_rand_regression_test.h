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
 * @file container_rand_regression_test.h
 * Contains a random regression test for a specific container type.
 */

#ifndef PB_DS_CONTAINER_RAND_REGRESSION_TEST_H
#define PB_DS_CONTAINER_RAND_REGRESSION_TEST_H

#include <algorithm>
#include <string>
#include <sstream>
#include <utility>
#include <ext/pb_ds/assoc_container.hpp>
#include <io/prog_bar.hpp>
#include <testsuite_rng.h>
#include <regression/trait/assoc/trait.hpp>
#include <common_type/assoc/string_form.hpp>
#include <regression/rand/xml_formatter.hpp>

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
  public:

    container_rand_regression_test(unsigned long, size_t, size_t, double,
				   double, double, double, double, bool);

    virtual
    ~container_rand_regression_test();

    void
    operator()();

  private:
    typedef Cntnr 					cntnr;
    typedef typename cntnr::allocator_type 		allocator_type;
    typedef typename cntnr::size_type 			size_type;
    typedef regression_test_traits<Cntnr> 		test_traits;
    typedef typename test_traits::key_type 		key_type;
    typedef typename test_traits::key_const_reference	key_const_reference;
    typedef typename test_traits::value_type 		value_type;
    typedef typename test_traits::native_type 		native_type;
    typedef twister_rand_gen 				gen;
    typedef __gnu_pbds::container_traits<Cntnr> 	container_traits;
    typedef __gnu_cxx::throw_allocator_random<char>    	alloc_t;

    enum op
      {
	insert_op,
	erase_op,
	clear_op,
	other_op
      };

    op
    get_next_op();

    size_t
    get_next_sub_op(size_t);

    static void
    defs();

    static void
    key_defs();

    static void
    mapped_defs();

    static void
    value_defs();

    static void
    ds_defs();

    static void
    iterator_defs();

    static void
    node_iterator_defs(__gnu_pbds::detail::false_type);

    static void
    node_iterator_defs(__gnu_pbds::detail::true_type);

    static void
    policy_defs();

    static void
    policy_defs(__gnu_pbds::basic_hash_tag);

    static void
    policy_defs(__gnu_pbds::cc_hash_tag);

    static void
    policy_defs(__gnu_pbds::gp_hash_tag);

    static void
    policy_defs(__gnu_pbds::tree_tag);

    static void
    policy_defs(__gnu_pbds::list_update_tag);

    static void
    policy_defs(__gnu_pbds::pat_trie_tag);

    void
    policy_access();

    void
    policy_access(__gnu_pbds::basic_hash_tag);

    void
    policy_access(__gnu_pbds::cc_hash_tag);

    void
    policy_access(__gnu_pbds::gp_hash_tag);

    void
    policy_access(__gnu_pbds::tree_tag);

    void
    policy_access(__gnu_pbds::list_update_tag);

    void
    policy_access(__gnu_pbds::pat_trie_tag);

    void
    it_copy();

    void
    it_assign();

    void
    rev_it_copy();

    void
    rev_it_assign();

    void
    rev_it_copy_imp(__gnu_pbds::detail::false_type);

    void
    rev_it_copy_imp(__gnu_pbds::detail::true_type);

    void
    rev_it_assign_imp(__gnu_pbds::detail::false_type);

    void
    rev_it_assign_imp(__gnu_pbds::detail::true_type);

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
    it_constructor_imp(__gnu_pbds::cc_hash_tag);

    bool
    it_constructor_imp(__gnu_pbds::gp_hash_tag);

    bool
    it_constructor_imp(__gnu_pbds::tree_tag);

    bool
    it_constructor_imp(__gnu_pbds::list_update_tag);

    bool
    it_constructor_imp(__gnu_pbds::pat_trie_tag);

    bool
    insert();

    bool
    erase();

    bool
    erase_it();

    bool
    erase_it_imp(__gnu_pbds::detail::false_type);

    bool
    erase_it_imp(__gnu_pbds::detail::true_type);

    bool
    erase_rev_it();

    bool
    erase_rev_it_imp(__gnu_pbds::detail::false_type);

    bool
    erase_rev_it_imp(__gnu_pbds::detail::true_type);

    bool
    erase_if();

    bool
    clear();

    bool
    resize();

    bool
    resize_imp(__gnu_pbds::detail::true_type);

    bool
    resize_imp(__gnu_pbds::detail::false_type);

    bool
    get_set_loads();

    bool
    get_set_loads_imp(__gnu_pbds::detail::true_type);

    bool
    get_set_loads_imp(__gnu_pbds::detail::false_type);

    void
    get_set_load();

    void
    get_set_load_imp(__gnu_pbds::detail::true_type);

    void
    get_set_load_imp(__gnu_pbds::detail::false_type);

    bool
    subscript();

    bool
    subscript_imp(__gnu_pbds::detail::false_type);

    bool
    subscript_imp(__gnu_pbds::detail::true_type);

    bool
    split_join();

    bool
    split_join_imp(__gnu_pbds::detail::false_type);

    bool
    split_join_imp(__gnu_pbds::detail::true_type);

    void
    cmp(const Cntnr&, const native_type&, const std::string&);

    void
    basic_cmp_(const Cntnr&, const native_type&);

    void
    cmp_(const Cntnr&, const native_type&);

    void
    order_preserving_cmp_imp(const Cntnr&, const native_type&,
			     __gnu_pbds::detail::false_type);

    void
    order_preserving_cmp_imp(const Cntnr&, const native_type&,
			     __gnu_pbds::detail::true_type);

    void
    back_order_preserving_cmp_imp(const Cntnr&, const native_type&,
				  __gnu_pbds::detail::false_type);

    void
    back_order_preserving_cmp_imp(const Cntnr&, const native_type&,
				  __gnu_pbds::detail::true_type);

    void
    reverse_iteration_cmp_imp(const Cntnr&, const native_type&,
			      __gnu_pbds::detail::false_type);

    void
    reverse_iteration_cmp_imp(const Cntnr&, const native_type&,
			      __gnu_pbds::detail::true_type);

    void
    order_statistics_cmp_imp(const Cntnr&, const native_type&,
			     __gnu_pbds::detail::false_type);

    void
    order_statistics_cmp_imp(const Cntnr&, const native_type&,
			     __gnu_pbds::detail::true_type);

    void
    prefix_search_cmp_imp(const Cntnr&, const native_type&,
			  __gnu_pbds::detail::false_type);

    void
    prefix_search_cmp_imp(const Cntnr&, const native_type&,
			  __gnu_pbds::detail::true_type);

    template<typename Const_It, class Const_Native_It>
    void
    it_cmp_imp(Const_It, Const_It, Const_Native_It, Const_Native_It);

    template<typename Const_It, class Const_Native_It>
    void
    back_it_cmp_imp(Const_It, Const_It, Const_Native_It, Const_Native_It);

    void
    lower_bound_cmp_imp(const Cntnr&, const native_type&,
			__gnu_pbds::detail::false_type);

    void
    lower_bound_cmp_imp(const Cntnr&, const native_type&,
			__gnu_pbds::detail::true_type);

    void
    upper_bound_cmp_imp(const Cntnr&, const native_type&,
			__gnu_pbds::detail::false_type);

    void
    upper_bound_cmp_imp(const Cntnr&, const native_type&,
			__gnu_pbds::detail::true_type);

    void
    print_container(const native_type&, std::ostream& r_os = std::cerr) const;

    void
    print_container(const cntnr&, std::ostream& r_os = std::cerr) const;

    struct destructor_printer
    {
      destructor_printer(const std::string& r_msg)
      : m_msg(r_msg), m_print(true) { }

      void
      cancel_print()
      { m_print = false; }

      ~destructor_printer()
      {
	if (!m_print)
	  return;

	std::cerr << std::endl << "Uncaught exception: " << std::endl
		  << m_msg << std::endl;
      }

    private:
      const std::string m_msg;
      bool m_print;
    };

    const unsigned long 	m_seed;
    const size_t 		m_n;
    const size_t 		m_m;
    const double 		m_tp;
    const double 		m_ip;
    const double 		m_ep;
    const double 		m_cp;
    const double 		m_mp;
    const bool 			m_disp;
    twister_rand_gen 		m_g;
    Cntnr* 			m_p_c;
    native_type 		m_native_c;
    alloc_t 			m_alloc;
    size_t 			m_i;
  };

#ifdef PB_DS_REGRESSION_TRACE
#define PB_DS_TRACE(X) std::cerr << X << std::endl
#else
#define PB_DS_TRACE(X)
#endif

#define PB_DS_CLASS_T_DEC \
  template<typename Cntnr>

#define PB_DS_CLASS_C_DEC \
  container_rand_regression_test<Cntnr>

#define PB_DS_COND_COMPARE(L, R) \
  if (m_g.get_prob() < m_mp)			\
    cmp(L, R, __FUNCTION__);

#define PB_DS_RUN_MTHD(MTHD) \
  {						\
    bool done = false;				\
						\
    while (!done)				\
      done = MTHD();				\
  }

#define PB_DS_THROW_IF_FAILED_(PRED, MORE, P_C, P_NC, F, L) \
  if (!(PRED))								\
    {									\
      std::cerr << "Failure at " << F << ": " << L << std::endl;	\
      std::cerr << MORE << std::endl;					\
      std::cerr << "container:" << std::endl;				\
      print_container(*(P_C));						\
      std::cerr << std::endl;						\
      std::cerr << "native container:" << std::endl;			\
      print_container(*(P_NC));						\
      std::cerr << std::endl;						\
      throw std::logic_error("fucked!");				\
    }

#define PB_DS_THROW_IF_FAILED(PRED, MORE, P_C, P_NC)			\
  PB_DS_THROW_IF_FAILED_(PRED, MORE, P_C, P_NC, __FILE__, __LINE__)

#define PB_DS_SET_DESTRUCT_PRINT \
  destructor_printer dest_print___(__FUNCTION__);

#define PB_DS_CANCEL_DESTRUCT_PRINT \
  dest_print___.cancel_print();

#include <regression/rand/assoc/container_rand_regression_test.tcc>

#undef PB_DS_COND_COMPARE
#undef PB_DS_RUN_MTHD
#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC
#undef PB_DS_THROW_IF_FAILED_
#undef PB_DS_THROW_IF_FAILED
#undef PB_DS_SET_DESTRUCT_PRINT
#undef PB_DS_CANCEL_DESTRUCT_PRINT
#undef PB_DS_TRACE

} // namespace detail
} // namespace test
} // namespace __gnu_pbds

#endif
