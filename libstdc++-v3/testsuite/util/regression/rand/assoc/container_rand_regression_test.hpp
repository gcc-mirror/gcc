// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
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
 * @file container_rand_regression_test.hpp
 * Contains a random regression test for a specific container type.
 */

#ifndef PB_DS_CONTAINER_RAND_REGRESSION_TEST_HPP
#define PB_DS_CONTAINER_RAND_REGRESSION_TEST_HPP

#include <algorithm>
#include <string>
#include <sstream>
#include <utility>
#include <ext/pb_ds/assoc_container.hpp>
#include <io/prog_bar.hpp>
#include <rng/twister_rand_gen.hpp>
#include <regression/trait/assoc/trait.hpp>
#include <common_type/assoc/string_form.hpp>
#include <regression/rand/io/assoc/xml_formatter.hpp>

namespace pb_ds
{
namespace test
{
namespace detail
{

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
    typedef typename cntnr::allocator 			allocator;
    typedef typename cntnr::size_type 			size_type;
    typedef regression_test_traits<Cntnr> 		test_traits;
    typedef typename test_traits::key_type 		key_type;
    typedef typename test_traits::const_key_reference	const_key_reference;
    typedef typename test_traits::value_type 		value_type;
    typedef typename test_traits::native_type 		native_type;
    typedef twister_rand_gen 				gen;
    typedef pb_ds::container_traits<Cntnr> 		container_traits;
    typedef __gnu_cxx::throw_allocator<char> 		alloc_t;

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
    node_iterator_defs(pb_ds::detail::false_type);

    static void
    node_iterator_defs(pb_ds::detail::true_type);

    static void
    policy_defs();

    static void
    policy_defs(pb_ds::basic_hash_tag);

    static void
    policy_defs(pb_ds::cc_hash_tag);

    static void
    policy_defs(pb_ds::gp_hash_tag);

    static void
    policy_defs(pb_ds::tree_tag);

    static void
    policy_defs(pb_ds::list_update_tag);

    static void
    policy_defs(pb_ds::pat_trie_tag);

    void
    policy_access();

    void
    policy_access(pb_ds::basic_hash_tag);

    void
    policy_access(pb_ds::cc_hash_tag);

    void
    policy_access(pb_ds::gp_hash_tag);

    void
    policy_access(pb_ds::tree_tag);

    void
    policy_access(pb_ds::list_update_tag);

    void
    policy_access(pb_ds::pat_trie_tag);

    void
    it_copy();

    void
    it_assign();

    void
    rev_it_copy();

    void
    rev_it_assign();

    void
    rev_it_copy_imp(pb_ds::detail::false_type);

    void
    rev_it_copy_imp(pb_ds::detail::true_type);

    void
    rev_it_assign_imp(pb_ds::detail::false_type);

    void
    rev_it_assign_imp(pb_ds::detail::true_type);

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
    it_constructor_imp(pb_ds::cc_hash_tag);

    bool
    it_constructor_imp(pb_ds::gp_hash_tag);

    bool
    it_constructor_imp(pb_ds::tree_tag);

    bool
    it_constructor_imp(pb_ds::list_update_tag);

    bool
    it_constructor_imp(pb_ds::pat_trie_tag);

    bool
    insert();

    bool
    erase();

    bool
    erase_it();

    bool
    erase_it_imp(pb_ds::detail::false_type);

    bool
    erase_it_imp(pb_ds::detail::true_type);

    bool
    erase_rev_it();

    bool
    erase_rev_it_imp(pb_ds::detail::false_type);

    bool
    erase_rev_it_imp(pb_ds::detail::true_type);

    bool
    erase_if();

    bool
    clear();

    bool
    resize();

    bool
    resize_imp(pb_ds::detail::true_type);

    bool
    resize_imp(pb_ds::detail::false_type);

    bool
    get_set_loads();

    bool
    get_set_loads_imp(pb_ds::detail::true_type);

    bool
    get_set_loads_imp(pb_ds::detail::false_type);

    void
    get_set_load();

    void
    get_set_load_imp(pb_ds::detail::true_type);

    void
    get_set_load_imp(pb_ds::detail::false_type);

    bool
    subscript();

    bool
    subscript_imp(pb_ds::detail::false_type);

    bool
    subscript_imp(pb_ds::detail::true_type);

    bool
    split_join();

    bool
    split_join_imp(pb_ds::detail::false_type);

    bool
    split_join_imp(pb_ds::detail::true_type);

    void
    cmp(const Cntnr&, const native_type&, const std::string&);

    void
    basic_cmp_(const Cntnr&, const native_type&);

    void
    cmp_(const Cntnr&, const native_type&);

    void
    order_preserving_cmp_imp(const Cntnr&, const native_type&, 
			     pb_ds::detail::false_type);

    void
    order_preserving_cmp_imp(const Cntnr&, const native_type&, 
			     pb_ds::detail::true_type);

    void
    back_order_preserving_cmp_imp(const Cntnr&, const native_type&, 
				  pb_ds::detail::false_type);

    void
    back_order_preserving_cmp_imp(const Cntnr&, const native_type&, 
				  pb_ds::detail::true_type);

    void
    reverse_iteration_cmp_imp(const Cntnr&, const native_type&, 
			      pb_ds::detail::false_type);

    void
    reverse_iteration_cmp_imp(const Cntnr&, const native_type&, 
			      pb_ds::detail::true_type);

    void
    order_statistics_cmp_imp(const Cntnr&, const native_type&, 
			     pb_ds::detail::false_type);

    void
    order_statistics_cmp_imp(const Cntnr&, const native_type&, 
			     pb_ds::detail::true_type);

    void
    prefix_search_cmp_imp(const Cntnr&, const native_type&, 
			  pb_ds::detail::false_type);

    void
    prefix_search_cmp_imp(const Cntnr&, const native_type&, 
			  pb_ds::detail::true_type);

    template<typename Const_It, class Const_Native_It>
    void
    it_cmp_imp(Const_It, Const_It, Const_Native_It, Const_Native_It);

    template<typename Const_It, class Const_Native_It>
    void
    back_it_cmp_imp(Const_It, Const_It, Const_Native_It, Const_Native_It);

    void
    lower_bound_cmp_imp(const Cntnr&, const native_type&, 
			pb_ds::detail::false_type);

    void
    lower_bound_cmp_imp(const Cntnr&, const native_type&, 
			pb_ds::detail::true_type);

    void
    upper_bound_cmp_imp(const Cntnr&, const native_type&, 
			pb_ds::detail::false_type);

    void
    upper_bound_cmp_imp(const Cntnr&, const native_type&, 
			pb_ds::detail::true_type);

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

#include <regression/rand/assoc/detail/constructor_destructor_fn_imps.hpp>
#include <regression/rand/assoc/detail/cmp_fn_imps.hpp>
#include <regression/rand/assoc/detail/operator_fn_imps.hpp>
#include <regression/rand/assoc/detail/insert_fn_imps.hpp>
#include <regression/rand/assoc/detail/subscript_fn_imps.hpp>
#include <regression/rand/assoc/detail/clear_fn_imps.hpp>
#include <regression/rand/assoc/detail/erase_fn_imps.hpp>
#include <regression/rand/assoc/detail/defs_fn_imps.hpp>
#include <regression/rand/assoc/detail/policy_access_fn_imps.hpp>
#include <regression/rand/assoc/detail/split_join_fn_imps.hpp>
#include <regression/rand/assoc/detail/it_conversion_fn_imps.hpp>
#include <regression/rand/assoc/detail/resize_fn_imps.hpp>
#include <regression/rand/assoc/detail/get_set_load_fn_imps.hpp>
#include <regression/rand/assoc/detail/get_set_loads_fn_imps.hpp>
#include <regression/rand/assoc/detail/diagnostic_fn_imps.hpp>

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
} // namespace pb_ds

#endif
