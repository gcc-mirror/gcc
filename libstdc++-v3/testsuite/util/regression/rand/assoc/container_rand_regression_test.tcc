// -*- C++ -*-

// Copyright (C) 2005, 2006, 2008, 2009 Free Software Foundation, Inc.
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
 * @file container_rand_regression_test.tcc
 * Contains a random regression test for a specific container type.
 */

#ifndef PB_DS_CONTAINER_RAND_REGRESSION_TEST_TCC
#define PB_DS_CONTAINER_RAND_REGRESSION_TEST_TCC

// Constructors/Destructors.
PB_DS_CLASS_T_DEC
PB_DS_CLASS_C_DEC::
container_rand_regression_test(unsigned long seed, size_t n, size_t m, 
			       double tp, double ip, double ep, double cp, 
			       double mp, bool disp) 
: m_seed((seed == 0) ? twister_rand_gen::get_time_determined_seed() : seed),
  m_n(n), m_m(m), m_tp(tp), m_ip(ip), m_ep(ep), m_cp(cp), m_mp(mp),
  m_disp(disp), m_p_c(NULL)
{ }

PB_DS_CLASS_T_DEC
PB_DS_CLASS_C_DEC::
~container_rand_regression_test()
{ }

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
default_constructor()
{
  PB_DS_TRACE("default_constructor");
  bool done = true;
  m_alloc.set_throw_prob(m_tp);

  try
    {
      m_p_c = new Cntnr;
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }

  if (m_p_c != NULL)
    PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
swap()
{
  PB_DS_TRACE("swap");
  m_alloc.set_throw_prob(0);
  Cntnr* p_c = new Cntnr;
  m_alloc.set_throw_prob(1);
  p_c->swap(*m_p_c);
  std::swap(p_c, m_p_c);
  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
copy_constructor()
{
  PB_DS_TRACE("copy_constructor");
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      p_c = new Cntnr(*m_p_c);
      std::swap(p_c, m_p_c);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
assignment_operator()
{
  PB_DS_TRACE("assignment operator");
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      p_c = new Cntnr();
      * p_c =* m_p_c;
      std::swap(p_c, m_p_c);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
it_constructor()
{
  PB_DS_TRACE("it_constructor");
  return it_constructor_imp(typename Cntnr::container_category());
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
it_constructor_imp(__gnu_pbds::cc_hash_tag)
{
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      switch(get_next_sub_op(8))
        {
        case 0:
	  p_c = new Cntnr(m_p_c->get_hash_fn());
	  m_native_c.clear();
	  break;
        case 1:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn());
	  m_native_c.clear();
	  break;
        case 2:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn(),
			  m_p_c->get_comb_hash_fn());
	  m_native_c.clear();
	  break;
        case 3:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn(),
			  m_p_c->get_comb_hash_fn(),
			  m_p_c->get_resize_policy());
	  m_native_c.clear();
	  break;
        case 4:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end());
	  break;
        case 5:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn());
	  break;
        case 6:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn());
	  break;
        case 7:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn(), m_p_c->get_comb_hash_fn());
	  break;
        case 8:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn(), m_p_c->get_comb_hash_fn(),
			  m_p_c->get_resize_policy());
	  break;
        default:
	  PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
        };
      std::swap(p_c, m_p_c);
    }
  catch (__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
it_constructor_imp(__gnu_pbds::gp_hash_tag)
{
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      switch(get_next_sub_op(11))
        {
        case 0:
	  p_c = new Cntnr(m_p_c->get_hash_fn());
	  m_native_c.clear();
	  break;
        case 1:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn());
	  m_native_c.clear();
	  break;
        case 2:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn(),
			  m_p_c->get_comb_probe_fn());
	  m_native_c.clear();
	  break;
        case 3:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn(),
			  m_p_c->get_comb_probe_fn());
	  m_native_c.clear();
	  break;
        case 4:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn(),
			  m_p_c->get_comb_probe_fn(), m_p_c->get_probe_fn());
	  m_native_c.clear();
	  break;
        case 5:
	  p_c = new Cntnr(m_p_c->get_hash_fn(), m_p_c->get_eq_fn(),
			  m_p_c->get_comb_probe_fn(), m_p_c->get_probe_fn(),
			  m_p_c->get_resize_policy());
	  m_native_c.clear();
	  break;
        case 6:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn());
	  break;
        case 7:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn());
	  break;
        case 8:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn(), m_p_c->get_comb_probe_fn());
	  break;
        case 9:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn(), m_p_c->get_comb_probe_fn());
	  break;
        case 10:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn(), m_p_c->get_comb_probe_fn(),
			  m_p_c->get_probe_fn());
	  break;
        case 11:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_hash_fn(),
			  m_p_c->get_eq_fn(), m_p_c->get_comb_probe_fn(),
			  m_p_c->get_probe_fn(), m_p_c->get_resize_policy());
	  break;
        default:
	  PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
        };
      std::swap(p_c, m_p_c);
    }
  catch (__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
it_constructor_imp(__gnu_pbds::tree_tag)
{
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      switch(get_next_sub_op(2))
        {
        case 0:
	  p_c = new Cntnr(m_p_c->get_cmp_fn());
	  m_native_c.clear();
	  break;
        case 1:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_cmp_fn());
	  break;
        default:
	  PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
        };
      std::swap(p_c, m_p_c);
    }
  catch (__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
it_constructor_imp(__gnu_pbds::list_update_tag)
{
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      p_c = new Cntnr(m_p_c->begin(), m_p_c->end());
      std::swap(p_c, m_p_c);
    }
  catch (__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
it_constructor_imp(__gnu_pbds::pat_trie_tag)
{
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

  try
    {
      switch(get_next_sub_op(2))
        {
        case 0:
	  p_c = new Cntnr(m_p_c->get_e_access_traits());
	  m_native_c.clear();
	  break;
        case 1:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), 
			  m_p_c->get_e_access_traits());
	  break;
        default:
	  PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
        };

      std::swap(p_c, m_p_c);
    }
  catch (__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }

  delete p_c;
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  return done;
}

// Cmp.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
cmp(const Cntnr& r_c, const native_type& r_native_c, 
    const std::string& r_call_fn)
{
  m_alloc.set_throw_prob(1);
  const size_t size = r_c.size();
  const size_t native_size = r_native_c.size();
  PB_DS_THROW_IF_FAILED(size == native_size,
			size << " " << native_size, &r_c, &r_native_c);

  const bool empty = r_c.empty();
  const bool native_empty = r_native_c.empty();
  PB_DS_THROW_IF_FAILED(empty == native_empty,
			empty << " " << native_empty, &r_c, &r_native_c);

  try
    {
      basic_cmp_(r_c, r_native_c);
      cmp_(r_c, r_native_c);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(false, "call-fn: " + r_call_fn, &r_c, &r_native_c);
    }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
basic_cmp_(const Cntnr& r_c, const native_type& r_native_c)
{
  PB_DS_SET_DESTRUCT_PRINT

  if (static_cast<size_t>(std::distance(r_c.begin(), r_c.end())) != r_c.size())
    PB_DS_THROW_IF_FAILED(false,
			  static_cast<unsigned long>(std::distance(r_c.begin(), r_c.end())) << " " << static_cast<unsigned long>(r_c.size()), &r_c, &r_native_c);
  
  typename native_type::const_iterator it = r_native_c.begin();
  while (it != r_native_c.end())
    {
      typename native_type::key_type native_key = test_traits::extract_native_key(*it);

      m_alloc.set_throw_prob(0);
      const key_type k = native_key;
      m_alloc.set_throw_prob(1);
      typename cntnr::const_point_iterator found_it = r_c.find(k);
      PB_DS_THROW_IF_FAILED(found_it != r_c.end(),
			    test_traits::native_val_to_string(*it),
			    &r_c, &r_native_c);

      if (!test_traits::cmp(*found_it, * it))
	PB_DS_THROW_IF_FAILED(false, "", &r_c, &r_native_c);

      ++it;
    }
  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
cmp_(const Cntnr& r_c, const native_type& r_native_c)
{
  enum
    {
      order_preserving = container_traits::order_preserving,
      back_order_preserving = container_traits::order_preserving 
      && 
      !__gnu_pbds::detail::is_same<
      typename std::iterator_traits<
      typename cntnr::const_iterator>::iterator_category,
      std::forward_iterator_tag>::value,
      reverse_iteration = container_traits::reverse_iteration,
      order_statistics = test_traits::order_statistics,
      prefix_search = test_traits::prefix_search,
      has_mapped = !__gnu_pbds::detail::is_same<
      typename Cntnr::mapped_type,
      __gnu_pbds::null_mapped_type>::value
    };

  order_preserving_cmp_imp(r_c, r_native_c,
			   __gnu_pbds::detail::integral_constant<int,order_preserving>());

  back_order_preserving_cmp_imp(r_c, r_native_c,
				__gnu_pbds::detail::integral_constant<int,back_order_preserving>());

  order_statistics_cmp_imp(r_c, r_native_c,
			   __gnu_pbds::detail::integral_constant<int,order_statistics>());

  prefix_search_cmp_imp(r_c, r_native_c,
			__gnu_pbds::detail::integral_constant<int,prefix_search>());

  reverse_iteration_cmp_imp(r_c, r_native_c,
			    __gnu_pbds::detail::integral_constant<int,reverse_iteration>());

  lower_bound_cmp_imp(r_c, r_native_c,
		      __gnu_pbds::detail::integral_constant<int,order_preserving>());

  upper_bound_cmp_imp(r_c, r_native_c,
		      __gnu_pbds::detail::integral_constant<int,order_preserving>());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
order_preserving_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
order_preserving_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
{
  PB_DS_SET_DESTRUCT_PRINT

    typename cntnr::const_iterator b = r_c.begin();
  typename cntnr::const_iterator e = r_c.end();

  typename native_type::const_iterator native_b = r_native_c.begin();
  typename native_type::const_iterator native_e = r_native_c.end();

  try
    {
      it_cmp_imp(b, e, native_b, native_e);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(false, "", &r_c, &r_native_c)
    }

  try
    {
      back_it_cmp_imp(b, e, native_b, native_e);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(false, "", &r_c, &r_native_c)
    }

  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
back_order_preserving_cmp_imp(const Cntnr&, const native_type&, 
				    __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
back_order_preserving_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, 
			      __gnu_pbds::detail::true_type)
{
  PB_DS_SET_DESTRUCT_PRINT
  typename cntnr::const_iterator b = r_c.begin();
  typename cntnr::const_iterator e = r_c.end();
  typename native_type::const_iterator native_b = r_native_c.begin();
  typename native_type::const_iterator native_e = r_native_c.end();
  it_cmp_imp(b, e, native_b, native_e);
  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
reverse_iteration_cmp_imp(const Cntnr&, const native_type&, 
			  __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
reverse_iteration_cmp_imp(const Cntnr& r_c, const native_type&r_native_c, __gnu_pbds::detail::true_type)
{
  PB_DS_SET_DESTRUCT_PRINT

    typename cntnr::const_reverse_iterator b = r_c.rbegin();
  typename cntnr::const_reverse_iterator e = r_c.rend();

  typename native_type::const_reverse_iterator native_b = r_native_c.rbegin();
  typename native_type::const_reverse_iterator native_e = r_native_c.rend();

  try
    {
      it_cmp_imp(b, e, native_b, native_e);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(false, "", &r_c, &r_native_c)
    }

  try
    {
      back_it_cmp_imp(b, e, native_b, native_e);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(false, "", &r_c, &r_native_c)
    }

  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
order_statistics_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
order_statistics_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
{
  {
    m_alloc.set_throw_prob(0);
    const key_type k = test_traits::generate_key(m_g, m_m);
    m_alloc.set_throw_prob(1);
    const size_type order = r_c.order_of_key(k);
    const size_type native_order = std::distance(r_native_c.begin(),
						 r_native_c.lower_bound(test_traits::native_key(k)));

    PB_DS_THROW_IF_FAILED(order == native_order,
			  test_traits::key_to_string(k) << " " <<
			  static_cast<unsigned long>(order) << " " <<
			  static_cast<unsigned long>(native_order),
			  &r_c,
			  &r_native_c);
  }

  const size_type rand_ord =
    static_cast<size_t>(m_g.get_unsigned_long(0,
					      2 * static_cast<unsigned long>(m_m)));

  typename cntnr::const_iterator it = r_c.find_by_order(rand_ord);
  typename native_type::const_iterator native_it = r_native_c.begin();
  std::advance(native_it, std::min(rand_ord, r_native_c.size()));
  if (it == r_c.end()&&  native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(false,
			  static_cast<unsigned long>(rand_ord),
			  m_p_c,
			  &m_native_c);

  if (it != r_c.end()&&  native_it == r_native_c.end())
    PB_DS_THROW_IF_FAILED(false,
			  static_cast<unsigned long>(rand_ord),
			  m_p_c,
			  &m_native_c);

  if (it != r_c.end()&&     native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(test_traits::cmp(*it, * native_it),
			  static_cast<unsigned long>(rand_ord),
			  m_p_c,
			  &m_native_c);
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
prefix_search_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
prefix_search_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
{
  PB_DS_SET_DESTRUCT_PRINT
  m_alloc.set_throw_prob(0);
  const key_type k = test_traits::generate_key(m_g, m_m);
  m_alloc.set_throw_prob(1);
  try
    {
      typedef
	std::pair<typename Cntnr::const_iterator, typename Cntnr::const_iterator>
	pref_ret_t;

      const pref_ret_t pref_ret = r_c.prefix_range(k);

      typename native_type::const_iterator native_start_it = r_native_c.begin();

      while (native_start_it != r_native_c.end() && 
	     !test_traits::prefix_match(k,
					test_traits::extract_native_key(*native_start_it)))
	++native_start_it;

      typename native_type::const_iterator native_end_it =
	native_start_it;

      do
        {
	  if (native_end_it != r_native_c.end())
	    ++native_end_it;
        }
      while (native_end_it != r_native_c.end() && 
	     test_traits::prefix_match(k,
				       test_traits::extract_native_key(*native_end_it)));

      it_cmp_imp(pref_ret.first, pref_ret.second, native_start_it, native_end_it);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(false, "prefix key " << k, &r_c, &r_native_c);
    }

  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
template<typename Const_It, class Const_Native_It>
void
PB_DS_CLASS_C_DEC::
it_cmp_imp(Const_It b, Const_It e, Const_Native_It native_b, 
	   Const_Native_It native_e)
{
  PB_DS_SET_DESTRUCT_PRINT

  if (std::distance(b, e) != std::distance(native_b, native_e))
    {
      const size_t dist = std::distance(b, e);
      const size_t native_dist = std::distance(native_b, native_e);
      PB_DS_THROW_IF_FAILED(false,
			    static_cast<unsigned long>(dist) << " "
			    << static_cast<unsigned long>(native_dist),
			    m_p_c, &m_native_c);
    }
  
  while (b != e)
    {
      PB_DS_THROW_IF_FAILED(native_b != native_e, "", m_p_c, &m_native_c);

      if (!test_traits::cmp(*b, * native_b))
	PB_DS_THROW_IF_FAILED(false,
			      test_traits::val_to_string(*b) << " " <<
			      test_traits::val_to_string(*native_b),
			      m_p_c, &m_native_c);

      ++b;
      ++native_b;
    }

  PB_DS_THROW_IF_FAILED(native_b == native_e, "", m_p_c, &m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
template<typename Const_It, class Const_Native_It>
void
PB_DS_CLASS_C_DEC::
back_it_cmp_imp(Const_It b, Const_It e, Const_Native_It native_b, 
		Const_Native_It native_e)
{
  PB_DS_SET_DESTRUCT_PRINT
  while (b != e)
  {
    PB_DS_THROW_IF_FAILED(native_b != native_e,
			  test_traits::val_to_string(*native_e),
			  m_p_c, &m_native_c);

    --e;
    --native_e;

    PB_DS_THROW_IF_FAILED(test_traits::cmp(*e, * native_e),
			  test_traits::val_to_string(*e) <<
			  test_traits::val_to_string(*native_e),
			  m_p_c, &m_native_c);
  }

  PB_DS_THROW_IF_FAILED(native_b == native_e,
			test_traits::val_to_string(*native_e),
			m_p_c, &m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
lower_bound_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
lower_bound_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
{
  PB_DS_SET_DESTRUCT_PRINT
  m_alloc.set_throw_prob(0);
  const key_type k = test_traits::generate_key(m_g, m_m);
  m_alloc.set_throw_prob(1);
  typename cntnr::const_iterator it = r_c.lower_bound(k);
  typename native_type::key_type native_k = test_traits::native_key(k);
  typename native_type::const_iterator native_it = r_native_c.lower_bound(native_k);

  if (it != r_c.end() && native_it == r_native_c.end())
    PB_DS_THROW_IF_FAILED("",
			  "it: " + test_traits::val_to_string(*it) + "\n\n",
			  &r_c, &r_native_c);

  if (it == r_c.end() && native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED("",
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  &r_c, &r_native_c);

  if (it != r_c.end() && !test_traits::cmp(*it, * native_it))
    PB_DS_THROW_IF_FAILED(false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "it: " + test_traits::val_to_string(*it) + "\n\n" +
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  &r_c, &r_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
upper_bound_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
upper_bound_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
{
  PB_DS_SET_DESTRUCT_PRINT
  m_alloc.set_throw_prob(0);
  const key_type k = test_traits::generate_key(m_g, m_m);
  m_alloc.set_throw_prob(1);
  typename cntnr::const_iterator it =  r_c.upper_bound(k);
  typename native_type::key_type native_k = test_traits::native_key(k);
  typename native_type::const_iterator native_it = r_native_c.upper_bound(native_k);

  if (it == r_c.end() && native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  &r_c, &r_native_c);

  if (it != r_c.end() && native_it == r_native_c.end())
    PB_DS_THROW_IF_FAILED(false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "it: " + test_traits::val_to_string(*it) + "\n\n",
			  &r_c, &r_native_c);

  if (it != r_c.end() && !test_traits::cmp(*it, * native_it))
    PB_DS_THROW_IF_FAILED(false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "it: " + test_traits::val_to_string(*it) + "\n\n" +
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  &r_c, &r_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT
}

// Operators.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
operator()()
{
  typedef xml_result_set_regression_formatter formatter_type;
  formatter_type* p_fmt = NULL;

  if (m_disp)
    p_fmt = new formatter_type(string_form<Cntnr>::name(),
			       string_form<Cntnr>::desc());

  m_g.init(m_seed);

  // Track allocation from this point only.
  const size_t memory_label = 775;
  m_alloc.init(m_seed);
  m_alloc.set_label(memory_label);  

  prog_bar pb(m_n, std::cout, m_disp);
  m_i = 0;

  try
    {
      for (m_i = 0; m_i < m_n; ++m_i)
        {
	  PB_DS_TRACE("Op #" << static_cast<unsigned long>(m_i));
	  allocator_type::set_label(m_i);
	  switch (m_i)
            {
            case 0:
	      PB_DS_RUN_MTHD(default_constructor);
	      break;
            case 1:
	      defs();
	      break;
            case 2:
	      policy_access();
	      break;
            case 3:
	      it_copy();
	      break;
            case 4:
	      it_assign();
	      break;
            case 5:
	      rev_it_copy();
	      break;
            case 6:
	      rev_it_assign();
	      break;
            default:
	      switch(get_next_op())
                {
                case insert_op:
		  switch(get_next_sub_op(2))
                    {
                    case 0:
		      PB_DS_RUN_MTHD(insert)
                        break;
                    case 1:
		      PB_DS_RUN_MTHD(subscript)
                        break;
                    default:
		      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
                    }
		  break;
                case erase_op:
		  switch(get_next_sub_op(4))
                    {
                    case 0:
		      PB_DS_RUN_MTHD(erase)
                        break;
                    case 1:
		      PB_DS_RUN_MTHD(erase_if)
                        break;
                    case 2:
		      PB_DS_RUN_MTHD(erase_it)
                        break;
                    case 3:
		      PB_DS_RUN_MTHD(erase_rev_it)
                        break;
                    default:
		      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
                    }
		  break;
                case clear_op:
		  PB_DS_RUN_MTHD(clear)
                    break;
                case other_op:
		  switch(get_next_sub_op(8))
                    {
                    case 0:
		      swap();
		      break;
                    case 1:
		      PB_DS_RUN_MTHD(copy_constructor)
                        break;
                    case 2:
		      PB_DS_RUN_MTHD(it_constructor)
                        break;
                    case 3:
		      PB_DS_RUN_MTHD(assignment_operator)
                        break;
                    case 4:
		      PB_DS_RUN_MTHD(split_join)
                        break;
                    case 5:
		      resize();
		      break;
                    case 6:
		      get_set_load();
		      break;
                    case 7:
		      get_set_loads();
		      break;
                    default:
		      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
                    }
		  break;
                default:
		  PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
                };
            }

	  pb.inc();
        }
    }
  catch (...)
    {
      std::cerr << "Failed at index " << static_cast<unsigned long>(m_i) 
		<< std::endl;
      delete m_p_c;
      throw;
    }

  // Clean up, then check for allocation by special label, set above.
  delete m_p_c;

  try 
    { m_alloc.check_allocated(memory_label); }
  catch (...)
    {
      std::cerr << "detected leaks!" << std::endl;
      std::cerr << m_alloc << std::endl;
      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
    }

  // Reset throw probability.
  m_alloc.set_throw_prob(0);

  if (m_disp)
    {
      std::cout << std::endl;
      delete p_fmt;
    }
}

PB_DS_CLASS_T_DEC
typename PB_DS_CLASS_C_DEC::op
PB_DS_CLASS_C_DEC::
get_next_op()
{
  const double prob = m_g.get_prob();
  if (prob < m_ip)
    return insert_op;

  if (prob < m_ip + m_ep)
    return erase_op;

  if (prob < m_ip + m_ep + m_cp)
    return clear_op;

  PB_DS_THROW_IF_FAILED(prob <= 1, prob, m_p_c, &m_native_c);
  return other_op;
}

PB_DS_CLASS_T_DEC
size_t
PB_DS_CLASS_C_DEC::
get_next_sub_op(size_t max)
{
  const double p = m_g.get_prob();
  const double delta = 1 / static_cast<double>(max);
  size_t i = 0;
  while (true)
    if (p <= (i + 1) * delta)
      {
	PB_DS_THROW_IF_FAILED(i < max, i << " " << max, m_p_c, &m_native_c);
	return i;
      }
    else
      ++i;
}

// Insert.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
insert()
{
  PB_DS_TRACE("insert");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT
  try
    {
      m_alloc.set_throw_prob(0);
      value_type v = test_traits::generate_value(m_g, m_m);
      m_alloc.set_throw_prob(m_tp);
      const_key_reference r_k = test_traits::extract_key(v);
      typename cntnr::const_point_iterator found_it = m_p_c->find(r_k);
      const bool existed = (found_it != m_p_c->end());
      const std::pair<typename cntnr::point_iterator, bool> ins_ret = m_p_c->insert(v);
      
      if (ins_ret.second)
	{
	  PB_DS_THROW_IF_FAILED(!existed, "", m_p_c, &m_native_c);
	}
      else
	{
	  PB_DS_THROW_IF_FAILED(existed, "", m_p_c, &m_native_c);
	  PB_DS_THROW_IF_FAILED(found_it == ins_ret.first, "", m_p_c, &m_native_c);
	}
      m_native_c.insert(test_traits::native_value(v));
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;
    }
  catch(__gnu_pbds::insert_error&)
    {
      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
    }
  
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

// Subscript.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
subscript()
{
  PB_DS_TRACE("subscript");

  enum
    {
      no_data = __gnu_pbds::detail::is_same<
      typename Cntnr::const_key_reference,
      typename Cntnr::const_reference>::value
    };

  return (subscript_imp(__gnu_pbds::detail::integral_constant<int,no_data>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
subscript_imp(__gnu_pbds::detail::false_type)
{
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT

  try
    {
      m_alloc.set_throw_prob(0);
      value_type v = test_traits::generate_value(m_g, m_m);

      m_alloc.set_throw_prob(m_tp);
      (*m_p_c)[v.first] = v.second;

      m_native_c[test_traits::native_value(v).first] =
	test_traits::native_value(v).second;
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;
    }
  
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
subscript_imp(__gnu_pbds::detail::true_type)
{
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT
  try
    {
      m_alloc.set_throw_prob(0);
      value_type v = test_traits::generate_value(m_g, m_m);
      m_alloc.set_throw_prob(m_tp);
      (*m_p_c)[v] = __gnu_pbds::null_mapped_type();
      m_native_c.insert(test_traits::native_value(v));
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;
    }
  
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

// Clear.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
clear()
{
  m_p_c->clear();
  m_native_c.clear();
  return true;
}


// Erase.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase()
{
  PB_DS_TRACE("erase");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT
  m_alloc.set_throw_prob(0);
  const key_type k = test_traits::generate_key(m_g, m_m);
  m_alloc.set_throw_prob(m_tp);

  try
    {
      const bool cntnd = m_p_c->find(k) != m_p_c->end();
      PB_DS_THROW_IF_FAILED(cntnd ==(m_native_c.find(test_traits::native_key(k)) != m_native_c.end()), test_traits::key_to_string(k), m_p_c, &m_native_c);

      const bool ersd = m_p_c->erase(k);
      const bool native_ersd = m_native_c.erase(test_traits::native_key(k)) != 0;

      PB_DS_THROW_IF_FAILED(ersd == native_ersd, ersd << " " << native_ersd, 
			    m_p_c, &m_native_c);

      PB_DS_THROW_IF_FAILED(m_p_c->find(k) == m_p_c->end(), "", 
			    m_p_c, &m_native_c);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;

      PB_DS_THROW_IF_FAILED(            container_traits::erase_can_throw, container_traits::erase_can_throw, m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_if()
{
  PB_DS_TRACE("erase_if");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT

  try
    {
      typedef
	typename std::iterator_traits<typename cntnr::iterator>::reference
	it_const_reference;
      
      typedef
	typename test_traits::template erase_if_fn<value_type>
	erase_if_fn_t;
      
      m_alloc.set_throw_prob(m_tp);
      
      const size_t ersd = m_p_c->erase_if(erase_if_fn_t());      
      const size_t native_ersd = test_traits::erase_if(m_native_c);      
      PB_DS_THROW_IF_FAILED(ersd == native_ersd,
			    ersd << " " << native_ersd, m_p_c, &m_native_c);
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;
      PB_DS_THROW_IF_FAILED(container_traits::erase_can_throw, 
			    container_traits::erase_can_throw, 
			    m_p_c, &m_native_c);
    }
  
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it()
{
  enum
    {
      erase_iterators = container_traits::order_preserving
    };

  return (erase_it_imp(__gnu_pbds::detail::integral_constant<int,erase_iterators>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it_imp(__gnu_pbds::detail::false_type)
{
  return true;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("erase_it");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT

  try
    {
      m_alloc.set_throw_prob(0);
      const key_type k = test_traits::generate_key(m_g, m_m);
      m_alloc.set_throw_prob(m_tp);

      typename cntnr::iterator found_it = m_p_c->find(k);

      typename native_type::iterator native_it = m_native_c.find(test_traits::native_key(k));

      const bool found = found_it != m_p_c->end();
      const bool native_found = native_it != m_native_c.end();
      
      PB_DS_THROW_IF_FAILED(
			    found == native_found,
			    found << " " <<    native_found,
			    m_p_c,
			    &m_native_c);
      
      typename cntnr::const_iterator next_it = found_it;
      if (next_it != m_p_c->end())
	++next_it;
      
      typename cntnr::iterator next_ers_it = m_p_c->erase(found_it);
      
      if (native_it != m_native_c.end())
	m_native_c.erase(native_it);
      
      bool range_guarantee = __gnu_pbds::detail::is_same<
      typename container_traits::invalidation_guarantee,
	__gnu_pbds::range_invalidation_guarantee>::value ;

      if (range_guarantee)
	PB_DS_THROW_IF_FAILED(next_ers_it == next_it, "", m_p_c, &m_native_c);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;
      PB_DS_THROW_IF_FAILED(container_traits::erase_can_throw, container_traits::erase_can_throw, m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_rev_it()
{
  enum
    {
      erase_iterators = container_traits::order_preserving 
      	                && container_traits::reverse_iteration
    };

  return (erase_rev_it_imp(__gnu_pbds::detail::integral_constant<int,erase_iterators>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_rev_it_imp(__gnu_pbds::detail::false_type)
{
  return true;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_rev_it_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("erase_rev_it");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT

  try
    {
      m_alloc.set_throw_prob(0);      
      const key_type k = test_traits::generate_key(m_g, m_m);      
      m_alloc.set_throw_prob(m_tp);
      
      typename cntnr::iterator found_it = m_p_c->find(k);
      typename native_type::iterator native_it = m_native_c.find(test_traits::native_key(k));
      
      typename cntnr::const_reverse_iterator next_it = found_it;
      if (next_it != m_p_c->end())
	++next_it;
      
      typename cntnr::reverse_iterator next_ers_it =
	m_p_c->erase((typename cntnr::reverse_iterator)found_it);
      
      PB_DS_THROW_IF_FAILED(next_ers_it == next_it, "", m_p_c, &m_native_c);

      if (native_it != m_native_c.end())
	m_native_c.erase(native_it);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;      
      PB_DS_THROW_IF_FAILED(container_traits::erase_can_throw, 
			    container_traits::erase_can_throw, 
			    m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

// Defs.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
defs()
{
  // General container types.
  typedef typename Cntnr::size_type test_size_type;
  typedef typename Cntnr::difference_type difference_type;

  key_defs();
  mapped_defs();
  value_defs();
  iterator_defs();
  node_iterator_defs(__gnu_pbds::detail::integral_constant<int,
		     container_traits::order_preserving>());
  policy_defs();
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
key_defs()
{
  typedef typename Cntnr::key_type test_key_type;
  typedef typename Cntnr::key_reference test_key_reference;
  typedef typename Cntnr::const_key_reference test_const_key_reference;
  typedef typename Cntnr::key_pointer test_key_pointer;
  typedef typename Cntnr::const_key_pointer test_const_key_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
mapped_defs()
{
  typedef typename Cntnr::mapped_type test_mapped_type;
  typedef typename Cntnr::mapped_reference test_mapped_reference;
  typedef typename Cntnr::const_mapped_reference test_const_mapped_reference;
  typedef typename Cntnr::mapped_pointer test_mapped_pointer;
  typedef typename Cntnr::const_mapped_pointer test_const_mapped_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
value_defs()
{
  typedef typename Cntnr::value_type test_value_type;
  typedef typename Cntnr::reference test_reference;
  typedef typename Cntnr::const_reference test_const_reference;
  typedef typename Cntnr::pointer test_pointer;
  typedef typename Cntnr::const_pointer test_const_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
ds_defs()
{
  typedef __gnu_pbds::container_traits< Cntnr> test_container_traits;

  typedef typename test_container_traits::container_category test_container_category;

  typedef
    typename test_container_traits::invalidation_guarantee
    test_invalidation_guarantee;

  enum
    {
      test_order_preserving = test_container_traits::order_preserving
    };

  enum
    {
      test_erase_can_throw = test_container_traits::erase_can_throw
    };
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
iterator_defs()
{
  typedef typename Cntnr::point_iterator test_point_iterator;
  typedef typename Cntnr::const_point_iterator const_test_point_iterator;
  typedef typename Cntnr::iterator test_iterator;
  typedef typename Cntnr::const_iterator const_test_iterator;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
node_iterator_defs(__gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
node_iterator_defs(__gnu_pbds::detail::true_type)
{
  typedef typename Cntnr::node_iterator test_node_iterator;
  typedef typename Cntnr::const_node_iterator test_const_node_iterator;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs()
{
  typedef typename Cntnr::allocator_type test_allocator;
  policy_defs(typename Cntnr::container_category());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::basic_hash_tag)
{
  typedef typename Cntnr::hash_fn test_hash_fn;
  typedef typename Cntnr::eq_fn test_eq_fn;
  typedef typename Cntnr::resize_policy test_resize_policy;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::cc_hash_tag)
{
  policy_defs(__gnu_pbds::basic_hash_tag());
  typedef typename Cntnr::comb_hash_fn test_comb_hash_fn;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::gp_hash_tag)
{
  policy_defs(__gnu_pbds::basic_hash_tag());
  typedef typename Cntnr::comb_probe_fn test_comb_probe_fn;
  typedef typename Cntnr::probe_fn test_probe_fn;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::tree_tag)
{
  typedef typename Cntnr::cmp_fn test_cmp_fn;
  typedef typename Cntnr::node_update test_node_update;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::list_update_tag)
{
  typedef typename Cntnr::eq_fn test_eq_fn;
  typedef typename Cntnr::update_policy test_update_policy;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs(__gnu_pbds::pat_trie_tag)
{
  typedef typename Cntnr::e_access_traits e_access_traits;
}


// Policy Access.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access()
{
  policy_access(typename Cntnr::container_category());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(__gnu_pbds::basic_hash_tag)
{
  {
    typename Cntnr::hash_fn& r_t = m_p_c->get_hash_fn();
    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::hash_fn& r_t =((const Cntnr& )*m_p_c).get_hash_fn();
    assert(&r_t != NULL);
  }

  {
    typename Cntnr::eq_fn& r_t = m_p_c->get_eq_fn();
    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::eq_fn& r_t =((const Cntnr& )*m_p_c).get_eq_fn();
    assert(&r_t != NULL);
  }

  {
    typename Cntnr::resize_policy& r_t = m_p_c->get_resize_policy();
    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::resize_policy& r_t =((const Cntnr& )*m_p_c).get_resize_policy();

    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(__gnu_pbds::cc_hash_tag)
{
  policy_access(__gnu_pbds::basic_hash_tag());

  {
    typename Cntnr::comb_hash_fn& r_t = m_p_c->get_comb_hash_fn();
    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::comb_hash_fn& r_t =((const Cntnr& )*m_p_c).get_comb_hash_fn();

    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(__gnu_pbds::gp_hash_tag)
{
  policy_access(__gnu_pbds::basic_hash_tag());

  {
    typename Cntnr::comb_probe_fn& r_t = m_p_c->get_comb_probe_fn();
    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::comb_probe_fn& r_t =((const Cntnr& )*m_p_c).get_comb_probe_fn();

    assert(&r_t != NULL);
  }

  {
    typename Cntnr::probe_fn& r_t = m_p_c->get_probe_fn();
    assert(&r_t != NULL);
  }
  {
    const typename Cntnr::probe_fn& r_t =((const Cntnr& )*m_p_c).get_probe_fn();
    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(__gnu_pbds::tree_tag)
{
  {
    typename Cntnr::cmp_fn& r_t = m_p_c->get_cmp_fn();
    assert(&r_t != NULL);
  }

  {
    const typename Cntnr::cmp_fn& r_t =((const Cntnr& )*m_p_c).get_cmp_fn();
    assert(&r_t != NULL);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(__gnu_pbds::list_update_tag)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access(__gnu_pbds::pat_trie_tag)
{
  typename Cntnr::e_access_traits& r_t = m_p_c->get_e_access_traits();
  assert(&r_t != NULL);
}


// Split/Join.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join()
{
  enum
    {
      split_join = container_traits::order_preserving
    };

  return (split_join_imp(__gnu_pbds::detail::integral_constant<int,split_join>()));
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join_imp(__gnu_pbds::detail::false_type)
{
  return true;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("split_join");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT

  try
    {
      m_alloc.set_throw_prob(0);
      Cntnr lhs(*m_p_c);
      Cntnr rhs;
      native_type native_lhs(m_native_c);
      native_type native_rhs;
      const key_type k = test_traits::generate_key(m_g, m_m);
      
      m_alloc.set_throw_prob(m_tp);
      lhs.split(k, rhs);
      
      typename native_type::const_iterator it =
	native_lhs.upper_bound(test_traits::native_key(k));
      
      while (!native_lhs.empty()&&  it != native_lhs.end())
	{
	  native_rhs.insert(*it);
	  typename native_type::const_iterator next_it = it;
	  ++next_it;
	  native_lhs.erase(test_traits::extract_native_key(*it));
	  it = next_it;
	}
      
      PB_DS_COND_COMPARE(lhs, native_lhs);
      PB_DS_COND_COMPARE(rhs, native_rhs);
      
      m_alloc.set_throw_prob(m_tp);
      
      if (m_g.get_prob() < 0.5)
	lhs.swap(rhs);
      
      lhs.join(rhs);
      PB_DS_THROW_IF_FAILED(rhs.size() == 0, rhs.size(), m_p_c, &m_native_c);
      PB_DS_THROW_IF_FAILED(rhs.empty(), rhs.size(), m_p_c, &m_native_c);
      m_p_c->swap(lhs);
    }
  catch(__gnu_cxx::forced_exception_error& )
    {
      done = false;
      PB_DS_THROW_IF_FAILED(container_traits::split_join_can_throw, 
			    container_traits::split_join_can_throw, 
			    m_p_c, &m_native_c);
    }
  
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

// Iterator conversions.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
it_copy()
{
  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_iterator const_it(it);
    PB_DS_THROW_IF_FAILED(const_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_it != it), "", m_p_c, &m_native_c);

    typename cntnr::const_point_iterator const_find_it(it);
    PB_DS_THROW_IF_FAILED(const_find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_find_it != it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it(it);
    PB_DS_THROW_IF_FAILED(find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(find_it != it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::const_iterator const_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it(const_it);
    PB_DS_THROW_IF_FAILED(const_find_it == const_it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_find_it != const_it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::point_iterator find_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it(find_it);
    PB_DS_THROW_IF_FAILED(find_it == const_find_it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(find_it != const_find_it), "", m_p_c, &m_native_c);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
it_assign()
{
  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_iterator const_it;
    const_it = it;
    PB_DS_THROW_IF_FAILED(const_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_it != it), "", m_p_c, &m_native_c);

    typename cntnr::const_point_iterator const_find_it;
    const_find_it = it;
    PB_DS_THROW_IF_FAILED(const_find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_find_it != it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it;
    find_it = it;
    PB_DS_THROW_IF_FAILED(find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(find_it != it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::const_iterator const_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it;
    const_find_it = const_it;
    PB_DS_THROW_IF_FAILED(const_find_it == const_it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_find_it != const_it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::point_iterator find_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it;
    const_find_it = find_it;
    PB_DS_THROW_IF_FAILED(find_it == const_find_it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(find_it != const_find_it), "", m_p_c, &m_native_c);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
rev_it_copy()
{
  enum
    {
      reverse_iteration = container_traits::reverse_iteration
    };

  rev_it_copy_imp(__gnu_pbds::detail::integral_constant<int,reverse_iteration>());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
rev_it_assign()
{
  enum
    {
      reverse_iteration = container_traits::reverse_iteration
    };

  rev_it_assign_imp(__gnu_pbds::detail::integral_constant<int,reverse_iteration>());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
rev_it_copy_imp(__gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
rev_it_copy_imp(__gnu_pbds::detail::true_type)
{
  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_reverse_iterator const_it(it);
    PB_DS_THROW_IF_FAILED(const_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_it != it), "", m_p_c, &m_native_c);

    typename cntnr::const_point_iterator const_find_it(it);
    PB_DS_THROW_IF_FAILED(const_find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_find_it != it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it(it);
    PB_DS_THROW_IF_FAILED(find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(find_it != it), "", m_p_c, &m_native_c);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
rev_it_assign_imp(__gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
rev_it_assign_imp(__gnu_pbds::detail::true_type)
{
  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_reverse_iterator const_it;
    const_it = it;
    PB_DS_THROW_IF_FAILED(const_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_it != it), "", m_p_c, &m_native_c);

    typename cntnr::const_point_iterator const_find_it;
    const_find_it = it;
    PB_DS_THROW_IF_FAILED(const_find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(const_find_it != it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it;
    find_it = it;
    PB_DS_THROW_IF_FAILED(find_it == it, "", m_p_c, &m_native_c);
    PB_DS_THROW_IF_FAILED(!(find_it != it), "", m_p_c, &m_native_c);
  }
}

// Resize.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
resize()
{
  typedef __gnu_pbds::detail::integral_constant<int, test_traits::resize> resize_ind;

  return resize_imp(resize_ind());
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
resize_imp(__gnu_pbds::detail::false_type)
{
  return true;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
resize_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("resize");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT
  const size_t old_size = m_p_c->get_actual_size();

  try
    {
      enum
        {
	  min_new_size = 200,
	  max_new_size = 2000
        };

      m_alloc.set_throw_prob(m_tp);
      typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());
      const size_t new_size = m_g.get_unsigned_long(min_new_size, max_new_size);
      m_p_c->resize(new_size);
      const size_t actual_new_size = m_p_c->get_actual_size();
      PB_DS_THROW_IF_FAILED(actual_new_size >= new_size,
			    actual_new_size << " " << new_size,
			    m_p_c, &m_native_c);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(m_p_c->get_actual_size() == old_size,
			    m_p_c->get_actual_size() << " " << old_size,
			    m_p_c, &m_native_c);

      done = false;
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}


// Get/Set load.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
get_set_load()
{
  typedef
    __gnu_pbds::detail::integral_constant<int, test_traits::get_set_load>
    get_set_load_ind;

  get_set_load_imp(get_set_load_ind());
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
get_set_load_imp(__gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
get_set_load_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("get_set_load");
  PB_DS_SET_DESTRUCT_PRINT
  m_p_c->get_load();
  m_alloc.set_throw_prob(1);
  typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());
  const float min_load = static_cast<float>(0.05);
  const float max_load = static_cast<float>(0.9);

  const float new_load = static_cast<float>(m_g.get_prob() * (max_load - min_load) + min_load);

  m_p_c->set_load(new_load);
  PB_DS_THROW_IF_FAILED(m_p_c->get_load() == new_load, "", m_p_c, &m_native_c);
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
}


// Get/Set loads.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
get_set_loads()
{
  typedef
    __gnu_pbds::detail::integral_constant<int, test_traits::get_set_loads>
    get_set_loads_ind;

  return get_set_loads_imp(get_set_loads_ind());
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
get_set_loads_imp(__gnu_pbds::detail::false_type)
{
  return true;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
get_set_loads_imp(__gnu_pbds::detail::true_type)
{
  PB_DS_TRACE("get_set_loads");
  bool done = true;
  PB_DS_SET_DESTRUCT_PRINT
  const std::pair<float, float> old_loads = m_p_c->get_loads();

  try
    {
      m_alloc.set_throw_prob(m_tp);

      typename alloc_t::group_throw_prob_adjustor adjust(m_p_c->size());

      const float min_min_load = static_cast<float>(0.05);
      const float max_min_load = static_cast<float>(0.2);

      const float new_min_load =
	static_cast<float>(m_g.get_prob()*  (max_min_load - min_min_load) +
			   min_min_load);

      const float new_max_load = static_cast<float>(new_min_load*  2.5);
      PB_DS_THROW_IF_FAILED(new_max_load < 1, new_max_load, m_p_c, &m_native_c);
      m_p_c->set_loads(std::make_pair(new_min_load, new_max_load));
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(old_loads == m_p_c->get_loads(),
			    old_loads.first << " " << old_loads.second << " " <<
			    m_p_c->get_loads().first << " " <<
			    m_p_c->get_loads().second,
			    m_p_c, &m_native_c);

      done = false;
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  PB_DS_CANCEL_DESTRUCT_PRINT
  return done;
}

// Diagnostics.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
print_container(const native_type& r_cnt, std::ostream& r_os) const
{
  m_alloc.set_throw_prob(0);
  typename native_type::const_iterator it = r_cnt.begin();
  while (it != r_cnt.end())
    {
      r_os << test_traits::val_to_string(*it) + "\n";
      ++it;
    }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
print_container(const cntnr& r_cnt, std::ostream& r_os) const
{
  m_alloc.set_throw_prob(0);
  typename cntnr::const_iterator it = r_cnt.begin();
  while (it != r_cnt.end())
    {
      r_os << test_traits::val_to_string(*it) + "\n";
      ++it;
    }
}

#endif
