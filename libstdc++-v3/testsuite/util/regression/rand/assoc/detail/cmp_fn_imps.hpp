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
 * @file cmp_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
cmp(const Cntnr& r_c, const native_type& r_native_c, const std::string& r_call_fn)
{
  m_alloc.set_throw_prob(1);

  const size_t size = r_c.size();
  const size_t native_size = r_native_c.size();

  PB_DS_THROW_IF_FAILED(
			size == native_size,
			static_cast<unsigned long>(size) << " " <<
			static_cast<unsigned long>(native_size),
			& r_c,
			& r_native_c);

  const bool empty = r_c.empty();
  const bool native_empty = r_native_c.empty();

  PB_DS_THROW_IF_FAILED(
			empty == native_empty,
			empty << " " << native_empty,
			& r_c,
			& r_native_c);

  try
    {
      basic_cmp_(r_c, r_native_c);

      cmp_(r_c, r_native_c);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(            false, "call-fn: " + r_call_fn, & r_c, & r_native_c);
    }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
basic_cmp_(const Cntnr& r_c, const native_type& r_native_c)
{
  PB_DS_SET_DESTRUCT_PRINT

    if (static_cast<size_t>(std::distance(r_c.begin(), r_c.end())) !=
	r_c.size())
      PB_DS_THROW_IF_FAILED(
			    false,
			    static_cast<unsigned long>(
						       std::distance(r_c.begin(), r_c.end())) <<
			    " " << static_cast<unsigned long>(r_c.size()),
			    & r_c,
			    & r_native_c);

  typename native_type::const_iterator it = r_native_c.begin();

  while (it != r_native_c.end())
    {
      typename native_type::key_type native_key =
	test_traits::extract_native_key(*it);

      m_alloc.set_throw_prob(0);

      const key_type k = native_key;

      m_alloc.set_throw_prob(1);

      typename cntnr::const_point_iterator found_it = r_c.find(k);

      PB_DS_THROW_IF_FAILED(
			    found_it != r_c.end(),
			    test_traits::native_val_to_string(*it),
			    & r_c,
			    & r_native_c);

      if (!test_traits::cmp(*found_it, * it))
	PB_DS_THROW_IF_FAILED(                false,  "", & r_c, & r_native_c);

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
      order_preserving =
      container_traits::order_preserving,
      back_order_preserving =
      container_traits::order_preserving&& 
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
      PB_DS_THROW_IF_FAILED(            false, "", & r_c, & r_native_c)
	}

  try
    {
      back_it_cmp_imp(b, e, native_b, native_e);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(            false, "", & r_c, & r_native_c)
	}

  PB_DS_CANCEL_DESTRUCT_PRINT
    }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
back_order_preserving_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
back_order_preserving_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
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
reverse_iteration_cmp_imp(const Cntnr& /*r_c*/, const native_type& /*r_native_c*/, __gnu_pbds::detail::false_type)
{ }

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
reverse_iteration_cmp_imp(const Cntnr& r_c, const native_type& r_native_c, __gnu_pbds::detail::true_type)
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
      PB_DS_THROW_IF_FAILED(            false, "", & r_c, & r_native_c)
	}

  try
    {
      back_it_cmp_imp(b, e, native_b, native_e);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(            false, "", & r_c, & r_native_c)
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

    const key_type k =
      test_traits::generate_key(m_g, m_m);

    m_alloc.set_throw_prob(1);

    const size_type order = r_c.order_of_key(k);

    const size_type native_order = std::distance(
						 r_native_c.begin(),
						 r_native_c.lower_bound(test_traits::native_key(k)));

    PB_DS_THROW_IF_FAILED(
			  order == native_order,
			  test_traits::key_to_string(k) << " " <<
			  static_cast<unsigned long>(order) << " " <<
			  static_cast<unsigned long>(native_order),
			  & r_c,
			  & r_native_c);
  }

  const size_type rand_ord =
    static_cast<size_t>(m_g.get_unsigned_long(
					      0,
					      2*  static_cast<unsigned long>(m_m)));

  typename cntnr::const_iterator it =
    r_c.find_by_order(rand_ord);

  typename native_type::const_iterator native_it =
    r_native_c.begin();

  std::advance(native_it, std::min(rand_ord, r_native_c.size()));

  if (it == r_c.end()&&  native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  false,
			  static_cast<unsigned long>(rand_ord),
			  m_p_c,
			  & m_native_c);

  if (it != r_c.end()&&  native_it == r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  false,
			  static_cast<unsigned long>(rand_ord),
			  m_p_c,
			  & m_native_c);

  if (it != r_c.end()&&     native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  test_traits::cmp(*it, * native_it),
			  static_cast<unsigned long>(rand_ord),
			  m_p_c,
			  & m_native_c);
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

  const key_type k =
    test_traits::generate_key(m_g, m_m);

  m_alloc.set_throw_prob(1);

  try
    {
      typedef
	std::pair<
	typename Cntnr::const_iterator,
	typename Cntnr::const_iterator>
	pref_ret_t;

      const pref_ret_t pref_ret = r_c.prefix_range(k);

      typename native_type::const_iterator native_start_it =
	r_native_c.begin();

      while (native_start_it != r_native_c.end()&& 
	     !test_traits::prefix_match(
					k,
					test_traits::extract_native_key(*native_start_it)))
	++native_start_it;

      typename native_type::const_iterator native_end_it =
	native_start_it;

      do
        {
	  if (native_end_it != r_native_c.end())
	    ++native_end_it;
        }
      while (native_end_it != r_native_c.end()&& 
	     test_traits::prefix_match(
				       k,
				       test_traits::extract_native_key(*native_end_it)));

      it_cmp_imp(            pref_ret.first, pref_ret.second, native_start_it, native_end_it);
    }
  catch(...)
    {
      PB_DS_THROW_IF_FAILED(
			    false,
			    "prefix key " << k,
			    & r_c,
			    & r_native_c);
    }

  PB_DS_CANCEL_DESTRUCT_PRINT
    }

PB_DS_CLASS_T_DEC
template<typename Const_It, class Const_Native_It>
void
PB_DS_CLASS_C_DEC::
it_cmp_imp(Const_It b, Const_It e, Const_Native_It native_b, Const_Native_It native_e)
{
  PB_DS_SET_DESTRUCT_PRINT

    if (std::distance(b, e) != std::distance(native_b, native_e))
      {
        const size_t dist = std::distance(b, e);

        const size_t native_dist = std::distance(native_b, native_e);

        PB_DS_THROW_IF_FAILED(
			      false,
			      static_cast<unsigned long>(dist) << " "
			      << static_cast<unsigned long>(native_dist),
			      m_p_c,
			      & m_native_c);
      }

  while (b != e)
    {
      PB_DS_THROW_IF_FAILED(            native_b != native_e, "", m_p_c, & m_native_c);

      if (!test_traits::cmp(*b, * native_b))
	PB_DS_THROW_IF_FAILED(
			      false,
			      test_traits::val_to_string(*b) << " " <<
			      test_traits::val_to_string(*native_b),
			      m_p_c,
			      & m_native_c);

      ++b;
      ++native_b;
    }

  PB_DS_THROW_IF_FAILED(        native_b == native_e, "", m_p_c,    & m_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT
    }

PB_DS_CLASS_T_DEC
template<typename Const_It, class Const_Native_It>
void
PB_DS_CLASS_C_DEC::
back_it_cmp_imp(Const_It b, Const_It e, Const_Native_It native_b, Const_Native_It native_e)
{
  PB_DS_SET_DESTRUCT_PRINT

    while (b != e)
      {
        PB_DS_THROW_IF_FAILED(
			      native_b != native_e,
			      test_traits::val_to_string(*native_e),
			      m_p_c,
			      & m_native_c);

        --e;
        --native_e;

        PB_DS_THROW_IF_FAILED(
			      test_traits::cmp(*e, * native_e),
			      test_traits::val_to_string(*e) <<
			      test_traits::val_to_string(*native_e),
			      m_p_c,
			      & m_native_c);
      }

  PB_DS_THROW_IF_FAILED(
			native_b == native_e,
			test_traits::val_to_string(*native_e),
			m_p_c,
			& m_native_c);

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

  const key_type k =
    test_traits::generate_key(m_g, m_m);

  m_alloc.set_throw_prob(1);

  typename cntnr::const_iterator it =    r_c.lower_bound(k);

  typename native_type::key_type native_k = test_traits::native_key(k);

  typename native_type::const_iterator native_it =
    r_native_c.lower_bound(native_k);

  if (it != r_c.end()&&  native_it == r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  "",
			  "it: " + test_traits::val_to_string(*it) + "\n\n",
			  & r_c,
			  & r_native_c);

  if (it == r_c.end()&&  native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  "",
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  & r_c,
			  & r_native_c);

  if (it != r_c.end()&&  !test_traits::cmp(*it, * native_it))
    PB_DS_THROW_IF_FAILED(
			  false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "it: " + test_traits::val_to_string(*it) + "\n\n" +
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  & r_c,
			  & r_native_c);

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

  const key_type k =
    test_traits::generate_key(m_g, m_m);

  m_alloc.set_throw_prob(1);

  typename cntnr::const_iterator it =    r_c.upper_bound(k);

  typename native_type::key_type native_k = test_traits::native_key(k);

  typename native_type::const_iterator native_it =
    r_native_c.upper_bound(native_k);

  if (it == r_c.end()&&  native_it != r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  & r_c,
			  & r_native_c);

  if (it != r_c.end()&&  native_it == r_native_c.end())
    PB_DS_THROW_IF_FAILED(
			  false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "it: " + test_traits::val_to_string(*it) + "\n\n",
			  & r_c,
			  & r_native_c);

  if (it != r_c.end()&&  !test_traits::cmp(*it, * native_it))
    PB_DS_THROW_IF_FAILED(
			  false,
			  "key: " + test_traits::key_to_string(k) + "\n\n" +
			  "it: " + test_traits::val_to_string(*it) + "\n\n" +
			  "native_it: " + test_traits::val_to_string(*native_it) + "\n\n",
			  & r_c,
			  & r_native_c);

  PB_DS_CANCEL_DESTRUCT_PRINT
    }
