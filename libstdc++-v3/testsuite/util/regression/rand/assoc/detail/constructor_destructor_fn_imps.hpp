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
 * @file constructor_destructor_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

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
	  PB_DS_THROW_IF_FAILED(false, "",  m_p_c, & m_native_c);
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
	  PB_DS_THROW_IF_FAILED(false, "",  m_p_c, & m_native_c);
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
	  PB_DS_THROW_IF_FAILED(false, "",  m_p_c, &m_native_c);
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
	  PB_DS_THROW_IF_FAILED(false, "",  m_p_c, & m_native_c);
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

