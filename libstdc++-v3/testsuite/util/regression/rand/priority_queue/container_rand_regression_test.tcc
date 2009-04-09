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

  // Constructor, copy constructor, assignment and destructor.
PB_DS_CLASS_T_DEC
PB_DS_CLASS_C_DEC::
container_rand_regression_test(unsigned long seed, size_t n, size_t m, 
			       double tp, double ip, double dp, double ep, 
			       double cp, double mp, bool disp) 
: m_seed(seed == 0 ? twister_rand_gen::get_time_determined_seed(): seed),
  m_n(n), m_m(m), m_tp(tp), m_ip(ip), m_dp(dp), m_ep(ep), m_cp(cp),
  m_mp(mp), m_disp(disp), m_p_c(NULL)
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
  catch(__gnu_cxx::forced_exception_error& )
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

  typedef typename allocator_type::group_throw_prob_adjustor adjustor;
  adjustor adjust(m_p_c->size());

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

  typedef typename allocator_type::group_throw_prob_adjustor adjustor;
  adjustor adjust(m_p_c->size());

  try
    {
      p_c = new Cntnr();
      *p_c = *m_p_c;
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
  bool done = true;
  Cntnr* p_c = NULL;
  m_alloc.set_throw_prob(m_tp);
  typedef typename allocator_type::group_throw_prob_adjustor adjustor;
  adjustor adjust(m_p_c->size());

  try
    {
      switch(get_next_sub_op(3))
        {
        case 0:
	  p_c = new Cntnr(m_p_c->get_cmp_fn());
	  m_native_c.clear();
	  break;
        case 1:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end());
	  break;
        case 2:
	  p_c = new Cntnr(m_p_c->begin(), m_p_c->end(), m_p_c->get_cmp_fn());
	  break;
        default:
	  _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
        };

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


  // Compare.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
cmp(const Cntnr& c, const native_type& native, const std::string& callfn)
{
  destructor_printer notify(__FUNCTION__);

  try
    {
      m_alloc.set_throw_prob(1);
      
      const size_t size = c.size();
      const size_t native_size = native.size();
      _GLIBCXX_THROW_IF(size != native_size, size << " " << native_size,
			&c, &native);
      
      const bool empty = c.empty();
      const bool native_empty = native.empty();
      _GLIBCXX_THROW_IF(empty != native_empty, empty << " " << native_empty, 
			&c, &native);
      
      const size_t it_size = std::distance(c.begin(), c.end());
      _GLIBCXX_THROW_IF(it_size != size, it_size << " " << size, &c, &native);
      
      if (!c.empty())
	{
	  const std::string native_top = native.top();
	  const std::string top = test_traits::native_value(c.top());
	  const bool top_smaller = std::less<std::string>()(top, native_top);
	  const bool top_larger = std::less<std::string>()(native_top, top);
	  
	  if (top_smaller || top_larger)
	    _GLIBCXX_THROW_IF(true, top << " " << native_top, &c, &native);
	}
    }
  catch(...)
    {
      _GLIBCXX_THROW_IF(true, "call-fn: " + callfn, &c, &native);
    }
  
  notify.cancel();
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
  m_alloc.init(m_seed);

  // The __throw_allocator::_S_label defaults to 0, so to make things
  // easier and more precise when debugging, start at 1.
  const size_t starting_label(1);

  try
    {
      prog_bar pb(m_n, std::cout, m_disp);

      for (m_i = starting_label; m_i <= m_n; ++m_i)
        {
	  PB_DS_TRACE("Op #" << m_i);

	  // Track allocation from this point only.
	  allocator_type::set_label(m_i);
	  switch(m_i)
            {
            case 1:
	      PB_DS_RUN_MTHD(default_constructor);
	      break;
            case 2:
	      defs();
	      break;
            case 3:
	      policy_access();
	      break;
            case 4:
	      it_copy();
	      break;
            case 5:
	      it_assign();
	      break;
            default:
	      switch(get_next_op())
                {
                case insert_op:
		  PB_DS_RUN_MTHD(push)
                    break;
                case modify_op:
		  PB_DS_RUN_MTHD(modify)
                    break;
                case erase_op:
		  switch(get_next_sub_op(3))
                    {
                    case 0:
		      PB_DS_RUN_MTHD(pop)
                        break;
                    case 1:
		      PB_DS_RUN_MTHD(erase_if)
                        break;
                    case 2:
		      PB_DS_RUN_MTHD(erase_it)
                        break;
                    default:
		      _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
                    }
		  break;
                case clear_op:
		  PB_DS_RUN_MTHD(clear)
                    break;
                case other_op:
		  switch(get_next_sub_op(5))
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
                    default:
		      _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
                    }
		  break;
                default:
		  _GLIBCXX_THROW_IF(true, "", m_p_c,  &m_native_c);
                };
            }
	  pb.inc();
        }
    }
  catch (...)
    {
      std::cerr << "Failed at index " << m_i << std::endl;
      delete m_p_c;
      throw;
    }

  // Clean up, then check for allocation by special label, set above.
  allocator_type::set_label(0);
  delete m_p_c;

  try 
    { 
      for (size_t n = starting_label; n <= m_n; ++n)
	m_alloc.check_allocated(n); 
    }
  catch (std::logic_error& obj)
    {
      // On fail, check_allocated should throw std::logic_error.
      std::cerr << obj.what() << std::endl;
      std::cerr << typeid(Cntnr).name() << std::endl;
      throw;
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

  if (prob < m_ip + m_dp)
    return modify_op;

  if (prob < m_ip + m_dp + m_ep)
    return erase_op;

  if (prob < m_ip + m_dp + m_ep + m_cp)
    return clear_op;

  _GLIBCXX_THROW_IF(prob > 1, prob, m_p_c, &m_native_c);
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
	_GLIBCXX_THROW_IF(i >= max, i << " " << max, m_p_c, &m_native_c);
	return i;
      }
    else
      ++i;
}

  // Insert.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
push()
{
  PB_DS_TRACE("push");
  bool done = true;
  destructor_printer notify(__FUNCTION__);

    try
      {
        m_alloc.set_throw_prob(0);
        value_type v = test_traits::generate_value(m_g, m_m);
        m_alloc.set_throw_prob(m_tp);
        const typename cntnr::size_type sz = m_p_c->size();
        m_p_c->push(v);
        _GLIBCXX_THROW_IF(sz != m_p_c->size() - 1, sz, m_p_c, &m_native_c);
        m_native_c.push(test_traits::native_value(v));
      }
    catch(__gnu_cxx::forced_exception_error& )
      {
        done = false;
      }
    catch(...)
      {
        _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
      }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  notify.cancel();
  return done;
}


  // Modify.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
modify()
{
  PB_DS_TRACE("modify");
  destructor_printer notify(__FUNCTION__);

  bool done = true;
  try
    {
      m_alloc.set_throw_prob(0);
      value_type v = test_traits::generate_value(m_g, m_m);

      m_alloc.set_throw_prob(m_tp);
      typename cntnr::iterator it = m_p_c->begin();
      std::advance(it, m_g.get_unsigned_long(0, m_p_c->size()));
      if (it != m_p_c->end())
	{
	  typedef typename test_traits::native_value_type native_value_type;
	  native_value_type native_v = test_traits::native_value(*it);
	  native_value_type new_native_v = test_traits::native_value(v);
	  m_p_c->modify(it, v);
	  m_native_c.modify(native_v, new_native_v);
	}
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;
      _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  notify.cancel();
  return done;
}

  // Clear.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
clear()
{
  PB_DS_TRACE("clear");
  m_p_c->clear();
  m_native_c.clear();
  return true;
}

  // Erase.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
pop()
{
  PB_DS_TRACE("pop");
  destructor_printer notify(__FUNCTION__);

  bool done = true;
  try
    {
      m_alloc.set_throw_prob(1);
      if (!m_p_c->empty())
        {
	  m_p_c->pop();
	  m_native_c.pop();
        }
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;
      _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  notify.cancel();
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_if()
{
  PB_DS_TRACE("erase_if");
  destructor_printer notify(__FUNCTION__);

  bool done = true;
  try
    {
      typedef
	typename std::iterator_traits<typename cntnr::iterator>::reference
	it_const_reference;
      
      m_alloc.set_throw_prob(1);
      
      typedef
	typename test_traits::template erase_if_fn<value_type>
	erase_if_fn_t;
      
      const size_t ersd = m_p_c->erase_if(erase_if_fn_t());
      
      typedef
	typename test_traits::template erase_if_fn<std::string>
	native_erase_if_fn_t;
      
      const size_t native_ersd = m_native_c.erase_if(native_erase_if_fn_t());

      _GLIBCXX_THROW_IF(ersd != native_ersd, ersd << " " << native_ersd,
			m_p_c, &m_native_c);
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;      
      _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  notify.cancel();
  return done;
}

PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
erase_it()
{
  PB_DS_TRACE("erase_it");
  destructor_printer notify(__FUNCTION__);

  bool done = true;
  try
    {
      m_alloc.set_throw_prob(1);      
      typename cntnr::iterator it = m_p_c->begin();      
      std::advance(it, m_g.get_unsigned_long(0, m_p_c->size()));
      
      if (it != m_p_c->end())
	{
	  m_native_c.erase(*it);	 
	  m_p_c->erase(it);
	}
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;      
      _GLIBCXX_THROW_IF(true, "", m_p_c, &m_native_c);
    }

  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  notify.cancel();
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
  value_defs();
  iterator_defs();
  policy_defs();
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
  typedef typename Cntnr::container_category test_container_category;
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
policy_defs()
{
  typedef typename Cntnr::allocator_type test_allocator;
  typedef typename Cntnr::cmp_fn test_cmp_fn;
}


// Policy access.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_access()
{
  PB_DS_TRACE("policy_access");

  {
    typename Cntnr::cmp_fn& r_t = m_p_c->get_cmp_fn();
    assert(&r_t != NULL);
  }

  {
    const typename Cntnr::cmp_fn& r_t =((const Cntnr& )*m_p_c).get_cmp_fn();
    assert(&r_t != NULL);
  }
}

// Split join.
PB_DS_CLASS_T_DEC
bool
PB_DS_CLASS_C_DEC::
split_join()
{
  PB_DS_TRACE("split_join");
  destructor_printer notify(__FUNCTION__);

  bool done = true;
  try
    {
      m_alloc.set_throw_prob(0);
      Cntnr lhs(*m_p_c);
      Cntnr rhs;
      native_type native_lhs(m_native_c);
      m_alloc.set_throw_prob(m_tp);
      
      typedef typename test_traits::template erase_if_fn<value_type> split_fn_t;
      lhs.split(split_fn_t(), rhs);
      
      typedef typename test_traits::template erase_if_fn<std::string>
	native_split_fn_t;
      
      native_type native_rhs;      
      native_lhs.split(native_split_fn_t(), native_rhs);      
      PB_DS_COND_COMPARE(lhs, native_lhs);
      PB_DS_COND_COMPARE(rhs, native_rhs);
      
      m_alloc.set_throw_prob(m_tp);
      
      if (m_g.get_prob() < 0.5)
	lhs.swap(rhs);      
      lhs.join(rhs);
      
      _GLIBCXX_THROW_IF(rhs.size() != 0, rhs.size(), m_p_c, &m_native_c);
      _GLIBCXX_THROW_IF(!rhs.empty(), rhs.size(), m_p_c, &m_native_c);
    }
  catch(__gnu_cxx::forced_exception_error&)
    {
      done = false;      
      const bool b = __gnu_pbds::container_traits<cntnr>::split_join_can_throw;
      _GLIBCXX_THROW_IF(!b, b, m_p_c, &m_native_c);
    }
  
  PB_DS_COND_COMPARE(*m_p_c, m_native_c);
  notify.cancel();
  return done;
}

// Iterator conversions.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
it_copy()
{
  PB_DS_TRACE("it_copy");

  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_iterator const_it(it);
    _GLIBCXX_THROW_IF(const_it != it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(const_it == it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::const_iterator const_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it(const_it);
    _GLIBCXX_THROW_IF(const_find_it != const_it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(const_find_it == const_it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it1(it);
    _GLIBCXX_THROW_IF(const_find_it1 != it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(const_find_it1 == it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it1(it);
    _GLIBCXX_THROW_IF(find_it1 != it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(find_it1 == it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it(find_it);
    _GLIBCXX_THROW_IF(find_it != const_find_it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(find_it == const_find_it), "", m_p_c, &m_native_c);
  }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
it_assign()
{
  PB_DS_TRACE("it_assign");

  {
    typename cntnr::iterator it = m_p_c->end();
    typename cntnr::const_iterator const_it;
    const_it = it;
    _GLIBCXX_THROW_IF(const_it != it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(const_it == it), "", m_p_c, &m_native_c);

    typename cntnr::const_point_iterator const_find_it;
    const_find_it = it;
    _GLIBCXX_THROW_IF(const_find_it != it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(const_find_it == it), "", m_p_c, &m_native_c);

    typename cntnr::point_iterator find_it;
    find_it = it;
    _GLIBCXX_THROW_IF(find_it != it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(find_it == it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::const_iterator const_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it;
    const_find_it = const_it;
    _GLIBCXX_THROW_IF(const_find_it != const_it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(const_find_it == const_it), "", m_p_c, &m_native_c);
  }

  {
    typename cntnr::point_iterator find_it = m_p_c->end();
    typename cntnr::const_point_iterator const_find_it;
    const_find_it = find_it;
    _GLIBCXX_THROW_IF(find_it != const_find_it, "", m_p_c, &m_native_c);
    _GLIBCXX_THROW_IF(!(find_it == const_find_it), "", m_p_c, &m_native_c);
  }
}


// Diagnostics.
PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
print_container(const native_type& cnt, std::ostream& os) const
{
  m_alloc.set_throw_prob(0);
  native_type cpy(cnt);
  while (!cpy.empty())
    {
      os << cpy.top() << std::endl;
      cpy.pop();
    }
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
print_container(const cntnr& cnt, std::ostream& os) const
{
  typedef typename cntnr::const_iterator const_iterator;
  m_alloc.set_throw_prob(0);
  for (const_iterator it = cnt.begin(); it != cnt.end(); ++it)
    os << *it << std::endl;
}

#endif
