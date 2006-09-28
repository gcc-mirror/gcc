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
 * @file operator_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

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
	  allocator::set_label(m_i);
	  switch(m_i)
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
		      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
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
		      PB_DS_THROW_IF_FAILED(false, "", m_p_c, &m_native_c);
                    }
		  break;
                default:
		  PB_DS_THROW_IF_FAILED(false, "", m_p_c,  &m_native_c);
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
    return (insert_op);

  if (prob < m_ip + m_dp)
    return (modify_op);

  if (prob < m_ip + m_dp + m_ep)
    return (erase_op);

  if (prob < m_ip + m_dp + m_ep + m_cp)
    return (clear_op);

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
    if (p <= (i + 1)*  delta)
      {
	PB_DS_THROW_IF_FAILED(i < max,
			      static_cast<unsigned long>(i) << " " <<
			      static_cast<unsigned long>(max),
			      m_p_c,
			      & m_native_c);
	return i;
      }
    else
      ++i;
}
