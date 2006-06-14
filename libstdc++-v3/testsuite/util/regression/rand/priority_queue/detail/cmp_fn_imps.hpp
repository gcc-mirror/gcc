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
  PB_DS_SET_DESTRUCT_PRINT

    try
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

        const size_t it_size = std::distance(r_c.begin(), r_c.end());

        PB_DS_THROW_IF_FAILED(
			      it_size == size,
			      it_size << " " << size,
			      & r_c,
			      & r_native_c);

        if (!r_c.empty())
	  {
            const std::string native_top = r_native_c.top();
            const std::string top = test_traits::native_value(r_c.top());

            const bool top_smaller = std::less<std::string>()(top, native_top);
            const bool top_larger = std::less<std::string>()(native_top, top);

            if (top_smaller || top_larger)
	      PB_DS_THROW_IF_FAILED(
				    false,
				    top << " " << native_top,
				    & r_c,
				    & r_native_c);
	  }
      }
    catch(...)
      {
        PB_DS_THROW_IF_FAILED(            false, "call-fn: " + r_call_fn, & r_c, & r_native_c);
      }

  PB_DS_CANCEL_DESTRUCT_PRINT
    }

