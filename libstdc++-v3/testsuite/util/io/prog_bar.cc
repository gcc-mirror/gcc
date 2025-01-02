// -*- C++ -*-

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
 * @file prog_bar.cpp
 * Contains a progress bar - idea taken from boost::timer by Beman Dawes.
 */

#include <util/io/prog_bar.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    prog_bar::
    prog_bar(std::size_t max, std::ostream& r_os, bool display/*= true*/) :
      m_cur(0),
      m_max(max),
      m_cur_disp(0),
      m_r_os(r_os),
      m_display(display)
    {
      if (m_display == false)
        return;

      for (std::size_t i = 0; i < num_disp; ++i)
        m_r_os << "-";

      m_r_os << std::endl;
    }

    void
    prog_bar::
    inc()
    {
      ++m_cur;

      if (m_display == false)
        return;

      while (m_cur * num_disp >= m_max * m_cur_disp && m_cur_disp < num_disp)
	{
	  m_r_os << '*';
	  m_r_os.flush();
	  ++m_cur_disp;
	}
    }

  } // namespace test

} // namespace __gnu_pbds
