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
 * @file prog_bar.hpp
 * Contains a progress bar - idea taken from boost::timer by Beman Dawes.
 */

#ifndef PB_DS_PROG_BAR_HPP
#define PB_DS_PROG_BAR_HPP

#include <limits.h>
#include <iostream>
#include <string>

namespace pb_ds
{

  namespace test
  {

    /**
     * Progress bar.
     * Simplified from part of boost::timer by Beman Dawes.
     **/
    class prog_bar
    {
    protected:
      enum{num_disp = 40};

    public:
      prog_bar(size_t max, std::ostream& r_os, bool display = true);

      void
      inc();

    private:
      prog_bar(const prog_bar& );

      prog_bar& 
      operator=(const prog_bar& );

    private:
      size_t m_cur;
      const size_t m_max;

      size_t m_cur_disp;

      std::ostream& m_r_os;

      bool m_display;
    };

  } // namespace test

} // namespace pb_ds

#endif // #ifndef PB_DS_PROG_BAR_HPP
