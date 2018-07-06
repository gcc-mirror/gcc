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
 * @file elapsed_timer.cpp
 * Contains utilities for tests.
 */

#include <util/performance/time/elapsed_timer.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    elapsed_timer::
    elapsed_timer()
    {
      reset();
    }

    void
    elapsed_timer::
    reset()
    {
      m_start = ::clock();
    }

    elapsed_timer::
    operator double() const
    {
      const clock_t now = ::clock();

      return (((double)now - m_start) / CLOCKS_PER_SEC);
    }

  } // namespace test

} // namespace __gnu_pbds
