// -*- C++ -*-

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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
 * @file verified_cmd_line_input.cpp
 * Contains definitions for tests - verified command line input.
 */

#include <util/io/verified_cmd_line_input.hpp>
#include <limits.h>
#include <utility>
#include <stdlib.h>
#include <bits/functexcept.h>

namespace __gnu_pbds
{
  namespace test
  {
    void
    verify_argc(size_t given, size_t required)
    {
      if (given != required)
	__throw_illegal_input_error();
    }

    void
    verify_prob(double prob)
    {
      if (prob < 0 || prob > 1)
        __throw_illegal_input_error();
    }

    std::string
    get_cmd_line_str(int argc, char* a_p_argv[], int argn)
    {
      if (argc <= argn)
        __throw_illegal_input_error();
      const std::string ret(a_p_argv[argn]);
      return ret;
    }

    double
    get_cmd_line_prob(int argc, char* a_p_argv[], int argn)
    {
      if (argc <= argn)
        __throw_illegal_input_error();
      const double ret = ::atof(a_p_argv[argn]);
      verify_prob(ret);
      return ret;
    }

    size_t
    get_cmd_line_size(int argc, char* a_p_argv[], int argn)
    {
      if (argc <= argn)
        __throw_illegal_input_error();
      const size_t ret = static_cast<size_t>(::atoi(a_p_argv[argn]));
      return ret;
    }

    bool
    get_cmd_line_bool(int argc, char* a_p_argv[], int argn)
    {
      if (argc <= argn)
        __throw_illegal_input_error();

      const std::string opt(a_p_argv[argn]);
      if (opt.size() != 1)
        __throw_illegal_input_error();
      if (opt[0] == 't')
        return true;
      if (opt[0] == 'f')
        return false;
      __throw_illegal_input_error();
      return false;
    }
  } // namespace test
} // namespace __gnu_pbds
