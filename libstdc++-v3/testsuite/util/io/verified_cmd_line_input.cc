// -*- C++ -*-

// Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
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
 * @file verified_cmd_line_input.cpp
 * Contains definitions for tests - verified command line input.
 */

#include <util/io/verified_cmd_line_input.hpp>
#include <limits.h>
#include <utility>
#include <stdlib.h>
#include <bits/functexcept.h>

namespace pb_ds
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
} // namespace pb_ds
