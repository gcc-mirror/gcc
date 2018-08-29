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
 * @file verified_cmd_line_input.hpp
 * Contains definitions for tests - verified command line input.
 */

#ifndef PB_DS_VERIFIED_CMD_LINE_INPUT_HPP
#define PB_DS_VERIFIED_CMD_LINE_INPUT_HPP

#include <io/illegal_input_error.hpp>
#include <string>

namespace __gnu_pbds
{
  namespace test
  {
    void
    verify_argc(std::size_t given, std::size_t required);

    void
    verify_prob(double prob);

    std::string
    get_cmd_line_str(int argc, char* a_p_argv[], int argn);

    double
    get_cmd_line_prob(int argc, char* a_p_argv[], int argn);

    std::size_t
    get_cmd_line_size(int argc, char* a_p_argv[], int argn);

    bool
    get_cmd_line_bool(int argc, char* a_p_argv[], int argn);
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_VERIFIED_CMD_LINE_INPUT_HPP
