// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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
 * @file illegal_input_error.hpp
 * Contains an input exception.
 */

#ifndef PB_DS_ILLEGAL_INPUT_EX_HPP
#define PB_DS_ILLEGAL_INPUT_EX_HPP

#include <exception>

namespace __gnu_pbds
{
  namespace test
  {
    class illegal_input_error : public std::exception
    { };

    // Substitute for concurrence_error object in the case of -fno-exceptions.
    inline void
    __throw_illegal_input_error()
    { _GLIBCXX_THROW_OR_ABORT(illegal_input_error()); }
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_ILLEGAL_INPUT_EX_HPP
