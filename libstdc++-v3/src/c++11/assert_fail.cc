// Debugging mode support code -*- C++ -*-

// Copyright (C) 2021-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <cstdio>	// for std::fprintf, stderr
#include <cstdlib>	// for std::abort

#ifdef _GLIBCXX_VERBOSE_ASSERT
namespace std
{
  [[__noreturn__]]
  void
  __glibcxx_assert_fail(const char* file, int line,
			const char* function, const char* condition) noexcept
  {
    if (file && function && condition)
      fprintf(stderr, "%s:%d: %s: Assertion '%s' failed.\n",
	      file, line, function, condition);
    else if (function)
      fprintf(stderr, "%s: Undefined behavior detected.\n", function);
    abort();
  }
}
#endif
