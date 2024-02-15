// Copyright (C) 2023-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU Proc Macro Library.  This library is free
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

#ifndef FFISTRING_H
#define FFISTRING_H

#include <cstdint>
#include <string>

namespace ProcMacro {

struct FFIString
{
  const unsigned char *data;
  std::uint64_t len;

public:
  FFIString clone () const;
  std::string to_string () const;
  static FFIString make_ffistring (const std::string &str);
  static FFIString make_ffistring (const unsigned char *data,
				   std::uint64_t len);
  static void drop (FFIString *str);
};

extern "C" {
FFIString
FFIString__new (const unsigned char *data, std::uint64_t len);

void
FFIString__drop (FFIString *str);
}

} // namespace ProcMacro

#endif /* ! FFISTRING_H */
