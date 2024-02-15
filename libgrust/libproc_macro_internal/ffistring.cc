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

#include <cstring>
#include "ffistring.h"

namespace ProcMacro {
void
FFIString::drop (FFIString *str)
{
  delete[] str->data;
  str->len = 0;
}

FFIString
FFIString::make_ffistring (const std::string &str)
{
  return make_ffistring (reinterpret_cast<const unsigned char *> (str.c_str ()),
			 str.length ());
}

FFIString
FFIString::make_ffistring (const unsigned char *data, std::uint64_t len)
{
  unsigned char *inner = new unsigned char[len];
  // FIXME: UTF-8 Update this with sizeof codepoint instead
  std::memcpy (inner, data, len * sizeof (unsigned char));
  return {inner, len};
}

FFIString
FFIString::clone () const
{
  unsigned char *inner = new unsigned char[this->len];
  // FIXME: UTF-8 Update this with sizeof codepoint instead
  std::memcpy (inner, this->data, this->len * sizeof (unsigned char));
  return {inner, this->len};
}

std::string
FFIString::to_string () const
{
  return std::string (reinterpret_cast<const char *> (this->data), this->len);
}

} // namespace ProcMacro
