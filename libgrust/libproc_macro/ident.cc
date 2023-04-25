// Copyright (C) 2023 Free Software Foundation, Inc.
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
#include "ident.h"

#include <cstring>

namespace ProcMacro {

extern "C" {

Ident
Ident__new (unsigned char *str, std::uint64_t len)
{
  return Ident::make_ident (str, len);
}

Ident
Ident__new_raw (unsigned char *str, std::uint64_t len)
{
  return Ident::make_ident (str, len, true);
}

void
Ident__drop (Ident *ident)
{
  Ident::drop (ident);
}

Ident
Ident__clone (const Ident *ident)
{
  return ident->clone ();
}
}

Ident
Ident::clone () const
{
  unsigned char *val = new unsigned char[this->len];
  std::memcpy (val, this->val, this->len);
  return {this->is_raw, val, this->len};
}

Ident
Ident::make_ident (std::string str, bool raw)
{
  return Ident::make_ident (reinterpret_cast<const unsigned char *> (
			      str.c_str ()),
			    str.length (), raw);
}

Ident
Ident::make_ident (const unsigned char *str, std::uint64_t len, bool raw)
{
  unsigned char *val = new unsigned char[len];
  std::memcpy (val, str, len);
  return {raw, val, len};
}

void
Ident::drop (Ident *ident)
{
  delete[] ident->val;
  ident->len = 0;
}

} // namespace ProcMacro
