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
#include "ident.h"

#include <cstring>

namespace ProcMacro {

extern "C" {

Ident
Ident__new (FFIString str, Span span)
{
  return Ident::make_ident (str, span);
}

Ident
Ident__new_raw (FFIString str, Span span)
{
  return Ident::make_ident (str, span, true);
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
  return {this->is_raw, value.clone (), this->span};
}

Ident
Ident::make_ident (std::string str, Span span, bool raw)
{
  return Ident::make_ident (FFIString::make_ffistring (str), span, raw);
}

Ident
Ident::make_ident (FFIString str, Span span, bool raw)
{
  return {raw, str, span};
}

void
Ident::drop (Ident *ident)
{
  FFIString::drop (&ident->value);
}

} // namespace ProcMacro
