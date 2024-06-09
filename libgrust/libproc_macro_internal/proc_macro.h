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

#ifndef PROC_MACRO_H
#define PROC_MACRO_H

#include <cstdint>
#include "literal.h"
#include "tokenstream.h"
#include "tokentree.h"
#include "group.h"
#include "punct.h"
#include "ident.h"
#include "registration.h"

namespace ProcMacro {

extern "C" {

using CustomDeriveMacro = TokenStream (*) (TokenStream);
using AttributeMacro = TokenStream (*) (TokenStream, TokenStream);
using BangMacro = TokenStream (*) (TokenStream);

struct CustomDerive
{
  // TODO: UTF-8 function name
  const char *trait_name;
  // TODO: UTF-8 attributes
  const char **attributes;
  std::uint64_t attr_size;
  CustomDeriveMacro macro;
};

struct Attribute
{
  // TODO: UTF-8 function name
  const char *name;
  AttributeMacro macro;
};

struct Bang
{
  const char *name;
  BangMacro macro;
};
}

enum ProcmacroTag
{
  CUSTOM_DERIVE = 0,
  ATTR,
  BANG,
};

union ProcmacroPayload
{
  CustomDerive custom_derive;
  Attribute attribute;
  Bang bang;
};

struct Procmacro
{
  ProcmacroTag tag;
  ProcmacroPayload payload;

public:
  Procmacro make_derive (const char *trait_name, const char **attribute,
			 std::uint64_t size, CustomDeriveMacro macro);
  Procmacro make_attribute (const char *name, AttributeMacro macro);
  Procmacro make_bang (const char *name, BangMacro macro);
};

struct ProcmacroArray
{
  std::uint64_t length;
  Procmacro *macros;
};

extern "C" bool
bridge_is_available ();

} // namespace ProcMacro

#endif /* ! PROC_MACRO_H */
