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

#include "proc_macro.h"
#include "bridge.h"

namespace ProcMacro {

Procmacro
Procmacro::make_derive (const char *trait_name, const char **attributes,
			std::uint64_t size, CustomDeriveMacro macro)
{
  ProcmacroPayload payload;
  payload.custom_derive = {trait_name, attributes, size, macro};
  return {CUSTOM_DERIVE, payload};
}

Procmacro
Procmacro::make_attribute (const char *name, AttributeMacro macro)
{
  ProcmacroPayload payload;
  payload.attribute = {name, macro};
  return {ATTR, payload};
}

Procmacro
Procmacro::make_bang (const char *name, BangMacro macro)
{
  ProcmacroPayload payload;
  payload.bang = {name, macro};
  return {BANG, payload};
}

extern "C" bool
bridge_is_available ()
{
  return __gccrs_proc_macro_is_available_ == ProcMacro::BridgeState::Available;
}

} // namespace ProcMacro

ProcMacro::ts_from_str_fn_t __gccrs_proc_macro_ts_from_str_ = nullptr;
ProcMacro::lit_from_str_fn_t __gccrs_proc_macro_lit_from_str_ = nullptr;
ProcMacro::BridgeState __gccrs_proc_macro_is_available_
  = ProcMacro::BridgeState::Unavailable;
