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

#include "literal.h"
#include <cstring>
#include <cstdlib>

#include "registration.h"

namespace ProcMacro {

extern "C" {
bool
Literal__from_string (FFIString str, Literal *lit)
{
  bool result;
  auto source = str.to_string ();

  *lit = Literal::make_literal (source, result);
  return result;
}
}

void
Literal::drop (Literal *lit)
{
  FFIString::drop (&lit->text);
  FFIString::drop (&lit->suffix);
}

Literal
Literal::clone () const
{
  return {this->kind, this->text.clone (), this->suffix.clone (), this->span};
}

Literal
Literal::make_literal (const std::string &text, bool &has_error)
{
  return __gccrs_proc_macro_lit_from_str_ (text, has_error);
}

Literal
Literal::make_literal (LitKind kind, Span span, const std::string &text,
		       const std::string &suffix)
{
  auto ffi_text = FFIString::make_ffistring (text);
  auto ffi_suffix = FFIString::make_ffistring (suffix);
  return {kind, ffi_text, ffi_suffix, span};
}

Literal
Literal::make_u8 (std::uint8_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "u8" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_u16 (std::uint16_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "u16" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_u32 (std::uint32_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "u32" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_u64 (std::uint64_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "u64" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_i8 (std::int8_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "i8" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_i16 (std::int16_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "i16" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_i32 (std::int32_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "i32" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_i64 (std::int64_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "i64" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_string (const std::string &str)
{
  auto text = FFIString::make_ffistring (str);
  auto suffix = FFIString::make_ffistring ("");
  return {LitKind::make_str (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_byte_string (const std::vector<std::uint8_t> &vec)
{
  auto text
    = FFIString::make_ffistring (std::string (vec.cbegin (), vec.cend ()));
  auto suffix = FFIString::make_ffistring ("");
  return {LitKind::make_byte_str (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_f32 (float value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "f32" : "");
  return {LitKind::make_float (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_f64 (double value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "f64" : "");
  return {LitKind::make_float (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_char (std::uint32_t ch)
{
  auto text = FFIString::make_ffistring (std::to_string ((char) ch));
  auto suffix = FFIString::make_ffistring ("");
  return {LitKind::make_char (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_usize (std::uint64_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "usize" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

Literal
Literal::make_isize (std::int64_t value, bool suffixed)
{
  auto text = FFIString::make_ffistring (std::to_string (value));
  auto suffix = FFIString::make_ffistring (suffixed ? "isize" : "");
  return {LitKind::make_integer (), text, suffix, Span::make_unknown ()};
}

LitKind
LitKind::make_byte ()
{
  LitKindPayload payload;
  return {BYTE, payload};
}

LitKind
LitKind::make_char ()
{
  LitKindPayload payload;
  return {CHAR, payload};
}

LitKind
LitKind::make_integer ()
{
  LitKindPayload payload;
  return {INTEGER, payload};
}

LitKind
LitKind::make_float ()
{
  LitKindPayload payload;
  return {FLOAT, payload};
}

LitKind
LitKind::make_str ()
{
  LitKindPayload payload;
  return {STR, payload};
}

LitKind
LitKind::make_str_raw (std::uint8_t val)
{
  LitKindPayload payload;
  payload.str_raw = val;
  return {STR_RAW, payload};
}

LitKind
LitKind::make_byte_str ()
{
  LitKindPayload payload;
  return {BYTE_STR, payload};
}

LitKind
LitKind::make_byte_str_raw (std::uint8_t val)
{
  LitKindPayload payload;
  payload.byte_str_raw = val;
  return {BYTE_STR_RAW, payload};
}

} // namespace ProcMacro
