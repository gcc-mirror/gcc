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

#ifndef LITERAL_H
#define LITERAL_H

#include <cstdint>
#include <string>
#include <vector>
#include "span.h"
#include "ffistring.h"

namespace ProcMacro {

enum LitKindTag
{
  BYTE,
  CHAR,
  INTEGER,
  FLOAT,
  STR,
  STR_RAW,
  BYTE_STR,
  BYTE_STR_RAW,
};

union LitKindPayload
{
  std::uint8_t str_raw;
  std::uint8_t byte_str_raw;
};

struct LitKind
{
  LitKindTag tag;
  LitKindPayload payload;

private:
public:
  static LitKind make_byte ();
  static LitKind make_char ();
  static LitKind make_integer ();
  static LitKind make_float ();
  static LitKind make_str ();
  static LitKind make_str_raw (std::uint8_t val);
  static LitKind make_byte_str ();
  static LitKind make_byte_str_raw (std::uint8_t val);
};

struct Literal
{
  LitKind kind;
  FFIString text;
  FFIString suffix;
  Span span;

public:
  Literal clone () const;
  bool has_suffix () const { return suffix.len != 0; };

  static Literal make_literal (const std::string &text, bool &has_error);
  static Literal make_literal (const LitKind kind, Span span,
			       const std::string &text,
			       const std::string &suffix = "");
  static Literal make_u8 (std::uint8_t value, bool suffixed = true);
  static Literal make_u16 (std::uint16_t value, bool suffixed = true);
  static Literal make_u32 (std::uint32_t value, bool suffixed = true);
  static Literal make_u64 (std::uint64_t value, bool suffixed = true);

  static Literal make_i8 (std::int8_t value, bool suffixed = true);
  static Literal make_i16 (std::int16_t value, bool suffixed = true);
  static Literal make_i32 (std::int32_t value, bool suffixed = true);
  static Literal make_i64 (std::int64_t value, bool suffixed = true);

  static Literal make_string (const std::string &str);
  static Literal make_byte_string (const std::vector<std::uint8_t> &vec);

  static Literal make_f32 (float value, bool suffixed = false);
  static Literal make_f64 (double value, bool suffixed = false);

  static Literal make_char (std::uint32_t ch);
  static Literal make_usize (std::uint64_t value, bool suffixed = true);
  static Literal make_isize (std::int64_t value, bool suffixed = true);

  static void drop (Literal *lit);
};

extern "C" {
bool
Literal__from_string (FFIString str, Literal *lit);
}
} // namespace ProcMacro

#endif /* ! LITERAL_H */
