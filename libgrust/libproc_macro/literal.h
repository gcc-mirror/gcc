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

#ifndef LITERAL_H
#define LITERAL_H

#include <cstdint>
#include <string>
#include <vector>

namespace ProcMacro {
namespace Literal {
enum UnsignedTag
{
  UNSIGNED_8,
  UNSIGNED_16,
  UNSIGNED_32,
  UNSIGNED_64,
  UNSIGNED_128
};

struct Payload128
{
  std::uint64_t low;
  std::uint64_t high;
};

union UnsignedPayload
{
  std::uint8_t unsigned8;
  std::uint16_t unsigned16;
  std::uint32_t unsigned32;
  std::uint64_t unsigned64;
  Payload128 unsigned128;
};

struct Unsigned
{
  UnsignedTag tag;
  UnsignedPayload payload;
};

enum SignedTag
{
  SIGNED_8,
  SIGNED_16,
  SIGNED_32,
  SIGNED_64,
  SIGNED_128
};

union SignedPayload
{
  std::int8_t signed8;
  std::int16_t signed16;
  std::int32_t signed32;
  std::int64_t signed64;
};

struct Signed
{
  SignedTag tag;
  SignedPayload payload;
};

enum LiteralTag
{
  STRING,
  BYTE_STRING,
  CHAR,
  UNSIGNED,
  SIGNED,
  USIZE,
  ISIZE,
  FLOAT32,
  FLOAT64
};

struct StringPayload
{
  unsigned char *data;
  std::uint64_t len;
};

struct ByteStringPayload
{
  std::uint8_t *data;
  std::uint64_t size;
};

struct UnsignedSuffixPayload
{
  Unsigned value;
  bool suffix;
};

struct SignedSuffixPayload
{
  Signed value;
  bool suffix;
};

struct UsizePayload
{
  std::uint64_t value;
  bool suffix;
};

struct IsizePayload
{
  std::int64_t value;
  bool suffix;
};

struct Float32Payload
{
  float value;
  bool suffix;
};

struct Float64Payload
{
  double value;
  bool suffix;
};

union LiteralPayload
{
  StringPayload string_payload;
  ByteStringPayload byte_string_payload;
  std::uint32_t char_payload;
  UnsignedSuffixPayload unsigned_payload;
  SignedSuffixPayload signed_payload;
  UsizePayload usize_payload;
  IsizePayload isize_payload;
  Float32Payload float32_payload;
  Float64Payload float64_payload;
};

struct Literal
{
  LiteralTag tag;
  LiteralPayload payload;

public:
  Literal clone () const;

  static Literal make_u8 (std::uint8_t value, bool suffixed = false);
  static Literal make_u16 (std::uint16_t value, bool suffixed = false);
  static Literal make_u32 (std::uint32_t value, bool suffixed = false);
  static Literal make_u64 (std::uint64_t value, bool suffixed = false);

  static Literal make_i8 (std::int8_t value, bool suffixed = false);
  static Literal make_i16 (std::int16_t value, bool suffixed = false);
  static Literal make_i32 (std::int32_t value, bool suffixed = false);
  static Literal make_i64 (std::int64_t value, bool suffixed = false);

  static Literal make_string (const std::string &str);
  static Literal make_string (const unsigned char *str, std::uint64_t len);
  static Literal make_byte_string (const std::vector<std::uint8_t> &vec);
  static Literal make_byte_string (const std::uint8_t *bytes,
				   std::uint64_t len);

  static Literal make_f32 (float value, bool suffixed = false);
  static Literal make_f64 (double value, bool suffixed = false);

  static Literal make_char (std::uint32_t ch);
  static Literal make_usize (std::uint64_t value, bool suffixed = false);
  static Literal make_isize (std::int64_t value, bool suffixed = false);

  static Literal make_unsigned (UnsignedSuffixPayload p);
  static Literal make_signed (SignedSuffixPayload p);

  static void drop (Literal *lit);
};

extern "C" {
void
Literal__drop (Literal *lit);

Literal
Literal__string (const unsigned char *str, std::uint64_t len);

Literal
Literal__byte_string (const std::uint8_t *bytes, std::uint64_t len);

bool
Literal__from_string (const unsigned char *str, std::uint64_t len,
		      Literal *lit);
}
} // namespace Literal
} // namespace ProcMacro

#endif /* ! LITERAL_H */
