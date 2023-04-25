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

#include "literal.h"
#include <cstring>
#include <cstdlib>

namespace ProcMacro {
namespace Literal {

void
Literal::drop (Literal *lit)
{
  switch (lit->tag)
    {
    case STRING:
      delete[] lit->payload.string_payload.data;
      lit->payload.string_payload.len = 0;
      break;
    case BYTE_STRING:
      delete[] lit->payload.byte_string_payload.data;
      lit->payload.byte_string_payload.size = 0;
      break;
    case CHAR:
    case UNSIGNED:
    case SIGNED:
    case USIZE:
    case ISIZE:
    case FLOAT32:
    case FLOAT64:
      break;
    }
}

extern "C" {

void
Literal__drop (Literal *lit)
{
  Literal::drop (lit);
}

Literal
Literal__string (const unsigned char *str, std::uint64_t len)
{
  return Literal::make_string (str, len);
}

Literal
Literal__byte_string (const std::uint8_t *bytes, std::uint64_t len)
{
  return Literal::make_byte_string (bytes, len);
}

bool
Literal__from_string (const unsigned char *str, std::uint64_t len, Literal *lit)
{
  // FIXME: implement this function with parser
  std::abort ();
  return false;
}
}

Literal
Literal::make_unsigned (UnsignedSuffixPayload p)
{
  LiteralPayload payload;
  payload.unsigned_payload = p;
  return {UNSIGNED, payload};
}

Literal
Literal::make_signed (SignedSuffixPayload p)
{
  LiteralPayload payload;
  payload.signed_payload = p;
  return {SIGNED, payload};
}

Literal
Literal::clone () const
{
  Literal lit = *this;
  switch (this->tag)
    {
    case STRING:
      lit.payload.string_payload.data
	= new unsigned char[lit.payload.string_payload.len];
      std::memcpy (lit.payload.string_payload.data,
		   this->payload.string_payload.data,
		   lit.payload.string_payload.len);
      break;
    case BYTE_STRING:
      lit.payload.byte_string_payload.data
	= new uint8_t[lit.payload.byte_string_payload.size];
      std::memcpy (lit.payload.byte_string_payload.data,
		   this->payload.byte_string_payload.data,
		   lit.payload.byte_string_payload.size);
      break;
    default:
      break;
    }
  return lit;
}

Literal
Literal::make_u8 (std::uint8_t value, bool suffixed)
{
  UnsignedPayload unsigned_payload;
  unsigned_payload.unsigned8 = value;
  Unsigned val{UNSIGNED_8, unsigned_payload};
  UnsignedSuffixPayload payload{val, suffixed};

  return make_unsigned (payload);
}

Literal
Literal::make_u16 (std::uint16_t value, bool suffixed)
{
  UnsignedPayload unsigned_payload;
  unsigned_payload.unsigned16 = value;
  Unsigned val{UNSIGNED_16, unsigned_payload};
  UnsignedSuffixPayload payload{val, suffixed};

  return make_unsigned (payload);
}

Literal
Literal::make_u32 (std::uint32_t value, bool suffixed)
{
  UnsignedPayload unsigned_payload;
  unsigned_payload.unsigned32 = value;
  Unsigned val{UNSIGNED_32, unsigned_payload};
  UnsignedSuffixPayload payload{val, suffixed};

  return make_unsigned (payload);
}

Literal
Literal::make_u64 (std::uint64_t value, bool suffixed)
{
  UnsignedPayload unsigned_payload;
  unsigned_payload.unsigned64 = value;
  Unsigned val{UNSIGNED_64, unsigned_payload};
  UnsignedSuffixPayload payload{val, suffixed};

  return make_unsigned (payload);
}

Literal
Literal::make_i8 (std::int8_t value, bool suffixed)
{
  SignedPayload signed_payload;
  signed_payload.signed8 = value;
  Signed val{SIGNED_8, signed_payload};
  SignedSuffixPayload payload{val, suffixed};

  return make_signed (payload);
}

Literal
Literal::make_i16 (std::int16_t value, bool suffixed)
{
  SignedPayload signed_payload;
  signed_payload.signed16 = value;
  Signed val{SIGNED_16, signed_payload};
  SignedSuffixPayload payload{val, suffixed};

  return make_signed (payload);
}

Literal
Literal::make_i32 (std::int32_t value, bool suffixed)
{
  SignedPayload signed_payload;
  signed_payload.signed32 = value;
  Signed val{SIGNED_32, signed_payload};
  SignedSuffixPayload payload = {val, suffixed};

  return make_signed (payload);
}

Literal
Literal::make_i64 (std::int64_t value, bool suffixed)
{
  SignedPayload signed_payload;
  signed_payload.signed64 = value;
  Signed val{SIGNED_64, signed_payload};
  SignedSuffixPayload payload{val, suffixed};

  return make_signed (payload);
}

Literal
Literal::make_string (const std::string &str)
{
  return make_string (reinterpret_cast<const unsigned char *> (str.c_str ()),
		      str.length ());
}

Literal
Literal::make_string (const unsigned char *str, std::uint64_t len)
{
  unsigned char *data = new unsigned char[len];
  StringPayload str_payload = {data, len};
  std::memcpy (data, str, len);
  LiteralPayload payload;
  payload.string_payload = str_payload;
  return {STRING, payload};
}

Literal
Literal::make_byte_string (const std::vector<std::uint8_t> &vec)
{
  return make_byte_string (vec.data (), vec.size ());
}

Literal
Literal::make_byte_string (const std::uint8_t *bytes, std::uint64_t len)
{
  std::uint8_t *data = new std::uint8_t[len];
  ByteStringPayload bstr_payload = {data, len};
  std::memcpy (data, bytes, len);
  LiteralPayload payload;
  payload.byte_string_payload = bstr_payload;
  return {BYTE_STRING, payload};
}

Literal
Literal::make_f32 (float value, bool suffixed)
{
  Float32Payload f{value, suffixed};
  LiteralPayload payload;
  payload.float32_payload = f;
  return {FLOAT32, payload};
}

Literal
Literal::make_f64 (double value, bool suffixed)
{
  Float64Payload f{value, suffixed};
  LiteralPayload payload;
  payload.float64_payload = f;
  return {FLOAT64, payload};
}

Literal
Literal::make_char (std::uint32_t ch)
{
  LiteralPayload payload;
  payload.char_payload = ch;
  return {CHAR, payload};
}

Literal
Literal::make_usize (std::uint64_t value, bool suffixed)
{
  UsizePayload p{value, suffixed};
  LiteralPayload payload;
  payload.usize_payload = p;
  return {USIZE, payload};
}

Literal
Literal::make_isize (std::int64_t value, bool suffixed)
{
  IsizePayload p{value, suffixed};
  LiteralPayload payload;
  payload.isize_payload = p;
  return {ISIZE, payload};
}

} // namespace Literal
} // namespace ProcMacro
