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

extern "C" {

void
Literal__drop (Literal *lit)
{
  switch (lit->tag)
    {
    case STRING:
      delete lit->payload.string_payload.data;
      lit->payload.string_payload.len = 0;
      break;
    case BYTE_STRING:
      delete lit->payload.byte_string_payload.data;
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

Literal
Literal__string (const unsigned char *str, std::uint64_t len)
{
  unsigned char *data = new unsigned char[len];
  StringPayload str_payload = {data, len};
  std::memcpy (data, str, len);
  LiteralPayload payload;
  payload.string_payload = str_payload;
  return {STRING, payload};
}

Literal
Literal__byte_string (const std::uint8_t *bytes, std::uint64_t len)
{
  std::uint8_t *data = new std::uint8_t[len];
  ByteStringPayload bstr_payload = {data, len};
  std::memcpy (data, bytes, len);
  LiteralPayload payload;
  payload.byte_string_payload = bstr_payload;
  return {BYTE_STRING, payload};
}

bool
Literal__from_string (const unsigned char *str, std::uint64_t len, Literal *lit)
{
  // FIXME: implement this function with parser
  return false;
}
}
