// Copyright (C) 2017-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do run { target c++11 } }

#include <locale>
#include <codecvt>
#include <testsuite_hooks.h>

using std::codecvt_base;
using std::codecvt_mode;
using std::codecvt_utf16;
using std::wstring_convert;
using std::mbstate_t;

constexpr codecvt_mode
operator|(codecvt_mode m1, codecvt_mode m2)
{
  using underlying = std::underlying_type<codecvt_mode>::type;
  return static_cast<codecvt_mode>(static_cast<underlying>(m1) | m2);
}

// Read/write UTF-16 code units from data not correctly aligned for char16_t

void
test01()
{
  mbstate_t st;
  constexpr codecvt_mode m = std::consume_header|std::generate_header;
  codecvt_utf16<char16_t, 0x10FFFF, m> conv;
  const char src[] = "-\xFE\xFF\0\x61\xAB\xCD";
  const char* const src_end = src + 7;

  int len = conv.length(st, src + 1, src_end, 1);
  VERIFY( len == 4 );
  len = conv.length(st, src + 1, src_end, 2);
  VERIFY( len == 6 );

  char16_t dst[2];
  char16_t* const dst_end = dst + 2;
  char16_t* dst_next;
  const char* src_cnext;
  auto res = conv.in(st, src + 1, src_end, src_cnext, dst, dst_end, dst_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( dst[0] == 0x0061 );
  VERIFY( dst[1] == 0xabcd );
  VERIFY( src_cnext == src_end );
  VERIFY( dst_next == dst_end );

  char out[sizeof(src)] = { src[0] };
  char* const out_end = out + 7;
  char* out_next;
  const char16_t* dst_cnext;
  res = conv.out(st, dst, dst_end, dst_cnext, out + 1, out_end, out_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( out_next == out_end );
  VERIFY( dst_cnext == dst_end );
  VERIFY( out[1] == src[1] );
  VERIFY( out[2] == src[2] );
  VERIFY( out[3] == src[3] );
  VERIFY( out[4] == src[4] );
  VERIFY( out[5] == src[5] );
  VERIFY( out[6] == src[6] );

  codecvt_utf16<char16_t, 0x10FFFF, m|std::little_endian> conv_le;

  len = conv_le.length(st, src + 1, src_end, 1);
  VERIFY( len == 4 );
  len = conv_le.length(st, src + 1, src_end, 2);
  VERIFY( len == 6 );

  res = conv_le.in(st, src + 1, src_end, src_cnext, dst, dst_end, dst_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( dst[0] == 0x0061 );
  VERIFY( dst[1] == 0xabcd );
  VERIFY( src_cnext == src_end );
  VERIFY( dst_next == dst_end );

  res = conv_le.out(st, dst, dst_end, dst_cnext, out + 1, out_end, out_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( out_next == out_end );
  VERIFY( dst_cnext == dst_end );
  VERIFY( out[1] == src[2] );
  VERIFY( out[2] == src[1] );
  VERIFY( out[3] == src[4] );
  VERIFY( out[4] == src[3] );
  VERIFY( out[5] == src[6] );
  VERIFY( out[6] == src[5] );
}

void
test02()
{
  mbstate_t st;
  constexpr codecvt_mode m = std::consume_header|std::generate_header;
  codecvt_utf16<char32_t, 0x10FFFF, m> conv;
  const char src[] = "-\xFE\xFF\0\x61\xAB\xCD\xD8\x08\xDF\x45";
  const char* const src_end = src + 11;

  int len = conv.length(st, src + 1, src_end, 1);
  VERIFY( len == 4 );
  len = conv.length(st, src + 1, src_end, 2);
  VERIFY( len == 6 );
  len = conv.length(st, src + 1, src_end, -1ul);
  VERIFY( len == 10 );

  char32_t dst[3];
  char32_t* const dst_end = dst + 3;
  char32_t* dst_next;
  const char* src_cnext;
  auto res = conv.in(st, src + 1, src_end, src_cnext, dst, dst_end, dst_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( dst[0] == 0x0061 );
  VERIFY( dst[1] == 0xabcd );
  VERIFY( dst[2] == 0x012345 );
  VERIFY( src_cnext == src_end );
  VERIFY( dst_next == dst_end );

  char out[sizeof(src)] = { src[0] };
  char* const out_end = out + 11;
  char* out_next;
  const char32_t* dst_cnext;
  res = conv.out(st, dst, dst_end, dst_cnext, out + 1, out_end, out_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( out_next == out_end );
  VERIFY( dst_cnext == dst_end );
  VERIFY( out[1] == src[1] );
  VERIFY( out[2] == src[2] );
  VERIFY( out[3] == src[3] );
  VERIFY( out[4] == src[4] );
  VERIFY( out[5] == src[5] );
  VERIFY( out[6] == src[6] );
  VERIFY( out[7] == src[7] );
  VERIFY( out[8] == src[8] );
  VERIFY( out[9] == src[9] );
  VERIFY( out[10] == src[10] );

  codecvt_utf16<char32_t, 0x10FFFF, m|std::little_endian> conv_le;

  len = conv_le.length(st, src + 1, src_end, 1);
  VERIFY( len == 4 );
  len = conv_le.length(st, src + 1, src_end, 2);
  VERIFY( len == 6 );
  len = conv.length(st, src + 1, src_end, -1ul);
  VERIFY( len == 10 );

  res = conv_le.in(st, src + 1, src_end, src_cnext, dst, dst_end, dst_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( dst[0] == 0x0061 );
  VERIFY( dst[1] == 0xabcd );
  VERIFY( dst[2] == 0x012345 );
  VERIFY( src_cnext == src_end );
  VERIFY( dst_next == dst_end );

  res = conv_le.out(st, dst, dst_end, dst_cnext, out + 1, out_end, out_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( out_next == out_end );
  VERIFY( dst_cnext == dst_end );
  VERIFY( out[1] == src[2] );
  VERIFY( out[2] == src[1] );
  VERIFY( out[3] == src[4] );
  VERIFY( out[4] == src[3] );
  VERIFY( out[5] == src[6] );
  VERIFY( out[6] == src[5] );
  VERIFY( out[7] == src[8] );
  VERIFY( out[8] == src[7] );
  VERIFY( out[9] == src[10] );
  VERIFY( out[10] == src[9] );
}

void
test03()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  mbstate_t st;
  constexpr codecvt_mode m = std::consume_header|std::generate_header;
  codecvt_utf16<wchar_t, 0x10FFFF, m> conv;
  const char src[] = "-\xFE\xFF\0\x61\xAB\xCD\xD8\x08\xDF\x45";
  const size_t in_len = sizeof(wchar_t) == 4 ? 11 : 7;
  const size_t out_len = sizeof(wchar_t) == 4 ? 3 : 2;
  const char* const src_end = src + in_len;

  int len = conv.length(st, src + 1, src_end, 1);
  VERIFY( len == 4 );
  len = conv.length(st, src + 1, src_end, 2);
  VERIFY( len == 6 );
  if (sizeof(wchar_t) == 4)
  {
    len = conv.length(st, src + 1, src_end, -1ul);
    VERIFY( len == 10 );
  }

  wchar_t dst[out_len];
  wchar_t* const dst_end = dst + out_len;
  wchar_t* dst_next;
  const char* src_cnext;
  auto res = conv.in(st, src + 1, src_end, src_cnext, dst, dst_end, dst_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( dst[0] == 0x0061 );
  VERIFY( dst[1] == 0xabcd );
  if (sizeof(wchar_t) == 4)
    VERIFY( dst[2] == 0x012345 );
  VERIFY( src_cnext == src_end );
  VERIFY( dst_next == dst_end );

  char out[sizeof(src)] = { src[0] };
  char* const out_end = out + in_len;
  char* out_next;
  const wchar_t* dst_cnext;
  res = conv.out(st, dst, dst_end, dst_cnext, out + 1, out_end, out_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( out_next == out_end );
  VERIFY( dst_cnext == dst_end );
  VERIFY( out[1] == src[1] );
  VERIFY( out[2] == src[2] );
  VERIFY( out[3] == src[3] );
  VERIFY( out[4] == src[4] );
  VERIFY( out[5] == src[5] );
  VERIFY( out[6] == src[6] );
  if (sizeof(wchar_t) == 4)
  {
    VERIFY( out[7] == src[7] );
    VERIFY( out[8] == src[8] );
    VERIFY( out[9] == src[9] );
    VERIFY( out[10] == src[10] );
  }

  codecvt_utf16<wchar_t, 0x10FFFF, m|std::little_endian> conv_le;

  len = conv_le.length(st, src + 1, src_end, 1);
  VERIFY( len == 4 );
  len = conv_le.length(st, src + 1, src_end, 2);
  VERIFY( len == 6 );
  if (sizeof(wchar_t) == 4)
  {
    len = conv.length(st, src + 1, src_end, -1ul);
    VERIFY( len == 10 );
  }

  res = conv_le.in(st, src + 1, src_end, src_cnext, dst, dst_end, dst_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( dst[0] == 0x0061 );
  VERIFY( dst[1] == 0xabcd );
  if (sizeof(wchar_t) == 4)
    VERIFY( dst[2] == 0x012345 );
  VERIFY( src_cnext == src_end );
  VERIFY( dst_next == dst_end );

  res = conv_le.out(st, dst, dst_end, dst_cnext, out + 1, out_end, out_next);
  VERIFY( res == codecvt_base::ok );
  VERIFY( out_next == out_end );
  VERIFY( dst_cnext == dst_end );
  VERIFY( out[1] == src[2] );
  VERIFY( out[2] == src[1] );
  VERIFY( out[3] == src[4] );
  VERIFY( out[4] == src[3] );
  VERIFY( out[5] == src[6] );
  VERIFY( out[6] == src[5] );
  if (sizeof(wchar_t) == 4)
  {
    VERIFY( out[7] == src[8] );
    VERIFY( out[8] == src[7] );
    VERIFY( out[9] == src[10] );
    VERIFY( out[10] == src[9] );
  }
#endif
}

int
main()
{
  test01();
  test02();
  test03();
}
