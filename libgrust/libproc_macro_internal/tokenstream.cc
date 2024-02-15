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

#include "tokenstream.h"
#include "tokentree.h"
#include "registration.h"

#include <cstring>

namespace ProcMacro {

TokenStream
TokenStream::make_tokenstream (std::vector<TokenTree> vec)
{
  auto stream = make_tokenstream (vec.size ());
  for (auto tt : vec)
    {
      stream.push (tt);
    }
  return stream;
}

TokenStream
TokenStream::make_tokenstream (std::uint64_t capacity)
{
  auto *data = new TokenTree[capacity];
  return {data, 0, capacity};
}

TokenStream
TokenStream::make_tokenstream (std::string &source, bool &has_error)
{
  return __gccrs_proc_macro_ts_from_str_ (source, has_error);
}

void
TokenStream::grow (std::uint64_t delta)
{
  auto new_capacity = capacity + (delta != 0 ? delta : 1);
  auto *new_data = new TokenTree[new_capacity];
  capacity = new_capacity;
  std::memcpy (new_data, data, size * sizeof (TokenTree));
  delete[] data;
  data = new_data;
}

void
TokenStream::push (TokenTree tree)
{
  if (size >= capacity)
    grow (capacity);
  data[size] = tree;
  size++;
}

void
TokenStream::drop (TokenStream *stream)
{
  for (std::uint64_t i = 0; i < stream->size; i++)
    {
      TokenTree::TokenTree::drop (&stream->data[i]);
    }
  delete[] stream->data;
  stream->capacity = 0;
  stream->size = 0;
}

extern "C" TokenStream
TokenStream__new ()
{
  return TokenStream::make_tokenstream ();
}

extern "C" TokenStream
TokenStream__with_capacity (std::uint64_t capacity)
{
  return TokenStream::make_tokenstream (capacity);
}

extern "C" void
TokenSream__push (TokenStream *stream, TokenTree tree)
{
  stream->push (tree);
}

extern "C" bool
TokenStream__from_string (FFIString str, TokenStream *ts)
{
  bool result;
  auto source = str.to_string ();

  *ts = TokenStream::make_tokenstream (source, result);
  return result;
}

extern "C" TokenStream
TokenStream__clone (const TokenStream *ts)
{
  auto *data = new TokenTree[ts->capacity];
  std::memcpy (data, ts->data, ts->size * sizeof (TokenTree));
  return {data, ts->size, ts->capacity};
}

extern "C" void
TokenStream__drop (TokenStream *stream)
{
  TokenStream::drop (stream);
}

} // namespace ProcMacro
