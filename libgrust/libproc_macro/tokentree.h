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

#ifndef TOKENTREE_H
#define TOKENTREE_H

#include "group.h"
#include "ident.h"
#include "punct.h"
#include "literal.h"

namespace ProcMacro {
namespace TokenTree {

enum TokenTreeTag
{
  GROUP,
  IDENT,
  PUNCT,
  LITERAL
};

union TokenTreePayload
{
  Group::Group group;
  Ident::Ident ident;
  Punct::Punct punct;
  Literal::Literal literal;
};

struct TokenTree
{
  TokenTreeTag tag;
  TokenTreePayload payload;

public:
  static TokenTree make_tokentree (Group::Group group);
  static TokenTree make_tokentree (Ident::Ident ident);
  static TokenTree make_tokentree (Punct::Punct punct);
  static TokenTree make_tokentree (Literal::Literal literal);

  static void drop (TokenTree *tt);
};

} // namespace TokenTree
} // namespace ProcMacro

#endif /* ! TOKENTREE_H */
