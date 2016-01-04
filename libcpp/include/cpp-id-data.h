/* Structures that hang off cpp_identifier, for PCH.
   Copyright (C) 1986-2016 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "cpplib.h"

#if !defined (HAVE_UCHAR) && !defined (IN_GCC)
typedef unsigned char uchar;
#endif

#define UC (const unsigned char *)  /* Intended use: UC"string" */

/* Chained list of answers to an assertion.  */
struct GTY(()) answer {
  struct answer *next;
  unsigned int count;
  cpp_token GTY ((length ("%h.count"))) first[1];
};

/* Each macro definition is recorded in a cpp_macro structure.
   Variadic macros cannot occur with traditional cpp.  */
struct GTY(()) cpp_macro {
  /* Parameters, if any.  If parameter names use extended identifiers,
     the original spelling of those identifiers, not the canonical
     UTF-8 spelling, goes here.  */
  cpp_hashnode ** GTY ((nested_ptr (union tree_node,
		"%h ? CPP_HASHNODE (GCC_IDENT_TO_HT_IDENT (%h)) : NULL",
			"%h ? HT_IDENT_TO_GCC_IDENT (HT_NODE (%h)) : NULL"),
			length ("%h.paramc")))
    params;

  /* Replacement tokens (ISO) or replacement text (traditional).  See
     comment at top of cpptrad.c for how traditional function-like
     macros are encoded.  */
  union cpp_macro_u
  {
    cpp_token * GTY ((tag ("0"), length ("%0.count"))) tokens;
    const unsigned char * GTY ((tag ("1"))) text;
  } GTY ((desc ("%1.traditional"))) exp;

  /* Definition line number.  */
  source_location line;

  /* Number of tokens in expansion, or bytes for traditional macros.  */
  unsigned int count;

  /* Number of parameters.  */
  unsigned short paramc;

  /* If a function-like macro.  */
  unsigned int fun_like : 1;

  /* If a variadic macro.  */
  unsigned int variadic : 1;

  /* If macro defined in system header.  */
  unsigned int syshdr   : 1;

  /* Nonzero if it has been expanded or had its existence tested.  */
  unsigned int used     : 1;

  /* Indicate which field of 'exp' is in use.  */
  unsigned int traditional : 1;

  /* Indicate whether the tokens include extra CPP_PASTE tokens at the
     end to track invalid redefinitions with consecutive CPP_PASTE
     tokens.  */
  unsigned int extra_tokens : 1;
};
