/* Structures that hang off cpp_identifier, for PCH.
   Copyright (C) 1986-2018 Free Software Foundation, Inc.

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
  cpp_token GTY ((length ("%h.count"))) exp[1];
};

/* The kind of the cpp_macro.  */
enum cpp_macro_kind {
  cmk_macro,	/* An ISO macro (token expansion).  */
  cmk_assert,   /* An assertion.  */
  cmk_traditional,	/* A traditional macro (text expansion).  */
};

/* Each macro definition is recorded in a cpp_macro structure.
   Variadic macros cannot occur with traditional cpp.  */
struct GTY(()) cpp_macro {
  union cpp_parm_u 
  {
    /* Parameters, if any.  If parameter names use extended identifiers,
       the original spelling of those identifiers, not the canonical
       UTF-8 spelling, goes here.  */
    cpp_hashnode ** GTY ((tag ("false"),
			  nested_ptr (union tree_node,
	"%h ? CPP_HASHNODE (GCC_IDENT_TO_HT_IDENT (%h)) : NULL",
	"%h ? HT_IDENT_TO_GCC_IDENT (HT_NODE (%h)) : NULL"),
			  length ("%1.paramc"))) params;

    /* If this is an assertion, the next one in the chain.  */
    cpp_macro *GTY ((tag ("true"))) next;
  } GTY ((desc ("%1.kind == cmk_assert"))) parm;

  /* Definition line number.  */
  source_location line;

  /* Number of tokens in body, or bytes for traditional macros.  */
  /* Do we really need 2^32-1 range here?  */
  unsigned int count;

  /* Number of parameters.  */
  unsigned short paramc;

  /* The kind of this macro (ISO, trad or assert) */
  unsigned kind : 2;

  /* If a function-like macro.  */
  unsigned int fun_like : 1;

  /* If a variadic macro.  */
  unsigned int variadic : 1;

  /* If macro defined in system header.  */
  unsigned int syshdr   : 1;

  /* Nonzero if it has been expanded or had its existence tested.  */
  unsigned int used     : 1;

  /* Indicate whether the tokens include extra CPP_PASTE tokens at the
     end to track invalid redefinitions with consecutive CPP_PASTE
     tokens.  */
  unsigned int extra_tokens : 1;

  /* 9 bits spare (32-bit). 41 on 64-bit target.  */

  union cpp_exp_u
  {
    /* Trailing array of replacement tokens (ISO), or assertion body value.  */
    cpp_token GTY ((tag ("false"), length ("%1.count"))) tokens[1];

    /* Pointer to replacement text (traditional).  See comment at top
       of cpptrad.c for how traditional function-like macros are
       encoded.  */
    const unsigned char *GTY ((tag ("true"))) text;
  } GTY ((desc ("%1.kind == cmk_traditional"))) exp;
};
