/* Interface between GCC C FE and GDB

   Copyright (C) 2014-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef GCC_C_INTERFACE_H
#define GCC_C_INTERFACE_H

#include "gcc-interface.h"

/* This header defines the interface to the GCC API.  It must be both
   valid C and valid C++, because it is included by both programs.  */

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declaration.  */

struct gcc_c_context;

/*
 * Definitions and declarations for the C front end.
 */

/* Defined versions of the C front-end API.  */

enum gcc_c_api_version
{
  GCC_C_FE_VERSION_0 = 0,

  /* Added char_type.  Added new version of int_type and float_type,
     deprecated int_type_v0 and float_type_v0.  */
  GCC_C_FE_VERSION_1 = 1
};

/* Qualifiers.  */

enum gcc_qualifiers
{
  GCC_QUALIFIER_CONST = 1,
  GCC_QUALIFIER_VOLATILE = 2,
  GCC_QUALIFIER_RESTRICT = 4
};

/* This enumerates the kinds of decls that GDB can create.  */

enum gcc_c_symbol_kind
{
  /* A function.  */

  GCC_C_SYMBOL_FUNCTION,

  /* A variable.  */

  GCC_C_SYMBOL_VARIABLE,

  /* A typedef.  */

  GCC_C_SYMBOL_TYPEDEF,

  /* A label.  */

  GCC_C_SYMBOL_LABEL
};

/* This enumerates the types of symbols that GCC might request from
   GDB.  */

enum gcc_c_oracle_request
{
  /* An ordinary symbol -- a variable, function, typedef, or enum
     constant.  */

  GCC_C_ORACLE_SYMBOL,

  /* A struct, union, or enum tag.  */

  GCC_C_ORACLE_TAG,

  /* A label.  */

  GCC_C_ORACLE_LABEL
};

/* The type of the function called by GCC to ask GDB for a symbol's
   definition.  DATUM is an arbitrary value supplied when the oracle
   function is registered.  CONTEXT is the GCC context in which the
   request is being made.  REQUEST specifies what sort of symbol is
   being requested, and IDENTIFIER is the name of the symbol.  */

typedef void gcc_c_oracle_function (void *datum,
				    struct gcc_c_context *context,
				    enum gcc_c_oracle_request request,
				    const char *identifier);

/* The type of the function called by GCC to ask GDB for a symbol's
   address.  This should return 0 if the address is not known.  */

typedef gcc_address gcc_c_symbol_address_function (void *datum,
						   struct gcc_c_context *ctxt,
						   const char *identifier);

/* The vtable used by the C front end.  */

struct gcc_c_fe_vtable
{
  /* The version of the C interface.  The value is one of the
     gcc_c_api_version constants.  */

  unsigned int c_version;

  /* Set the callbacks for this context.

     The binding oracle is called whenever the C parser needs to look
     up a symbol.  This gives the caller a chance to lazily
     instantiate symbols using other parts of the gcc_c_fe_interface
     API.

     The address oracle is called whenever the C parser needs to look
     up a symbol.  This is only called for symbols not provided by the
     symbol oracle -- that is, just built-in functions where GCC
     provides the declaration.

     DATUM is an arbitrary piece of data that is passed back verbatim
     to the callbacks in requests.  */

  void (*set_callbacks) (struct gcc_c_context *self,
			 gcc_c_oracle_function *binding_oracle,
			 gcc_c_symbol_address_function *address_oracle,
			 void *datum);

#define GCC_METHOD0(R, N) \
  R (*N) (struct gcc_c_context *);
#define GCC_METHOD1(R, N, A) \
  R (*N) (struct gcc_c_context *, A);
#define GCC_METHOD2(R, N, A, B) \
  R (*N) (struct gcc_c_context *, A, B);
#define GCC_METHOD3(R, N, A, B, C) \
  R (*N) (struct gcc_c_context *, A, B, C);
#define GCC_METHOD4(R, N, A, B, C, D) \
  R (*N) (struct gcc_c_context *, A, B, C, D);
#define GCC_METHOD5(R, N, A, B, C, D, E) \
  R (*N) (struct gcc_c_context *, A, B, C, D, E);
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G) \
  R (*N) (struct gcc_c_context *, A, B, C, D, E, F, G);

#include "gcc-c-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7

};

/* The C front end object.  */

struct gcc_c_context
{
  /* Base class.  */

  struct gcc_base_context base;

  /* Our vtable.  This is a separate field because this is simpler
     than implementing a vtable inheritance scheme in C.  */

  const struct gcc_c_fe_vtable *c_ops;
};

/* The name of the .so that the compiler builds.  We dlopen this
   later.  */

#define GCC_C_FE_LIBCC libcc1.so

/* The compiler exports a single initialization function.  This macro
   holds its name as a symbol.  */

#define GCC_C_FE_CONTEXT gcc_c_fe_context

/* The type of the initialization function.  The caller passes in the
   desired base version and desired C-specific version.  If the
   request can be satisfied, a compatible gcc_context object will be
   returned.  Otherwise, the function returns NULL.  */

typedef struct gcc_c_context *gcc_c_fe_context_function
    (enum gcc_base_api_version,
     enum gcc_c_api_version);

#ifdef __cplusplus
}
#endif

#endif /* GCC_C_INTERFACE_H */
