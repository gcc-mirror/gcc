/* The library used by gdb.
   Copyright (C) 2014-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <cc1plugin-config.h>
#include <vector>
#include <string>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <stdlib.h>
#include "marshall-cp.hh"
#include "rpc.hh"
#include "connection.hh"
#include "names.hh"
#include "callbacks.hh"
#include "libiberty.h"
#include "compiler-name.hh"
#include "compiler.hh"
#include "gdbctx.hh"

// The C compiler context that we hand back to our caller.
struct libcp1 : public cc1_plugin::base_gdb_plugin<gcc_cp_context>
{
  explicit libcp1 (const gcc_cp_fe_vtable *);

  void add_callbacks () override;

  gcc_cp_oracle_function *binding_oracle = nullptr;
  gcc_cp_symbol_address_function *address_oracle = nullptr;
  gcc_cp_enter_leave_user_expr_scope_function *enter_scope = nullptr;
  gcc_cp_enter_leave_user_expr_scope_function *leave_scope = nullptr;
  void *oracle_datum = nullptr;
};

libcp1::libcp1 (const gcc_cp_fe_vtable *cv)
  : cc1_plugin::base_gdb_plugin<gcc_cp_context> ("libcp1plugin",
						 CP_COMPILER_NAME,
						 GCC_CP_FE_VERSION_0)
{
  cp_ops = cv;
}



// Enclose these functions in an anonymous namespace because they
// shouldn't be exported, but they can't be static because they're
// used as template arguments.
namespace {
  // This is a wrapper function that is called by the RPC system and
  // that then forwards the call to the library user.  Note that the
  // return value is not used; the type cannot be 'void' due to
  // limitations in our simple RPC.
  int
  cp_call_binding_oracle (cc1_plugin::connection *conn,
		       enum gcc_cp_oracle_request request,
		       const char *identifier)
  {
    libcp1 *self = (libcp1 *) (((libcp1::local_connection *) conn)->back_ptr);

    self->binding_oracle (self->oracle_datum, self, request, identifier);
    return 1;
  }

  // This is a wrapper function that is called by the RPC system and
  // that then forwards the call to the library user.
  gcc_address
  cp_call_symbol_address (cc1_plugin::connection *conn, const char *identifier)
  {
    libcp1 *self = (libcp1 *) (((libcp1::local_connection *) conn)->back_ptr);

    return self->address_oracle (self->oracle_datum, self, identifier);
  }

  int
  cp_call_enter_scope (cc1_plugin::connection *conn)
  {
    libcp1 *self = (libcp1 *) (((libcp1::local_connection *) conn)->back_ptr);

    self->enter_scope (self->oracle_datum, self);
    return 1;
  }

  int
  cp_call_leave_scope (cc1_plugin::connection *conn)
  {
    libcp1 *self = (libcp1 *) (((libcp1::local_connection *) conn)->back_ptr);

    self->leave_scope (self->oracle_datum, self);
    return 1;
  }
} /* anonymous namespace */



static void
set_callbacks (struct gcc_cp_context *s,
	       gcc_cp_oracle_function *binding_oracle,
	       gcc_cp_symbol_address_function *address_oracle,
	       gcc_cp_enter_leave_user_expr_scope_function *enter_scope,
	       gcc_cp_enter_leave_user_expr_scope_function *leave_scope,
	       void *datum)
{
  libcp1 *self = (libcp1 *) s;

  self->binding_oracle = binding_oracle;
  self->address_oracle = address_oracle;
  self->enter_scope = enter_scope;
  self->leave_scope = leave_scope;
  self->oracle_datum = datum;
}

static const struct gcc_cp_fe_vtable cp_vtable =
{
  GCC_CP_FE_VERSION_0,
  set_callbacks,

#define GCC_METHOD0(R, N) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N>,
#define GCC_METHOD1(R, N, A) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N, A>,
#define GCC_METHOD2(R, N, A, B) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N, A, B>,
#define GCC_METHOD3(R, N, A, B, C) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N, A, B, C>,
#define GCC_METHOD4(R, N, A, B, C, D) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N, A, B, C, D>,
#define GCC_METHOD5(R, N, A, B, C, D, E) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N, A, B, C, D, E>,
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G) \
  cc1_plugin::rpc<gcc_cp_context, R, cc1_plugin::cp::N, A, B, C, D, E, F, G>,

#include "gcc-cp-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7
};



void
libcp1::add_callbacks ()
{
  cc1_plugin::callback_ftype *fun
    = cc1_plugin::invoker<int, enum gcc_cp_oracle_request,
			  const char *>::invoke<cp_call_binding_oracle>;
  connection->add_callback ("binding_oracle", fun);

  fun = cc1_plugin::invoker<gcc_address,
			    const char *>::invoke<cp_call_symbol_address>;
  connection->add_callback ("address_oracle", fun);

  fun = cc1_plugin::invoker<int>::invoke<cp_call_enter_scope>;
  connection->add_callback ("enter_scope", fun);

  fun = cc1_plugin::invoker<int>::invoke<cp_call_leave_scope>;
  connection->add_callback ("leave_scope", fun);
}

extern "C" gcc_cp_fe_context_function gcc_cp_fe_context;

#ifdef __GNUC__
#pragma GCC visibility push(default)
#endif

extern "C"
struct gcc_cp_context *
gcc_cp_fe_context (enum gcc_base_api_version base_version,
		    enum gcc_cp_api_version cp_version)
{
  if ((base_version != GCC_FE_VERSION_0 && base_version != GCC_FE_VERSION_1)
      || cp_version != GCC_CP_FE_VERSION_0)
    return NULL;

  return new libcp1 (&cp_vtable);
}
