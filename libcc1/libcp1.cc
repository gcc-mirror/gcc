/* The library used by gdb.
   Copyright (C) 2014-2021 Free Software Foundation, Inc.

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
  libcp1 (const gcc_base_vtable *, const gcc_cp_fe_vtable *);

  gcc_cp_oracle_function *binding_oracle = nullptr;
  gcc_cp_symbol_address_function *address_oracle = nullptr;
  gcc_cp_enter_leave_user_expr_scope_function *enter_scope = nullptr;
  gcc_cp_enter_leave_user_expr_scope_function *leave_scope = nullptr;
  void *oracle_datum = nullptr;
};

libcp1::libcp1 (const gcc_base_vtable *v, const gcc_cp_fe_vtable *cv)
  : cc1_plugin::base_gdb_plugin<gcc_cp_context> (v)
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



static void
libcp1_set_verbose (struct gcc_base_context *s, int /* bool */ verbose)
{
  libcp1 *self = (libcp1 *) s;

  self->set_verbose (verbose != 0);
}

static char *
libcp1_set_arguments (struct gcc_base_context *s,
		      int argc, char **argv)
{
  libcp1 *self = (libcp1 *) s;

  std::string compiler;
  char *errmsg = self->compilerp->find (CP_COMPILER_NAME, compiler);
  if (errmsg != NULL)
    return errmsg;

  self->args.push_back (compiler);

  for (int i = 0; i < argc; ++i)
    self->args.push_back (argv[i]);

  return NULL;
}

static char *
libcp1_set_triplet_regexp (struct gcc_base_context *s,
			   const char *triplet_regexp)
{
  libcp1 *self = (libcp1 *) s;

  self->compilerp.reset
    (new cc1_plugin::compiler_triplet_regexp (self->verbose,
					      triplet_regexp));
  return NULL;
}

static char *
libcp1_set_driver_filename (struct gcc_base_context *s,
			    const char *driver_filename)
{
  libcp1 *self = (libcp1 *) s;

  self->compilerp.reset
    (new cc1_plugin::compiler_driver_filename (self->verbose,
					       driver_filename));
  return NULL;
}

static char *
libcp1_set_arguments_v0 (struct gcc_base_context *s,
			 const char *triplet_regexp,
			 int argc, char **argv)
{
  char *errmsg = libcp1_set_triplet_regexp (s, triplet_regexp);
  if (errmsg != NULL)
    return errmsg;

  return libcp1_set_arguments (s, argc, argv);
}

static void
libcp1_set_source_file (struct gcc_base_context *s,
			 const char *file)
{
  libcp1 *self = (libcp1 *) s;

  self->source_file = file;
}

static void
libcp1_set_print_callback (struct gcc_base_context *s,
			    void (*print_function) (void *datum,
						    const char *message),
			    void *datum)
{
  libcp1 *self = (libcp1 *) s;

  self->print_function = print_function;
  self->print_datum = datum;
}

static int
fork_exec (libcp1 *self, char **argv, int spair_fds[2], int stderr_fds[2])
{
  pid_t child_pid = fork ();

  if (child_pid == -1)
    {
      close (spair_fds[0]);
      close (spair_fds[1]);
      close (stderr_fds[0]);
      close (stderr_fds[1]);
      return 0;
    }

  if (child_pid == 0)
    {
      // Child.
      dup2 (stderr_fds[1], 1);
      dup2 (stderr_fds[1], 2);
      close (stderr_fds[0]);
      close (stderr_fds[1]);
      close (spair_fds[0]);

      execvp (argv[0], argv);
      _exit (127);
    }
  else
    {
      // Parent.
      close (spair_fds[1]);
      close (stderr_fds[1]);

      cc1_plugin::status result = cc1_plugin::FAIL;
      if (self->connection->send ('H')
	  && ::cc1_plugin::marshall (self->connection.get (),
				     GCC_CP_FE_VERSION_0))
	result = self->connection->wait_for_query ();

      close (spair_fds[0]);
      close (stderr_fds[0]);

      while (true)
	{
	  int status;

	  if (waitpid (child_pid, &status, 0) == -1)
	    {
	      if (errno != EINTR)
		return 0;
	    }

	  if (!WIFEXITED (status) || WEXITSTATUS (status) != 0)
	    return 0;
	  break;
	}

      if (!result)
	return 0;
      return 1;
    }
}

static int
libcp1_compile (struct gcc_base_context *s,
		const char *filename)
{
  libcp1 *self = (libcp1 *) s;

  int fds[2];
  if (socketpair (AF_UNIX, SOCK_STREAM, 0, fds) != 0)
    {
      self->print ("could not create socketpair\n");
      return 0;
    }

  int stderr_fds[2];
  if (pipe (stderr_fds) != 0)
    {
      self->print ("could not create pipe\n");
      close (fds[0]);
      close (fds[1]);
      return 0;
    }

  self->args.push_back ("-fplugin=libcp1plugin");
  char buf[100];
  if (snprintf (buf, sizeof (buf), "-fplugin-arg-libcp1plugin-fd=%d", fds[1])
      >= (long) sizeof (buf))
    abort ();
  self->args.push_back (buf);

  self->args.push_back (self->source_file);
  self->args.push_back ("-c");
  self->args.push_back ("-o");
  self->args.push_back (filename);
  if (self->verbose)
    self->args.push_back ("-v");

  self->set_connection (fds[0], stderr_fds[0]);

  cc1_plugin::callback_ftype *fun
    = cc1_plugin::callback<int,
			   enum gcc_cp_oracle_request,
			   const char *,
			   cp_call_binding_oracle>;
  self->connection->add_callback ("binding_oracle", fun);

  fun = cc1_plugin::callback<gcc_address,
			     const char *,
			     cp_call_symbol_address>;
  self->connection->add_callback ("address_oracle", fun);

  fun = cc1_plugin::callback<int,
			     cp_call_enter_scope>;
  self->connection->add_callback ("enter_scope", fun);

  fun = cc1_plugin::callback<int,
			     cp_call_leave_scope>;
  self->connection->add_callback ("leave_scope", fun);

  char **argv = new (std::nothrow) char *[self->args.size () + 1];
  if (argv == NULL)
    return 0;

  for (unsigned int i = 0; i < self->args.size (); ++i)
    argv[i] = const_cast<char *> (self->args[i].c_str ());
  argv[self->args.size ()] = NULL;

  return fork_exec (self, argv, fds, stderr_fds);
}

static int
libcp1_compile_v0 (struct gcc_base_context *s, const char *filename,
		   int verbose)
{
  libcp1_set_verbose (s, verbose);
  return libcp1_compile (s, filename);
}

static void
libcp1_destroy (struct gcc_base_context *s)
{
  libcp1 *self = (libcp1 *) s;

  delete self;
}

static const struct gcc_base_vtable vtable =
{
  GCC_FE_VERSION_1,
  libcp1_set_arguments_v0,
  libcp1_set_source_file,
  libcp1_set_print_callback,
  libcp1_compile_v0,
  libcp1_destroy,
  libcp1_set_verbose,
  libcp1_compile,
  libcp1_set_arguments,
  libcp1_set_triplet_regexp,
  libcp1_set_driver_filename,
};

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

  return new libcp1 (&vtable, &cp_vtable);
}
