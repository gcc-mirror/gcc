/* The library used by gdb.
   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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
#include <sstream>
#include "rpc.hh"
#include "connection.hh"
#include "names.hh"
#include "callbacks.hh"
#include "gcc-interface.h"
#include "libiberty.h"
#include "xregex.h"
#include "findcomp.hh"
#include "compiler-name.h"

struct libcc1;

class libcc1_connection;

// The C compiler context that we hand back to our caller.
struct libcc1 : public gcc_c_context
{
  libcc1 (const gcc_base_vtable *, const gcc_c_fe_vtable *);
  ~libcc1 ();

  // A convenience function to print something.
  void print (const char *str)
  {
    this->print_function (this->print_datum, str);
  }

  libcc1_connection *connection;

  gcc_c_oracle_function *binding_oracle;
  gcc_c_symbol_address_function *address_oracle;
  void *oracle_datum;

  void (*print_function) (void *datum, const char *message);
  void *print_datum;

  std::vector<std::string> args;
  std::string source_file;
};

// A local subclass of connection that holds a back-pointer to the
// gcc_c_context object that we provide to our caller.
class libcc1_connection : public cc1_plugin::connection
{
public:

  libcc1_connection (int fd, int aux_fd, libcc1 *b)
    : connection (fd, aux_fd),
      back_ptr (b)
  {
  }

  virtual void print (const char *buf)
  {
    back_ptr->print (buf);
  }

  libcc1 *back_ptr;
};

libcc1::libcc1 (const gcc_base_vtable *v,
		const gcc_c_fe_vtable *cv)
  : connection (NULL),
    binding_oracle (NULL),
    address_oracle (NULL),
    oracle_datum (NULL),
    print_function (NULL),
    print_datum (NULL),
    args (),
    source_file ()
{
  base.ops = v;
  c_ops = cv;
}

libcc1::~libcc1 ()
{
  delete connection;
}



// This is a wrapper function that is called by the RPC system and
// that then forwards the call to the library user.  Note that the
// return value is not used; the type cannot be 'void' due to
// limitations in our simple RPC.
int
call_binding_oracle (cc1_plugin::connection *conn,
		     enum gcc_c_oracle_request request,
		     const char *identifier)
{
  libcc1 *self = ((libcc1_connection *) conn)->back_ptr;

  self->binding_oracle (self->oracle_datum, self, request, identifier);
  return 1;
}

// This is a wrapper function that is called by the RPC system and
// that then forwards the call to the library user.
gcc_address
call_symbol_address (cc1_plugin::connection *conn, const char *identifier)
{
  libcc1 *self = ((libcc1_connection *) conn)->back_ptr;

  return self->address_oracle (self->oracle_datum, self, identifier);
}



static void
set_callbacks (struct gcc_c_context *s,
	       gcc_c_oracle_function *binding_oracle,
	       gcc_c_symbol_address_function *address_oracle,
	       void *datum)
{
  libcc1 *self = (libcc1 *) s;

  self->binding_oracle = binding_oracle;
  self->address_oracle = address_oracle;
  self->oracle_datum = datum;
}

// Instances of these rpc<> template functions are installed into the
// "c_vtable".  These functions are parameterized by type and method
// name and forward the call via the connection.

template<typename R, const char *&NAME>
R rpc (struct gcc_c_context *s)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result))
    return 0;
  return result;
}

template<typename R, const char *&NAME, typename A>
R rpc (struct gcc_c_context *s, A arg)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result, arg))
    return 0;
  return result;
}

template<typename R, const char *&NAME, typename A1, typename A2>
R rpc (struct gcc_c_context *s, A1 arg1, A2 arg2)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result, arg1, arg2))
    return 0;
  return result;
}

template<typename R, const char *&NAME, typename A1, typename A2, typename A3>
R rpc (struct gcc_c_context *s, A1 arg1, A2 arg2, A3 arg3)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result, arg1, arg2, arg3))
    return 0;
  return result;
}

template<typename R, const char *&NAME, typename A1, typename A2, typename A3,
	 typename A4>
R rpc (struct gcc_c_context *s, A1 arg1, A2 arg2, A3 arg3, A4 arg4)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result, arg1, arg2, arg3,
			 arg4))
    return 0;
  return result;
}

template<typename R, const char *&NAME, typename A1, typename A2, typename A3,
	 typename A4, typename A5>
R rpc (struct gcc_c_context *s, A1 arg1, A2 arg2, A3 arg3, A4 arg4, A5 arg5)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result, arg1, arg2, arg3,
			 arg4, arg5))
    return 0;
  return result;
}

template<typename R, const char *&NAME, typename A1, typename A2, typename A3,
	 typename A4, typename A5, typename A6, typename A7>
R rpc (struct gcc_c_context *s, A1 arg1, A2 arg2, A3 arg3, A4 arg4, A5 arg5,
       A6 arg6, A7 arg7)
{
  libcc1 *self = (libcc1 *) s;
  R result;

  if (!cc1_plugin::call (self->connection, NAME, &result, arg1, arg2, arg3,
			 arg4, arg5, arg6, arg7))
    return 0;
  return result;
}

static const struct gcc_c_fe_vtable c_vtable =
{
  GCC_C_FE_VERSION_0,
  set_callbacks,

#define GCC_METHOD0(R, N) \
  rpc<R, cc1_plugin::N>,
#define GCC_METHOD1(R, N, A) \
  rpc<R, cc1_plugin::N, A>,
#define GCC_METHOD2(R, N, A, B) \
  rpc<R, cc1_plugin::N, A, B>,
#define GCC_METHOD3(R, N, A, B, C) \
  rpc<R, cc1_plugin::N, A, B, C>,
#define GCC_METHOD4(R, N, A, B, C, D) \
  rpc<R, cc1_plugin::N, A, B, C, D>,
#define GCC_METHOD5(R, N, A, B, C, D, E) \
  rpc<R, cc1_plugin::N, A, B, C, D, E>,
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G) \
  rpc<R, cc1_plugin::N, A, B, C, D, E, F, G>,

#include "gcc-c-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7
};



// Construct an appropriate regexp to match the compiler name.
static std::string
make_regexp (const char *triplet_regexp, const char *compiler)
{
  std::stringstream buf;

  buf << "^" << triplet_regexp << "-";

  // Quote the compiler name in case it has something funny in it.
  for (const char *p = compiler; *p; ++p)
    {
      switch (*p)
	{
	case '.':
	case '^':
	case '$':
	case '*':
	case '+':
	case '?':
	case '(':
	case ')':
	case '[':
	case '{':
	case '\\':
	case '|':
	  buf << '\\';
	  break;
	}
      buf << *p;
    }
  buf << "$";

  return buf.str ();
}

static char *
libcc1_set_arguments (struct gcc_base_context *s,
		      const char *triplet_regexp,
		      int argc, char **argv)
{
  libcc1 *self = (libcc1 *) s;
  regex_t triplet;
  int code;

  std::string rx = make_regexp (triplet_regexp, COMPILER_NAME);
  code = regcomp (&triplet, rx.c_str (), REG_EXTENDED | REG_NOSUB);
  if (code != 0)
    {
      size_t len = regerror (code, &triplet, NULL, 0);
      char err[len];

      regerror (code, &triplet, err, len);

      return concat ("Could not compile regexp \"",
		     rx.c_str (),
		     "\": ",
		     err,
		     (char *) NULL);
    }

  std::string compiler;
  if (!find_compiler (triplet, &compiler))
    {
      regfree (&triplet);
      return concat ("Could not find a compiler matching \"",
		     rx.c_str (),
		     "\"",
		     (char *) NULL);
    }
  regfree (&triplet);

  self->args.push_back (compiler);

  for (int i = 0; i < argc; ++i)
    self->args.push_back (argv[i]);

  return NULL;
}

static void
libcc1_set_source_file (struct gcc_base_context *s,
			const char *file)
{
  libcc1 *self = (libcc1 *) s;

  self->source_file = file;
}

static void
libcc1_set_print_callback (struct gcc_base_context *s,
			   void (*print_function) (void *datum,
						   const char *message),
			   void *datum)
{
  libcc1 *self = (libcc1 *) s;

  self->print_function = print_function;
  self->print_datum = datum;
}

static int
fork_exec (libcc1 *self, char **argv, int spair_fds[2], int stderr_fds[2])
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
	  && ::cc1_plugin::marshall (self->connection, GCC_C_FE_VERSION_0))
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
libcc1_compile (struct gcc_base_context *s,
		const char *filename,
		int verbose)
{
  libcc1 *self = (libcc1 *) s;

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

  self->args.push_back ("-fplugin=libcc1plugin");
  char buf[100];
  if (snprintf (buf, sizeof (buf), "-fplugin-arg-libcc1plugin-fd=%d", fds[1])
      >= (long) sizeof (buf))
    abort ();
  self->args.push_back (buf);

  self->args.push_back (self->source_file);
  self->args.push_back ("-c");
  self->args.push_back ("-o");
  self->args.push_back (filename);
  if (verbose)
    self->args.push_back ("-v");

  self->connection = new libcc1_connection (fds[0], stderr_fds[0], self);

  cc1_plugin::callback_ftype *fun
    = cc1_plugin::callback<int,
			   enum gcc_c_oracle_request,
			   const char *,
			   call_binding_oracle>;
  self->connection->add_callback ("binding_oracle", fun);

  fun = cc1_plugin::callback<gcc_address,
			     const char *,
			     call_symbol_address>;
  self->connection->add_callback ("address_oracle", fun);

  char **argv = new (std::nothrow) char *[self->args.size () + 1];
  if (argv == NULL)
    return 0;

  for (unsigned int i = 0; i < self->args.size (); ++i)
    argv[i] = const_cast<char *> (self->args[i].c_str ());
  argv[self->args.size ()] = NULL;

  return fork_exec (self, argv, fds, stderr_fds);
}

static void
libcc1_destroy (struct gcc_base_context *s)
{
  libcc1 *self = (libcc1 *) s;

  delete self;
}

static const struct gcc_base_vtable vtable =
{
  GCC_FE_VERSION_0,
  libcc1_set_arguments,
  libcc1_set_source_file,
  libcc1_set_print_callback,
  libcc1_compile,
  libcc1_destroy
};

extern "C" gcc_c_fe_context_function gcc_c_fe_context;

#ifdef __GNUC__
#pragma GCC visibility push(default)
#endif

extern "C"
struct gcc_c_context *
gcc_c_fe_context (enum gcc_base_api_version base_version,
		  enum gcc_c_api_version c_version)
{
  if (base_version != GCC_FE_VERSION_0 || c_version != GCC_C_FE_VERSION_0)
    return NULL;

  return new libcc1 (&vtable, &c_vtable);
}
