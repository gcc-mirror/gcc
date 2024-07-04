/* Generic GDB-side plugin
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_GDBCTX_HH
#define CC1_PLUGIN_GDBCTX_HH

namespace cc1_plugin
{
  // The compiler context that we hand back to our caller.
  // Due to this, the entire implementation is in this header.
  template<typename T>
  struct base_gdb_plugin : public T
  {
    base_gdb_plugin (const char *plugin_name_, const char *base_name,
		     int version)
      : verbose (false),
	plugin_name (plugin_name_),
	fe_version (version),
	compiler_name (base_name),
	compilerp (new compiler (verbose))
    {
      vtable =
	{
	  GCC_FE_VERSION_1,
	  do_set_arguments_v0,
	  do_set_source_file,
	  do_set_print_callback,
	  do_compile_v0,
	  do_destroy,
	  do_set_verbose,
	  do_compile,
	  do_set_arguments,
	  do_set_triplet_regexp,
	  do_set_driver_filename,
	};

      this->base.ops = &vtable;
    }

    virtual ~base_gdb_plugin () = default;

    // A convenience function to print something.
    void print (const char *str)
    {
      this->print_function (this->print_datum, str);
    }

    // Set the verbose flag.
    void set_verbose (bool v)
    {
      verbose = v;
      if (compilerp != nullptr)
	compilerp->set_verbose (v);
    }

    // Make a new connection.
    void set_connection (int fd, int aux_fd)
    {
      connection.reset (new local_connection (fd, aux_fd, this));
    }

    // This is called just before compilation begins.  It should set
    // any needed callbacks on the connection.
    virtual void add_callbacks () = 0;

    // A local subclass of connection that holds a back-pointer to the
    // context object that we provide to our caller.
    class local_connection : public cc1_plugin::connection
    {
    public:

      local_connection (int fd, int aux_fd, base_gdb_plugin<T> *b)
	: connection (fd, aux_fd),
	  back_ptr (b)
      {
      }

      void print (const char *buf) override
      {
	back_ptr->print (buf);
      }

      base_gdb_plugin<T> *back_ptr;
    };

    std::unique_ptr<local_connection> connection;

    void (*print_function) (void *datum, const char *message) = nullptr;
    void *print_datum = nullptr;

    std::vector<std::string> args;
    std::string source_file;

    /* Non-zero as an equivalent to gcc driver option "-v".  */
    bool verbose;

    const char *plugin_name;
    int fe_version;

    const char *compiler_name;
    std::unique_ptr<cc1_plugin::compiler> compilerp;

  private:

    struct gcc_base_vtable vtable;

    static inline base_gdb_plugin<T> *
    get_self (gcc_base_context *s)
    {
      T *sub = (T *) s;
      return static_cast<base_gdb_plugin<T> *> (sub);
    }

    static void
    do_set_verbose (struct gcc_base_context *s, int /* bool */ verbose)
    {
      base_gdb_plugin<T> *self = get_self (s);

      self->set_verbose (verbose != 0);
    }

    static char *
    do_set_arguments (struct gcc_base_context *s,
		      int argc, char **argv)
    {
      base_gdb_plugin<T> *self = get_self (s);

      std::string compiler;
      char *errmsg = self->compilerp->find (self->compiler_name, compiler);
      if (errmsg != NULL)
	return errmsg;

      self->args.push_back (compiler);

      for (int i = 0; i < argc; ++i)
	self->args.push_back (argv[i]);

      return NULL;
    }

    static char *
    do_set_triplet_regexp (struct gcc_base_context *s,
			   const char *triplet_regexp)
    {
      base_gdb_plugin<T> *self = get_self (s);

      self->compilerp.reset
	(new cc1_plugin::compiler_triplet_regexp (self->verbose,
						  triplet_regexp));
      return NULL;
    }

    static char *
    do_set_driver_filename (struct gcc_base_context *s,
			    const char *driver_filename)
    {
      base_gdb_plugin<T> *self = get_self (s);

      self->compilerp.reset
	(new cc1_plugin::compiler_driver_filename (self->verbose,
						   driver_filename));
      return NULL;
    }

    static char *
    do_set_arguments_v0 (struct gcc_base_context *s,
			 const char *triplet_regexp,
			 int argc, char **argv)
    {
      char *errmsg = do_set_triplet_regexp (s, triplet_regexp);
      if (errmsg != NULL)
	return errmsg;

      return do_set_arguments (s, argc, argv);
    }

    static void
    do_set_source_file (struct gcc_base_context *s,
			const char *file)
    {
      base_gdb_plugin<T> *self = get_self (s);

      self->source_file = file;
    }

    static void
    do_set_print_callback (struct gcc_base_context *s,
			   void (*print_function) (void *datum,
						   const char *message),
			   void *datum)
    {
      base_gdb_plugin<T> *self = get_self (s);

      self->print_function = print_function;
      self->print_datum = datum;
    }

    int fork_exec (char **argv, int spair_fds[2], int stderr_fds[2])
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
	  if (connection->send ('H')
	      && ::cc1_plugin::marshall (connection.get (), fe_version))
	    result = connection->wait_for_query ();

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
    do_compile (struct gcc_base_context *s,
		const char *filename)
    {
      base_gdb_plugin<T> *self = get_self (s);

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

      self->args.push_back (std::string ("-fplugin=") + self->plugin_name);
      self->args.push_back (std::string ("-fplugin-arg-") + self->plugin_name
			    + "-fd=" + std::to_string (fds[1]));

      self->args.push_back (self->source_file);
      self->args.push_back ("-c");
      self->args.push_back ("-o");
      self->args.push_back (filename);
      if (self->verbose)
	self->args.push_back ("-v");

      self->set_connection (fds[0], stderr_fds[0]);

      self->add_callbacks ();

      std::vector<char *> argv (self->args.size () + 1);
      for (unsigned int i = 0; i < self->args.size (); ++i)
	argv[i] = const_cast<char *> (self->args[i].c_str ());

      return self->fork_exec (argv.data (), fds, stderr_fds);
    }

    static int
    do_compile_v0 (struct gcc_base_context *s, const char *filename,
		   int verbose)
    {
      do_set_verbose (s, verbose);
      return do_compile (s, filename);
    }

    static void
    do_destroy (struct gcc_base_context *s)
    {
      base_gdb_plugin<T> *self = get_self (s);

      delete self;
    }
  };

  // Instances of this rpc<> template function are installed into the
  // "vtable"s.  These functions are parameterized by type and method
  // name and forward the call via the connection.
  template<typename CTX, typename R, const char *&NAME, typename... Arg>
  R rpc (CTX *s, Arg... rest)
  {
    base_gdb_plugin<CTX> *self = (base_gdb_plugin<CTX> *) s;
    R result;
    
    if (!cc1_plugin::call (self->connection.get (), NAME, &result, rest...))
      return 0;
    return result;
  }
}

#endif // CC1_PLUGIN_GDBCTX_HH
