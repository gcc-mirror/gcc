/* Generic GDB-side plugin
   Copyright (C) 2020 Free Software Foundation, Inc.

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
  template<typename T>
  struct base_gdb_plugin : public T
  {
    explicit base_gdb_plugin (const gcc_base_vtable *v)
      : verbose (false),
	compilerp (new compiler (verbose))
    {
      this->base.ops = v;
    }

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

    std::unique_ptr<cc1_plugin::compiler> compilerp;
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
