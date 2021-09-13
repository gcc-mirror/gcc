/* Plugin connection declarations
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

#ifndef CC1_PLUGIN_CONNECTION_HH
#define CC1_PLUGIN_CONNECTION_HH

#include "status.hh"
#include "callbacks.hh"

namespace cc1_plugin
{
  // The connection class represents one side of the connection
  // between the gdb-side library and the gcc plugin.  It handles the
  // low-level details of reading and writing data.
  class connection
  {
  public:

    connection (int fd)
      : m_fd (fd),
	m_aux_fd (-1),
	m_callbacks ()
    {
    }

    connection (int fd, int aux_fd)
      : m_fd (fd),
	m_aux_fd (aux_fd),
	m_callbacks ()
    {
    }

    virtual ~connection () = default;

    connection (const connection &) = delete;
    connection &operator= (const connection &) = delete;

    // Send a single character.  This is used to introduce various
    // higher-level protocol elements.
    status send (char c);

    // Send data in bulk.
    status send (const void *buf, int len);

    // Read a single byte from the connection and verify that it
    // matches the argument C.
    status require (char c);

    // Read data in bulk.
    status get (void *buf, int len);

    // This is called after a query (remote function call) has been
    // sent to the remote.  It waits for a response packet.  The
    // response character is read before returning.  Any query packets
    // sent from the remote while waiting for a response are handled
    // by this function.
    status wait_for_result ()
    {
      return do_wait (true);
    }

    // Read and respond to query packets sent by the remote.  This
    // function returns when the connection is closed.
    status wait_for_query ()
    {
      return do_wait (false);
    }

    // Register a callback with this connection.  NAME is the name of
    // the method being registered.  FUNC is the function.  It must
    // know how to decode its own arguments.  When a query packet is
    // received by one of the wait_* methods, the corresponding
    // callback is invoked.
    void add_callback (const char *name, callback_ftype *func)
    {
      m_callbacks.add_callback (name, func);
    }

    virtual void print (const char *)
    {
    }

  private:

    // Helper function for the wait_* methods.
    status do_wait (bool);

    // The file descriptor.
    int m_fd;

    // An auxiliary file descriptor, or -1 if none.
    int m_aux_fd;

    // Callbacks associated with this connection.
    callbacks m_callbacks;
  };
}

#endif // CC1_PLUGIN_CONNECTION_HH
