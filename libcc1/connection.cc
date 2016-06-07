/* Connect implementation
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
#include <string>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>
#include "marshall.hh"
#include "connection.hh"
#include "rpc.hh"

cc1_plugin::connection::~connection ()
{
}

void
cc1_plugin::connection::print (const char *)
{
}

cc1_plugin::status
cc1_plugin::connection::send (char c)
{
  if (write (m_fd, &c, 1) != 1)
    return FAIL;
  return OK;
}

cc1_plugin::status
cc1_plugin::connection::send (const void *buf, int len)
{
  if (write (m_fd, buf, len) != len)
    return FAIL;
  return OK;
}

cc1_plugin::status
cc1_plugin::connection::require (char c)
{
  char result;

  if (read (m_fd, &result, 1) != 1
      || result != c)
    return FAIL;

  return OK;
}

cc1_plugin::status
cc1_plugin::connection::get (void *buf, int len)
{
  if (read (m_fd, buf, len) != len)
    return FAIL;
  return OK;
}

cc1_plugin::status
cc1_plugin::connection::do_wait (bool want_result)
{
  while (true)
    {
      char result;
      fd_set read_set;

      FD_ZERO (&read_set);
      FD_SET (m_fd, &read_set);
      if (m_aux_fd != -1)
	FD_SET (m_aux_fd, &read_set);

      int nfds = select (FD_SETSIZE, &read_set, NULL, NULL, NULL);
      if (nfds == -1)
	{
	  if (errno == EINTR)
	    continue;
	  return FAIL;
	}

      // We have to check the stderr fd first, to avoid a possible
      // blocking scenario when do_wait is called reentrantly.  In
      // such a call, if we handle the primary fd first, then we may
      // re-enter this function, read from gcc's stderr, causing the
      // outer invocation of this function to block when trying to
      // read.
      if (m_aux_fd != -1 && FD_ISSET (m_aux_fd, &read_set))
	{
	  char buf[1024];
	  int n = read (m_aux_fd, buf, sizeof (buf) - 1);
	  if (n < 0)
	    return FAIL;
	  if (n > 0)
	    {
	      buf[n] = '\0';
	      print (buf);
	    }
	}

      if (FD_ISSET (m_fd, &read_set))
	{
	  int n = read (m_fd, &result, 1);
	  if (n == 0)
	    return want_result ? FAIL : OK;
	  if (n != 1)
	    return FAIL;

	  switch (result)
	    {
	    case 'R':
	      // The reply is ready; the caller will unmarshall it.
	      return want_result ? OK : FAIL;

	    case 'Q':
	      // While waiting for a reply, the other side made a method
	      // call.
	      {
		// Use an argument_wrapper here to simplify management
		// of the string's lifetime.
		argument_wrapper<char *> method_name;

		if (!method_name.unmarshall (this))
		  return FAIL;

		callback_ftype *callback
		  = m_callbacks.find_callback (method_name);
		// The call to CALLBACK is where we may end up in a
		// reentrant call.
		if (callback == NULL || !callback (this))
		  return FAIL;
	      }
	      break;

	    default:
	      return FAIL;
	    }
	}
    }
}
