/* Marshalling and unmarshalling.
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

#ifndef CC1_PLUGIN_MARSHALL_HH
#define CC1_PLUGIN_MARSHALL_HH

#include <type_traits>

#include "status.hh"
#include "gcc-interface.h"

namespace cc1_plugin
{
  class connection;

  // Only a single kind of integer is ever sent over the wire, and
  // this is it.
  typedef unsigned long long protocol_int;

  // Read an integer from the connection and verify that it has the
  // value V.
  status unmarshall_check (connection *, protocol_int v);

  // Write an integer, prefixed with the integer type marker, to the
  // connection.
  status marshall_intlike (connection *, protocol_int);

  // Read a type marker from the connection and verify that it is an
  // integer type marker.  If not, return FAIL.  If so, read an
  // integer store it in the out argument.
  status unmarshall_intlike (connection *, protocol_int *);

  status marshall_array_start (connection *, char, size_t);
  status marshall_array_elmts (connection *, size_t, void *);

  status unmarshall_array_start (connection *, char, size_t *);
  status unmarshall_array_elmts (connection *, size_t, void *);

  // An "empty" marshall call -- used to handle the base case for some
  // variadic templates.
  static inline
  status marshall (connection *)
  {
    return OK;
  }

  // A template function that can handle marshalling various integer
  // objects to the connection.
  template<typename T>
  status marshall (connection *conn, T scalar)
  {
    return marshall_intlike (conn, scalar);
  }

  // A template function that can handle unmarshalling various integer
  // objects from the connection.  Note that there's no way at the
  // protocol level to distinguish different int types.
  template<typename T>
  status unmarshall (connection *conn, T *scalar,
		     typename std::enable_if<std::is_integral<T>::value, T>::type * = 0)
  {
    protocol_int result;

    if (!unmarshall_intlike (conn, &result))
      return FAIL;
    *scalar = (T) result;
    return OK;
  }

  // A template function that can handle unmarshalling various enum
  // objects from the connection.
  template<typename T>
  status unmarshall (connection *conn, T *e_val,
		     typename std::enable_if<std::is_enum<T>::value, T>::type * = 0)
  {
    protocol_int result;

    if (!unmarshall_intlike (conn, &result))
      return FAIL;
    *e_val = (T) result;
    return OK;
  }

  // Send a string type marker followed by a string.
  status marshall (connection *, const char *);

  // Read a string type marker followed by a string.  The caller is
  // responsible for freeing the resulting string using 'delete[]'.
  status unmarshall (connection *, char **);

  // Send a gcc_type_array marker followed by the array.
  status marshall (connection *, const gcc_type_array *);

  // Read a gcc_type_array marker, followed by a gcc_type_array.  The
  // resulting array must be freed by the caller, using 'delete[]' on
  // the elements, and 'delete' on the array object itself.
  status unmarshall (connection *, struct gcc_type_array **);

  template<typename T1, typename T2, typename... Arg>
  status marshall (connection *c, T1 arg1, T2 arg2, Arg... rest)
  {
    if (!marshall (c, arg1))
      return FAIL;
    return marshall (c, arg2, rest...);
  }
};

#endif // CC1_PLUGIN_MARSHALL_HH
