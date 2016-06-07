/* Marshalling and unmarshalling.
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
#include <new>
#include <string.h>
#include "marshall.hh"
#include "connection.hh"

cc1_plugin::status
cc1_plugin::unmarshall_check (connection *conn, unsigned long long check)
{
  unsigned long long r;

  if (!unmarshall (conn, &r))
    return FAIL;
  return check == r ? OK : FAIL;
}

cc1_plugin::status
cc1_plugin::marshall_intlike (connection *conn, unsigned long long val)
{
  if (!conn->send ('i'))
    return FAIL;
  return conn->send (&val, sizeof (val));
}

cc1_plugin::status
cc1_plugin::unmarshall_intlike (connection *conn, unsigned long long *result)
{
  if (!conn->require ('i'))
    return FAIL;
  return conn->get (result, sizeof (*result));
}

cc1_plugin::status
cc1_plugin::unmarshall (connection *conn, enum gcc_c_symbol_kind *result)
{
  protocol_int p;
  if (!unmarshall_intlike (conn, &p))
    return FAIL;
  *result = (enum gcc_c_symbol_kind) p;
  return OK;
}

cc1_plugin::status
cc1_plugin::unmarshall (connection *conn, enum gcc_c_oracle_request *result)
{
  protocol_int p;
  if (!unmarshall_intlike (conn, &p))
    return FAIL;
  *result = (enum gcc_c_oracle_request) p;
  return OK;
}

cc1_plugin::status
cc1_plugin::unmarshall (connection *conn, enum gcc_qualifiers *result)
{
  protocol_int p;
  if (!unmarshall_intlike (conn, &p))
    return FAIL;
  *result = (enum gcc_qualifiers) p;
  return OK;
}

cc1_plugin::status
cc1_plugin::marshall (connection *conn, const char *str)
{
  if (!conn->send ('s'))
    return FAIL;

  unsigned long long len = str == NULL ? -1ULL : strlen (str);
  if (!conn->send (&len, sizeof (len)))
    return FAIL;

  if (str == NULL)
    return OK;

  return conn->send (str, len);
}

cc1_plugin::status
cc1_plugin::unmarshall (connection *conn, char **result)
{
  unsigned long long len;

  if (!conn->require ('s'))
    return FAIL;
  if (!conn->get (&len, sizeof (len)))
    return FAIL;

  if (len == -1ULL)
    {
      *result = NULL;
      return OK;
    }

  char *str = new (std::nothrow) char[len + 1];
  if (str == NULL)
    return FAIL;

  if (!conn->get (str, len))
    {
      delete[] str;
      return FAIL;
    }

  str[len] = '\0';
  *result = str;

  return OK;
}

cc1_plugin::status
cc1_plugin::marshall (connection *conn, const gcc_type_array *a)
{
  if (!conn->send ('a'))
    return FAIL;

  unsigned long long r = a->n_elements;
  if (!conn->send (&r, sizeof (r)))
    return FAIL;

  return conn->send (a->elements, r * sizeof (a->elements[0]));
}

cc1_plugin::status
cc1_plugin::unmarshall (connection *conn, gcc_type_array **result)
{
  unsigned long long len;

  if (!conn->require ('a'))
    return FAIL;
  if (!conn->get (&len, sizeof (len)))
    return FAIL;

  *result = new gcc_type_array;

  (*result)->n_elements = len;
  (*result)->elements = new gcc_type[len];

  if (!conn->get ((*result)->elements, len * sizeof ((*result)->elements[0])))
    {
      delete[] (*result)->elements;
      delete *result;
      return FAIL;
    }

  return OK;
}
