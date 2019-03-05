/* Marshalling and unmarshalling of C++-specific types.
   Copyright (C) 2014-2019 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_MARSHALL_CXX_HH
#define CC1_PLUGIN_MARSHALL_CXX_HH

#include "marshall.hh"
#include "gcc-cp-interface.h"

namespace cc1_plugin
{
  status
  unmarshall (connection *conn, enum gcc_cp_symbol_kind *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_cp_symbol_kind) p;
    return OK;
  }

  status
  unmarshall (connection *conn, enum gcc_cp_oracle_request *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_cp_oracle_request) p;
    return OK;
  }

  status
  unmarshall (connection *conn, enum gcc_cp_qualifiers *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_cp_qualifiers) p;
    return OK;
  }

  status
  unmarshall (connection *conn, enum gcc_cp_ref_qualifiers *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_cp_ref_qualifiers) p;
    return OK;
  }

  // Send a gcc_vbase_array marker followed by the array.
  status
  marshall (connection *conn, const gcc_vbase_array *a)
  {
    size_t len;

    if (a)
      len = a->n_elements;
    else
      len = (size_t)-1;

    if (!marshall_array_start (conn, 'v', len))
      return FAIL;

    if (!a)
      return OK;

    if (!marshall_array_elmts (conn, len * sizeof (a->elements[0]),
			       a->elements))
      return FAIL;

    return marshall_array_elmts (conn, len * sizeof (a->flags[0]),
				 a->flags);
  }

  // Read a gcc_vbase_array marker, followed by a gcc_vbase_array.  The
  // resulting array must be freed by the caller, using 'delete[]' on
  // elements and virtualp, and 'delete' on the array object itself.
  status
  unmarshall (connection *conn, struct gcc_vbase_array **result)
  {
    size_t len;

    if (!unmarshall_array_start (conn, 'v', &len))
      return FAIL;

    if (len == (size_t)-1)
      {
	*result = NULL;
	return OK;
      }

    struct gcc_vbase_array *gva = new gcc_vbase_array;

    gva->n_elements = len;
    gva->elements = new gcc_type[len];

    if (!unmarshall_array_elmts (conn,
				 len * sizeof (gva->elements[0]),
				 gva->elements))
      {
	delete[] gva->elements;
	delete gva;
	return FAIL;
      }

    gva->flags = new enum gcc_cp_symbol_kind[len];

    if (!unmarshall_array_elmts (conn,
				 len * sizeof (gva->flags[0]),
				 gva->flags))
      {
	delete[] gva->flags;
	delete[] gva->elements;
	delete gva;
	return FAIL;
      }

    *result = gva;
    return OK;
  }

  // Send a gcc_cp_template_args marker followed by the array.
  status
  marshall (connection *conn, const gcc_cp_template_args *a)
  {
    size_t len;

    if (a)
      len = a->n_elements;
    else
      len = (size_t)-1;

    if (!marshall_array_start (conn, 't', len))
      return FAIL;

    if (!a)
      return OK;

    if (!marshall_array_elmts (conn, len * sizeof (a->kinds[0]),
			       a->kinds))
      return FAIL;

    return marshall_array_elmts (conn, len * sizeof (a->elements[0]),
				 a->elements);
  }

  // Read a gcc_vbase_array marker, followed by a gcc_vbase_array.  The
  // resulting array must be freed by the caller, using 'delete[]' on
  // elements and virtualp, and 'delete' on the array object itself.
  status
  unmarshall (connection *conn, struct gcc_cp_template_args **result)
  {
    size_t len;

    if (!unmarshall_array_start (conn, 't', &len))
      return FAIL;

    if (len == (size_t)-1)
      {
	*result = NULL;
	return OK;
      }

    struct gcc_cp_template_args *gva = new gcc_cp_template_args;

    gva->n_elements = len;
    gva->kinds = new char[len];

    if (!unmarshall_array_elmts (conn,
				 len * sizeof (gva->kinds[0]),
				 gva->kinds))
      {
	delete[] gva->kinds;
	delete gva;
	return FAIL;
      }

    gva->elements = new gcc_cp_template_arg[len];

    if (!unmarshall_array_elmts (conn,
				 len * sizeof (gva->elements[0]),
				 gva->elements))
      {
	delete[] gva->elements;
	delete[] gva->kinds;
	delete gva;
	return FAIL;
      }

    *result = gva;
    return OK;
  }

  // Send a gcc_cp_function_args marker followed by the array.
  status
  marshall (connection *conn, const gcc_cp_function_args *a)
  {
    size_t len;

    if (a)
      len = a->n_elements;
    else
      len = (size_t)-1;

    if (!marshall_array_start (conn, 'd', len))
      return FAIL;

    if (!a)
      return OK;

    return marshall_array_elmts (conn, len * sizeof (a->elements[0]),
				 a->elements);
  }

  // Read a gcc_cp_function_args marker, followed by a
  // gcc_cp_function_args.  The resulting array must be freed
  // by the caller, using 'delete[]' on elements and virtualp, and
  // 'delete' on the array object itself.
  status
  unmarshall (connection *conn, struct gcc_cp_function_args **result)
  {
    size_t len;

    if (!unmarshall_array_start (conn, 'd', &len))
      return FAIL;

    if (len == (size_t)-1)
      {
	*result = NULL;
	return OK;
      }

    struct gcc_cp_function_args *gva = new gcc_cp_function_args;

    gva->n_elements = len;
    gva->elements = new gcc_expr[len];

    if (!unmarshall_array_elmts (conn,
				 len * sizeof (gva->elements[0]),
				 gva->elements))
      {
	delete[] gva->elements;
	delete gva;
	return FAIL;
      }

    *result = gva;

    return OK;
  }
}

#endif // CC1_PLUGIN_MARSHALL_CP_HH
