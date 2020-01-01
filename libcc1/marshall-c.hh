/* Marshalling and unmarshalling of C-specific types.
   Copyright (C) 2014-2020 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_MARSHALL_C_HH
#define CC1_PLUGIN_MARSHALL_C_HH

#include "marshall.hh"
#include "gcc-c-interface.h"

namespace cc1_plugin
{
  status
  unmarshall (connection *conn, enum gcc_c_symbol_kind *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_c_symbol_kind) p;
    return OK;
  }

  status
  unmarshall (connection *conn, enum gcc_c_oracle_request *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_c_oracle_request) p;
    return OK;
  }

  status
  unmarshall (connection *conn, enum gcc_qualifiers *result)
  {
    protocol_int p;
    if (!unmarshall_intlike (conn, &p))
      return FAIL;
    *result = (enum gcc_qualifiers) p;
    return OK;
  }
}

#endif // CC1_PLUGIN_MARSHALL_C_HH
