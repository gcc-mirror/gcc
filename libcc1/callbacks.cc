/* Callback management.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

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
#include <string.h>
#include <stdlib.h>
#include "callbacks.hh"
#include "libiberty.h"

// An entry in the hash table.
struct method
{
  const char *name;
  cc1_plugin::callback_ftype *func;
};

// Hash function for struct method.
static hashval_t
hash_method (const void *a)
{
  const struct method *m = (const struct method *) a;

  return htab_hash_string (m->name);
}

// Equality function for struct method.
static int
eq_method (const void *a, const void *b)
{
  const struct method *ma = (const struct method *) a;
  const struct method *mb = (const struct method *) b;

  return strcmp (ma->name, mb->name) == 0;
}

cc1_plugin::callbacks::callbacks ()
  : m_registry (htab_create_alloc (10, hash_method, eq_method,
				   free, xcalloc, free))
{
}

cc1_plugin::callbacks::~callbacks ()
{
  htab_delete (m_registry);
}

void
cc1_plugin::callbacks::add_callback (const char *name,
				     cc1_plugin::callback_ftype *func)
{
  method m;
  method **slot;

  m.name = name;
  m.func = func;

  slot = (method **) htab_find_slot (m_registry, &m, INSERT);
  *slot = XNEW (method);
  **slot = m;
}

cc1_plugin::callback_ftype *
cc1_plugin::callbacks::find_callback (const char *name)
{
  method m, *found;

  m.name = name;

  found = (method *) htab_find (m_registry, &m);
  if (found == NULL)
    return NULL;

  return found->func;
}
