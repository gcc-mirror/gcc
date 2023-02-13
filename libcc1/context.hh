/* Generic plugin context
   Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_CONTEXT_HH
#define CC1_PLUGIN_CONTEXT_HH

#include "system.h"
#include "coretypes.h"
#include "tree.h"

#include "connection.hh"

namespace cc1_plugin
{
  static inline unsigned long long
  convert_out (tree t)
  {
    return (unsigned long long) (uintptr_t) t;
  }

  static inline tree
  convert_in (unsigned long long v)
  {
    return (tree) (uintptr_t) v;
  }

  struct decl_addr_value
  {
    tree decl;
    tree address;
  };

  struct decl_addr_hasher : free_ptr_hash<decl_addr_value>
  {
    static hashval_t hash (const decl_addr_value *e)
    {
      return DECL_UID (e->decl);
    }

    static bool equal (const decl_addr_value *p1,
		       const decl_addr_value *p2)
    {
      return p1->decl == p2->decl;
    }
  };

  struct string_hasher : nofree_ptr_hash<const char>
  {
    static inline hashval_t hash (const char *s)
    {
      return htab_hash_string (s);
    }

    static inline bool equal (const char *p1, const char *p2)
    {
      return strcmp (p1, p2) == 0;
    }
  };

  struct plugin_context : public cc1_plugin::connection
  {
    plugin_context (int fd)
      : cc1_plugin::connection (fd),
	address_map (30),
	preserved (30),
	file_names (30)
    {
    }

    // Map decls to addresses.
    hash_table<decl_addr_hasher> address_map;

    // A collection of trees that are preserved for the GC.
    hash_table< nofree_ptr_hash<tree_node> > preserved;

    // File name cache.
    hash_table<string_hasher> file_names;

    // Perform GC marking.
    void mark ();

    // Preserve a tree during the plugin's operation.
    tree preserve (tree t)
    {
      tree_node **slot = preserved.find_slot (t, INSERT);
      *slot = t;
      return t;
    }

    location_t get_location_t (const char *filename,
			       unsigned int line_number);

  private:

    // Add a file name to FILE_NAMES and return the canonical copy.
    const char *intern_filename (const char *filename);
  };

  extern plugin_context *current_context;

  void generic_plugin_init (struct plugin_name_args *plugin_info,
			    unsigned int version);
}

#endif // CC1_PLUGIN_CONTEXT_HH
