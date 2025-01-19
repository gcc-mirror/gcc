/* Generic plugin context
   Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#include "../gcc/config.h"

#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "stringpool.h"
#include "hash-set.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "diagnostic-format-text.h"

#include "gcc-interface.h"

#include "context.hh"
#include "marshall.hh"



#ifdef __GNUC__
#pragma GCC visibility push(default)
#endif
int plugin_is_GPL_compatible;
#ifdef __GNUC__
#pragma GCC visibility pop
#endif

cc1_plugin::plugin_context *cc1_plugin::current_context;



// This is put into the lang hooks when the plugin starts.

static void
plugin_print_error_function (diagnostic_text_output_format &text_output,
			     const char *file,
			     const diagnostic_info *diagnostic)
{
  if (current_function_decl != NULL_TREE
      && DECL_NAME (current_function_decl) != NULL_TREE
      && strcmp (IDENTIFIER_POINTER (DECL_NAME (current_function_decl)),
		 GCC_FE_WRAPPER_FUNCTION) == 0)
    return;
  lhd_print_error_function (text_output, file, diagnostic);
}



location_t
cc1_plugin::plugin_context::get_location_t (const char *filename,
					    unsigned int line_number)
{
  if (filename == NULL)
    return UNKNOWN_LOCATION;

  filename = intern_filename (filename);
  linemap_add (line_table, LC_ENTER, false, filename, line_number);
  location_t loc = linemap_line_start (line_table, line_number, 0);
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
  return loc;
}

// Add a file name to FILE_NAMES and return the canonical copy.
const char *
cc1_plugin::plugin_context::intern_filename (const char *filename)
{
  const char **slot = file_names.find_slot (filename, INSERT);
  if (*slot == NULL)
    {
      /* The file name must live as long as the line map, which
	 effectively means as long as this compilation.  So, we copy
	 the string here but never free it.  */
      *slot = xstrdup (filename);
    }
  return *slot;
}

void
cc1_plugin::plugin_context::mark ()
{
  for (const auto &item : address_map)
    {
      ggc_mark (item->decl);
      ggc_mark (item->address);
    }

  for (const auto &item : preserved)
    ggc_mark (&item);
}



// Perform GC marking.

static void
gc_mark (void *, void *)
{
  if (cc1_plugin::current_context != NULL)
    cc1_plugin::current_context->mark ();
}

void
cc1_plugin::generic_plugin_init (struct plugin_name_args *plugin_info,
				 unsigned int version)
{
  long fd = -1;
  for (int i = 0; i < plugin_info->argc; ++i)
    {
      if (strcmp (plugin_info->argv[i].key, "fd") == 0)
	{
	  char *tail;
	  errno = 0;
	  fd = strtol (plugin_info->argv[i].value, &tail, 0);
	  if (*tail != '\0' || errno != 0)
	    fatal_error (input_location,
			 "%s: invalid file descriptor argument to plugin",
			 plugin_info->base_name);
	  break;
	}
    }
  if (fd == -1)
    fatal_error (input_location,
		 "%s: required plugin argument %<fd%> is missing",
		 plugin_info->base_name);

  current_context = new plugin_context (fd);

  // Handshake.
  cc1_plugin::protocol_int h_version;
  if (!current_context->require ('H')
      || ! ::cc1_plugin::unmarshall (current_context, &h_version))
    fatal_error (input_location,
		 "%s: handshake failed", plugin_info->base_name);
  if (h_version != version)
    fatal_error (input_location,
		 "%s: unknown version in handshake", plugin_info->base_name);

  register_callback (plugin_info->base_name, PLUGIN_GGC_MARKING,
		     gc_mark, NULL);

  lang_hooks.print_error_function = plugin_print_error_function;
}
