/* Common declarations for LTO plugin for gold and/or GNU ld.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Rafael Avila de Espindola (espindola@google.com).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <stdbool.h>
#include "plugin-api.h"

/* LTO magic section name.  */

#define LTO_SECTION_PREFIX ".gnu.lto_.symtab"

/* The part of the symbol table the plugin has to keep track of. Note that we
   must keep SYMS until all_symbols_read is called to give the linker time to
   copy the symbol information. */

struct sym_aux
{
  uint32_t slot;
  unsigned id;
  unsigned next_conflict;
};

struct plugin_symtab
{
  int nsyms;
  struct sym_aux *aux;
  struct ld_plugin_symbol *syms;
  unsigned id;
};

/* All that we have to remember about a file. */

struct plugin_file_info
{
  char *name;
  void *handle;
  struct plugin_symtab symtab;
  struct plugin_symtab conflicts;
};

/* These are the methods supplied by one of the object format
   dependent files lto-plugin-elf.c or lto-plugin-coff.c  */

extern enum ld_plugin_status claim_file_handler 
		(const struct ld_plugin_input_file *file, int *claimed);

extern enum ld_plugin_status onload_format_checks (struct ld_plugin_tv *tv);

/* These methods are made available to the object format
   dependent files.  */

extern void check (bool gate, enum ld_plugin_level level, const char *text);

extern void translate (char *data, char *end, struct plugin_symtab *out);

extern char *parse_table_entry (char *p, struct ld_plugin_symbol *entry,
			struct sym_aux *aux);

extern void resolve_conflicts (struct plugin_symtab *t,
			struct plugin_symtab *conflicts);

/* And this callback function is exposed.  */

extern ld_plugin_add_symbols add_symbols;

/* Along with these two variables.  */

extern struct plugin_file_info *claimed_files;
extern unsigned int num_claimed_files;

