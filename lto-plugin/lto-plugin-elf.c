/* LTO plugin for gold.
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

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <libiberty.h>
#include <stdlib.h>
#include <inttypes.h>

/* The presence of gelf.h is checked by the toplevel configure script.  */
#include <gelf.h>

/* Common definitions that the object format dependent code needs.  */
#include "lto-plugin.h"

/* Process all lto symtabs of file ELF. */

static int
process_symtab (Elf *elf, struct plugin_symtab *out)
{
  int found = 0;
  Elf_Scn *section = 0;
  GElf_Ehdr header;
  GElf_Ehdr *t = gelf_getehdr (elf, &header);
  if (t == NULL)
    return 0;
  assert (t == &header);

  while ((section = elf_nextscn(elf, section)) != 0)
    {
      GElf_Shdr shdr;
      GElf_Shdr *tshdr = gelf_getshdr (section, &shdr);
      Elf_Data *symtab;
      const char *t;
      assert (tshdr == &shdr);
      t = elf_strptr (elf, header.e_shstrndx, shdr.sh_name);
      assert (t != NULL);
      if (strncmp (t, LTO_SECTION_PREFIX, strlen (LTO_SECTION_PREFIX)) == 0) 
	{
	  char *s = strrchr (t, '.');
	  if (s)
	      sscanf (s, ".%x", &out->id);
	  symtab = elf_getdata (section, NULL);
	  translate (symtab->d_buf, symtab->d_buf + symtab->d_size, out);
	  found++;
	}
    }
  return found;
}

/* Callback used by gold to check if the plugin will claim FILE. Writes
   the result in CLAIMED. */

enum ld_plugin_status
claim_file_handler (const struct ld_plugin_input_file *file, int *claimed)
{
  enum ld_plugin_status status;
  Elf *elf;
  struct plugin_file_info lto_file;
  int n;

  memset (&lto_file, 0, sizeof (struct plugin_file_info));

  if (file->offset != 0)
    {
      char *objname;
      Elf *archive;
      off_t offset;
      /* We pass the offset of the actual file, not the archive header. */
      int t = asprintf (&objname, "%s@0x%" PRIx64, file->name,
                        (int64_t) file->offset);
      check (t >= 0, LDPL_FATAL, "asprintf failed");
      lto_file.name = objname;

      archive = elf_begin (file->fd, ELF_C_READ, NULL);
      check (elf_kind (archive) == ELF_K_AR, LDPL_FATAL,
             "Not an archive and offset not 0");

      /* elf_rand expects the offset to point to the ar header, not the
         object itself. Subtract the size of the ar header (60 bytes).
         We don't uses sizeof (struct ar_hd) to avoid including ar.h */

      offset = file->offset - 60;
      check (offset == elf_rand (archive, offset), LDPL_FATAL,
             "could not seek in archive");
      elf = elf_begin (file->fd, ELF_C_READ, archive);
      check (elf != NULL, LDPL_FATAL, "could not find archive member");
      elf_end (archive);
    }
  else
    {
      lto_file.name = xstrdup (file->name);
      elf = elf_begin (file->fd, ELF_C_READ, NULL);
    }
  lto_file.handle = file->handle;

  *claimed = 0;

  if (!elf)
    goto err;

  n = process_symtab (elf, &lto_file.symtab);
  if (n == 0)
    goto err;

  if (n > 1)
    resolve_conflicts (&lto_file.symtab, &lto_file.conflicts);

  status = add_symbols (file->handle, lto_file.symtab.nsyms,
			lto_file.symtab.syms);
  check (status == LDPS_OK, LDPL_FATAL, "could not add symbols");

  *claimed = 1;
  num_claimed_files++;
  claimed_files =
    xrealloc (claimed_files,
	      num_claimed_files * sizeof (struct plugin_file_info));
  claimed_files[num_claimed_files - 1] = lto_file;

  goto cleanup;

 err:
  free (lto_file.name);

 cleanup:
  if (elf)
    elf_end (elf);

  return LDPS_OK;
}

/* Method called first thing at onload time to perform sanity checks.  */

enum ld_plugin_status
onload_format_checks (struct ld_plugin_tv *tv)
{
  unsigned version = elf_version (EV_CURRENT);
  check (version != EV_NONE, LDPL_FATAL, "invalid ELF version");
  return LDPS_OK;
}

