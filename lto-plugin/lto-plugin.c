/* LTO plugin for gold.
   Copyright (C) 2009 Free Software Foundation, Inc.
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

/* The plugin has only one external function: onload. Gold passes it an array of
   function that the plugin uses to communicate back to gold.

   With the functions provided by gold, the plugin can be notified when
   gold first analyzes a file and pass a symbol table back to gold. The plugin
   is also notified when all symbols have been read and it is time to generate
   machine code for the necessary symbols.

   More information at http://gcc.gnu.org/wiki/whopr/driver.

   This plugin should be passed the lto-wrapper options and will forward them.
   It also has 2 options of its own:
   -debug: Print the command line used to run lto-wrapper.
   -nop: Instead of running lto-wrapper, pass the original to the plugin. This
   only works if the input files are hybrid.  */

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdbool.h>
#include <libiberty.h>

/* The presence of gelf.h is checked by the toplevel configure script.  */
#include <gelf.h>

#include "plugin-api.h"
#include "../gcc/lto/common.h"

/* The part of the symbol table the plugin has to keep track of. Note that we
   must keep SYMS until all_symbols_read is called to give the linker time to
   copy the symbol information. */

struct plugin_symtab
{
  int nsyms;
  uint32_t *slots;
  struct ld_plugin_symbol *syms;
};

/* All that we have to remember about a file. */

struct plugin_file_info
{
  char *name;
  void *handle;
  struct plugin_symtab symtab;
};


static char *arguments_file_name;
static ld_plugin_register_claim_file register_claim_file;
static ld_plugin_add_symbols add_symbols;
static ld_plugin_register_all_symbols_read register_all_symbols_read;
static ld_plugin_get_symbols get_symbols;
static ld_plugin_register_cleanup register_cleanup;
static ld_plugin_add_input_file add_input_file;
static ld_plugin_add_input_library add_input_library;
static ld_plugin_message message;

static struct plugin_file_info *claimed_files = NULL;
static unsigned int num_claimed_files = 0;

static char **output_files = NULL;
static unsigned int num_output_files = 0;

static char **lto_wrapper_argv;
static int lto_wrapper_num_args;

static char **pass_through_items = NULL;
static unsigned int num_pass_through_items;

static bool debug;
static bool nop;
static char *resolution_file = NULL;

static void
check (bool gate, enum ld_plugin_level level, const char *text)
{
  if (gate)
    return;

  if (message)
    message (level, text);
  else
    {
      /* If there is no nicer way to inform the user, fallback to stderr. */
      fprintf (stderr, "%s\n", text);
      if (level == LDPL_FATAL)
	abort ();
    }
}

/* Parse an entry of the IL symbol table. The data to be parsed is pointed
   by P and the result is written in ENTRY. The slot number is stored in SLOT.
   Returns the address of the next entry. */

static char *
parse_table_entry (char *p, struct ld_plugin_symbol *entry, uint32_t *slot)
{
  unsigned char t;
  enum ld_plugin_symbol_kind translate_kind[] =
    {
      LDPK_DEF,
      LDPK_WEAKDEF,
      LDPK_UNDEF,
      LDPK_WEAKUNDEF,
      LDPK_COMMON
    };

  enum ld_plugin_symbol_visibility translate_visibility[] =
    {
      LDPV_DEFAULT,
      LDPV_PROTECTED,
      LDPV_INTERNAL,
      LDPV_HIDDEN
    };

  entry->name = strdup (p);
  while (*p)
    p++;
  p++;

  entry->version = NULL;

  entry->comdat_key = p;
  while (*p)
    p++;
  p++;

  if (strlen (entry->comdat_key) == 0)
    entry->comdat_key = NULL;
  else
    entry->comdat_key = strdup (entry->comdat_key);

  t = *p;
  check (t <= 4, LDPL_FATAL, "invalid symbol kind found");
  entry->def = translate_kind[t];
  p++;

  t = *p;
  check (t <= 3, LDPL_FATAL, "invalid symbol visibility found");
  entry->visibility = translate_visibility[t];
  p++;

  entry->size = *(uint64_t *) p;
  p += 8;

  *slot = *(uint32_t *) p;
  p += 4;

  entry->resolution = LDPR_UNKNOWN;

  return p;
}

/* Return the section in ELF that is named NAME. */

static Elf_Scn *
get_section (Elf *elf, const char *name)
{
  Elf_Scn *section = 0;
  GElf_Ehdr header;
  GElf_Ehdr *t = gelf_getehdr (elf, &header);
  if (t == NULL)
    return NULL;
  assert (t == &header);

  while ((section = elf_nextscn(elf, section)) != 0)
    {
      GElf_Shdr shdr;
      GElf_Shdr *tshdr = gelf_getshdr (section, &shdr);
      const char *t;
      assert (tshdr == &shdr);
      t = elf_strptr (elf, header.e_shstrndx, shdr.sh_name);
      assert (t != NULL);
      if (strcmp (t, name) == 0)
	return section;
    }
  return NULL;
}

/* Returns the IL symbol table of file ELF. */

static Elf_Data *
get_symtab (Elf *elf)
{
  Elf_Data *data = 0;
  Elf_Scn *section = get_section (elf, ".gnu.lto_.symtab");
  if (!section)
    return NULL;

  data = elf_getdata (section, data);
  assert (data);
  return data;
}

/* Translate the IL symbol table SYMTAB. Write the slots and symbols in OUT. */

static void
translate (Elf_Data *symtab, struct plugin_symtab *out)
{
  uint32_t *slots = NULL;
  char *data = symtab->d_buf;
  char *end = data + symtab->d_size;
  struct ld_plugin_symbol *syms = NULL;
  int n = 0;

  while (data < end)
    {
      n++;
      syms = realloc (syms, n * sizeof (struct ld_plugin_symbol));
      check (syms, LDPL_FATAL, "could not allocate memory");
      slots = realloc (slots, n * sizeof (uint32_t));
      check (slots, LDPL_FATAL, "could not allocate memory");
      data = parse_table_entry (data, &syms[n - 1], &slots[n - 1]);
    }

  out->nsyms = n;
  out->syms = syms;
  out->slots = slots;
}

/* Free all memory that is no longer needed after writing the symbol
   resolution. */

static void
free_1 (void)
{
  unsigned int i;
  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];
      struct plugin_symtab *symtab = &info->symtab;
      unsigned int j;
      for (j = 0; j < symtab->nsyms; j++)
	{
	  struct ld_plugin_symbol *s = &symtab->syms[j];
	  free (s->name);
	  if (s->comdat_key)
	    free (s->comdat_key);
	}
      free (symtab->syms);
      symtab->syms = NULL;
    }
}

/* Free all remaining memory. */

static void
free_2 (void)
{
  unsigned int i;
  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];
      struct plugin_symtab *symtab = &info->symtab;
      free (symtab->slots);
      free (info->name);
    }

  for (i = 0; i < num_output_files; i++)
    free (output_files[i]);
  free (output_files);

  free (claimed_files);
  claimed_files = NULL;
  num_claimed_files = 0;

  if (arguments_file_name)
    free (arguments_file_name);
  arguments_file_name = NULL;

  if (resolution_file)
    {
      free (resolution_file);
      resolution_file = NULL;
    }
}

/*  Writes the relocations to disk. */

static void
write_resolution (void)
{
  unsigned int i;
  FILE *f;

  f = fopen (resolution_file, "w");
  check (f, LDPL_FATAL, "could not open file");

  fprintf (f, "%d\n", num_claimed_files);

  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];
      struct plugin_symtab *symtab = &info->symtab;
      struct ld_plugin_symbol *syms = symtab->syms;
      unsigned j;

      assert (syms);
      get_symbols (info->handle, symtab->nsyms, syms);

      fprintf (f, "%s %d\n", info->name, info->symtab.nsyms);

      for (j = 0; j < info->symtab.nsyms; j++)
	{
	  uint32_t slot = symtab->slots[j];
	  unsigned int resolution = syms[j].resolution;
	  fprintf (f, "%d %s %s\n", slot, lto_resolution_str[resolution], syms[j].name);
	}
    }
  fclose (f);
}

/* Pass files generated by the lto-wrapper to the linker. FD is lto-wrapper's
   stdout. */

static void
add_output_files (FILE *f)
{
  char fname[1000]; /* FIXME: Remove this restriction. */

  for (;;)
    {
      size_t len;
      char *s = fgets (fname, sizeof (fname), f);
      if (!s)
	break;

      len = strlen (s);
      check (s[len - 1] == '\n', LDPL_FATAL, "file name too long");
      s[len - 1] = '\0';

      num_output_files++;
      output_files = realloc (output_files, num_output_files * sizeof (char *));
      output_files[num_output_files - 1] = strdup (s);
      add_input_file (output_files[num_output_files - 1]);
    }
}

/* Execute the lto-wrapper. ARGV[0] is the binary. The rest of ARGV is the
   argument list. */

static void
exec_lto_wrapper (char *argv[])
{
  int t, i;
  int status;
  char *at_args;
  FILE *args;
  FILE *wrapper_output;
  char *new_argv[3];
  struct pex_obj *pex;
  const char *errmsg;

  /* Write argv to a file to avoid a command line that is too long. */
  arguments_file_name = make_temp_file ("");
  check (arguments_file_name, LDPL_FATAL,
         "Failed to generate a temorary file name");

  args = fopen (arguments_file_name, "w");
  check (args, LDPL_FATAL, "could not open arguments file");

  t = writeargv (&argv[1], args);
  check (t == 0, LDPL_FATAL, "could not write arguments");
  t = fclose (args);
  check (t == 0, LDPL_FATAL, "could not close arguments file");

  at_args = concat ("@", arguments_file_name, NULL);
  check (at_args, LDPL_FATAL, "could not allocate");

  for (i = 1; argv[i]; i++)
    {
      char *a = argv[i];
      if (a[0] == '-' && a[1] == 'v' && a[2] == '\0')
	{
	  for (i = 0; argv[i]; i++)
	    fprintf (stderr, "%s ", argv[i]);
	  fprintf (stderr, "\n");
	  break;
	}
    }

  new_argv[0] = argv[0];
  new_argv[1] = at_args;
  new_argv[2] = NULL;

  if (debug)
    {
      for (i = 0; new_argv[i]; i++)
	fprintf (stderr, "%s ", new_argv[i]);
      fprintf (stderr, "\n");
    }


  pex = pex_init (PEX_USE_PIPES, "lto-wrapper", NULL);
  check (pex != NULL, LDPL_FATAL, "could not pex_init lto-wrapper");

  errmsg = pex_run (pex, 0, new_argv[0], new_argv, NULL, NULL, &t);
  check (errmsg == NULL, LDPL_FATAL, "could not run lto-wrapper");
  check (t == 0, LDPL_FATAL, "could not run lto-wrapper");

  wrapper_output = pex_read_output (pex, 0);
  check (wrapper_output, LDPL_FATAL, "could not read lto-wrapper output");

  add_output_files (wrapper_output);

  t = pex_get_status (pex, 1, &status);
  check (t == 1, LDPL_FATAL, "could not get lto-wrapper exit status");
  check (WIFEXITED (status) && WEXITSTATUS (status) == 0, LDPL_FATAL,
         "lto-wrapper failed");

  pex_free (pex);

  free (at_args);
}

/* Pass the original files back to the linker. */

static void
use_original_files (void)
{
  unsigned i;
  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];
      add_input_file (info->name);
    }
}


/* Called by the linker once all symbols have been read. */

static enum ld_plugin_status
all_symbols_read_handler (void)
{
  unsigned i;
  unsigned num_lto_args = num_claimed_files + lto_wrapper_num_args + 2 + 1;
  char **lto_argv;
  const char **lto_arg_ptr;
  if (num_claimed_files == 0)
    return LDPS_OK;

  if (nop)
    {
      use_original_files ();
      return LDPS_OK;
    }

  lto_argv = (char **) calloc (sizeof (char *), num_lto_args);
  lto_arg_ptr = (const char **) lto_argv;
  assert (lto_wrapper_argv);

  resolution_file = make_temp_file ("");

  write_resolution ();

  free_1 ();

  for (i = 0; i < lto_wrapper_num_args; i++)
    *lto_arg_ptr++ = lto_wrapper_argv[i];

  *lto_arg_ptr++ = "-fresolution";
  *lto_arg_ptr++ = resolution_file;

  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];

      *lto_arg_ptr++ = info->name;
    }

  *lto_arg_ptr++ = NULL;
  exec_lto_wrapper (lto_argv);

  free (lto_argv);

  if (pass_through_items)
    {
      unsigned int i;
      for (i = 0; i < num_pass_through_items; i++)
        {
          if (strncmp (pass_through_items[i], "-l", 2) == 0)
            add_input_library (pass_through_items[i] + 2);
          else
            add_input_file (pass_through_items[i]);
          free (pass_through_items[i]);
          pass_through_items[i] = NULL;
        }
      free (pass_through_items);
      pass_through_items = NULL;
    }

  return LDPS_OK;
}

/* Remove temporary files at the end of the link. */

static enum ld_plugin_status
cleanup_handler (void)
{
  int t;

  if (debug)
    return LDPS_OK;

  if (arguments_file_name)
    {
      t = unlink (arguments_file_name);
      check (t == 0, LDPL_FATAL, "could not unlink arguments file");
    }

  if (resolution_file)
    {
      t = unlink (resolution_file);
      check (t == 0, LDPL_FATAL, "could not unlink resolution file");
    }

  free_2 ();
  return LDPS_OK;
}

/* Callback used by gold to check if the plugin will claim FILE. Writes
   the result in CLAIMED. */

static enum ld_plugin_status
claim_file_handler (const struct ld_plugin_input_file *file, int *claimed)
{
  enum ld_plugin_status status;
  Elf *elf;
  struct plugin_file_info lto_file;
  Elf_Data *symtab;

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
      lto_file.name = strdup (file->name);
      elf = elf_begin (file->fd, ELF_C_READ, NULL);
    }
  lto_file.handle = file->handle;

  *claimed = 0;

  if (!elf)
    goto err;

  symtab = get_symtab (elf);
  if (!symtab)
    goto err;

  translate (symtab, &lto_file.symtab);

  status = add_symbols (file->handle, lto_file.symtab.nsyms,
			lto_file.symtab.syms);
  check (status == LDPS_OK, LDPL_FATAL, "could not add symbols");

  *claimed = 1;
  num_claimed_files++;
  claimed_files =
    realloc (claimed_files,
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

/* Parse the plugin options. */

static void
process_option (const char *option)
{
  if (strcmp (option, "-debug") == 0)
    debug = 1;
  else if (strcmp (option, "-nop") == 0)
    nop = 1;
  else if (!strncmp (option, "-pass-through=", strlen("-pass-through=")))
    {
      num_pass_through_items++;
      pass_through_items = realloc (pass_through_items,
                                    num_pass_through_items * sizeof (char *));
      pass_through_items[num_pass_through_items - 1] =
          strdup (option + strlen ("-pass-through="));
    }
  else
    {
      int size;
      lto_wrapper_num_args += 1;
      size = lto_wrapper_num_args * sizeof (char *);
      lto_wrapper_argv = (char **) realloc (lto_wrapper_argv, size);
      lto_wrapper_argv[lto_wrapper_num_args - 1] = strdup(option);
    }
}

/* Called by gold after loading the plugin. TV is the transfer vector. */

enum ld_plugin_status
onload (struct ld_plugin_tv *tv)
{
  struct ld_plugin_tv *p;
  enum ld_plugin_status status;

  unsigned version = elf_version (EV_CURRENT);
  check (version != EV_NONE, LDPL_FATAL, "invalid ELF version");

  p = tv;
  while (p->tv_tag)
    {
      switch (p->tv_tag)
	{
        case LDPT_MESSAGE:
          message = p->tv_u.tv_message;
          break;
	case LDPT_REGISTER_CLAIM_FILE_HOOK:
	  register_claim_file = p->tv_u.tv_register_claim_file;
	  break;
	case LDPT_ADD_SYMBOLS:
	  add_symbols = p->tv_u.tv_add_symbols;
	  break;
	case LDPT_REGISTER_ALL_SYMBOLS_READ_HOOK:
	  register_all_symbols_read = p->tv_u.tv_register_all_symbols_read;
	  break;
	case LDPT_GET_SYMBOLS:
	  get_symbols = p->tv_u.tv_get_symbols;
	  break;
	case LDPT_REGISTER_CLEANUP_HOOK:
	  register_cleanup = p->tv_u.tv_register_cleanup;
	  break;
	case LDPT_ADD_INPUT_FILE:
	  add_input_file = p->tv_u.tv_add_input_file;
	  break;
	case LDPT_ADD_INPUT_LIBRARY:
	  add_input_library = p->tv_u.tv_add_input_library;
	  break;
	case LDPT_OPTION:
	  process_option (p->tv_u.tv_string);
	  break;
	default:
	  break;
	}
      p++;
    }

  check (register_claim_file, LDPL_FATAL, "register_claim_file not found");
  check (add_symbols, LDPL_FATAL, "add_symbols not found");
  status = register_claim_file (claim_file_handler);
  check (status == LDPS_OK, LDPL_FATAL,
	 "could not register the claim_file callback");

  if (register_cleanup)
    {
      status = register_cleanup (cleanup_handler);
      check (status == LDPS_OK, LDPL_FATAL,
	     "could not register the cleanup callback");
    }

  if (register_all_symbols_read)
    {
      check (get_symbols, LDPL_FATAL, "get_symbols not found");
      status = register_all_symbols_read (all_symbols_read_handler);
      check (status == LDPS_OK, LDPL_FATAL,
	     "could not register the all_symbols_read callback");
    }

  return LDPS_OK;
}
