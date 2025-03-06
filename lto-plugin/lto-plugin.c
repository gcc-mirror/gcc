/* LTO plugin for linkers like gold, GNU ld or mold.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

/* The plugin has only one external function: onload. A linker passes it an array of
   function that the plugin uses to communicate back to the linker.

   With the functions provided by the linker, the plugin can be notified when
   the linker first analyzes a file and pass a symbol table back to the linker. The plugin
   is also notified when all symbols have been read and it is time to generate
   machine code for the necessary symbols.

   More information at http://gcc.gnu.org/wiki/whopr/driver.

   This plugin should be passed the lto-wrapper options and will forward them.
   It also has options at his own:
   -debug: Print the command line used to run lto-wrapper.
   -nop: Instead of running lto-wrapper, pass the original to the plugin. This
   only works if the input files are hybrid. 
   -linker-output-known: Do not determine linker output
   -linker-output-auto-nolto-rel: Switch from rel to nolto-rel mode without
   warning.  This is used on systems like VxWorks (kernel) where the link is
   always partial and repeated incremental linking is generally not used.
   -sym-style={none,win32,underscore|uscore}
   -pass-through  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#if HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#if HAVE_PTHREAD_LOCKING
#include <pthread.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WIFEXITED
#define WIFEXITED(S) (((S) & 0xff) == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(S) (((S) & 0xff00) >> 8)
#endif
#include <libiberty.h>
#include <hashtab.h>
#include "../gcc/lto/common.h"
#include "simple-object.h"
#include "plugin-api.h"

/* We need to use I64 instead of ll width-specifier on native Windows.
   The reason for this is that older MS-runtimes don't support the ll.  */
#ifdef __MINGW32__
#define PRI_LL "I64"
#else
#define PRI_LL "ll"
#endif

/* Handle opening elf files on hosts, such as Windows, that may use
   text file handling that will break binary access.  */
#ifndef O_BINARY
# define O_BINARY 0
#endif

/* Segment name for LTO sections.  This is only used for Mach-O.
   FIXME: This needs to be kept in sync with darwin.c.  */

#define LTO_SEGMENT_NAME "__GNU_LTO"

/* Return true if STR string starts with PREFIX.  */

static inline bool
startswith (const char *str, const char *prefix)
{
  return strncmp (str, prefix, strlen (prefix)) == 0;
}

/* The part of the symbol table the plugin has to keep track of. Note that we
   must keep SYMS until all_symbols_read is called to give the linker time to
   copy the symbol information. 
   The id must be 64bit to minimze collisions. */

struct sym_aux
{
  uint32_t slot;
  unsigned long long id;
  unsigned next_conflict;
};

struct plugin_symtab
{
  int nsyms;
  int last_sym;
  struct sym_aux *aux;
  struct ld_plugin_symbol *syms;
  unsigned long long id;
};

/* Encapsulates object file data during symbol scan.  */
struct plugin_objfile
{
  int found;
  bool offload;
  simple_object_read *objfile;
  struct plugin_symtab *out;
  const struct ld_plugin_input_file *file;
};

/* All that we have to remember about a file. */

struct plugin_file_info
{
  char *name;
  void *handle;
  struct plugin_symtab symtab;
  struct plugin_symtab conflicts;
  bool skip_file;
};

/* List item with name of the file with offloading.  */

struct plugin_offload_file
{
  char *name;
  struct plugin_offload_file *next;
};

/* Until ASM_OUTPUT_LABELREF can be hookized and decoupled from
   stdio file streams, we do simple label translation here.  */

enum symbol_style
{
  ss_none,	/* No underscore prefix. */
  ss_win32,	/* Underscore prefix any symbol not beginning with '@'.  */
  ss_uscore,	/* Underscore prefix all symbols.  */
};

#if HAVE_PTHREAD_LOCKING
/* Plug-in mutex.  */
static pthread_mutex_t plugin_lock;

#define LOCK_SECTION pthread_mutex_lock (&plugin_lock)
#define UNLOCK_SECTION pthread_mutex_unlock (&plugin_lock)
#else
#define LOCK_SECTION
#define UNLOCK_SECTION
#endif

static char *arguments_file_name;
static ld_plugin_register_claim_file register_claim_file;
static ld_plugin_register_claim_file_v2 register_claim_file_v2;
static ld_plugin_register_all_symbols_read register_all_symbols_read;
static ld_plugin_get_symbols get_symbols, get_symbols_v2, get_symbols_v3;
static ld_plugin_register_cleanup register_cleanup;
static ld_plugin_add_input_file add_input_file;
static ld_plugin_add_input_library add_input_library;
static ld_plugin_message message;
static ld_plugin_add_symbols add_symbols, add_symbols_v2;
static ld_plugin_get_api_version get_api_version;

/* By default, use version LAPI_V0 if there is not negotiation.  */
static enum linker_api_version api_version = LAPI_V0;

static struct plugin_file_info *claimed_files = NULL;
static unsigned int num_claimed_files = 0;
static unsigned int non_claimed_files = 0;

/* List of files with offloading.  */
static struct plugin_offload_file *offload_files;
/* Last file in the list.  */
static struct plugin_offload_file *offload_files_last;
/* Last non-archive file in the list.  */
static struct plugin_offload_file *offload_files_last_obj;
/* Last LTO file in the list.  */
static struct plugin_offload_file *offload_files_last_lto;
/* Total number of files with offloading.  */
static unsigned num_offload_files;

static char **output_files = NULL;
static unsigned int num_output_files = 0;

static char **lto_wrapper_argv;
static int lto_wrapper_num_args;

static char **pass_through_items = NULL;
static unsigned int num_pass_through_items;

static char *ltrans_objects = NULL;

static bool debug;
static bool save_temps;
static bool flto_incremental;
static bool verbose;
static char nop;
static char *resolution_file = NULL;
static enum ld_plugin_output_file_type linker_output;
static bool linker_output_set;
static bool linker_output_known;
static bool linker_output_auto_nolto_rel;
static const char *link_output_name = NULL;

/* This indicates link_output_name already contains the dot of the
   suffix, so we can skip it in extensions.  */
static bool skip_in_suffix = false;

/* The version of gold being used, or -1 if not gold.  The number is
   MAJOR * 100 + MINOR.  */
static int gold_version = -1;

/* Not used by default, but can be overridden at runtime
   by using -plugin-opt=-sym-style={none,win32,underscore|uscore}
   (in fact, only first letter of style arg is checked.)  */
static enum symbol_style sym_style = ss_none;

static void
check_1 (int gate, enum ld_plugin_level level, const char *text)
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

/* This little wrapper allows check to be called with a non-integer
   first argument, such as a pointer that must be non-NULL.  We can't
   use c99 bool type to coerce it into range, so we explicitly test.  */
#define check(GATE, LEVEL, TEXT) check_1 (((GATE) != 0), (LEVEL), (TEXT))

/* Parse an entry of the IL symbol table. The data to be parsed is pointed
   by P and the result is written in ENTRY. The slot number is stored in SLOT.
   Returns the address of the next entry. */

static char *
parse_table_entry (char *p, struct ld_plugin_symbol *entry, 
		   struct sym_aux *aux)
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

  switch (sym_style)
    {
    case ss_win32:
      if (p[0] == '@')
	{
    /* cf. Duff's device.  */
    case ss_none:
	  entry->name = xstrdup (p);
	  break;
	}
    /* FALL-THROUGH.  */
    case ss_uscore:
      entry->name = concat ("_", p, NULL);
      break;
    default:
      check (0, LDPL_FATAL, "invalid symbol style requested");
      break;
    }
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
    entry->comdat_key = xstrdup (entry->comdat_key);

  entry->unused = entry->section_kind = entry->symbol_type = 0;

  t = *p;
  check (t <= 4, LDPL_FATAL, "invalid symbol kind found");
  entry->def = translate_kind[t];
  p++;

  t = *p;
  check (t <= 3, LDPL_FATAL, "invalid symbol visibility found");
  entry->visibility = translate_visibility[t];
  p++;

  memcpy (&entry->size, p, sizeof (uint64_t));
  p += 8;

  memcpy (&aux->slot, p, sizeof (uint32_t));
  p += 4;

  entry->resolution = LDPR_UNKNOWN;

  aux->next_conflict = -1;

  return p;
}

/* Parse an entry of the IL symbol table. The data to be parsed is pointed
   by P and the result is written in ENTRY. The slot number is stored in SLOT.
   Returns the address of the next entry. */

static char *
parse_table_entry_extension (char *p, struct ld_plugin_symbol *entry)
{
  unsigned char t;
  enum ld_plugin_symbol_type symbol_types[] =
    {
      LDST_UNKNOWN,
      LDST_FUNCTION,
      LDST_VARIABLE,
    };

  t = *p;
  check (t <= 2, LDPL_FATAL, "invalid symbol type found");
  entry->symbol_type = symbol_types[t];
  p++;
  entry->section_kind = *p;
  p++;

  return p;
}


/* Translate the IL symbol table located between DATA and END. Append the
   slots and symbols to OUT. */

static void
translate (char *data, char *end, struct plugin_symtab *out)
{
  struct sym_aux *aux;
  struct ld_plugin_symbol *syms = NULL;
  int n, len;

  /* This overestimates the output buffer sizes, but at least 
     the algorithm is O(1) now. */

  len = (end - data)/8 + out->nsyms + 1;
  syms = xrealloc (out->syms, len * sizeof (struct ld_plugin_symbol));
  aux = xrealloc (out->aux, len * sizeof (struct sym_aux));
  
  for (n = out->nsyms; data < end; n++) 
    { 
      aux[n].id = out->id; 
      data = parse_table_entry (data, &syms[n], &aux[n]);
    }

  assert(n < len);

  out->nsyms = n;
  out->syms = syms;
  out->aux = aux;
}

static void
parse_symtab_extension (char *data, char *end, struct plugin_symtab *out)
{
  unsigned long i;
  unsigned char version;

  if (data >= end)
    /* FIXME: Issue an error ?  */
    return;

  version = *data;
  data++;

  if (version != 1)
    return;

  /* Version 1 contains the following data per entry:
     - symbol_type
     - section_kind
     .  */

  unsigned long nsyms = (end - data) / 2;

  for (i = 0; i < nsyms; i++)
    data = parse_table_entry_extension (data, out->syms + i + out->last_sym);

  out->last_sym += nsyms;
}

/* Free all memory that is no longer needed after writing the symbol
   resolution. */

static void
free_1 (struct plugin_file_info *files, unsigned num_files)
{
  unsigned int i;
  for (i = 0; i < num_files; i++)
    {
      struct plugin_file_info *info = &files[i];
      struct plugin_symtab *symtab = &info->symtab;
      unsigned int j;
      for (j = 0; j < symtab->nsyms; j++)
	{
	  struct ld_plugin_symbol *s = &symtab->syms[j];
	  free (s->name);
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
      free (symtab->aux);
      free (info->name);
    }

  for (i = 0; i < num_output_files; i++)
    free (output_files[i]);
  free (output_files);

  free (claimed_files);
  claimed_files = NULL;
  num_claimed_files = 0;

  while (offload_files)
    {
      struct plugin_offload_file *ofld = offload_files;
      offload_files = offload_files->next;
      free (ofld);
    }
  num_offload_files = 0;

  free (arguments_file_name);
  arguments_file_name = NULL;
}

/* Dump SYMTAB to resolution file F. */

static void
dump_symtab (FILE *f, struct plugin_symtab *symtab)
{
  unsigned j;

  for (j = 0; j < symtab->nsyms; j++)
    {
      uint32_t slot = symtab->aux[j].slot;
      unsigned int resolution = symtab->syms[j].resolution;
      
      assert (resolution != LDPR_UNKNOWN);

      fprintf (f, "%u %" PRI_LL "x %s %s\n",
               (unsigned int) slot, symtab->aux[j].id,
	       lto_resolution_str[resolution], 
	       symtab->syms[j].name);
    }
}

/* Finish the conflicts' resolution information after the linker resolved
   the original symbols */

static void
finish_conflict_resolution (struct plugin_symtab *symtab, 
			   struct plugin_symtab *conflicts)
{
  int i, j;

  if (conflicts->nsyms == 0)
    return;

  for (i = 0; i < symtab->nsyms; i++)
    { 
      char resolution = LDPR_UNKNOWN;

      if (symtab->aux[i].next_conflict == -1)
	continue;

      switch (symtab->syms[i].def) 
	{
	case LDPK_DEF:
	case LDPK_COMMON: /* ??? */
	  resolution = LDPR_RESOLVED_IR; 
	  break;
	case LDPK_WEAKDEF:
	  resolution = LDPR_PREEMPTED_IR;
	  break;
	case LDPK_UNDEF:
	case LDPK_WEAKUNDEF:
	  resolution = symtab->syms[i].resolution;
	  break;
	default:
	  assert (0);
	}

      assert (resolution != LDPR_UNKNOWN);

      for (j = symtab->aux[i].next_conflict; 
	   j != -1; 
	   j = conflicts->aux[j].next_conflict)
	conflicts->syms[j].resolution = resolution;
    }
}

/* Free symbol table SYMTAB. */

static void
free_symtab (struct plugin_symtab *symtab)
{
  free (symtab->syms);
  symtab->syms = NULL;
  free (symtab->aux);
  symtab->aux = NULL;
}

/*  Writes the relocations to disk. */

static void
write_resolution (void)
{
  unsigned int i, included_files = 0;
  FILE *f;

  check (resolution_file, LDPL_FATAL, "resolution file not specified");
  f = fopen (resolution_file, "w");
  check (f, LDPL_FATAL, "could not open file");

  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];
      struct plugin_symtab *symtab = &info->symtab;
      struct ld_plugin_symbol *syms = symtab->syms;

      /* Version 2 of API supports IRONLY_EXP resolution that is
	 accepted by GCC-4.7 and newer.
	 Version 3 can return LDPS_NO_SYMS that means the object
	 will not be used at all.  */
      if (get_symbols_v3)
	{
	  enum ld_plugin_status status
	    = get_symbols_v3 (info->handle, symtab->nsyms, syms);
	  if (status == LDPS_NO_SYMS)
	    {
	      info->skip_file = true;
	      continue;
	    }
	}
      else if (get_symbols_v2)
        get_symbols_v2 (info->handle, symtab->nsyms, syms);
      else
        get_symbols (info->handle, symtab->nsyms, syms);

      ++included_files;

      finish_conflict_resolution (symtab, &info->conflicts);
    }

  fprintf (f, "%d\n", included_files);

  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];
      struct plugin_symtab *symtab = &info->symtab;

      if (info->skip_file)
	continue;

      fprintf (f, "%s %d\n", info->name, symtab->nsyms + info->conflicts.nsyms);
      dump_symtab (f, symtab);
      if (info->conflicts.nsyms)
	{
	  dump_symtab (f, &info->conflicts);
	  free_symtab (&info->conflicts);
	}
    }
  fclose (f);
}

/* Pass files generated by the lto-wrapper to the linker. FD is lto-wrapper's
   stdout. */

static void
add_output_files (FILE *f)
{
  for (;;)
    {
      const unsigned piece = 32;
      char *buf, *s = xmalloc (piece);
      size_t len;

      buf = s;
cont:
      if (!fgets (buf, piece, f))
	{
	  free (s);
	  break;
	}
      len = strlen (s);
      if (s[len - 1] != '\n')
	{
	  s = xrealloc (s, len + piece);
	  buf = s + len;
	  goto cont;
	}
      s[len - 1] = '\0';

      num_output_files++;
      output_files
	= xrealloc (output_files, num_output_files * sizeof (char *));
      output_files[num_output_files - 1] = s;
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

  /* Write argv to a file to avoid a command line that is too long
     Save the file locally on save-temps.  */
  const char *suffix = ".lto_wrapper_args";
  if (skip_in_suffix)
    suffix++;
  if (save_temps && link_output_name)
    arguments_file_name = concat (link_output_name, suffix, NULL);
  else
    arguments_file_name = make_temp_file (".lto_wrapper_args");
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
      /* Check the input argument list for a verbose marker too.  */
      if (a[0] == '-' && a[1] == 'v' && a[2] == '\0')
	{
	  verbose = true;
	  break;
	}
    }

  if (verbose)
    {
      for (i = 0; argv[i]; i++)
	fprintf (stderr, "%s ", argv[i]);
      fprintf (stderr, "\n");
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
  const unsigned num_lto_args
    = num_claimed_files + lto_wrapper_num_args + 2
      + !linker_output_known + !linker_output_auto_nolto_rel;
  unsigned i;
  char **lto_argv;
  const char *linker_output_str = NULL;
  const char **lto_arg_ptr;
  if (num_claimed_files + num_offload_files == 0)
    return LDPS_OK;

  if (nop)
    {
      use_original_files ();
      return LDPS_OK;
    }

  if (ltrans_objects)
    {
      FILE *objs = fopen (ltrans_objects, "r");
      add_output_files (objs);
      fclose (objs);
      return LDPS_OK;
    }

  lto_argv = (char **) xcalloc (sizeof (char *), num_lto_args);
  lto_arg_ptr = (const char **) lto_argv;
  assert (lto_wrapper_argv);

  write_resolution ();

  free_1 (claimed_files, num_claimed_files);

  for (i = 0; i < lto_wrapper_num_args; i++)
    *lto_arg_ptr++ = lto_wrapper_argv[i];

  if (!linker_output_known)
    {
      assert (linker_output_set);
      switch (linker_output)
	{
	case LDPO_REL:
	  if (non_claimed_files)
	    {
	      if (!linker_output_auto_nolto_rel)
		message (LDPL_WARNING, "incremental linking of LTO and non-LTO"
			 " objects; using -flinker-output=nolto-rel which will"
			 " bypass whole program optimization");
	      linker_output_str = "-flinker-output=nolto-rel";
	    }
	  else
	    linker_output_str = "-flinker-output=rel";
	  break;
	case LDPO_DYN:
	  linker_output_str = "-flinker-output=dyn";
	  break;
	case LDPO_PIE:
	  linker_output_str = "-flinker-output=pie";
	  break;
	case LDPO_EXEC:
	  linker_output_str = "-flinker-output=exec";
	  break;
	default:
	  message (LDPL_FATAL, "unsupported linker output %i", linker_output);
	  break;
	}
      *lto_arg_ptr++ = xstrdup (linker_output_str);
    }

  if (num_offload_files > 0)
    {
      FILE *f;
      char *arg;
      char *offload_objects_file_name;
      struct plugin_offload_file *ofld;
      const char *suffix = ".ofldlist";

      if (save_temps && link_output_name)
	{
	  suffix += skip_in_suffix;
	  offload_objects_file_name = concat (link_output_name, suffix, NULL);
	}
      else
	offload_objects_file_name = make_temp_file (suffix);
      check (offload_objects_file_name, LDPL_FATAL,
	     "Failed to generate a temporary file name");
      f = fopen (offload_objects_file_name, "w");
      check (f, LDPL_FATAL, "could not open file with offload objects");
      fprintf (f, "%u\n", num_offload_files);

      /* Skip the dummy item at the start of the list.  */
      ofld = offload_files->next;
      while (ofld)
	{
	  fprintf (f, "%s\n", ofld->name);
	  ofld = ofld->next;
	}
      fclose (f);

      arg = concat ("-foffload-objects=", offload_objects_file_name, NULL);
      check (arg, LDPL_FATAL, "could not allocate");
      *lto_arg_ptr++ = arg;
    }

  for (i = 0; i < num_claimed_files; i++)
    {
      struct plugin_file_info *info = &claimed_files[i];

      if (!info->skip_file)
	*lto_arg_ptr++ = info->name;
    }

  *lto_arg_ptr++ = NULL;
  exec_lto_wrapper (lto_argv);

  free (lto_argv);

  /* --pass-through is not needed when using gold 1.11 or later.  */
  if (pass_through_items && gold_version < 111)
    {
      unsigned int i;
      for (i = 0; i < num_pass_through_items; i++)
        {
	  if (startswith (pass_through_items[i], "-l"))
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

/* Helper, as used in collect2.  */
static int
file_exists (const char *name)
{
  return access (name, R_OK) == 0;
}

/* Unlink FILE unless we have save-temps set.
   Note that we're saving files if verbose output is set. */

static void
maybe_unlink (const char *file)
{
  if (save_temps && file_exists (file))
    {
      if (verbose)
	fprintf (stderr, "[Leaving %s]\n", file);
      return;
    }

  unlink_if_ordinary (file);
}

/* Remove temporary files at the end of the link. */

static enum ld_plugin_status
cleanup_handler (void)
{
  unsigned int i;

  if (debug)
    return LDPS_OK;

  if (arguments_file_name)
    maybe_unlink (arguments_file_name);

  if (!flto_incremental)
    for (i = 0; i < num_output_files; i++)
      maybe_unlink (output_files[i]);
  else
    {
      /* Keep files in ltrans cache.  */
      const char* suffix = ".ltrans.o";
      for (i = 0; i < num_output_files; i++)
	{
	  int offset = strlen (output_files[i]) - strlen (suffix);
	  if (offset < 0 || strcmp (output_files[i] + offset, suffix))
	    maybe_unlink (output_files[i]);
	}
    }

  free_2 ();
  return LDPS_OK;
}

#define SWAP(type, a, b) \
  do { type tmp_; tmp_ = (a); (a) = (b); (b) = tmp_; } while(0)

/* Compare two hash table entries */

static int eq_sym (const void *a, const void *b)
{
  const struct ld_plugin_symbol *as = (const struct ld_plugin_symbol *)a;
  const struct ld_plugin_symbol *bs = (const struct ld_plugin_symbol *)b;

  return !strcmp (as->name, bs->name);
}

/* Hash a symbol */

static hashval_t hash_sym (const void *a)
{
  const struct ld_plugin_symbol *as = (const struct ld_plugin_symbol *)a;

  return htab_hash_string (as->name);
}

/* Determine how strong a symbol is */

static int symbol_strength (struct ld_plugin_symbol *s)
{
  switch (s->def) 
    { 
    case LDPK_UNDEF:
    case LDPK_WEAKUNDEF:
      return 0;
    case LDPK_WEAKDEF:
      return 1;
    default:
      return 2;
    }
}

/* In the ld -r case we can get dups in the LTO symbol tables, where
   the same symbol can have different resolutions (e.g. undefined and defined).

   We have to keep that in the LTO symbol tables, but the dups confuse
   gold and then finally gcc by supplying incorrect resolutions.

   Problem is that the main gold symbol table doesn't know about subids
   and does not distingush the same symbols in different states.

   So we drop duplicates from the linker visible symbol table
   and keep them in a private table. Then later do own symbol
   resolution for the duplicated based on the results for the
   originals.

   Then when writing out the resolution file readd the dropped symbols.
   
   XXX how to handle common? */

static void
resolve_conflicts (struct plugin_symtab *t, struct plugin_symtab *conflicts)
{
  htab_t symtab = htab_create (t->nsyms, hash_sym, eq_sym, NULL);
  int i;
  int out;
  int outlen;

  outlen = t->nsyms;
  conflicts->syms = xmalloc (sizeof (struct ld_plugin_symbol) * outlen);
  conflicts->aux = xmalloc (sizeof (struct sym_aux) * outlen);

  /* Move all duplicate symbols into the auxiliary conflicts table. */
  out = 0;
  for (i = 0; i < t->nsyms; i++) 
    {
      struct ld_plugin_symbol *s = &t->syms[i];
      struct sym_aux *aux = &t->aux[i];
      void **slot;

      slot = htab_find_slot (symtab, s, INSERT);
      if (*slot != NULL)
	{
	  int cnf;
	  struct ld_plugin_symbol *orig = (struct ld_plugin_symbol *)*slot;
	  struct sym_aux *orig_aux = &t->aux[orig - t->syms];

	  /* Always let the linker resolve the strongest symbol */
	  if (symbol_strength (orig) < symbol_strength (s)) 
	    {
	      SWAP (struct ld_plugin_symbol, *orig, *s);
	      SWAP (uint32_t, orig_aux->slot, aux->slot);
	      SWAP (unsigned long long, orig_aux->id, aux->id);
	      /* Don't swap conflict chain pointer */
	    } 

	  /* Move current symbol into the conflicts table */
	  cnf = conflicts->nsyms++;
	  conflicts->syms[cnf] = *s;
	  conflicts->aux[cnf] = *aux;
	  aux = &conflicts->aux[cnf];

	  /* Update conflicts chain of the original symbol */
	  aux->next_conflict = orig_aux->next_conflict;
	  orig_aux->next_conflict = cnf;

	  continue;
	}

      /* Remove previous duplicates in the main table */
      if (out < i)
	{
	  t->syms[out] = *s;
	  t->aux[out] = *aux;
	}

      /* Put original into the hash table */
      *slot = &t->syms[out];
      out++;
    }

  assert (conflicts->nsyms <= outlen);
  assert (conflicts->nsyms + out == t->nsyms);
  
  t->nsyms = out;
  htab_delete (symtab);
}

/* Process one section of an object file.  */

static int 
process_symtab (void *data, const char *name, off_t offset, off_t length)
{
  struct plugin_objfile *obj = (struct plugin_objfile *)data;
  char *s;
  char *secdatastart, *secdata;

  if (!startswith (name, ".gnu.lto_.symtab"))
    return 1;

  s = strrchr (name, '.');
  if (s)
    sscanf (s, ".%" PRI_LL "x", &obj->out->id);
  secdata = secdatastart = xmalloc (length);
  offset += obj->file->offset;
  if (offset != lseek (obj->file->fd, offset, SEEK_SET))
    goto err;

  do
    {
      ssize_t got = read (obj->file->fd, secdata, length);
      if (got == 0)
	break;
      else if (got > 0)
	{
	  secdata += got;
	  length -= got;
	}
      else if (errno != EINTR)
	goto err;
    }
  while (length > 0);
  if (length > 0)
    goto err;

  translate (secdatastart, secdata, obj->out);
  obj->found++;
  free (secdatastart);
  return 1;

err:
  if (message)
    message (LDPL_FATAL, "%s: corrupt object file", obj->file->name);
  /* Force claim_file_handler to abandon this file.  */
  obj->found = 0;
  free (secdatastart);
  return 0;
}

/* Process one section of an object file.  */

static int
process_symtab_extension (void *data, const char *name, off_t offset,
			  off_t length)
{
  struct plugin_objfile *obj = (struct plugin_objfile *)data;
  char *s;
  char *secdatastart, *secdata;

  if (!startswith (name, ".gnu.lto_.ext_symtab"))
    return 1;

  s = strrchr (name, '.');
  if (s)
    sscanf (s, ".%" PRI_LL "x", &obj->out->id);
  secdata = secdatastart = xmalloc (length);
  offset += obj->file->offset;
  if (offset != lseek (obj->file->fd, offset, SEEK_SET))
    goto err;

  do
    {
      ssize_t got = read (obj->file->fd, secdata, length);
      if (got == 0)
	break;
      else if (got > 0)
	{
	  secdata += got;
	  length -= got;
	}
      else if (errno != EINTR)
	goto err;
    }
  while (length > 0);
  if (length > 0)
    goto err;

  parse_symtab_extension (secdatastart, secdata, obj->out);
  obj->found++;
  free (secdatastart);
  return 1;

err:
  if (message)
    message (LDPL_FATAL, "%s: corrupt object file", obj->file->name);
  /* Force claim_file_handler to abandon this file.  */
  obj->found = 0;
  free (secdatastart);
  return 0;
}


/* Find an offload section of an object file.  */

static int
process_offload_section (void *data, const char *name, off_t offset, off_t len)
{
  if (startswith (name, ".gnu.offload_lto_.opts"))
    {
      struct plugin_objfile *obj = (struct plugin_objfile *) data;
      obj->offload = true;
      return 0;
    }

  return 1;
}

/* Callback used by a linker to check if the plugin can claim FILE.
   Writes the result in CAN_BE_CLAIMED.  If KNOWN_USED != 0, the object
   is known by the linker to be included in link output, or an older API
   version is in use that does not provide that information.  Otherwise,
   the linker is only determining whether this is a plugin object and
   only the symbol table is needed by the linker.  In this case, the
   object should not be included in link output and this function will
   be called by the linker again with KNOWN_USED != 0 after the linker
   decides the object should be included in link output. */

static enum ld_plugin_status
claim_file_handler_v2 (const struct ld_plugin_input_file *file,
		       int *can_be_claimed, int known_used)
{
  enum ld_plugin_status status;
  struct plugin_objfile obj;
  struct plugin_file_info lto_file;
  int err;
  const char *errmsg;

  memset (&lto_file, 0, sizeof (struct plugin_file_info));

  if (file->offset != 0)
    {
      /* We pass the offset of the actual file, not the archive header.
         Can't use PRIx64, because that's C99, so we have to print the
	 64-bit hex int as two 32-bit ones.  Use xasprintf instead of
	 asprintf because asprintf doesn't work as expected on some older
	 mingw32 hosts.  */
      int lo, hi;
      lo = file->offset & 0xffffffff;
      hi = ((int64_t)file->offset >> 32) & 0xffffffff;
      lto_file.name = hi ? xasprintf ("%s@0x%x%08x", file->name, hi, lo)
      			 : xasprintf ("%s@0x%x", file->name, lo);
    }
  else
    {
      lto_file.name = xstrdup (file->name);
    }
  lto_file.handle = file->handle;

  *can_be_claimed = 0;
  obj.file = file;
  obj.found = 0;
  obj.offload = false;
  obj.out = &lto_file.symtab;
  errmsg = NULL;
  obj.objfile = simple_object_start_read (file->fd, file->offset, LTO_SEGMENT_NAME,
			&errmsg, &err);
  /* No file, but also no error code means unrecognized format; just skip it.  */
  if (!obj.objfile && !err)
    goto err;

   if (obj.objfile)
    {
      errmsg = simple_object_find_sections (obj.objfile, process_symtab, &obj,
					    &err);
      /*  Parsing symtab extension should be done only for add_symbols_v2 and
	  later versions.  */
      if (!errmsg && add_symbols_v2 != NULL)
	{
	  obj.out->last_sym = 0;
	  errmsg = simple_object_find_sections (obj.objfile,
						process_symtab_extension,
						&obj, &err);
	}
    }

  if (!obj.objfile || errmsg)
    {
      if (err && message)
	message (LDPL_FATAL, "%s: %s: %s", file->name, errmsg,
		xstrerror (err));
      else if (message)
	message (LDPL_FATAL, "%s: %s", file->name, errmsg);
      goto err;
    }

  if (obj.objfile)
    simple_object_find_sections (obj.objfile, process_offload_section,
				 &obj, &err);

  if (obj.found == 0 && !obj.offload)
    goto err;

  if (obj.found > 1)
    resolve_conflicts (&lto_file.symtab, &lto_file.conflicts);

  if (obj.found > 0)
    {
      if (add_symbols_v2)
	status = add_symbols_v2 (file->handle, lto_file.symtab.nsyms,
				 lto_file.symtab.syms);
      else
	status = add_symbols (file->handle, lto_file.symtab.nsyms,
			      lto_file.symtab.syms);
      check (status == LDPS_OK, LDPL_FATAL, "could not add symbols");

      /* Include it only if it is known to be used for link output.  */
      if (known_used)
	{
	  LOCK_SECTION;
	  num_claimed_files++;
	  claimed_files =
	    xrealloc (claimed_files,
		      num_claimed_files * sizeof (struct plugin_file_info));
	  claimed_files[num_claimed_files - 1] = lto_file;
	  UNLOCK_SECTION;
	}

      *can_be_claimed = 1;
    }

  LOCK_SECTION;
  if (offload_files == NULL)
    {
      /* Add dummy item to the start of the list.  */
      offload_files = xmalloc (sizeof (struct plugin_offload_file));
      offload_files->name = NULL;
      offload_files->next = NULL;
      offload_files_last = offload_files;
    }

  /* If this is an LTO file without offload, and it is the first LTO file, save
     the pointer to the last offload file in the list.  Further offload LTO
     files will be inserted after it, if any.  */
  if (*can_be_claimed && !obj.offload && offload_files_last_lto == NULL)
    offload_files_last_lto = offload_files_last;

  if (obj.offload && known_used)
    {
      /* Add file to the list.  The order must be exactly the same as the final
	 order after recompilation and linking, otherwise host and target tables
	 with addresses wouldn't match.  If a static library contains both LTO
	 and non-LTO objects, ld and gold link them in a different order.  */
      struct plugin_offload_file *ofld
	= xmalloc (sizeof (struct plugin_offload_file));
      ofld->name = lto_file.name;
      ofld->next = NULL;

      if (*can_be_claimed
	  && offload_files_last_lto == NULL
	  && file->offset != 0
	  && gold_version == -1)
	{
	  /* ld only: insert first LTO file from the archive after the last real
	     object file immediately preceding the archive, or at the begin of
	     the list if there was no real objects before archives.  */
	  if (offload_files_last_obj != NULL)
	    {
	      ofld->next = offload_files_last_obj->next;
	      offload_files_last_obj->next = ofld;
	    }
	  else
	    {
	      ofld->next = offload_files->next;
	      offload_files->next = ofld;
	    }
	}
      else if (*can_be_claimed && offload_files_last_lto != NULL)
	{
	  /* Insert LTO file after the last LTO file in the list.  */
	  ofld->next = offload_files_last_lto->next;
	  offload_files_last_lto->next = ofld;
	}
      else
	/* Add non-LTO file or first non-archive LTO file to the end of the
	   list.  */
	offload_files_last->next = ofld;

      if (ofld->next == NULL)
	offload_files_last = ofld;
      if (file->offset == 0)
	offload_files_last_obj = ofld;
      if (*can_be_claimed)
	offload_files_last_lto = ofld;
      num_offload_files++;
    }

  UNLOCK_SECTION;

  goto cleanup;

 err:
  LOCK_SECTION;
  non_claimed_files++;
  UNLOCK_SECTION;
  free (lto_file.name);

 cleanup:
  if (obj.objfile)
    simple_object_release_read (obj.objfile);

  return LDPS_OK;
}

/* Callback used by a linker to check if the plugin will claim FILE. Writes
   the result in CLAIMED. */

static enum ld_plugin_status
claim_file_handler (const struct ld_plugin_input_file *file, int *claimed)
{
  return claim_file_handler_v2 (file, claimed, true);
}

/* Parse the plugin options. */

static void
process_option (const char *option)
{
  if (strcmp (option, "-linker-output-known") == 0)
    linker_output_known = true;
  /* Also accept "notlo" for backwards compatibility.  */
  else if ((strcmp (option, "-linker-output-auto-nolto-rel") == 0)
           || (strcmp (option, "-linker-output-auto-notlo-rel") == 0))
    linker_output_auto_nolto_rel = true;
  else if (strcmp (option, "-debug") == 0)
    debug = true;
  else if ((strcmp (option, "-v") == 0)
           || (strcmp (option, "--verbose") == 0))
    verbose = true;
  else if (strcmp (option, "-save-temps") == 0)
    save_temps = true;
  else if (strcmp (option, "-nop") == 0)
    nop = 1;
  else if (startswith (option, "-pass-through="))
    {
      num_pass_through_items++;
      pass_through_items = xrealloc (pass_through_items,
				     num_pass_through_items * sizeof (char *));
      pass_through_items[num_pass_through_items - 1] =
          xstrdup (option + strlen ("-pass-through="));
    }
  else if (startswith (option, "-sym-style="))
    {
      switch (option[sizeof ("-sym-style=") - 1])
	{
	case 'w':
	  sym_style = ss_win32;
	  break;
	case 'u':
	  sym_style = ss_uscore;
	  break;
	default:
	  sym_style = ss_none;
	  break;
	}
    }
  else if (startswith (option, "-ltrans-objects="))
    ltrans_objects = xstrdup (option + strlen ("-ltrans-objects="));
  else
    {
      int size;
      char *opt = xstrdup (option);
      lto_wrapper_num_args += 1;
      size = lto_wrapper_num_args * sizeof (char *);
      lto_wrapper_argv = (char **) xrealloc (lto_wrapper_argv, size);
      lto_wrapper_argv[lto_wrapper_num_args - 1] = opt;
      if (startswith (option, "-fresolution="))
	resolution_file = opt + sizeof ("-fresolution=") - 1;
    }
  save_temps = save_temps || debug;
  verbose = verbose || debug;
}

/* Negotiate linker API version.  */

static void
negotiate_api_version (void)
{
  const char *linker_identifier;
  const char *linker_version;

  enum linker_api_version supported_api = LAPI_V0;
#if HAVE_PTHREAD_LOCKING
  supported_api = LAPI_V1;
#endif

  api_version = get_api_version ("GCC", BASE_VERSION, LAPI_V0,
				 supported_api, &linker_identifier, &linker_version);
  if (api_version > supported_api)
    {
      fprintf (stderr, "requested an unsupported API version (%d)\n", api_version);
      abort ();
    }

  switch (api_version)
    {
    case LAPI_V0:
      break;
    case LAPI_V1:
      check (get_symbols_v3, LDPL_FATAL,
	     "get_symbols_v3 required for API version 1");
      check (add_symbols_v2, LDPL_FATAL,
	     "add_symbols_v2 required for API version 1");
      break;
    default:
      fprintf (stderr, "unsupported API version (%d)\n", api_version);
      abort ();
    }
}

/* Called by a linker after loading the plugin. TV is the transfer vector. */

enum ld_plugin_status
onload (struct ld_plugin_tv *tv)
{
  struct ld_plugin_tv *p;
  enum ld_plugin_status status;

#if HAVE_PTHREAD_LOCKING
  if (pthread_mutex_init (&plugin_lock, NULL) != 0)
    {
      fprintf (stderr, "mutex init failed\n");
      abort ();
    }
#endif

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
	case LDPT_REGISTER_CLAIM_FILE_HOOK_V2:
	  register_claim_file_v2 = p->tv_u.tv_register_claim_file_v2;
	  break;
	case LDPT_ADD_SYMBOLS_V2:
	  add_symbols_v2 = p->tv_u.tv_add_symbols;
	  break;
	case LDPT_ADD_SYMBOLS:
	  add_symbols = p->tv_u.tv_add_symbols;
	  break;
	case LDPT_REGISTER_ALL_SYMBOLS_READ_HOOK:
	  register_all_symbols_read = p->tv_u.tv_register_all_symbols_read;
	  break;
	case LDPT_GET_SYMBOLS_V3:
	  get_symbols_v3 = p->tv_u.tv_get_symbols;
	  break;
	case LDPT_GET_SYMBOLS_V2:
	  get_symbols_v2 = p->tv_u.tv_get_symbols;
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
	case LDPT_GOLD_VERSION:
	  gold_version = p->tv_u.tv_val;
	  break;
	case LDPT_LINKER_OUTPUT:
	  linker_output = (enum ld_plugin_output_file_type) p->tv_u.tv_val;
	  linker_output_set = true;
	  break;
	case LDPT_OUTPUT_NAME:
	  /* We only use this to make user-friendly temp file names.  */
	  link_output_name = p->tv_u.tv_string;
	  break;
	case LDPT_GET_API_VERSION:
	  get_api_version = p->tv_u.tv_get_api_version;
	  break;
	default:
	  break;
	}
      p++;
    }

  if (get_api_version)
    negotiate_api_version ();

  check (register_claim_file, LDPL_FATAL, "register_claim_file not found");
  check (add_symbols, LDPL_FATAL, "add_symbols not found");
  status = register_claim_file (claim_file_handler);
  check (status == LDPS_OK, LDPL_FATAL,
	 "could not register the claim_file callback");

  if (register_claim_file_v2)
    {
      status = register_claim_file_v2 (claim_file_handler_v2);
      check (status == LDPS_OK, LDPL_FATAL,
	     "could not register the claim_file_v2 callback");
    }

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

  char *collect_gcc_options = getenv ("COLLECT_GCC_OPTIONS");
  if (collect_gcc_options)
    {
      /* Support -fno-use-linker-plugin by failing to load the plugin
	 for the case where it is auto-loaded by BFD.  */
      if (strstr (collect_gcc_options, "'-fno-use-linker-plugin'"))
	return LDPS_ERR;

      if (strstr (collect_gcc_options, "'-save-temps'"))
	save_temps = true;

      if (strstr (collect_gcc_options, "'-flto-incremental="))
	flto_incremental = true;

      if (strstr (collect_gcc_options, "'-v'")
          || strstr (collect_gcc_options, "'--verbose'"))
	verbose = true;

      const char *p;
      if ((p = strstr (collect_gcc_options, "'-dumpdir'")))
	{
	  p += sizeof ("'-dumpdir'");
	  while (*p == ' ')
	    p++;
	  const char *start = p;
	  int ticks = 0, escapes = 0;
	  /* Count ticks (') and escaped (\.) characters.  Stop at the
	     end of the options or at a blank after an even number of
	     ticks (not counting escaped ones.  */
	  for (p = start; *p; p++)
	    {
	      if (*p == '\'')
		{
		  ticks++;
		  continue;
		}
	      else if ((ticks % 2) != 0)
		{
		  if (*p == ' ')
		    break;
		  if (*p == '\\')
		    {
		      if (*++p)
			escapes++;
		      else
			p--;
		    }
		}
	    }

	  /* Now allocate a new link_output_name and decode dumpdir
	     into it.  The loop uses the same logic, except it counts
	     ticks and escapes backwards (so ticks is adjusted if we
	     find an odd number of them), and it copies characters
	     that are escaped or not otherwise skipped.  */
	  int len = p - start - ticks - escapes + 1;
	  char *q = xmalloc (len);
	  link_output_name = q;
	  int oddticks = (ticks % 2);
	  ticks += oddticks;
	  for (p = start; *p; p++)
	    {
	      if (*p == '\'')
		{
		  ticks--;
		  continue;
		}
	      else if ((ticks % 2) != 0)
		{
		  if (*p == ' ')
		    break;
		  if (*p == '\\')
		    {
		      if (*++p)
			escapes--;
		      else
			p--;
		    }
		}
	      *q++ = *p;
	    }
	  *q = '\0';
	  assert (escapes == 0);
	  assert (ticks == oddticks);
	  assert (q - link_output_name == len - 1);
	  skip_in_suffix = true;
	}
    }

  return LDPS_OK;
}
