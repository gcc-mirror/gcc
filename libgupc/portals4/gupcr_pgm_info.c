/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/**
 * @file gupcr_pgm_info.c
 * GUPC Runtime program information routines
 */

/**
 * @addtogroup GUPCUTILS GUPCR Utility Functions
 * @{
 */

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_utils.h"


typedef enum
{
  upc_threads_model_none = 0,
  upc_threads_model_process = 1,
} upc_threads_model_t;

typedef struct upc_compiled_thread_info_struct
{
    /** Next on the list of files */
  struct upc_compiled_thread_info_struct *next;
    /** File name */
  char *filename;
    /** Number of compiled threads for the file */
  int nthreads;
    /** Thread's model (process/pthreads) */
  upc_threads_model_t threads_model;
} upc_compiled_thread_info_t;
typedef upc_compiled_thread_info_t *upc_compiled_thread_info_p;

/* List of compiled UPC files, and the value of THREADS
   specified at compile-time (-1 means no value given
   at compile-time). */
static upc_compiled_thread_info_p gupcr_compiled_thread_info = 0;

static void
gupcr_print_upc_compiled_thread_info (void)
{
  upc_compiled_thread_info_p p;
  gupcr_error ("   THREADS   Threads Model  Filename\n");
  for (p = gupcr_compiled_thread_info; p; p = p->next)
    {
      if (p->nthreads > 0)
	gupcr_error ("%10d", p->nthreads);
      else
	gupcr_error (" <dynamic>");
      if (p->threads_model == upc_threads_model_process)
	gupcr_error ("         process");
      gupcr_error (" %s\n", p->filename);
    }
}

static void
gupcr_register_pgm_info (char *filename, int nthreads,
			 upc_threads_model_t threads_model)
{
  upc_compiled_thread_info_p info, *p;
  gupcr_malloc (info, (sizeof (upc_compiled_thread_info_t)));
  /* Insertion into list is ordered by file name. */
  for (p = &gupcr_compiled_thread_info;
       *p && strcmp (filename, (*p)->filename) >= 0;
       p = &(*p)->next) /* loop */ ;
  info->filename = filename;
  info->nthreads = nthreads;
  info->threads_model = threads_model;
  info->next = *p;
  *p = info;
}

static void
gupcr_skip_spaces (const char **s)
{
  while (**s == ' ')
    ++(*s);
}

static int
gupcr_match_string (const char **s, const char *string)
{
  int slen = strlen (string);
  if (strncmp (*s, string, slen) != 0)
    return 0;
  *s += slen;
  return 1;
}

static int
gupcr_match_until (const char **s, const char *string)
{
  int slen = strlen (string);
  while (**s && (strncmp (*s, string, slen) != 0))
    ++(*s);
  if (!**s)
    return 0;
  *s += slen;
  return 1;
}

static int
gupcr_match_num (const char **s, int *num)
{
  *num = 0;
  while (**s >= '0' && **s <= '9')
    {
      *num = *num * 10 + (**s - '0');
      ++(*s);
    }
  if (*num == 0)
    return 0;
  return 1;
}

/* Examples:
 $GCCUPCConfig: (t.upc) dynamicthreads process$
 $GCCUPCConfig: (t.upc) staticcthreads=4 pthreads-tls staticpthreads=4$ */
static void
gupcr_parse_program_info (char *info)
{
  char *filename;
  int nthreads = -1;
  upc_threads_model_t threads_model = upc_threads_model_none;
  const char *fname;
  int fname_len;
  const char *s = info;
  if (!gupcr_match_string (&s, "$GCCUPCConfig:"))
    return;
  gupcr_skip_spaces (&s);
  if (!gupcr_match_string (&s, "("))
    return;
  fname = s;
  if (!gupcr_match_until (&s, ")"))
    return;
  fname_len = (s - fname - 1);
  gupcr_malloc (filename, (fname_len + 1));
  strncpy (filename, fname, fname_len);
  filename[fname_len] = '\0';
  while (*s)
    {
      gupcr_skip_spaces (&s);
      if (gupcr_match_string (&s, "$"))
	{
	  break;
	}
      else if (gupcr_match_string (&s, "dynamicthreads"))
	{
	  nthreads = -1;
	}
      else if (gupcr_match_string (&s, "staticthreads="))
	{
	  if (!gupcr_match_num (&s, &nthreads))
	    return;
	}
      else if (gupcr_match_string (&s, "process"))
	{
	  threads_model = upc_threads_model_process;
	}
      else
	return;
    }
  gupcr_register_pgm_info (filename, nthreads, threads_model);
}

void
gupcr_validate_pgm_info (void)
{
  upc_compiled_thread_info_p p;
  char *info;
  int nthreads = -1;
  /* Process all the strings within the program information section.
     (Ignore intervening null bytes.)  */
  for (info = GUPCR_PGM_INFO_SECTION_START;
       info < GUPCR_PGM_INFO_SECTION_END; ++info)
    {
      if (*info)
	{
	  gupcr_parse_program_info (info);
	  info += strlen (info);
	}
    }
  if (!gupcr_compiled_thread_info)
    gupcr_abort_with_msg ("there are no UPC source files "
			  "compiled into this program, "
			  "or perhaps <upc.h> was not included");
  for (p = gupcr_compiled_thread_info; p; p = p->next)
    {
      if (p->nthreads > 0 && nthreads <= 0)
	nthreads = p->nthreads;
      /* Static threads compilations can be intermixed
         with dynamic threads compilations, but the static values
         must agree.  */
      if (((p->nthreads != nthreads)
	   && (p->nthreads > 0)
	   && (nthreads > 0))
	  || (p->threads_model != gupcr_compiled_thread_info->threads_model))
	{
	  gupcr_assert (MYTHREAD >= 0);
	  if (!MYTHREAD)
	    {
	      gupcr_error ("the UPC source files in this "
			   "program were not compiled with the same value "
			   "of UPC settings;\n"
			   "a list of each UPC source file and "
			   "its compiled UPC settings follows");
	      gupcr_print_upc_compiled_thread_info ();
	    }
	  exit (2);
	}
    }
  THREADS = nthreads;
}

/** @} */
