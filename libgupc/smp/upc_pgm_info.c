/* Copyright (c) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc. 
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


#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"


typedef enum
  {
    upc_threads_model_none         = 0,
    upc_threads_model_process      = 1,
    upc_threads_model_pthreads_tls = 2
  } upc_threads_model_t;

typedef struct upc_compiled_thread_info_struct
  {
    struct upc_compiled_thread_info_struct *next;
    char *filename;
    int nthreads;
    int npthreads;
    upc_threads_model_t threads_model;
  } upc_compiled_thread_info_t;
typedef upc_compiled_thread_info_t *upc_compiled_thread_info_p;

/* List of compiled UPC files, and the value of THREADS (and pthreads
   per process) specified at compile-time (-1 means no value given
   at compile-time). */
static upc_compiled_thread_info_p __upc_compiled_thread_info = 0;

static
void
__upc_print_upc_compiled_thread_info (void)
{
   upc_compiled_thread_info_p p;
   fprintf (stderr, "   THREADS   Threads Model  PTHREADS Filename\n");
   for (p = __upc_compiled_thread_info; p; p = p->next)
     {
	if (p->nthreads > 0)
	  {
	    fprintf (stderr, "%10d", p->nthreads);
	  }
	else
	  {
	    fprintf (stderr, " <dynamic>");
	  }
       if (p->threads_model == upc_threads_model_process)
          {
	    fprintf (stderr, "         process");
	  }
       else if (p->threads_model == upc_threads_model_pthreads_tls)
          {
	    fprintf (stderr, "    pthreads-tls");
	  }
	if (p->npthreads > 0)
	  {
	    fprintf (stderr, "%10d", p->npthreads);
	  }
	else
	  {
	    fprintf (stderr, " <dynamic>");
	  }
       fprintf (stderr, " %s\n", p->filename);
     }
}

static
void
__upc_register_pgm_info (char *filename, int nthreads,
                   upc_threads_model_t threads_model, int npthreads)
{
   upc_compiled_thread_info_p info =
	   malloc (sizeof (upc_compiled_thread_info_t));
   upc_compiled_thread_info_p *p;
   /* Sort the list by file name. */
   for (p = &__upc_compiled_thread_info;
	*p && strcmp (filename, (*p)->filename) >= 0;
	p = &(*p)->next) /* loop */;
   info->filename      = filename;
   info->nthreads      = nthreads;
   info->threads_model = threads_model;
   info->npthreads     = npthreads;
   info->next = *p;
   *p = info;
}

static
void
__upc_skip_spaces (const char **s)
{
  while (**s == ' ') ++(*s);
}

static
int
__upc_match_string (const char **s, const char *string)
{
  int slen = strlen(string);
  if (strncmp(*s, string, slen) != 0)
    return 0;
  *s += slen;
  return 1;
}

static
int
__upc_match_until (const char **s, const char *string)
{
  int slen = strlen (string);
  while (**s && (strncmp(*s, string, slen) != 0)) ++(*s);
  if (!**s)
    return 0;
  *s += slen;
  return 1;
}

static
int
__upc_match_num (const char **s, int *num)
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
static
void
__upc_parse_program_info (char *info)
{
  char *filename;
  int nthreads = -1;
  upc_threads_model_t threads_model = upc_threads_model_none;
  int npthreads = -1;
  const char *fname;
  int fname_len;
  const char *s = info;
  if (!__upc_match_string(&s, "$GCCUPCConfig:"))
    return;
  __upc_skip_spaces (&s);
  if (!__upc_match_string(&s, "("))
    return;
  fname = s;
  if (!__upc_match_until(&s, ")"))
    return;
  fname_len = (s - fname - 1);
  filename = (char *)malloc(fname_len + 1);
  strncpy (filename, fname, fname_len);
  filename[fname_len] = '\0';
  while (*s)
    {
      __upc_skip_spaces (&s);
      if (__upc_match_string(&s, "$"))
        {
          break;
        }
      else if (__upc_match_string(&s, "dynamicthreads"))
        {
          nthreads = -1;
        }
      else if (__upc_match_string(&s, "staticthreads="))
        {
	  if (!__upc_match_num(&s, &nthreads))
	    return;
        }
      else if (__upc_match_string(&s, "process"))
        {
	  threads_model = upc_threads_model_process;
        }
      else if (__upc_match_string(&s, "pthreads-tls"))
        {
	  threads_model = upc_threads_model_pthreads_tls;
        }
      else if (__upc_match_string(&s, "dynamicpthreads"))
        {
	  npthreads = -1;
        }
      else if (__upc_match_string(&s, "staticpthreads="))
        {
	  if (!__upc_match_num(&s, &npthreads))
	    return;
        }
      else
        return;
    }
  __upc_register_pgm_info (filename, nthreads, threads_model, npthreads);
}

void
__upc_validate_pgm_info (char *pgm)
{
   upc_compiled_thread_info_p p;
   char *info;
   int nthreads = -1;
   int npthreads = -1;
   /* Process all the strings within the program information section.
      (Ignore intervening null bytes.)  */
   for (info = GUPCR_PGM_INFO_SECTION_START;
        info < GUPCR_PGM_INFO_SECTION_END;
	++info)
     {
       if (*info)
         {
	   __upc_parse_program_info (info);
	   info += strlen(info);
         }
     }
   if (!__upc_compiled_thread_info)
     {
       fprintf (stderr, "%s: UPC Warning: There are no UPC source files"
			" compiled into this program,"
			" or <upc.h> was not included?\n", pgm);
       return;
     }
   for (p = __upc_compiled_thread_info; p; p = p->next)
     {
        if (p->nthreads > 0 && nthreads <= 0)
	  nthreads = p->nthreads;
        if (p->npthreads > 0 && npthreads <= 0)
	  npthreads = p->npthreads;
        /* Static thread/pthread compilations can be intermixed
	   with dynamic threads compilations, but static values must agree.  */
        if (((p->nthreads != nthreads)
	     && (p->nthreads > 0)
	     && (nthreads > 0))
	    || ((p->npthreads != npthreads)
	     && (p->npthreads > 0)
	     && (npthreads > 0))
	    || (p->threads_model != __upc_compiled_thread_info->threads_model))
	  {
	    fprintf (stderr, "%s: UPC error: The UPC source files in this"
			     " program were not compiled with the same value"
			     " of UPC settings.\n", pgm);
	    fprintf (stderr, "%s: A list of each UPC source file and"
			     " its compiled UPC settings follows.\n", pgm);
	    __upc_print_upc_compiled_thread_info ();
	    exit (2);
	  }
     }

#ifndef GUPCR_USE_PTHREADS
  if (__upc_compiled_thread_info->threads_model != upc_threads_model_process)
    {
      fprintf (stderr, "%s: The selected GUPC runtime library"
                       " supports only the process model."
		       " Did you link with the correct runtime library?\n",
		       pgm);
      exit (2);
    }
#else
  if (__upc_compiled_thread_info->threads_model != upc_threads_model_pthreads_tls)
    {
      fprintf (stderr, "%s: The selected GUPC runtime library"
                       " supports only the POSIX threads model."
		       " Did you link with the correct runtime library?\n",
		       pgm);
      exit (2);
    }
#endif /* !GUPCR_USE_PTHREADS */

  THREADS = nthreads;
#ifdef GUPCR_USE_PTHREADS
  UPC_PTHREADS = npthreads;
  if (UPC_PTHREADS == -1)
    {
      UPC_PTHREADS = THREADS;
    }
#endif /* GUPCR_USE_PTHREADS */

}
