/* mdtest.c -- Test for libbacktrace library with moredata flag
   Copyright (C) 2026 Free Software Foundation, Inc.
   Written by Ian Lance Taylor.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

    (3) The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.  */

/* This program tests the externally visible interfaces of the
   libbacktrace library.  */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#include "filenames.h"

#include "backtrace.h"
#include "backtrace-supported.h"
#include "internal.h"

#include "testlib.h"

/* Used to collect moredata info from backtraces.  */

struct mdinfo
{
  int decl_lineno;
};

/* We pass a pointer to this as the data field to the backtrace
   functions.  */

struct mddata
{
  struct bdata bdata;
  struct mdinfo *mdall;
};

/* backtrace_full_callback with moredata flag.  */

static int
md_callback_one (void *vdata, uintptr_t pc, const char *filename, int lineno,
		 const char *function)
{
  struct backtrace_moredata *md = (struct backtrace_moredata *) vdata;
  struct mddata *mddata = (struct mddata *) md->backtrace_data;
  struct bdata *data = &mddata->bdata;

  if (md->backtrace_version != BACKTRACE_MOREDATA_VERSION)
    {
      fprintf (stderr,
	       "md_callback_one: wrong moredata version: got %u, want %u\n",
	       md->backtrace_version, BACKTRACE_MOREDATA_VERSION);
      data->failed = 1;
      return 1;
    }

  if (data->index < data->max)
    mddata->mdall[data->index].decl_lineno = md->backtrace_decl_lineno;

  return callback_one ((void *) data, pc, filename, lineno, function);
}

/* backtrace_syminfo_callback with moredata flag.  */

static void
md_callback_three (void *vdata, uintptr_t pc, const char *symname,
		  uintptr_t symval, uintptr_t symsize)
{
  struct backtrace_moredata *md = (struct backtrace_moredata *) vdata;
  struct symdata *data = (struct symdata *) md->backtrace_data;

  if (md->backtrace_version != BACKTRACE_MOREDATA_VERSION)
    {
      fprintf (stderr,
	       "md_callback_three: wrong moredata version: got %u, want %u\n",
	       md->backtrace_version, BACKTRACE_MOREDATA_VERSION);
      data->failed = 1;
      return;
    }

  if (md->backtrace_decl_lineno != 0)
    {
      fprintf (stderr,
	       "md_callback_three: unexpected non-zero decl_lineno %d\n",
	       md->backtrace_decl_lineno);
      data->failed = 1;
      return;
    }

  callback_three ((void *) data, pc, symname, symval, symsize);
}

/* Test the backtrace function with non-inlined functions.  */

static int test1 (void) __attribute__ ((noinline, noclone, optnone, unused));
static int f2 (int) __attribute__ ((noinline, noclone));
static int f3 (int, int) __attribute__ ((noinline, noclone));

static int test1 (void) {
  return f2 (__LINE__) + 1;
}

static int f2 (int f1line) {
  return f3 (f1line, __LINE__) + 2;
}

static int
f3 (int f1line, int f2line)
{
  struct info all[20];
  struct mdinfo mdall[20];
  struct mddata data;
  int f3line;
  int i;

  data.bdata.all = &all[0];
  data.bdata.index = 0;
  data.bdata.max = 20;
  data.bdata.failed = 0;
  data.mdall = &mdall[0];

  f3line = __LINE__ + 1;
  i = backtrace_full (state, 0, md_callback_one, error_callback_one, &data);

  if (i != 0)
    {
      fprintf (stderr, "test1: unexpected return value %d\n", i);
      data.bdata.failed = 1;
    }

  if (data.bdata.index < 3)
    {
      fprintf (stderr,
	       "test1: not enough frames; got %zu, expected at least 3\n",
	       data.bdata.index);
      data.bdata.failed = 1;
    }

  check ("test1", 0, all, f3line, "f3", "mdtest.c", &data.bdata.failed);
  check ("test1", 1, all, f2line, "f2", "mdtest.c", &data.bdata.failed);
  check ("test1", 2, all, f1line, "test1", "mdtest.c", &data.bdata.failed);

  if (!data.bdata.failed)
    {
      /* This assumes a particular pattern for the definitions of f1 and
	 f2, above.  */
      if (data.mdall[1].decl_lineno != f2line - 1)
	{
	  fprintf (stderr,
		   "test1: incorrect f2 decl_lineno; got %d, expected %d\n",
		   data.mdall[1].decl_lineno, f2line - 1);
	  data.bdata.failed = 1;
	}
      if (data.mdall[2].decl_lineno != f1line - 1)
	{
	  fprintf (stderr,
		   "test1: incorrect f1 decl_lineno; got %d, expected %d\n",
		   data.mdall[2].decl_lineno, f1line - 1);
	  data.bdata.failed = 1;
	}
    }

  printf ("%s: backtrace_full noinline\n", data.bdata.failed ? "FAIL" : "PASS");

  if (data.bdata.failed)
    ++failures;

  return failures;
}

/* Test the backtrace function with inlined functions.  */

static inline int test2 (void) __attribute__ ((always_inline, unused));
static inline int f12 (int) __attribute__ ((always_inline));
static inline int f13 (int, int) __attribute__ ((always_inline));

static inline int
test2 (void)
{
  return f12 (__LINE__) + 1;
}

static inline int
f12 (int f1line)
{
  return f13 (f1line, __LINE__) + 2;
}

static inline int
f13 (int f1line, int f2line)
{
  struct info all[20];
  struct mdinfo mdall[20];
  struct mddata data;
  int f3line;
  int i;

  data.bdata.all = &all[0];
  data.bdata.index = 0;
  data.bdata.max = 20;
  data.bdata.failed = 0;
  data.mdall = &mdall[0];

  f3line = __LINE__ + 1;
  i = backtrace_full (state, 0, md_callback_one, error_callback_one, &data);

  if (i != 0)
    {
      fprintf (stderr, "test2: unexpected return value %d\n", i);
      data.bdata.failed = 1;
    }

  check ("test2", 0, all, f3line, "f13", "mdtest.c", &data.bdata.failed);
  check ("test2", 1, all, f2line, "f12", "mdtest.c", &data.bdata.failed);
  check ("test2", 2, all, f1line, "test2", "mdtest.c", &data.bdata.failed);

  printf ("%s: backtrace_full inline\n", data.bdata.failed ? "FAIL" : "PASS");

  if (data.bdata.failed)
    ++failures;

  return failures;
}

static int test5 (void) __attribute__ ((unused));

int global = 1;

static int
test5 (void)
{
  struct symdata symdata;
  int i;
  uintptr_t addr = (uintptr_t) &global;

  if (sizeof (global) > 1)
    addr += 1;

  symdata.name = NULL;
  symdata.val = 0;
  symdata.size = 0;
  symdata.failed = 0;

  i = backtrace_syminfo (state, addr, md_callback_three,
			 error_callback_three, &symdata);
  if (i == 0)
    {
      fprintf (stderr,
	       "test5: unexpected return value from backtrace_syminfo %d\n",
	       i);
      symdata.failed = 1;
    }

  if (!symdata.failed)
    {
      if (symdata.name == NULL)
	{
	  fprintf (stderr, "test5: NULL syminfo name\n");
	  symdata.failed = 1;
	}
      else if (!(strncmp (symdata.name, "global", 6) == 0
		 && (symdata.name[6] == '\0'|| symdata.name[6] == '.')))
	{
	  fprintf (stderr,
		   "test5: unexpected syminfo name got %s expected %s\n",
		   symdata.name, "global");
	  symdata.failed = 1;
	}
      else if (symdata.val != (uintptr_t) &global)
	{
	  fprintf (stderr,
		   "test5: unexpected syminfo value got %lx expected %lx\n",
		   (unsigned long) symdata.val,
		   (unsigned long) (uintptr_t) &global);
	  symdata.failed = 1;
	}
      else if (symdata.size != sizeof (global) && symdata.size != 0)
	{
	  fprintf (stderr,
		   "test5: unexpected syminfo size got %lx expected %lx\n",
		   (unsigned long) symdata.size,
		   (unsigned long) sizeof (global));
	  symdata.failed = 1;
	}
    }

  printf ("%s: backtrace_syminfo variable\n",
	  symdata.failed ? "FAIL" : "PASS");

  if (symdata.failed)
    ++failures;

  return failures;
}

/* Run all the tests.  */

int
main (int argc ATTRIBUTE_UNUSED, char **argv)
{
  state = backtrace_create_state (argv[0], BACKTRACE_SUPPORTS_THREADS | 2,
				  error_callback_create, NULL);

#if BACKTRACE_SUPPORTED
  test1 ();
  test2 ();
#if BACKTRACE_SUPPORTS_DATA
  test5 ();
#endif
#endif

  exit (failures ? EXIT_FAILURE : EXIT_SUCCESS);
}
