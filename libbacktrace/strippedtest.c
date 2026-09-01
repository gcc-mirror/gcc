/* strippedtest.c -- Test for libbacktrace library with no debug info
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

/* Test libbacktrace when there is no debug info.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/* The check function for a stripped executable.  */

static void
stripped_check (const char *name, int index, const struct info *all,
		const char *want_function, int *failed)
{
  if (*failed)
    return;
  if (all[index].filename != NULL)
    {
      fprintf (stderr, "%s: [%d]: unexpected file name in stripped program\n",
	       name, index);
      *failed = 1;
    }
  if (all[index].function == NULL)
    {
      fprintf (stderr, "%s: [%d]: missing function name\n",
	       name, index);
      *failed = 1;
      return;
    }
  if (all[index].lineno != 0)
    {
      fprintf (stderr, "%s: [%d]: got %d expected 0\n", name, index,
	       all[index].lineno);
      *failed = 1;
    }
  if (strcmp (all[index].function, want_function) != 0)
    {
      fprintf (stderr, "%s: [%d]: got %s expected %s\n", name, index,
	       all[index].function, want_function);
      *failed = 1;
    }
}

static int test1 (int) __attribute__ ((noinline, noclone, optnone, unused));
static int f2 (int, int) __attribute__ ((noinline, noclone));
static int f3 (int, int, int) __attribute__ ((noinline, noclone));

static int
test1 (int mdtest)
{
  return f2 (mdtest, __LINE__) + 1;
}

static int
f2 (int mdtest, int f1line)
{
  return f3 (mdtest, f1line, __LINE__) + 2;
}

static int
f3 (int mdtest, int f1line ATTRIBUTE_UNUSED, int f2line ATTRIBUTE_UNUSED)
{
  struct info all[20];
  struct mdinfo mdall[20];
  struct bdata bdata;
  struct mddata mddata;
  void *data;
  backtrace_full_callback callback;
  int i;
  int failed;
  size_t index;

  bdata.all = &all[0];
  bdata.index = 0;
  bdata.max = 20;
  bdata.failed = 0;
  data = (void *) &bdata;
  callback = callback_one;

  if (mdtest)
    {
      mddata.bdata = bdata;
      mddata.mdall = &mdall[0];
      data = (void *) &mddata;
      callback = md_callback_one;
    }

  i = backtrace_full (state, 0, callback, error_callback_one, data);

  failed = mdtest ? mddata.bdata.failed : bdata.failed;

  if (i != 0)
    {
      fprintf (stderr, "test1: unexpected return value %d\n", i);
      failed = 1;
    }

  index = mdtest ? mddata.bdata.index : bdata.index;
  if (index < 3)
    {
      fprintf (stderr,
	       "test1: not enough frames; got %zu, expected at least 3\n",
	       index);
      failed = 1;
    }

  stripped_check ("test1", 0, all, "f3", &failed);
  stripped_check ("test1", 1, all, "f2", &failed);
  stripped_check ("test1", 2, all, "test1", &failed);

  printf ("%s: backtrace_full noinline%s\n", failed ? "FAIL" : "PASS",
	  mdtest ? " mdtest" : "");

  if (failed)
    ++failures;

  return failures;
}

int
main (int argc ATTRIBUTE_UNUSED, char **argv)
{
  state = backtrace_create_state (argv[0], BACKTRACE_SUPPORTS_THREADS,
				  error_callback_create, NULL);

#if BACKTRACE_SUPPORTED
  test1 (0);
#endif

  state = backtrace_create_state (argv[0], BACKTRACE_SUPPORTS_THREADS | 2,
				  error_callback_create, NULL);

#if BACKTRACE_SUPPORTED
  test1 (1);
#endif

  exit (failures ? EXIT_FAILURE : EXIT_SUCCESS);
}
