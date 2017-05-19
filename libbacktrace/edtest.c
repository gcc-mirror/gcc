/* edtest.c -- Test for libbacktrace storage allocation stress handling
   Copyright (C) 2017 Free Software Foundation, Inc.

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

#include "config.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "backtrace.h"
#include "backtrace-supported.h"
#include "internal.h"

#if defined(__MSDOS__) || defined(_WIN32) || defined(__OS2__) || defined (__CYGWIN__)
# define IS_DIR_SEPARATOR(c) ((c) == '/' || (c) == '\\')
#else
# define IS_DIR_SEPARATOR(c) ((c) == '/')
#endif

/* The backtrace state.  */

static void *state;

/* The number of failures.  */

int failures = 0;

static int test1 (void) __attribute__ ((noinline, unused));
static int test1 (void) __attribute__ ((noinline, unused));
extern int f2 (int);
extern int f3 (int, int);

static int
test1 (void)
{
  /* Returning a value here and elsewhere avoids a tailcall which
     would mess up the backtrace.  */
  return f2 (__LINE__) + 1;
}

/* Used to collect backtrace info.  */

struct info
{
  char *filename;
  int lineno;
  char *function;
};

/* Return the base name in a path.  */

static const char *
base (const char *p)
{
  const char *last;
  const char *s;

  last = NULL;
  for (s = p; *s != '\0'; ++s)
    {
      if (IS_DIR_SEPARATOR (*s))
        last = s + 1;
    }
  return last != NULL ? last : p;
}

/* Check an entry in a struct info array.  */

static void
check (const char *name, int index, const struct info *all, int want_lineno,
       const char *want_function, const char *want_file, int *failed)
{
  if (*failed)
    return;
  if (all[index].filename == NULL || all[index].function == NULL)
    {
      fprintf (stderr, "%s: [%d]: missing file name or function name\n",
               name, index);
      *failed = 1;
      return;
    }
  if (strcmp (base (all[index].filename), want_file) != 0)
    {
      fprintf (stderr, "%s: [%d]: got %s expected %s\n", name, index,
               all[index].filename, want_file);
      *failed = 1;
    }
  if (all[index].lineno != want_lineno)
    {
      fprintf (stderr, "%s: [%d]: got %d expected %d\n", name, index,
               all[index].lineno, want_lineno);
      *failed = 1;
    }
  if (strcmp (all[index].function, want_function) != 0)
    {
      fprintf (stderr, "%s: [%d]: got %s expected %s\n", name, index,
               all[index].function, want_function);
      *failed = 1;
    }
}

/* Passed to backtrace callback function.  */

struct bdata
{
  struct info *all;
  size_t index;
  size_t max;
  int failed;
};

/* An error callback passed to backtrace.  */

static void
error_callback_one (void *vdata, const char *msg, int errnum)
{
  struct bdata *data = (struct bdata *) vdata;

  fprintf (stderr, "%s", msg);
  if (errnum > 0)
    fprintf (stderr, ": %s", strerror (errnum));
  fprintf (stderr, "\n");
  data->failed = 1;
}

/* The backtrace callback function.  */

static int
callback_one (void *vdata, uintptr_t pc ATTRIBUTE_UNUSED,
              const char *filename, int lineno, const char *function)
{
  struct bdata *data = (struct bdata *) vdata;
  struct info *p;

  if (data->index >= data->max)
    {
      fprintf (stderr, "callback_one: callback called too many times\n");
      data->failed = 1;
      return 1;
    }

  p = &data->all[data->index];
  if (filename == NULL)
    p->filename = NULL;
  else
    {
      p->filename = strdup (filename);
      assert (p->filename != NULL);
    }
  p->lineno = lineno;
  if (function == NULL)
    p->function = NULL;
  else
    {
      p->function = strdup (function);
      assert (p->function != NULL);
    }
  ++data->index;

  return 0;
}

int
f3 (int f1line, int f2line)
{
  struct info all[20];
  struct bdata data;
  int f3line;
  int i;

  data.all = &all[0];
  data.index = 0;
  data.max = 20;
  data.failed = 0;

  f3line = __LINE__ + 1;
  i = backtrace_full (state, 0, callback_one, error_callback_one, &data);

  if (i != 0)
    {
      fprintf (stderr, "test1: unexpected return value %d\n", i);
      data.failed = 1;
    }

  if (data.index < 3)
    {
      fprintf (stderr,
               "test1: not enough frames; got %zu, expected at least 3\n",
               data.index);
      data.failed = 1;
    }

  check ("test1", 0, all, f3line, "f3", "edtest.c", &data.failed);
  check ("test1", 1, all, f2line, "f2", "edtest2_build.c", &data.failed);
  check ("test1", 2, all, f1line, "test1", "edtest.c", &data.failed);

  printf ("%s: backtrace_full alloc stress\n", data.failed ? "FAIL" : "PASS");

  if (data.failed)
    ++failures;

  return failures;
}

static void
error_callback_create (void *data ATTRIBUTE_UNUSED, const char *msg,
                       int errnum)
{
  fprintf (stderr, "%s", msg);
  if (errnum > 0)
    fprintf (stderr, ": %s", strerror (errnum));
  fprintf (stderr, "\n");
  exit (EXIT_FAILURE);
}

int
main (int argc ATTRIBUTE_UNUSED, char **argv ATTRIBUTE_UNUSED)
{
  state = backtrace_create_state (argv[0], BACKTRACE_SUPPORTS_THREADS,
                                  error_callback_create, NULL);

  // Grab the storage allocation lock prior to doing anything interesting.
  // The intent here is to insure that the backtrace_alloc code is forced
  // to always call mmap() for new memory as opposed to reusing previously
  // allocated memory from the free list. Doing things this way helps
  // simulate what you might see in a multithreaded program in which there
  // are racing calls to the allocator.
  struct backtrace_state *state_internal =
      (struct backtrace_state *) state;
  state_internal->lock_alloc = 1;

  // Kick off the test
  test1();

  exit (failures > 0 ? EXIT_FAILURE : EXIT_SUCCESS);
}
