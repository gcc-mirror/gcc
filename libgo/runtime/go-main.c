/* go-main.c -- the main function for a Go program.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <stdlib.h>
#include <time.h>

#ifdef HAVE_FPU_CONTROL_H
#include <fpu_control.h>
#endif

#include "go-alloc.h"
#include "array.h"
#include "go-signal.h"
#include "go-string.h"

#include "runtime.h"
#include "malloc.h"

#undef int
#undef char
#undef unsigned

/* The main function for a Go program.  This records the command line
   parameters, calls the real main function, and returns a zero status
   if the real main function returns.  */

extern char **environ;

extern struct __go_open_array Args asm ("libgo_os.os.Args");

extern struct __go_open_array Envs asm ("libgo_os.os.Envs");

/* These functions are created for the main package.  */
extern void __go_init_main (void);
extern void real_main (void) asm ("main.main");

/* The main function.  */

int
main (int argc, char **argv)
{
  int i;
  struct __go_string *values;

  runtime_mallocinit ();
  __go_gc_goroutine_init (&argc);

  Args.__count = argc;
  Args.__capacity = argc;
  values = __go_alloc (argc * sizeof (struct __go_string));
  for (i = 0; i < argc; ++i)
    {
      values[i].__data = (unsigned char *) argv[i];
      values[i].__length = __builtin_strlen (argv[i]);
    }
  Args.__values = values;

  for (i = 0; environ[i] != NULL; ++i)
    ;
  Envs.__count = i;
  Envs.__capacity = i;
  values = __go_alloc (i * sizeof (struct __go_string));
  for (i = 0; environ[i] != NULL; ++i)
    {
      values[i].__data = (unsigned char *) environ[i];
      values[i].__length = __builtin_strlen (environ[i]);
    }
  Envs.__values = values;

  __initsig ();

#if defined(HAVE_SRANDOM)
  srandom ((unsigned int) time (NULL));
#else
  srand ((unsigned int) time (NULL));
#endif
  __go_init_main ();

  __go_enable_gc ();

  real_main ();

  return 0;
}
