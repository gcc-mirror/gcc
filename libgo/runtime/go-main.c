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
#include "arch.h"
#include "malloc.h"

#undef int
#undef char
#undef unsigned

/* The main function for a Go program.  This records the command line
   parameters, calls the real main function, and returns a zero status
   if the real main function returns.  */

extern char **environ;

/* These functions are created for the main package.  */
extern void __go_init_main (void);
extern void real_main (void) asm ("main.main");

/* The main function.  */

int
main (int argc, char **argv)
{
  runtime_args (argc, (byte **) argv);

  m = &runtime_m0;
  g = &runtime_g0;
  m->curg = g;
  g->m = m;
  runtime_initpanic ();
  runtime_mallocinit ();
  runtime_cpuprofinit ();
  __go_gc_goroutine_init (&argc);

  runtime_goargs();
  runtime_goenvs();

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
