/* go-main.c -- the main function for a Go program.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#ifdef HAVE_FPU_CONTROL_H
#include <fpu_control.h>
#endif

#include "runtime.h"
#include "array.h"
#include "arch.h"

#undef int
#undef char
#undef unsigned

/* The main function for a Go program.  This records the command line
   parameters, calls the real main function, and returns a zero status
   if the real main function returns.  */

extern char **environ;

/* A copy of _end that a shared library can reasonably refer to.  */
uintptr __go_end;

extern byte _end[];

/* The main function.  */

int
main (int argc, char **argv)
{
  runtime_isarchive = false;

  if (runtime_isstarted)
    return 0;
  runtime_isstarted = true;

  if (runtime_iscgo)
    setIsCgo ();

  __go_end = (uintptr)_end;
  runtime_ginit ();
  runtime_cpuinit ();
  runtime_check ();
  runtime_args (argc, (byte **) argv);
  runtime_osinit ();
  runtime_schedinit ();
  __go_go ((uintptr)(runtime_main), NULL);
  runtime_mstart (runtime_m ());
  abort ();
}
