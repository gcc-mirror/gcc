/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <jvm.h>
#include <gcj/cni.h>
#include <stdio.h>

#include <java/lang/System.h>
#include <java/util/Properties.h>

// This is used to initialize the compiled-in system properties.
const char *_Jv_Compiler_Properties[] =
{
  NULL
};

int
main (int argc, const char **argv)
{
  if (argc < 2)
    {
      printf ("usage: %s CLASS [ARGS]...\n", argv[0]);
      exit (1);
    }

  _Jv_RunMain (argv[1], argc - 1, argv + 1);
}
