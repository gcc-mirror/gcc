/* Copyright (C) 1999, 2000, 2001, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <java-props.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <java/lang/System.h>
#include <java/util/Properties.h>

static void
help ()
{
  printf ("Usage: gij [OPTION] ... CLASS [ARGS] ...\n");
  printf ("          to interpret Java bytecodes, or\n");
  printf ("       gij -jar [OPTION] ... JARFILE [ARGS] ...\n");
  printf ("          to execute a jar file\n\n");
  printf ("  --cp LIST         set class path\n");
  printf ("  --classpath LIST  set class path\n");
  printf ("  -DVAR=VAL         define property VAR with value VAL\n");
  printf ("  -?, --help        print this help, then exit\n");
  printf ("  -X                print help on supported -X options, then exit\n");
  printf ("  --ms=NUMBER       set initial heap size\n");
  printf ("  --mx=NUMBER       set maximum heap size\n");
  printf ("  --showversion     print version number, then keep going\n");
  printf ("  --version         print version number, then exit\n");
  printf ("\nOptions can be specified with `-' or `--'.\n");
  printf ("\nSee http://gcc.gnu.org/java/ for information on reporting bugs\n");
  exit (0);
}

static void
version ()
{
  printf ("gij (GNU libgcj) version %s\n\n", __VERSION__);
  printf ("Copyright (C) 2002 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.  There is NO\n");
  printf ("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n");
}

int
main (int argc, const char **argv)
{
  /* We rearrange ARGV so that all the -D options appear near the
     beginning.  */
  int last_D_option = 0;
  bool jar_mode = false;

  int i;
  for (i = 1; i < argc; ++i)
    {
      const char *arg = argv[i];

      /* A non-option stops processing.  */
      if (arg[0] != '-')
	break;
      /* A "--" stops processing.  */
      if (! strcmp (arg, "--"))
	{
	  ++i;
	  break;
	}

      if (! strncmp (arg, "-D", 2))
	{
	  argv[last_D_option++] = arg + 2;
	  continue;
	}

      if (! strcmp (arg, "-jar"))
	{
	  jar_mode = true;
	  continue;
	}

      /* Allow both single or double hyphen for all remaining
	 options.  */
      if (arg[1] == '-')
	++arg;

      if (! strcmp (arg, "-help") || ! strcmp (arg, "-?"))
	help ();
      else if (! strcmp (arg, "-version"))
	{
	  version ();
	  exit (0);
	}
      else if (! strcmp (arg, "-showversion"))
	version ();
      /* FIXME: use getopt and avoid the ugliness here.
	 We at least need to handle the argument in a better way.  */
      else if (! strncmp (arg, "-ms=", 4))
	_Jv_SetInitialHeapSize (arg + 4);
      else if (! strcmp (arg, "-ms"))
	{
	  if (i >= argc - 1)
	    {
	    no_arg:
	      fprintf (stderr, "gij: option requires an argument -- `%s'\n",
		       argv[i]);
	      fprintf (stderr, "Try `gij --help' for more information.\n");
	      exit (1);
	    }
	  _Jv_SetInitialHeapSize (argv[++i]);
	}
      else if (! strncmp (arg, "-mx=", 4))
	_Jv_SetMaximumHeapSize (arg + 4);
      else if (! strcmp (arg, "-mx"))
	{
	  if (i >= argc - 1)
	    goto no_arg;
	  _Jv_SetMaximumHeapSize (argv[++i]);
	}
      else if (! strcmp (arg, "-cp") || ! strcmp (arg, "-classpath"))
	{
	  if (i >= argc - 1)
	    goto no_arg;
	  // We set _Jv_Jar_Class_Path.  If the user specified `-jar'
	  // then the jar code will override this.  This is the
	  // correct behavior.
	  _Jv_Jar_Class_Path = argv[++i];
	}
      else if (arg[1] == 'X')
	{
	  if (arg[2] == '\0')
	    {
	      printf ("gij: currently no -X options are recognized\n");
	      exit (0);
	    }
	  /* Ignore other -X options.  */
	}
      else
	{
	  fprintf (stderr, "gij: unrecognized option -- `%s'\n", argv[i]);
	  fprintf (stderr, "Try `gij --help' for more information.\n");
	  exit (1);
	}
    }

  argv[last_D_option] = NULL;
  _Jv_Compiler_Properties = argv;

  if (argc - i < 1)
    {
      fprintf (stderr, "Usage: gij [OPTION] ... CLASS [ARGS] ...\n");
      fprintf (stderr, "          to invoke CLASS.main, or\n");
      fprintf (stderr, "       gij -jar [OPTION] ... JARFILE [ARGS] ...\n");
      fprintf (stderr, "          to execute a jar file\n");
      fprintf (stderr, "Try `gij --help' for more information.\n");
      exit (1);
    }

  _Jv_RunMain (NULL, argv[i], argc - i, argv + i, jar_mode);
}
