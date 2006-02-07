/* Copyright (C) 1999-2006  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details. */

#include <config.h>

#include <jvm.h>
#include <gcj/cni.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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
  printf ("  --verbose[:class] print information about class loading\n");
  printf ("  --showversion     print version number, then keep going\n");
  printf ("  --version         print version number, then exit\n");
  printf ("\nOptions can be specified with `-' or `--'.\n");
  printf ("\nSee http://gcc.gnu.org/java/ for information on reporting bugs\n");
  exit (0);
}

static void
version ()
{
  printf ("java version \"" JV_VERSION "\"\n");
  printf ("gij (GNU libgcj) version %s\n\n", __VERSION__);
  printf ("Copyright (C) 2006 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.  There is NO\n");
  printf ("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n");
}

static void
nonstandard_opts_help ()
{
  printf ("  -Xms<size>         set initial heap size\n");
  printf ("  -Xmx<size>         set maximum heap size\n");
  printf ("  -Xss<size>         set thread stack size\n");
  exit (0);
}

static void
add_option (JvVMInitArgs& vm_args, char const* option, void const* extra)
{
  vm_args.options =
    (JvVMOption*) JvRealloc (vm_args.options,
                             (vm_args.nOptions + 1) * sizeof (JvVMOption));

  vm_args.options[vm_args.nOptions].optionString = const_cast<char*> (option);
  vm_args.options[vm_args.nOptions].extraInfo = const_cast<void*> (extra);
  ++vm_args.nOptions;
}

int
main (int argc, char const** argv)
{
  JvVMInitArgs vm_args;
  bool jar_mode = false;

  vm_args.options = NULL;
  vm_args.nOptions = 0;
  vm_args.ignoreUnrecognized = true;

  // Command-line options always override the CLASSPATH environment
  // variable.
  char *classpath = getenv("CLASSPATH");

  if (classpath)
    {
      char* darg = (char*) JvMalloc (strlen (classpath)
                                     + sizeof ("-Djava.class.path="));
      sprintf (darg, "-Djava.class.path=%s", classpath);
      add_option (vm_args, darg, NULL);
    }

  // Handle arguments to the java command.  Store in vm_args arguments
  // handled by the invocation API.
  int i;
  for (i = 1; i < argc; ++i)
    {
      char* arg = const_cast<char*> (argv[i]);

      // A non-option stops processing.
      if (arg[0] != '-')
	break;

      // A "--" stops processing.
      if (! strcmp (arg, "--"))
	{
	  ++i;
	  break;
	}

      // Allow both single or double hyphen for all options.
      if (arg[1] == '-')
	++arg;

      // Ignore JIT options
      if (! strcmp (arg, "-client"))
        continue;
      else if (! strcmp (arg, "-server"))
        continue;
      else if (! strcmp (arg, "-hotspot"))
        continue;
      else if (! strcmp (arg, "-jrockit"))
        continue;
      // Ignore JVM Tool Interface options
      else if (! strncmp (arg, "-agentlib:", sizeof ("-agentlib:") - 1))
        continue;
      else if (! strncmp (arg, "-agentpath:", sizeof ("-agentpath:") - 1))
        continue;
      else if (! strcmp (arg, "-classpath") || ! strcmp (arg, "-cp"))
        {
          if (i >= argc - 1)
            {
	    no_arg:
	      fprintf (stderr, "gij: option requires an argument -- `%s'\n",
		       argv[i]);
	      fprintf (stderr, "Try `gij --help' for more information.\n");
	      exit (1);
            }

          // Sun seems to translate the -classpath option into
          // -Djava.class.path because if both -classpath and
          // -Djava.class.path are specified on the java command line,
          // the last one always wins.
          char* darg = (char*) JvMalloc (strlen (argv[++i])
                                         + sizeof ("-Djava.class.path="));
          sprintf (darg, "-Djava.class.path=%s", argv[i]);
          add_option (vm_args, darg, NULL);
        }
      else if (! strcmp (arg, "-debug"))
        {
          char* xarg = strdup ("-Xdebug");
          add_option (vm_args, xarg, NULL);
        }
      else if (! strncmp (arg, "-D", sizeof ("-D") - 1))
        add_option (vm_args, arg, NULL);
      // Ignore 32/64-bit JIT options
      else if (! strcmp (arg, "-d32") || ! strcmp (arg, "-d64"))
        continue;
      else if (! strncmp (arg, "-enableassertions", sizeof ("-enableassertions") - 1)
               || ! strncmp (arg, "-ea", sizeof ("-ea") - 1))
        {
          // FIXME: hook up assertion support
          continue;
        }
      else if (! strncmp (arg, "-disableassertions", sizeof ("-disableassertions") - 1)
               || ! strncmp (arg, "-da", sizeof ("-da") - 1))
        {
          // FIXME: hook up assertion support
          continue;
        }
      else if (! strcmp (arg, "-enablesystemassertions")
               || ! strcmp (arg, "-esa"))
        {
          // FIXME: hook up system assertion support
          continue;
        }
      else if (! strcmp (arg, "-disablesystemassertions")
               || ! strcmp (arg, "-dsa"))
        {
          // FIXME
          continue;
        }
      else if (! strcmp (arg, "-jar"))
	{
	  jar_mode = true;
	  continue;
	}
      // Ignore java.lang.instrument option
      else if (! strncmp (arg, "-javaagent:", sizeof ("-javaagent:") - 1))
        continue;
      else if (! strcmp (arg, "-noclassgc"))
        {
          char* xarg = strdup ("-Xnoclassgc");
          add_option (vm_args, xarg, NULL);
        }
      // -ms=n
      else if (! strncmp (arg, "-ms=", sizeof ("-ms=") - 1))
        {
          arg[1] = 'X';
          arg[2] = 'm';
          arg[3] = 's';
          add_option (vm_args, arg, NULL);
        }
      // -ms n
      else if (! strcmp (arg, "-ms"))
	{
	  if (i >= argc - 1)
            goto no_arg;

          char* xarg = (char*) JvMalloc (strlen (argv[++i])
                                         + sizeof ("-Xms"));
          sprintf (xarg, "-Xms%s", argv[i]);
          add_option (vm_args, xarg, NULL);
	}
      // -msn
      else if (! strncmp (arg, "-ms", sizeof ("-ms") - 1))
	{
          char* xarg = (char*) JvMalloc (strlen (arg) + sizeof ("X"));
          sprintf (xarg, "-Xms%s", arg + sizeof ("-Xms") - 1);
          add_option (vm_args, xarg, NULL);
	}
      // -mx=n
      else if (! strncmp (arg, "-mx=", sizeof ("-mx=") - 1))
        {
          arg[1] = 'X';
          arg[2] = 'm';
          arg[3] = 'x';
          add_option (vm_args, arg, NULL);
        }
      // -mx n
      else if (! strcmp (arg, "-mx"))
	{
	  if (i >= argc - 1)
            goto no_arg;

          char* xarg = (char*) JvMalloc (strlen (argv[++i])
                                         + sizeof ("-Xmx"));
          sprintf (xarg, "-Xmx%s", argv[i]);
          add_option (vm_args, xarg, NULL);
	}
      // -mxn
      else if (! strncmp (arg, "-mx", sizeof ("-mx") - 1))
	{
          char* xarg = (char*) JvMalloc (strlen (arg) + sizeof ("X"));
          sprintf (xarg, "-Xmx%s", arg + sizeof ("-Xmx") - 1);
          add_option (vm_args, xarg, NULL);
	}
      // -ss=n
      else if (! strncmp (arg, "-ss=", sizeof ("-ss=") - 1))
        {
          arg[1] = 'X';
          arg[2] = 's';
          arg[3] = 's';
          add_option (vm_args, arg, NULL);
        }
      // -ss n
      else if (! strcmp (arg, "-ss"))
	{
	  if (i >= argc - 1)
            goto no_arg;

          char* xarg = (char*) JvMalloc (strlen (argv[++i])
                                         + sizeof ("-Xss"));
          sprintf (xarg, "-Xss%s", argv[i]);
          add_option (vm_args, xarg, NULL);
	}
      // -ssn
      else if (! strncmp (arg, "-ss", sizeof ("-ss") - 1))
	{
          char* xarg = (char*) JvMalloc (strlen (arg) + sizeof ("X"));
          sprintf (xarg, "-Xss%s", arg + sizeof ("-Xss") - 1);
          add_option (vm_args, xarg, NULL);
	}
      // This handles all the option variants that begin with
      // -verbose.
      else if (! strncmp (arg, "-verbose", 8))
        add_option (vm_args, arg, NULL);
      else if (! strcmp (arg, "-version"))
	{
	  version ();
	  exit (0);
	}
      else if (! strcmp (arg, "-fullversion"))
        {
          printf ("java full version \"gcj-" JV_VERSION "\"\n");
          exit (0);
        }
      else if (! strcmp (arg, "-showversion"))
        version ();
      else if (! strcmp (arg, "-help") || ! strcmp (arg, "-?"))
	help ();
      else if (! strcmp (arg, "-X"))
        nonstandard_opts_help ();
      else if (! strncmp (arg, "-X", 2))
        add_option (vm_args, arg, NULL);
      // Obsolete options recognized for backwards-compatibility.
      else if (! strcmp (arg, "-verify")
               || ! strcmp (arg, "-verifyremote"))
	continue;
      else if (! strcmp (arg, "-noverify"))
        {
	  gcj::verifyClasses = false;
	}
      else
	{
	  fprintf (stderr, "gij: unrecognized option -- `%s'\n", argv[i]);
	  fprintf (stderr, "Try `gij --help' for more information.\n");
	  exit (1);
	}
    }

  if (argc - i < 1)
    {
      fprintf (stderr, "Usage: gij [OPTION] ... CLASS [ARGS] ...\n");
      fprintf (stderr, "          to invoke CLASS.main, or\n");
      fprintf (stderr, "       gij -jar [OPTION] ... JARFILE [ARGS] ...\n");
      fprintf (stderr, "          to execute a jar file\n");
      fprintf (stderr, "Try `gij --help' for more information.\n");
      exit (1);
    }

  // -jar mode overrides all other modes of specifying class path:
  // CLASSPATH, -Djava.class.path, -classpath and -cp.
  if (jar_mode)
    {
      char* darg = (char*) JvMalloc (strlen (argv[i])
                                      + sizeof ("-Djava.class.path="));
      sprintf (darg, "-Djava.class.path=%s", argv[i]);
      add_option (vm_args, darg, NULL);
    }

  _Jv_RunMain (&vm_args, NULL, argv[i], argc - i,
               (char const**) (argv + i), jar_mode);
}
