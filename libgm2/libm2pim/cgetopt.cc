/* cgetopt.cc provide access to the C getopt library.

Copyright (C) 2009-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <unistd.h>
#include <stdlib.h>
#include <getopt.h>
#include <m2rts.h>
#include <stdio.h>
#include <config.h>

#define EXPORT(FUNC) m2pim ## _cgetopt_ ## FUNC
#define M2EXPORT(FUNC) m2pim ## _M2_cgetopt_ ## FUNC
#define M2LIBNAME "m2pim"

#if !defined(_)
#define _(X) X
#endif

extern "C" {char *EXPORT(optarg);}
extern "C" {int EXPORT(optind);}
extern "C" {int EXPORT(opterr);}
extern "C" {int EXPORT(optopt);}

int
libiberty_getopt_long_only (int argc, char *const *argv, const char *options,
			    const struct option *long_options, int *opt_index);
int
libiberty_getopt_long (int argc,  char *const *argv,  const char *options,
		       const struct option *long_options, int *opt_index);


typedef struct cgetopt_Options_s
{
  struct option *cinfo;
  unsigned int empty_slot;
} cgetopt_Options;

/* InitOptions a constructor for Options.  */

extern "C" cgetopt_Options *
EXPORT(InitOptions) (void)
{
  cgetopt_Options *o = (cgetopt_Options *)malloc (sizeof (cgetopt_Options));
  o->cinfo = (struct option *)malloc (sizeof (struct option));
  o->empty_slot = 1;
  return o;
}

/* KillOptions a deconstructor for Options.  Returns NULL after freeing
   up all allocated memory associated with o.  */

extern "C" cgetopt_Options *
EXPORT(KillOptions) (cgetopt_Options *o)
{
  free (o->cinfo);
  free (o);
  return NULL;
}

/* SetOption set option[index] with {name, has_arg, flag, val}.  */

extern "C" void
EXPORT(SetOption) (cgetopt_Options *o, unsigned int index, char *name,
 		   int has_arg, int *flag, int val)
{
  if (index >= o->empty_slot)
    {
      o->cinfo
	= (struct option *)realloc (o->cinfo,
				    sizeof (struct option) * (index + 1));
      o->empty_slot = index + 1;
    }
  o->cinfo[index].name = name;
  o->cinfo[index].has_arg = has_arg;
  /* Set flag to NULL if name is NULL, as flag comes from a VAR parameter
     (which cannot be NIL in modula-2).  */
  if (name == NULL)
    flag = NULL;
  o->cinfo[index].flag = flag;
  o->cinfo[index].val = val;
}

/* GetLongOptionArray returns a pointer to the C array containing all
   long options.  */

extern "C" struct option *
EXPORT(GetLongOptionArray) (cgetopt_Options *o)
{
  return o->cinfo;
}


/* The following code has been taken from libiberty/getopt.c and is
   conditionally compiled if long option support is absent on the target.
   It has been changed just to deal with the long option functions as we
   assume all targets have getopt.  This code also keeps all data and code
   static as the interface code only comes though functions defined in
   cgetopt.def.  */

#if (! defined(HAVE_GETOPT_LONG_ONLY)) || (! defined(HAVE_GETOPT_LONG))
/* For communication from `getopt' to the caller.
   When `getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */

static char *cgetopt_optarg = NULL;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `getopt'.

   On entry to `getopt', zero means this is the first call; initialize.

   When `getopt' returns -1, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, `optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

/* 1003.2 says this must be 1 before any call.  */
static int cgetopt_optind = 1;

/* Formerly, initialization of getopt depended on optind==0, which
   causes problems with re-calling getopt as programs generally don't
   know that. */

static int __getopt_initialized = 0;

/* The next char to be scanned in the option-element
   in which the last option character we returned was found.
   This allows us to pick up the scan where we left off.

   If this is zero, or a null string, it means resume the scan
   by advancing to the next ARGV-element.  */

static char *nextchar;

/* Callers store zero here to inhibit the error message
   for unrecognized options.  */

static int cgetopt_opterr = 1;

/* Set to an option character which was unrecognized.
   This must be initialized on some systems to avoid linking in the
   system's own getopt implementation.  */

static int cgetopt_optopt = '?';

/* Describe how to deal with options that follow non-option ARGV-elements.

   If the caller did not specify anything,
   the default is REQUIRE_ORDER if the environment variable
   POSIXLY_CORRECT is defined, PERMUTE otherwise.

   REQUIRE_ORDER means don't recognize them as options;
   stop option processing when the first non-option is seen.
   This is what Unix does.
   This mode of operation is selected by either setting the environment
   variable POSIXLY_CORRECT, or using `+' as the first character
   of the list of option characters.

   PERMUTE is the default.  We permute the contents of ARGV as we scan,
   so that eventually all the non-options are at the end.  This allows options
   to be given in any order, even with programs that were not written to
   expect this.

   RETURN_IN_ORDER is an option available to programs that were written
   to expect options and other ARGV-elements in any order and that care about
   the ordering of the two.  We describe each non-option ARGV-element
   as if it were the argument of an option with character code 1.
   Using `-' as the first character of the list of option characters
   selects this mode of operation.

   The special argument `--' forces an end of option-scanning regardless
   of the value of `ordering'.  In the case of RETURN_IN_ORDER, only
   `--' can cause `getopt' to return -1 with `optind' != ARGC.  */

static enum
{
  REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER
} ordering;

/* Value of POSIXLY_CORRECT environment variable.  */
static char *posixly_correct;

#ifdef	__GNU_LIBRARY__
/* We want to avoid inclusion of string.h with non-GNU libraries
   because there are many ways it can cause trouble.
   On some systems, it contains special magic macros that don't work
   in GCC.  */
# include <string.h>
# define my_index	strchr
#else

# if HAVE_STRING_H
#  include <string.h>
# else
#  if HAVE_STRINGS_H
#   include <strings.h>
#  endif
# endif

/* Avoid depending on library functions or files
   whose names are inconsistent.  */

#if HAVE_STDLIB_H && HAVE_DECL_GETENV
#  include <stdlib.h>
#elif !defined(getenv)
#  ifdef __cplusplus
extern "C" {
#  endif /* __cplusplus */
extern char *getenv (const char *);
#  ifdef __cplusplus
}
#  endif /* __cplusplus */
#endif

static char *
my_index (const char *str, int chr)
{
  while (*str)
    {
      if (*str == chr)
	return (char *) str;
      str++;
    }
  return 0;
}

/* If using GCC, we can safely declare strlen this way.
   If not using GCC, it is ok not to declare it.  */
#ifdef __GNUC__
/* Note that Motorola Delta 68k R3V7 comes with GCC but not stddef.h.
   That was relevant to code that was here before.  */
# if (!defined __STDC__ || !__STDC__) && !defined strlen
/* gcc with -traditional declares the built-in strlen to return int,
   and has done so at least since version 2.4.5. -- rms.  */
extern int strlen (const char *);
# endif /* not __STDC__ */
#endif /* __GNUC__ */

#endif /* not __GNU_LIBRARY__ */

/* Handle permutation of arguments.  */

/* Describe the part of ARGV that contains non-options that have
   been skipped.  `first_nonopt' is the index in ARGV of the first of them;
   `last_nonopt' is the index after the last of them.  */

static int first_nonopt;
static int last_nonopt;

#ifdef _LIBC
/* Bash 2.0 gives us an environment variable containing flags
   indicating ARGV elements that should not be considered arguments.  */

/* Defined in getopt_init.c  */
extern char *__getopt_nonoption_flags;

static int nonoption_flags_max_len;
static int nonoption_flags_len;

static int original_argc;
static char *const *original_argv;

/* Make sure the environment variable bash 2.0 puts in the environment
   is valid for the getopt call we must make sure that the ARGV passed
   to getopt is that one passed to the process.  */
static void
__attribute__ ((unused))
store_args_and_env (int argc, char *const *argv)
{
  /* XXX This is no good solution.  We should rather copy the args so
     that we can compare them later.  But we must not use malloc(3).  */
  original_argc = argc;
  original_argv = argv;
}
# ifdef text_set_element
text_set_element (__libc_subinit, store_args_and_env);
# endif /* text_set_element */

# define SWAP_FLAGS(ch1, ch2) \
  if (nonoption_flags_len > 0)						      \
    {									      \
      char __tmp = __getopt_nonoption_flags[ch1];			      \
      __getopt_nonoption_flags[ch1] = __getopt_nonoption_flags[ch2];	      \
      __getopt_nonoption_flags[ch2] = __tmp;				      \
    }
#else	/* !_LIBC */
# define SWAP_FLAGS(ch1, ch2)
#endif	/* _LIBC */

/* Exchange two adjacent subsequences of ARGV.
   One subsequence is elements [first_nonopt,last_nonopt)
   which contains all the non-options that have been skipped so far.
   The other is elements [last_nonopt,optind), which contains all
   the options processed since those non-options were skipped.

   `first_nonopt' and `last_nonopt' are relocated so that they describe
   the new indices of the non-options in ARGV after they are moved.  */

#if defined __STDC__ && __STDC__
static void exchange (char **);
#endif

static void
exchange (char **argv)
{
  int bottom = first_nonopt;
  int middle = last_nonopt;
  int top = cgetopt_optind;
  char *tem;

  /* Exchange the shorter segment with the far end of the longer segment.
     That puts the shorter segment into the right place.
     It leaves the longer segment in the right place overall,
     but it consists of two parts that need to be swapped next.  */

#ifdef _LIBC
  /* First make sure the handling of the `__getopt_nonoption_flags'
     string can work normally.  Our top argument must be in the range
     of the string.  */
  if (nonoption_flags_len > 0 && top >= nonoption_flags_max_len)
    {
      /* We must extend the array.  The user plays games with us and
	 presents new arguments.  */
      char *new_str = (char *) malloc (top + 1);
      if (new_str == NULL)
	nonoption_flags_len = nonoption_flags_max_len = 0;
      else
	{
	  memset (mempcpy (new_str, __getopt_nonoption_flags,
			   nonoption_flags_max_len),
		  '\0', top + 1 - nonoption_flags_max_len);
	  nonoption_flags_max_len = top + 1;
	  __getopt_nonoption_flags = new_str;
	}
    }
#endif

  while (top > middle && middle > bottom)
    {
      if (top - middle > middle - bottom)
	{
	  /* Bottom segment is the short one.  */
	  int len = middle - bottom;
	  int i;

	  /* Swap it with the top part of the top segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[top - (middle - bottom) + i];
	      argv[top - (middle - bottom) + i] = tem;
	      SWAP_FLAGS (bottom + i, top - (middle - bottom) + i);
	    }
	  /* Exclude the moved bottom segment from further swapping.  */
	  top -= len;
	}
      else
	{
	  /* Top segment is the short one.  */
	  int len = top - middle;
	  int i;

	  /* Swap it with the bottom part of the bottom segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[middle + i];
	      argv[middle + i] = tem;
	      SWAP_FLAGS (bottom + i, middle + i);
	    }
	  /* Exclude the moved top segment from further swapping.  */
	  bottom += len;
	}
    }

  /* Update records for the slots the non-options now occupy.  */

  first_nonopt += (cgetopt_optind - last_nonopt);
  last_nonopt = cgetopt_optind;
}

/* Initialize the internal data when the first call is made.  */

#if defined __STDC__ && __STDC__
static const char *_getopt_initialize (int, char *const *, const char *);
#endif
static const char *
_getopt_initialize (int argc, char *const *argv, const char *optstring)
{
  /* Start processing options with ARGV-element 1 (since ARGV-element 0
     is the program name); the sequence of previously skipped
     non-option ARGV-elements is empty.  */

  first_nonopt = last_nonopt = cgetopt_optind;

  nextchar = NULL;

  posixly_correct = getenv ("POSIXLY_CORRECT");

  /* Determine how to handle the ordering of options and nonoptions.  */

  if (optstring[0] == '-')
    {
      ordering = RETURN_IN_ORDER;
      ++optstring;
    }
  else if (optstring[0] == '+')
    {
      ordering = REQUIRE_ORDER;
      ++optstring;
    }
  else if (posixly_correct != NULL)
    ordering = REQUIRE_ORDER;
  else
    ordering = PERMUTE;

#ifdef _LIBC
  if (posixly_correct == NULL
      && argc == original_argc && argv == original_argv)
    {
      if (nonoption_flags_max_len == 0)
	{
	  if (__getopt_nonoption_flags == NULL
	      || __getopt_nonoption_flags[0] == '\0')
	    nonoption_flags_max_len = -1;
	  else
	    {
	      const char *orig_str = __getopt_nonoption_flags;
	      int len = nonoption_flags_max_len = strlen (orig_str);
	      if (nonoption_flags_max_len < argc)
		nonoption_flags_max_len = argc;
	      __getopt_nonoption_flags =
		(char *) malloc (nonoption_flags_max_len);
	      if (__getopt_nonoption_flags == NULL)
		nonoption_flags_max_len = -1;
	      else
		memset (mempcpy (__getopt_nonoption_flags, orig_str, len),
			'\0', nonoption_flags_max_len - len);
	    }
	}
      nonoption_flags_len = nonoption_flags_max_len;
    }
  else
    nonoption_flags_len = 0;
#endif

  return optstring;
}

/* Scan elements of ARGV (whose length is ARGC) for option characters
   given in OPTSTRING.

   If an element of ARGV starts with '-', and is not exactly "-" or "--",
   then it is an option element.  The characters of this element
   (aside from the initial '-') are option characters.  If `getopt'
   is called repeatedly, it returns successively each of the option characters
   from each of the option elements.

   If `getopt' finds another option character, it returns that character,
   updating `optind' and `nextchar' so that the next call to `getopt' can
   resume the scan with the following option character or ARGV-element.

   If there are no more option characters, `getopt' returns -1.
   Then `optind' is the index in ARGV of the first ARGV-element
   that is not an option.  (The ARGV-elements have been permuted
   so that those that are not options now come last.)

   OPTSTRING is a string containing the legitimate option characters.
   If an option character is seen that is not listed in OPTSTRING,
   return '?' after printing an error message.  If you set `opterr' to
   zero, the error message is suppressed but we still return '?'.

   If a char in OPTSTRING is followed by a colon, that means it wants an arg,
   so the following text in the same ARGV-element, or the text of the following
   ARGV-element, is returned in `optarg'.  Two colons mean an option that
   wants an optional arg; if there is text in the current ARGV-element,
   it is returned in `optarg', otherwise `optarg' is set to zero.

   If OPTSTRING starts with `-' or `+', it requests different methods of
   handling the non-option ARGV-elements.
   See the comments about RETURN_IN_ORDER and REQUIRE_ORDER, above.

   Long-named options begin with `--' instead of `-'.
   Their names may be abbreviated as long as the abbreviation is unique
   or is an exact match for some defined option.  If they have an
   argument, it follows the option name in the same ARGV-element, separated
   from the option name by a `=', or else the in next ARGV-element.
   When `getopt' finds a long-named option, it returns 0 if that option's
   `flag' field is nonzero, the value of the option's `val' field
   if the `flag' field is zero.

   The elements of ARGV aren't really const, because we permute them.
   But we pretend they're const in the prototype to be compatible
   with other systems.

   LONGOPTS is a vector of `struct option' terminated by an
   element containing a name which is zero.

   LONGIND returns the index in LONGOPT of the long-named option found.
   It is only valid when a long-named option has been found by the most
   recent call.

   If LONG_ONLY is nonzero, '-' as well as '--' can introduce
   long-named options.  */

static
int
_getopt_internal (int argc, char *const *argv, const char *optstring,
                  const struct option *longopts,
                  int *longind, int long_only)
{
  cgetopt_optarg = NULL;

  if (cgetopt_optind == 0 || !__getopt_initialized)
    {
      if (cgetopt_optind == 0)
	cgetopt_optind = 1;	/* Don't scan ARGV[0], the program name.  */
      optstring = _getopt_initialize (argc, argv, optstring);
      __getopt_initialized = 1;
    }

  /* Test whether ARGV[optind] points to a non-option argument.
     Either it does not have option syntax, or there is an environment flag
     from the shell indicating it is not an option.  The later information
     is only used when the used in the GNU libc.  */
#ifdef _LIBC
# define NONOPTION_P (argv[cgetopt_optind][0] != '-' || argv[cgetopt_optind][1] == '\0'	      \
		      || (cgetopt_optind < nonoption_flags_len			      \
			  && __getopt_nonoption_flags[cgetopt_optind] == '1'))
#else
# define NONOPTION_P (argv[cgetopt_optind][0] != '-' || argv[cgetopt_optind][1] == '\0')
#endif

  if (nextchar == NULL || *nextchar == '\0')
    {
      /* Advance to the next ARGV-element.  */

      /* Give FIRST_NONOPT & LAST_NONOPT rational values if OPTIND has been
	 moved back by the user (who may also have changed the arguments).  */
      if (last_nonopt > cgetopt_optind)
	last_nonopt = cgetopt_optind;
      if (first_nonopt > cgetopt_optind)
	first_nonopt = cgetopt_optind;

      if (ordering == PERMUTE)
	{
	  /* If we have just processed some options following some non-options,
	     exchange them so that the options come first.  */

	  if (first_nonopt != last_nonopt && last_nonopt != cgetopt_optind)
	    exchange ((char **) argv);
	  else if (last_nonopt != cgetopt_optind)
	    first_nonopt = cgetopt_optind;

	  /* Skip any additional non-options
	     and extend the range of non-options previously skipped.  */

	  while (cgetopt_optind < argc && NONOPTION_P)
	    cgetopt_optind++;
	  last_nonopt = cgetopt_optind;
	}

      /* The special ARGV-element `--' means premature end of options.
	 Skip it like a null option,
	 then exchange with previous non-options as if it were an option,
	 then skip everything else like a non-option.  */

      if (cgetopt_optind != argc && !strcmp (argv[cgetopt_optind], "--"))
	{
	  cgetopt_optind++;

	  if (first_nonopt != last_nonopt && last_nonopt != cgetopt_optind)
	    exchange ((char **) argv);
	  else if (first_nonopt == last_nonopt)
	    first_nonopt = cgetopt_optind;
	  last_nonopt = argc;

	  cgetopt_optind = argc;
	}

      /* If we have done all the ARGV-elements, stop the scan
	 and back over any non-options that we skipped and permuted.  */

      if (cgetopt_optind == argc)
	{
	  /* Set the next-arg-index to point at the non-options
	     that we previously skipped, so the caller will digest them.  */
	  if (first_nonopt != last_nonopt)
	    cgetopt_optind = first_nonopt;
	  return -1;
	}

      /* If we have come to a non-option and did not permute it,
	 either stop the scan or describe it to the caller and pass it by.  */

      if (NONOPTION_P)
	{
	  if (ordering == REQUIRE_ORDER)
	    return -1;
	  cgetopt_optarg = argv[cgetopt_optind++];
	  return 1;
	}

      /* We have found another option-ARGV-element.
	 Skip the initial punctuation.  */

      nextchar = (argv[cgetopt_optind] + 1
		  + (longopts != NULL && argv[cgetopt_optind][1] == '-'));
    }

  /* Decode the current option-ARGV-element.  */

  /* Check whether the ARGV-element is a long option.

     If long_only and the ARGV-element has the form "-f", where f is
     a valid short option, don't consider it an abbreviated form of
     a long option that starts with f.  Otherwise there would be no
     way to give the -f short option.

     On the other hand, if there's a long option "fubar" and
     the ARGV-element is "-fu", do consider that an abbreviation of
     the long option, just like "--fu", and not "-f" with arg "u".

     This distinction seems to be the most useful approach.  */

  if (longopts != NULL
      && (argv[cgetopt_optind][1] == '-'
	  || (long_only && (argv[cgetopt_optind][2] || !my_index (optstring, argv[cgetopt_optind][1])))))
    {
      char *nameend;
      const struct option *p;
      const struct option *pfound = NULL;
      int exact = 0;
      int ambig = 0;
      int indfound = -1;
      int option_index;

      for (nameend = nextchar; *nameend && *nameend != '='; nameend++)
	/* Do nothing.  */ ;

      /* Test all long options for either exact match
	 or abbreviated matches.  */
      for (p = longopts, option_index = 0; p->name; p++, option_index++)
	if (!strncmp (p->name, nextchar, nameend - nextchar))
	  {
	    if ((unsigned int) (nameend - nextchar)
		== (unsigned int) strlen (p->name))
	      {
		/* Exact match found.  */
		pfound = p;
		indfound = option_index;
		exact = 1;
		break;
	      }
	    else if (pfound == NULL)
	      {
		/* First nonexact match found.  */
		pfound = p;
		indfound = option_index;
	      }
	    else
	      /* Second or later nonexact match found.  */
	      ambig = 1;
	  }

      if (ambig && !exact)
	{
	  if (cgetopt_opterr)
	    fprintf (stderr, _("%s: option `%s' is ambiguous\n"),
		     argv[0], argv[cgetopt_optind]);
	  nextchar += strlen (nextchar);
	  cgetopt_optind++;
	  cgetopt_optopt = 0;
	  return '?';
	}

      if (pfound != NULL)
	{
	  option_index = indfound;
	  cgetopt_optind++;
	  if (*nameend)
	    {
	      /* Don't test has_arg with >, because some C compilers don't
		 allow it to be used on enums.  */
	      if (pfound->has_arg)
		cgetopt_optarg = nameend + 1;
	      else
		{
		  if (cgetopt_opterr)
		    {
		      if (argv[cgetopt_optind - 1][1] == '-')
			/* --option */
			fprintf (stderr,
				 _("%s: option `--%s' doesn't allow an argument\n"),
				 argv[0], pfound->name);
		      else
			/* +option or -option */
			fprintf (stderr,
				 _("%s: option `%c%s' doesn't allow an argument\n"),
				 argv[0], argv[cgetopt_optind - 1][0], pfound->name);

		      nextchar += strlen (nextchar);

		      cgetopt_optopt = pfound->val;
		      return '?';
		    }
		}
	    }
	  else if (pfound->has_arg == 1)
	    {
	      if (cgetopt_optind < argc)
		cgetopt_optarg = argv[cgetopt_optind++];
	      else
		{
		  if (cgetopt_opterr)
		    fprintf (stderr,
			   _("%s: option `%s' requires an argument\n"),
			   argv[0], argv[cgetopt_optind - 1]);
		  nextchar += strlen (nextchar);
		  cgetopt_optopt = pfound->val;
		  return optstring[0] == ':' ? ':' : '?';
		}
	    }
	  nextchar += strlen (nextchar);
	  if (longind != NULL)
	    *longind = option_index;
	  if (pfound->flag)
	    {
	      *(pfound->flag) = pfound->val;
	      return 0;
	    }
	  return pfound->val;
	}

      /* Can't find it as a long option.  If this is not getopt_long_only,
	 or the option starts with '--' or is not a valid short
	 option, then it's an error.
	 Otherwise interpret it as a short option.  */
      if (!long_only || argv[cgetopt_optind][1] == '-'
	  || my_index (optstring, *nextchar) == NULL)
	{
	  if (cgetopt_opterr)
	    {
	      if (argv[cgetopt_optind][1] == '-')
		/* --option */
		fprintf (stderr, _("%s: unrecognized option `--%s'\n"),
			 argv[0], nextchar);
	      else
		/* +option or -option */
		fprintf (stderr, _("%s: unrecognized option `%c%s'\n"),
			 argv[0], argv[cgetopt_optind][0], nextchar);
	    }
	  nextchar = (char *) "";
	  cgetopt_optind++;
	  cgetopt_optopt = 0;
	  return '?';
	}
    }

  /* Look at and handle the next short option-character.  */

  {
    char c = *nextchar++;
    const char *temp = my_index (optstring, c);

    /* Increment `cgetopt_optind' when we start to process its last character.  */
    if (*nextchar == '\0')
      ++cgetopt_optind;

    if (temp == NULL || c == ':')
      {
	if (cgetopt_opterr)
	  {
	    if (posixly_correct)
	      /* 1003.2 specifies the format of this message.  */
	      fprintf (stderr, _("%s: illegal option -- %c\n"),
		       argv[0], c);
	    else
	      fprintf (stderr, _("%s: invalid option -- %c\n"),
		       argv[0], c);
	  }
	cgetopt_optopt = c;
	return '?';
      }
    /* Convenience. Treat POSIX -W foo same as long option --foo */
    if (temp[0] == 'W' && temp[1] == ';')
      {
	char *nameend;
	const struct option *p;
	const struct option *pfound = NULL;
	int exact = 0;
	int ambig = 0;
	int indfound = 0;
	int option_index;

	/* This is an option that requires an argument.  */
	if (*nextchar != '\0')
	  {
	    cgetopt_optarg = nextchar;
	    /* If we end this ARGV-element by taking the rest as an arg,
	       we must advance to the next element now.  */
	    cgetopt_optind++;
	  }
	else if (cgetopt_optind == argc)
	  {
	    if (cgetopt_opterr)
	      {
		/* 1003.2 specifies the format of this message.  */
		fprintf (stderr, _("%s: option requires an argument -- %c\n"),
			 argv[0], c);
	      }
	    cgetopt_optopt = c;
	    if (optstring[0] == ':')
	      c = ':';
	    else
	      c = '?';
	    return c;
	  }
	else
	  /* We already incremented `optind' once;
	     increment it again when taking next ARGV-elt as argument.  */
	  cgetopt_optarg = argv[cgetopt_optind++];

	/* cgetopt_optarg is now the argument, see if it's in the
	   table of longopts.  */

	for (nextchar = nameend = cgetopt_optarg; *nameend && *nameend != '='; nameend++)
	  /* Do nothing.  */ ;

	/* Test all long options for either exact match
	   or abbreviated matches.  */
	for (p = longopts, option_index = 0; p->name; p++, option_index++)
	  if (!strncmp (p->name, nextchar, nameend - nextchar))
	    {
	      if ((unsigned int) (nameend - nextchar) == strlen (p->name))
		{
		  /* Exact match found.  */
		  pfound = p;
		  indfound = option_index;
		  exact = 1;
		  break;
		}
	      else if (pfound == NULL)
		{
		  /* First nonexact match found.  */
		  pfound = p;
		  indfound = option_index;
		}
	      else
		/* Second or later nonexact match found.  */
		ambig = 1;
	    }
	if (ambig && !exact)
	  {
	    if (cgetopt_opterr)
	      fprintf (stderr, _("%s: option `-W %s' is ambiguous\n"),
		       argv[0], argv[cgetopt_optind]);
	    nextchar += strlen (nextchar);
	    cgetopt_optind++;
	    return '?';
	  }
	if (pfound != NULL)
	  {
	    option_index = indfound;
	    if (*nameend)
	      {
		/* Don't test has_arg with >, because some C compilers don't
		   allow it to be used on enums.  */
		if (pfound->has_arg)
		  cgetopt_optarg = nameend + 1;
		else
		  {
		    if (cgetopt_opterr)
		      fprintf (stderr, _("\
%s: option `-W %s' doesn't allow an argument\n"),
			       argv[0], pfound->name);

		    nextchar += strlen (nextchar);
		    return '?';
		  }
	      }
	    else if (pfound->has_arg == 1)
	      {
		if (cgetopt_optind < argc)
		  cgetopt_optarg = argv[cgetopt_optind++];
		else
		  {
		    if (cgetopt_opterr)
		      fprintf (stderr,
			       _("%s: option `%s' requires an argument\n"),
			       argv[0], argv[cgetopt_optind - 1]);
		    nextchar += strlen (nextchar);
		    return optstring[0] == ':' ? ':' : '?';
		  }
	      }
	    nextchar += strlen (nextchar);
	    if (longind != NULL)
	      *longind = option_index;
	    if (pfound->flag)
	      {
		*(pfound->flag) = pfound->val;
		return 0;
	      }
	    return pfound->val;
	  }
	  nextchar = NULL;
	  return 'W';	/* Let the application handle it.   */
      }
    if (temp[1] == ':')
      {
	if (temp[2] == ':')
	  {
	    /* This is an option that accepts an argument optionally.  */
	    if (*nextchar != '\0')
	      {
		cgetopt_optarg = nextchar;
		cgetopt_optind++;
	      }
	    else
	      cgetopt_optarg = NULL;
	    nextchar = NULL;
	  }
	else
	  {
	    /* This is an option that requires an argument.  */
	    if (*nextchar != '\0')
	      {
		cgetopt_optarg = nextchar;
		/* If we end this ARGV-element by taking the rest as an arg,
		   we must advance to the next element now.  */
		cgetopt_optind++;
	      }
	    else if (cgetopt_optind == argc)
	      {
		if (cgetopt_opterr)
		  {
		    /* 1003.2 specifies the format of this message.  */
		    fprintf (stderr,
			   _("%s: option requires an argument -- %c\n"),
			   argv[0], c);
		  }
		cgetopt_optopt = c;
		if (optstring[0] == ':')
		  c = ':';
		else
		  c = '?';
	      }
	    else
	      /* We already incremented `optind' once;
		 increment it again when taking next ARGV-elt as argument.  */
	      cgetopt_optarg = argv[cgetopt_optind++];
	    nextchar = NULL;
	  }
      }
    return c;
  }
}

int
libiberty_getopt_long (int argc,  char *const *argv,  const char *options,
		       const struct option *long_options, int *opt_index)
{
  return _getopt_internal (argc, argv, options, long_options, opt_index, 0);
}

/* Like getopt_long, but '-' as well as '--' can indicate a long option.
   If an option that starts with '-' (not '--') doesn't match a long option,
   but does match a short option, it is parsed as a short option
   instead.  */

int
libiberty_getopt_long_only (int argc, char *const *argv, const char *options,
			    const struct option *long_options, int *opt_index)
{
  return _getopt_internal (argc, argv, options, long_options, opt_index, 1);
}

#endif


extern "C" char
EXPORT(getopt) (int argc, char *argv[], char *optstring)
{
  char r = getopt (argc, argv, optstring);

  EXPORT(optarg) = optarg;
  EXPORT(optind) = optind;
  EXPORT(opterr) = opterr;
  EXPORT(optopt) = optopt;

  if (r == (char)-1)
    return (char)0;
  return r;
}

extern "C" int
EXPORT(getopt_long) (int argc, char *argv[], char *optstring,
		     const struct option *longopts, int *longindex)
{
#if defined(HAVE_GETOPT_LONG)
  int r = getopt_long (argc, argv, optstring, longopts, longindex);

  EXPORT(optarg) = optarg;
  EXPORT(optind) = optind;
  EXPORT(opterr) = opterr;
  EXPORT(optopt) = optopt;
#else
  int r = libiberty_getopt_long (argc, argv, optstring, longopts, longindex);

  EXPORT(optarg) = cgetopt_optarg;
  EXPORT(optind) = cgetopt_optind;
  EXPORT(opterr) = cgetopt_opterr;
  EXPORT(optopt) = cgetopt_optopt;
#endif

  return r;
}


extern "C" int
EXPORT(getopt_long_only) (int argc, char *argv[], char *optstring,
			  const struct option *longopts, int *longindex)
{
#if defined(HAVE_GETOPT_LONG_ONLY)
  int r = getopt_long_only (argc, argv, optstring, longopts, longindex);

  EXPORT(optarg) = optarg;
  EXPORT(optind) = optind;
  EXPORT(opterr) = opterr;
  EXPORT(optopt) = optopt;
#else
  int r = libiberty_getopt_long_only (argc, argv, optstring, longopts, longindex);

  EXPORT(optarg) = cgetopt_optarg;
  EXPORT(optind) = cgetopt_optind;
  EXPORT(opterr) = cgetopt_opterr;
  EXPORT(optopt) = cgetopt_optopt;
#endif

  return r;
}


/* GNU Modula-2 linking fodder.  */

extern "C" void
M2EXPORT(init) (int, char *argv[], char *env[])
{
}

extern "C" void
M2EXPORT(fini) (int, char *argv[], char *env[])
{
}

extern "C" void
M2EXPORT(dep) (void)
{
}

extern "C" void __attribute__((__constructor__))
M2EXPORT(ctor) (void)
{
  m2pim_M2RTS_RegisterModule ("cgetopt", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
