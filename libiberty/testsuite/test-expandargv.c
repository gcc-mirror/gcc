/* expandargv test program,
   Copyright (C) 2006-2025 Free Software Foundation, Inc.
   Written by Carlos O'Donell <carlos@codesourcery.com>

   This file is part of the libiberty library, which is part of GCC.

   This file is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   In addition to the permissions in the GNU General Public License, the
   Free Software Foundation gives you unlimited permission to link the
   compiled version of this file into combinations with other programs,
   and to distribute those combinations without any restriction coming
   from the use of this file.  (The General Public License restrictions
   do apply in other respects; for example, they cover modification of
   the file, and distribution when not linked into a combined
   executable.)

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA. 
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "libiberty.h"
#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

static void fatal_error (int, const char *, int) ATTRIBUTE_NORETURN;
void writeout_test (int, const char *);
void run_replaces (char *);
void hook_char_replace (char *, size_t, char, char);
int run_tests (const char **);
void erase_test (int);

/* Test input data, argv before, and argv after:
 
   The \n is an important part of test_data since expandargv
   may have to work in environments where \n is translated
   as \r\n. Thus \n is included in the test data for the file. 

   We use \b to indicate that the test data is the null character.
   This is because we use \0 normally to represent the end of the 
   file data, so we need something else for this. */

#define FILENAME_PATTERN "test-expandargv-%d.lst"
#define ARGV0 "test-expandargv"

const char *test_data[] = {
  /* Test 0 - Check for expansion with \r\n */
  "a\r\nb",	/* Test 0 data */
  ARGV0,
  "@test-expandargv-0.lst",
  0, /* End of argv[] before expansion */
  ARGV0,
  "a",
  "b",
  0, /* End of argv[] after expansion */

  /* Test 1 - Check for expansion with \n */
  "a\nb",	/* Test 1 data */
  ARGV0,
  "@test-expandargv-1.lst",
  0,
  ARGV0,
  "a",
  "b",
  0,

  /* Test 2 - Check for expansion with \0 */
  "a\bb",	/* Test 2 data */
  ARGV0,
  "@test-expandargv-2.lst",
  0,
  ARGV0,
  "a",
  0,

  /* Test 3 - Check for expansion with only \0 */
  "\b",		/* Test 3 data */
  ARGV0,
  "@test-expandargv-3.lst",
  0,
  ARGV0,
  0,

  /* Test 4 - Check for options beginning with an empty line.  */
  "\na\nb",	/* Test 4 data */
  ARGV0,
  "@test-expandargv-4.lst",
  0,
  ARGV0,
  "a",
  "b",
  0,

  /* Test 5 - Check for options containing an empty argument.  */
  "a\n''\nb",    /* Test 5 data */
  ARGV0,
  "@test-expandargv-5.lst",
  0,
  ARGV0,
  "a",
  "",
  "b",
  0,

  /* Test 6 - Check for options containing a quoted newline.  */
  "a\n'a\n\nb'\nb",    /* Test 6 data */
  ARGV0,
  "@test-expandargv-6.lst",
  0,
  ARGV0,
  "a",
  "a\n\nb",
  "b",
  0,

  /* Test 7 - No backslash removal within single quotes.  */
  "'a\\$VAR' '\\\"'",    /* Test 7 data */
  ARGV0,
  "@test-expandargv-7.lst",
  0,
  ARGV0,
  "a\\$VAR",
  "\\\"",
  0,

  /* Test 8 - Remove backslash / newline pairs.  */
  "\"ab\\\ncd\" ef\\\ngh",    /* Test 8 data */
  ARGV0,
  "@test-expandargv-8.lst",
  0,
  ARGV0,
  "abcd",
  "efgh",
  0,

  /* Test 9 - Backslash within double quotes.  */
  "\"\\$VAR\" \"\\`\" \"\\\"\" \"\\\\\" \"\\n\" \"\\t\"",    /* Test 9 data */
  ARGV0,
  "@test-expandargv-9.lst",
  0,
  ARGV0,
  "$VAR",
  "`",
  "\"",
  "\\",
  "\\n",
  "\\t",
  0,

  /* Test 10 - Mixed white space characters.  */
  "\t \n \t ",		/* Test 10 data */
  ARGV0,
  "@test-expandargv-10.lst",
  0,
  ARGV0,
  0,

  /* Test 11 - Single ' ' character.  */
  " ",		/* Test 11 data */
  ARGV0,
  "@test-expandargv-11.lst",
  0,
  ARGV0,
  0,

  /* Test 12 - Multiple ' ' characters.  */
  "   ",		/* Test 12 data */
  ARGV0,
  "@test-expandargv-12.lst",
  0,
  ARGV0,
  0,

  0 /* Test done marker, don't remove. */
};

/* Print a fatal error and exit.  LINE is the line number where we
   detected the error, ERRMSG is the error message to print, and ERR
   is 0 or an errno value to print.  */

static void
fatal_error (int line, const char *errmsg, int err)
{
  fprintf (stderr, "test-expandargv:%d: %s", line, errmsg);
  if (errno != 0)
    fprintf (stderr, ": %s", xstrerror (err));
  fprintf (stderr, "\n");
  exit (EXIT_FAILURE);
}

/* hook_char_replace:
     Replace 'replacethis' with 'withthis' */

void
hook_char_replace (char *string, size_t len, char replacethis, char withthis)
{
  int i = 0;
  for (i = 0; i < len; i++)
    if (string[i] == replacethis)
      string[i] = withthis;
}

/* run_replaces:
     Hook here all the character for character replaces.
     Be warned that expanding the string or contracting the string
     should be handled with care. */

void
run_replaces (char * string)
{
  /* Store original string size */
  size_t len = strlen (string);
  hook_char_replace (string, len, '\b', '\0');
}

/* write_test:
   Write test datafile */

void
writeout_test (int test, const char * test_data)
{
  char filename[256];
  FILE *fd;
  size_t len, sys_fwrite;
  char * parse;

  /* Unique filename per test */
  sprintf (filename, FILENAME_PATTERN, test);
  fd = fopen (filename, "w");
  if (fd == NULL)
    fatal_error (__LINE__, "Failed to create test file.", errno);

  /* Generate RW copy of data for replaces */
  len = strlen (test_data);
  parse = malloc (sizeof (char) * (len + 1));
  if (parse == NULL)
    fatal_error (__LINE__, "Failed to malloc parse.", errno);
      
  memcpy (parse, test_data, sizeof (char) * (len + 1));
  /* Run all possible replaces */
  run_replaces (parse);

  sys_fwrite = fwrite (parse, sizeof (char), len, fd);
  if (sys_fwrite != len)
    fatal_error (__LINE__, "Failed to write to test file.", errno);

  free (parse);
  fclose (fd);
}

/* erase_test:
     Erase the test file */

void 
erase_test (int test)
{
  char filename[256]; 
  sprintf (filename, FILENAME_PATTERN, test);
  if (unlink (filename) != 0)
    fatal_error (__LINE__, "Failed to erase test file.", errno);
}

/* compare_argv:
     TEST is the current test number, and NAME is a short string to identify
     which libibery function is being tested.  ARGC_A and ARGV_A describe an
     argument array, and this is compared to ARGC_B and ARGV_B, return 0 if
     the two arrays match, otherwise return 1.  */

static int
compare_argv (int test, const char *name, int argc_a, char *argv_a[],
	      int argc_b, char *argv_b[])
{
  int failed = 0, k;

  if (argc_a != argc_b)
    {
      printf ("FAIL: test-%s-%d.  Argument count didn't match\n", name, test);
      failed = 1;
    }
  /* Compare each of the argv's ... */
  else
    for (k = 0; k < argc_a; k++)
      if (strcmp (argv_a[k], argv_b[k]) != 0)
	{
	  printf ("FAIL: test-%s-%d. Arguments don't match.\n", name, test);
	  failed = 1;
	  break;
	}

  if (!failed)
    printf ("PASS: test-%s-%d.\n", name, test);

  return failed;
}

/* test_buildargv
     Test the buildargv function from libiberty.  TEST is the current test
     number and TEST_INPUT is the string to pass to buildargv (after calling
     run_replaces on it).  ARGC_AFTER and ARGV_AFTER are the expected
     results.  Return 0 if the test passes, otherwise return 1.  */

static int
test_buildargv (int test, const char * test_input, int argc_after,
		char *argv_after[])
{
  char * input, ** argv;
  size_t len;
  int argc, failed;

  /* Generate RW copy of data for replaces */
  len = strlen (test_input);
  input = malloc (sizeof (char) * (len + 1));
  if (input == NULL)
    fatal_error (__LINE__, "Failed to malloc buildargv input buffer.", errno);

  memcpy (input, test_input, sizeof (char) * (len + 1));
  /* Run all possible replaces */
  run_replaces (input);

  /* Split INPUT into separate arguments.  */
  argv = buildargv (input);

  /* Count the arguments we got back.  */
  argc = 0;
  while (argv[argc])
    ++argc;

  failed = compare_argv (test, "buildargv", argc_after, argv_after, argc, argv);

  free (input);
  freeargv (argv);

  return failed;
}

/* run_tests:
    Run expandargv
    Compare argv before and after.
    Return number of fails */

int
run_tests (const char **test_data)
{
  int argc_after, argc_before;
  char ** argv_before, ** argv_after;
  int i, j, k, fails;
  const char * input_str;

  i = j = fails = 0;
  /* Loop over all the tests */
  while (test_data[j])
    {
      /* Save original input in case we run a buildargv test.  */
      input_str = test_data[j];

      /* Write test data */
      writeout_test (i, test_data[j++]);
      /* Copy argv before */
      argv_before = dupargv ((char **) &test_data[j]);

      /* Count argc before/after */
      argc_before = 0;
      argc_after = 0;
      while (test_data[j + argc_before])
        argc_before++;
      j += argc_before + 1; /* Skip null */
      while (test_data[j + argc_after])
        argc_after++;

      /* Copy argv after */
      argv_after = dupargv ((char **) &test_data[j]);

      /* Run all possible replaces */
      for (k = 0; k < argc_before; k++)
        run_replaces (argv_before[k]);
      for (k = 0; k < argc_after; k++)
        run_replaces (argv_after[k]);

      /* If the test input is just a file to expand then we can also test
	 calling buildargv directly as the expected output is equivalent to
	 calling buildargv on the contents of the file.

	 The results of calling buildargv will not include the ARGV0 constant,
	 which is why we pass 'argc_after - 1' and 'argv_after + 1', this skips
	 over the ARGV0 in the expected results.  */
      if (argc_before == 2)
	fails += test_buildargv (i, input_str, argc_after - 1, argv_after + 1);
      else
	printf ("SKIP: test-buildargv-%d.  This test isn't for buildargv\n", i);

      /* Run test: Expand arguments */
      expandargv (&argc_before, &argv_before);

      fails += compare_argv (i, "expandargv", argc_before, argv_before,
			     argc_after, argv_after);

      freeargv (argv_before);
      freeargv (argv_after);
      /* Advance to next test */
      j += argc_after + 1;
      /* Erase test file */
      erase_test (i);
      i++;
    }
  return fails;
}

/* main:
    Run tests. 
    Check result and exit with appropriate code. */

int 
main(int argc, char **argv)
{
  int fails;
  /* Repeat for all the tests:
     - Parse data array and write into file.
       - Run replace hooks before writing to file.
     - Parse data array and build argv before/after.
       - Run replace hooks on argv before/after
     - Run expandargv.
     - Compare output of expandargv argv to after argv.
       - If they compare the same then test passes
         else the test fails. 
     - Erase test file. */

  fails = run_tests (test_data);
  if (!fails)
    exit (EXIT_SUCCESS);
  else
    exit (EXIT_FAILURE);
}

