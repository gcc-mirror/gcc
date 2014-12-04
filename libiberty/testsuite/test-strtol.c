/* Test program for strtol family of funtions,
   Copyright (C) 2014 Free Software Foundation, Inc.
   Written by Yury Gribov <y.gribov@samsung.com>

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


/* Test input data. */

enum conversion_fun
{
  STRTOL,
  STRTOLL,
  STRTOUL,
  STRTOULL,
};

#ifdef HAVE_LONG_LONG
typedef unsigned long long integer_type;
#else
typedef unsigned long integer_type;
#endif

struct test_data_t
{
  enum conversion_fun fun;
  const char *nptr;
  int base;
  integer_type res;
  int errnum;
};

const struct test_data_t test_data[] = {
  { STRTOL,  "0x123",       0, 0x123L,        0      },
  { STRTOL,  "123",         0, 123L,          0      },
  { STRTOL,  "0123",        0, 0123L,         0      },
  { STRTOL,  "0x7FFFFFFF",  0, 0x7fffffffL,   0      },
  { STRTOL,  "-0x80000000", 0, -0x80000000L,  0      },
  { STRTOUL, "0x123",       0, 0x123UL,       0      },
  { STRTOUL, "123",         0, 123UL,         0      },
  { STRTOUL, "0123",        0, 0123UL,        0      },
  { STRTOUL, "0xFFFFFFFF",  0, 0xffffffffUL,  0      },
#if SIZEOF_LONG == 4
  { STRTOL,  "0x80000000",  0, 0x7fffffffL,   ERANGE },
  { STRTOL,  "-0x80000001", 0, -0x80000000L,  ERANGE },
  { STRTOUL, "0x100000000", 0, 0xffffffffUL,  ERANGE },
#endif
#ifdef HAVE_LONG_LONG
  { STRTOLL,  "0x123",               0, 0x123LL,               0      },
  { STRTOLL,  "123",                 0, 123LL,                 0      },
  { STRTOLL,  "0123",                0, 0123LL,                0      },
  { STRTOLL,  "0x7FFFFFFFFFFFFFFF",  0, 0x7fffffffffffffffLL,  0      },
  { STRTOLL,  "-0x8000000000000000", 0, -0x8000000000000000LL, 0      },
  { STRTOULL, "0x123",               0, 0x123ULL,              0      },
  { STRTOULL, "123",                 0, 123ULL,                0      },
  { STRTOULL, "0123",                0, 0123ULL,               0      },
  { STRTOULL, "0xFFFFFFFFFFFFFFFF",  0, 0xffffffffffffffffULL, 0      },
#if SIZEOF_LONG_LONG == 8
  { STRTOLL,  "0x8000000000000000",  0, 0x7fffffffffffffffLL,  ERANGE },
  { STRTOLL,  "-0x8000000000000001", 0, -0x8000000000000000LL, ERANGE },
  { STRTOULL, "0x10000000000000000", 0, 0xffffffffffffffffULL, ERANGE },
#endif
#endif
};

/* run_tests:
    Run conversion function
    Compare results
    Return number of fails */

int
run_tests (const struct test_data_t *test_data, size_t ntests)
{
  int fails = 0, failed;
  size_t i;

  for (i = 0; i < ntests; ++i)
    {
      integer_type res;
      int saved_errno;

      errno = 0;

      switch (test_data[i].fun)
	{
	case STRTOL:
	  res = (unsigned long) strtol (test_data[i].nptr,
					0, test_data[i].base);
	  break;
	case STRTOUL:
	  res = strtoul (test_data[i].nptr, 0, test_data[i].base);
	  break;
#ifdef HAVE_LONG_LONG
	case STRTOLL:
	  res = strtoll (test_data[i].nptr, 0, test_data[i].base);
	  break;
	case STRTOULL:
	  res = strtoull (test_data[i].nptr, 0, test_data[i].base);
	  break;
#endif
	}

      saved_errno = errno;

      failed = 0;

      /* Compare result */
      if (res != test_data[i].res)
        {
          printf ("FAIL: test-strtol-%zd. Results don't match.\n", i);
	  failed++;
        }

      /* Compare errno */
      if (saved_errno != test_data[i].errnum)
        {
          printf ("FAIL: test-strtol-%zd. Errnos don't match.\n", i);
	  failed++;
        }

      if (!failed)
        printf ("PASS: test-strtol-%zd.\n", i);
      else
        fails++;
    }

  return fails;
}

int 
main(int argc, char **argv)
{
  int fails;
  fails = run_tests (test_data, sizeof (test_data) / sizeof (test_data[0]));
  exit (fails ? EXIT_FAILURE : EXIT_SUCCESS);
}

