/* Basic struct timeval utilities.
   Copyright (C) 2011-2020 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"

/* On some systems (such as WindISS), you must include <sys/types.h>
   to get the definition of "time_t" before you include <time.h>.  */
#include <sys/types.h>

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  ifdef HAVE_TIME_H
#   include <time.h>
#  endif
# endif
#endif

#include "timeval-utils.h"

/* 

@deftypefn Extension void timeval_add (struct timeval *@var{a}, @
  struct timeval *@var{b}, struct timeval *@var{result})

Adds @var{a} to @var{b} and stores the result in @var{result}.

@end deftypefn

*/ 

void
timeval_add (struct timeval *result,
	     const struct timeval *a, const struct timeval *b)
{
  result->tv_sec = a->tv_sec + b->tv_sec;
  result->tv_usec = a->tv_usec + b->tv_usec;
  if (result->tv_usec >= 1000000)
    {
      ++result->tv_sec;
      result->tv_usec -= 1000000;
    }
}

/* 

@deftypefn Extension void timeval_sub (struct timeval *@var{a}, @
  struct timeval *@var{b}, struct timeval *@var{result})

Subtracts @var{b} from @var{a} and stores the result in @var{result}.

@end deftypefn

*/ 

void
timeval_sub (struct timeval *result,
	     const struct timeval *a, const struct timeval *b)
{
  result->tv_sec = a->tv_sec - b->tv_sec;
  result->tv_usec = a->tv_usec - b->tv_usec;
  if (result->tv_usec < 0)
    {
      --result->tv_sec;
      result->tv_usec += 1000000;
    }
}
