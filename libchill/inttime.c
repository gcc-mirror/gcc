/* Implement timing-related runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include <time.h>

typedef struct
{
  void          *p;
  unsigned long len;
} Descr;

typedef Descr   **Toutlist;

#define ASSIGN_VALUE(OUT,VAL)          \
do                                     \
{                                      \
  if (OUT)                             \
    switch (OUT->len)                  \
      {                                \
      case 1:                          \
	*(char *)((OUT)->p) = VAL;     \
	break;                         \
      case 2:                          \
	*(short *)((OUT)->p) = VAL;    \
	break;                         \
      case 4:                          \
	*(int *)((OUT)->p) = VAL;      \
	break;                         \
      }                                \
} while (0)


/*
 * function _inttime
 *
 * parameters:
 *     t    time_t
 *     list the pointers to the results
 *
 * returns:
 *     void
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     perform the INTTIME builtin call
 *
 */

void
_inttime (timer, outlist)
     time_t   timer;
     Toutlist outlist;
{
  struct tm  *time_str;

  /* get struct tm from time_t */
  time_str = localtime (&timer);

  /* assign the values */
  ASSIGN_VALUE (outlist[0], time_str->tm_year + 1900);
  ASSIGN_VALUE (outlist[1], time_str->tm_mon + 1);
  ASSIGN_VALUE (outlist[2], time_str->tm_mday);
  ASSIGN_VALUE (outlist[3], time_str->tm_hour);
  ASSIGN_VALUE (outlist[4], time_str->tm_min);
  ASSIGN_VALUE (outlist[5], time_str->tm_sec);
}
