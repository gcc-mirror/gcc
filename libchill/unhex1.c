/* Implement runtime actions for CHILL.
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

#define __CHILL_LIB__

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

extern void cause_exception (char *ex, char *file, int lineno, int arg);
extern void unhandled_exception (char *ex, char *file, int lineno, int arg);

/*
 * function __unhandled_ex
 *
 * parameter:
 *  exname		name of exception
 *  file		filename
 *  lineno		line number
 *
 * returns:
 *  never
 *
 * abstract:
 *  This function gets called by compiler generated code when an unhandled
 *  exception occures.
 *  First cause_exception gets called (which may be user defined) and
 *  then the standard unhandled exception routine gets called.
 *
 */

void
__unhandled_ex (exname, file, lineno)
     char *exname;
     char *file;
     int lineno;
{
    cause_exception (exname, file, lineno, 0);
    unhandled_exception (exname, file, lineno, 0);
} /* unhandled_exception */
