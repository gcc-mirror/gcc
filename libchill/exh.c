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
#include <setjmp.h>
#include "rtltypes.h"

extern void cause_exception (char *exname, char *file, int lineno, int user_arg);
extern void unhandled_exception (char *exname, char *file, int lineno, int user_arg);

/* An action with a handler:
	BODY ON (e1, e2): H12; (e3): H3; ELSE HE; END;
   is translated into:

    struct __ch_handler __tmp;
    static struct __ch_handler_excepts _H[4] =
      {
        { <<e1>>, 1 },
        { <<e2>>, 1 },
        { <<e3>>, 2 },
        { __ch_else_except, 3 },
      };
    __ch_link_handler(&__tmp);
    __tmp.handlers = _H;
    switch (setmp(&__tmp.jbuf))
      {	
      case 0:  BODY; __ch_unlink_handler(&__tmp); break;
      case 1:  H12; break;
      case 2:  H3; break;
      case 3:  HE; break;
      }
*/

/* this part contains all neccessary functions to handle exceptions in CHILL */

/* These two trivial function aren't inlines, to allow for
   more flexibility (e.g. a per-thread exception stack). */

extern void __setexceptionStack (TExceptionHandlerStack *new);
extern TExceptionHandlerStack * __getexceptionStack (void);

void
__ch_link_handler (handler)
     struct __ch_handler *handler;
{
  handler->prev = __getexceptionStack ();
  __setexceptionStack (handler);
}

 void
__ch_unlink_handler (handler)
     struct __ch_handler *handler;
{
  __setexceptionStack (handler->prev);
}

/*
 * function __cause_exception
 *
 * parameters:
 *  exnum		name string of exception to raise
 *  file		filename of CAUSE statement
 *  lineno		linenumber of CAUSE statement
 *  user_arg		user specified argument
 *
 * returns:
 *  never		leave function with longjmp or abort
 *
 * abstract:
 *  search exceptionstack for last handler of caused exception,
 *  call userdefined function to signal exception,
 *  jump to handler with longjmp or call unhandled_exception
 *
 */

void
__cause_exception  (ex, file, lineno, user_arg)
     char *ex;
     char *file;
     int  lineno;
     int user_arg;
{
  register struct __ch_handler *handler = __getexceptionStack();

  /* call user defined cause function */
  cause_exception (ex, file, lineno, user_arg);
  
  for ( ; handler != NULL; handler = handler->prev)
    {
      register struct __ch_handled_excepts *list = handler->handlers;
      for ( ; list->code != 0; list++ )
	{
	  if (list->ex == __ch_else_except || EX_EQ(list->ex, ex)) /* found */
	    {
	      __setexceptionStack (handler->prev);
	      longjmp(handler->jbuf, list->code);	  
	    }
	}
    }

  /* no handler found -- call unhandled_exception */
  unhandled_exception (ex, file, lineno, user_arg);
  abort ();
}

/*
 * function __cause_ex1
 *
 * parameters:
 *  exnum		name string of exception to raise
 *  file		filename of CAUSE statement
 *  lineno		linenumber of CAUSE statement
 *
 * returns:
 *  never		leave function with longjmp or abort
 *
 * abstract:
 *  This is the function the compiler generated code calls.
 *  Search exceptionstack for last handler of caused exception,
 *  call userdefined function to signal exception,
 *  jump to handler with longjmp or call unhandled_exception
 *
 */

void
__cause_ex1  (ex, file, lineno)
     char *ex;
     char *file;
     int  lineno;
{
  __cause_exception (ex, file, lineno, 0);
}
