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

#ifndef __rtltypes_h__
#define __rtltypes_h__

#include <setjmp.h>

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

/* argc, argv */
typedef struct
{
    unsigned short	len;
    char		body[0];
} TVaryingCharType;

#ifndef __CHILL_LIB__
extern TVaryingCharType	**chill_argv;
extern int		chill_argc;
#endif

/* definitions for exceptions */
typedef struct
{
    char	*exname;
    short	exnumber;
} TExceptionDefinition;

#if 1
typedef char *__ch_exception;
#define EX_EQ(e1, e2) (strcmp(e1, e2)==0)
#else
typedef void *__ch_exception;
#define EX_EQ(e1, e2) (e1 == e2)
#endif
#define __ch_else_except ((__ch_exception)0)

struct __ch_handled_excepts
{
  /* List is ended by a code==0, or ex==__ch_else_except (ELSE handler). */
  __ch_exception ex;
  int code; /* Positive number indicating ordinal in handler list. */
};

/* definitions for exception handlers */
typedef struct  __ch_handler
{
  struct __ch_handler *prev;
  struct __ch_handled_excepts *handlers;
  jmp_buf jbuf;
} TExceptionHandlerStack;

/* exceptions */
#define EXCEPTION(x)	/* nothing */

#endif /* __rtltypes_h__ */
