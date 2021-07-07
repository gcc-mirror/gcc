/* Break.c implements an interrupt handler for SIGINT.

Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

#include <config.h>

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#if defined(HAVE_STDARG_H)
#include <stdarg.h>
#endif

#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif

#if defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

typedef void (*PROC) (void);

#if defined(HAVE_SIGNAL_H)
#include <signal.h>

struct plist
{
  PROC proc;
  struct plist *next;
};

static struct plist *head = NULL;

/* localHandler - dismisses the parameter, p, and invokes the GNU
   Modula-2 handler.  */

static void
localHandler (int p)
{
  if (head != NULL)
    head->proc ();
}

/* EnableBreak - enable the current break handler.  */

void
Break_EnableBreak (void)
{
  signal (SIGINT, localHandler);
}

/* DisableBreak - disable the current break handler (and all
   installed handlers).  */

void
Break_DisableBreak (void)
{
  signal (SIGINT, SIG_IGN);
}

/* InstallBreak - installs a procedure, p, to be invoked when a
   ctrl-c is caught.  Any number of these procedures may be stacked.
   Only the top procedure is run when ctrl-c is caught.  */

void
Break_InstallBreak (PROC p)
{
  struct plist *q = (struct plist *)malloc (sizeof (struct plist));

  if (q == NULL)
    {
      perror ("out of memory error in module Break");
      exit (1);
    }
  q->next = head;
  head = q;
  head->proc = p;
}

/* UnInstallBreak - pops the break handler stack.  */

void
Break_UnInstallBreak (void)
{
  struct plist *q = head;

  if (head != NULL)
    {
      head = head->next;
      free (q);
    }
}
#else
void
Break_EnableBreak (void)
{
}
void
Break_DisableBreak (void)
{
}
void
Break_InstallBreak (PROC *p)
{
}
void
Break_UnInstallBreak (void)
{
}
#endif
