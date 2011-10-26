/* Copyright (c) 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#include "gasp.h"
#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_pupc.h"

static GUPCR_THREAD_LOCAL gasp_context_t __upc_gasp_ctx;

int
pupc_control (int on)
{
  return gasp_control (__upc_gasp_ctx, on);
}

unsigned int
pupc_create_event (const char *name, const char *desc)
{
  return gasp_create_event (__upc_gasp_ctx, name, desc);
}

void
pupc_event_start (unsigned int evttag, ...)
{
  va_list argptr;
  va_start (argptr, evttag);
  gasp_event_notifyVA (__upc_gasp_ctx, evttag, GASP_START, NULL, 0, 0,
		       argptr);
  va_end (argptr);
}

void
pupc_event_end (unsigned int evttag, ...)
{
  va_list argptr;
  va_start (argptr, evttag);
  gasp_event_notifyVA (__upc_gasp_ctx, evttag, GASP_END, NULL, 0, 0, argptr);
  va_end (argptr);
}

void
pupc_event_atomic (unsigned int evttag, ...)
{
  va_list argptr;
  va_start (argptr, evttag);
  gasp_event_notifyVA (__upc_gasp_ctx, evttag, GASP_ATOMIC, NULL, 0, 0,
		       argptr);
  va_end (argptr);
}

void
pupc_event_startg (unsigned int evttag, const char *filename, int linenum, ...)
{
  va_list argptr;
  va_start (argptr, linenum);
  gasp_event_notifyVA (__upc_gasp_ctx, evttag, GASP_START, filename, linenum, 0,
		       argptr);
  va_end (argptr);
}

void
pupc_event_endg (unsigned int evttag, const char *filename, int linenum, ...)
{
  va_list argptr;
  va_start (argptr, linenum);
  gasp_event_notifyVA (__upc_gasp_ctx, evttag, GASP_END, filename, linenum, 0,
		       argptr);
  va_end (argptr);
}

void
pupc_event_atomicg (unsigned int evttag, const char *filename, int linenum, ...)
{
  va_list argptr;
  va_start (argptr, linenum);
  gasp_event_notifyVA (__upc_gasp_ctx, evttag, GASP_ATOMIC,
		       filename, linenum, 0, argptr);
  va_end (argptr);
}

void
__upc_pupc_init (int *argc, char ***argv)
{
  __upc_gasp_ctx =  gasp_init (GASP_MODEL_UPC, argc, argv);
}
