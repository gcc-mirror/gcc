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


#ifndef _GASP_H_
#define _GASP_H_

#include <stdarg.h>

/* See GASP Specification (version 1.5)
   http://gasp.hcs.ufl.edu/gasp-1.5-61606.pdf  */

#define GASP_VERSION 20060914

typedef enum
{
  GASP_MODEL_UPC,
  GASP_MODEL_TITANIUM,
  GASP_MODEL_CAF,
  GASP_MODEL_MPI,
  GASP_MODEL_SHMEM
} gasp_model_t;

typedef enum
{
  GASP_START,
  GASP_END,
  GASP_ATOMIC,
} gasp_evttype_t;

struct _gasp_context_S;
typedef struct _gasp_context_S *gasp_context_t;

gasp_context_t gasp_init (gasp_model_t srcmodel, int *argc, char ***argv);

void gasp_event_notify (gasp_context_t context, unsigned int evttag,
			gasp_evttype_t evttype, const char *filename,
			int linenum, int colnum, ...);

void gasp_event_notifyVA (gasp_context_t context, unsigned int evttag,
			  gasp_evttype_t evttype, const char *filename,
			  int linenum, int colnum, va_list varargs);

int gasp_control (gasp_context_t context, int on);

unsigned int gasp_create_event (gasp_context_t context,
				const char *name, const char *desc);

#endif /* _GASP_H_ */
