/* Copyright (C) 2005-2009 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>.
   Original Implementation by Adam Leko <adam@leko.org>.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "gasp.h"

/* Since libupc contains references to these functions, we provide dummy
   implementations to prevent linker warnings when GASP support has been
   compiled into GCC UPC, but the user compiles their app regularly.
   We define these as weak symbols so tools can override them
   appropriately.  */

#pragma weak gasp_init
#pragma weak gasp_event_notify
#pragma weak gasp_event_notifyVA
#pragma weak gasp_control
#pragma weak gasp_create_event

gasp_context_t
gasp_init (gasp_model_t ARG_UNUSED (srcmodel),
	   int *ARG_UNUSED (argc), char ***ARG_UNUSED (argv))
{
  return 0;
}

void
gasp_event_notify (gasp_context_t ARG_UNUSED (context),
		   unsigned int ARG_UNUSED (evttag),
		   gasp_evttype_t ARG_UNUSED (evttype),
		   const char *ARG_UNUSED (filename),
		   int ARG_UNUSED (linenum), int ARG_UNUSED (colnum), ...)
{
}

void
gasp_event_notifyVA (gasp_context_t ARG_UNUSED (context),
		     unsigned int ARG_UNUSED (evttag),
		     gasp_evttype_t ARG_UNUSED (evttype),
		     const char *ARG_UNUSED (filename),
		     int ARG_UNUSED (linenum),
		     int ARG_UNUSED (colnum), va_list ARG_UNUSED (varargs))
{
}

int
gasp_control (gasp_context_t ARG_UNUSED (context), int ARG_UNUSED (on))
{
  return 0;
}

unsigned int
gasp_create_event (gasp_context_t ARG_UNUSED (context),
		   const char *ARG_UNUSED (name),
		   const char *ARG_UNUSED (desc))
{
  return 0;
}
