/* Define compiler-visible UPC runtime entry points and variables.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _UPC_RTS_NAMES_H_
#define _UPC_RTS_NAMES_H_ 1

/* Name of initialization routine that is called to initialize
   shared variables and calculate shared address.  Both of these
   operations must be performed at runtime before UPC's main
   program is called.  */
#define UPC_INIT_DECLS_FUNC "__upc_init_decls"

/* Name of runtime variable that is used by the code generated
   for the 'upc_forall' statement to implement nested upc_forall
   semantics.  Per the language specification, a dynamically nested
   upc_forall statement with an affinity clause will operate
   as if "continue" had been supplied for the affinity clause.  */
#define UPC_FORALL_DEPTH_NAME "__upc_forall_depth"

/* Name of the runtime variable holding the address of the beginning of
   the global shared region. */
#define UPC_GLOBAL_BASE_NAME "__upc_global_base"

/* Names of various UPC runtime library routines that implement various
   UPC statement constructs. */
#define UPC_BARRIER_LIBCALL "__upc_barrier"
#define UPC_GETADDR_LIBCALL "__getaddr"
#define UPC_NOTIFY_LIBCALL "__upc_notify"
#define UPC_WAIT_LIBCALL "__upc_wait"

/* Profiled/debugged runtime library routines  */
#define UPC_BARRIERG_LIBCALL "__upc_barrierg"
#define UPC_GETADDRG_LIBCALL "__getaddrg"
#define UPC_NOTIFYG_LIBCALL "__upc_notifyg"
#define UPC_NOTIFYG_LIBCALL "__upc_notifyg"
#define UPC_WAITG_LIBCALL "__upc_waitg"

/* Runtime library function that records upc_forall begin/end
   when -fupc-instrument is asserted.  */
#define UPC_INSTRUMENT_FORALL "__upc_forallg"

/* Runtime library function that records function entry/exit 
   when -fupc-instrument-functions is asserted.  */
#define UPC_INSTRUMENT_FUNC "__upc_funcg"

#endif  /* _UPC_RTS_NAMES_H_ */
