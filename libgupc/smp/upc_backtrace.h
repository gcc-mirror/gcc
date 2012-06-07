/* Copyright (c) 2012
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


#ifndef _UPC_BACKTRACE_H_
#define _UPC_BACKTRACE_H_

/* Environment variables. */
/** Enable/Disable backtrace env variable. */
#define GUPCR_BACKTRACE_ENV "UPC_BACKTRACE"
/** GDB command for backtrace env variable. */
#define GUPCR_BACKTRACE_GDB_ENV "UPC_BACKTRACE_GDB"

/* Interfaces. */
extern void __upc_backtrace (void);
extern void __upc_fatal_backtrace (void);
extern void __upc_backtrace_init (const char *execname);

#if HAVE_LIBBFD
extern char **backtrace_src_symbols(void *const *buffer, int size,
				    const char *filename);
#endif

#endif /* !_UPC_BACKTRACE_H_ */
