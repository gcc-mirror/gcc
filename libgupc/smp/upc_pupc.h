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


#ifndef _UPC_PUPC_H_
#define _UPC_PUPC_H_

/* See GASP Specification (version 1.5)
   http://gasp.hcs.ufl.edu/gasp-1.5-61606.pdf  */

extern int pupc_control (int on);
extern unsigned int pupc_create_event (const char *name, const char *desc);

extern void pupc_event_start (unsigned int evttag, ...);
extern void pupc_event_end (unsigned int evttag, ...);
extern void pupc_event_atomic (unsigned int evttag, ...);

extern void pupc_event_startg (unsigned int evttag, const char *file, int line, ...);
extern void pupc_event_endg (unsigned int evttag, const char *file, int line, ...);
extern void pupc_event_atomicg (unsigned int evttag, const char *file, int line, ...);

extern void __upc_pupc_init (int *, char ***);

/* The "##__VAR_ARGS__" syntax below, is required to support an empty optional argument
   see: http://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html  */
#define p_start(evttag, ...)  pupc_event_startg (evttag, filename, linenum, ##__VA_ARGS__)
#define p_end(evttag, ...)    pupc_event_endg (evttag, filename, linenum, ##__VA_ARGS__)
#define p_atomic(evttag, ...) pupc_event_atomicg (evttag, filename, linenum, ##__VA_ARGS__)

#define p_startx(evttag, ...)  pupc_event_startg (evttag, NULL, 0, ##__VA_ARGS__)
#define p_endx(evttag, ...)    pupc_event_endg (evttag, NULL, 0, ##__VA_ARGS__)
#define p_atomicx(evttag, ...) pupc_event_atomicg (evttag, NULL, 0, ##__VA_ARGS__)

#endif /* _UPC_PUPC_H_ */
