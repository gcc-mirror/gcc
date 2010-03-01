/* Copyright (C) 2005-2009 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>
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
