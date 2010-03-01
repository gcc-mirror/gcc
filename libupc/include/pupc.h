/* Copyright (C) 2006-2009 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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

#ifndef _PUPC_H_
#define _PUPC_H_

/* See GASP Specification (version 1.5)
   http://gasp.hcs.ufl.edu/gasp-1.5-61606.pdf  */

#if __UPC_PUPC__
    extern int pupc_control (int on);
    extern unsigned int pupc_create_event (const char *name, const char *desc);
#else
    #define pupc_control(on) 0
    #define pupc_create_event(name, desc) 0
#endif

#if __UPC_PUPC__ && __UPC_PUPC_INST__
    extern void pupc_event_startg (unsigned int evttag, const char *file, int line, ...);
    extern void pupc_event_endg (unsigned int evttag, const char *file, int line, ...);
    extern void pupc_event_atomicg (unsigned int evttag, const char *file, int line, ...);
    #define pupc_event_start(evttag, args...)  pupc_event_startg (evttag, __FILE__, __LINE__, args)
    #define pupc_event_end(evttag, args...)    pupc_event_endg (evttag, __FILE__, __LINE__, args)
    #define pupc_event_atomic(evttag, args...) pupc_event_atomicg (evttag, __FILE__, __LINE__, args)
#else
    #define pupc_event_start(evttag, args...)
    #define pupc_event_end(evttag, args...)
    #define pupc_event_atomic(evttag, args...)
#endif

#ifndef pupc_event_start
    /* These prototypes won't be compiled, because the
       macro definitions above will over-ride them.
       The prototypes are here for ducumentation
       purposes only.  */
    extern void pupc_event_start (unsigned int evttag, ...);
    extern void pupc_event_end (unsigned int evttag, ...);
    extern void pupc_event_atomic (unsigned int evttag, ...);
#endif

#endif /* _PUPC_H_ */
