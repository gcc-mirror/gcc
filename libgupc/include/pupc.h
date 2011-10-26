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
