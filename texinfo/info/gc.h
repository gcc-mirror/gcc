/* gc.h -- Functions for garbage collecting unused node contents.
   $Id: gc.h,v 1.1.1.2 1998/03/22 20:42:32 law Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993, 97 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

#ifndef INFO_GC_H
#define INFO_GC_H

/* Add POINTER to the list of garbage collectible pointers.  A pointer
   is not actually garbage collected until no info window contains a node
   whose contents member is equal to the pointer. */
extern void add_gcable_pointer ();

/* Grovel the list of info windows and gc-able pointers finding those
   node->contents which are collectible, and free them. */
extern void gc_pointers ();

#endif /* not INFO_GC_H */
