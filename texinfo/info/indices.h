/* indices.h -- Functions defined in indices.c.
   $Id: indices.h,v 1.1.1.2 1998/03/22 20:42:34 law Exp $

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

#ifndef INFO_INDICES_H
#define INFO_INDICES_H

/* User-visible variable controls the output of info-index-next. */
extern int show_index_match;

extern REFERENCE **info_indices_of_window (), **info_indices_of_file_buffer ();
extern void info_apropos ();

/* For every menu item in DIR, search the indices of that file for STRING. */
REFERENCE **apropos_in_all_indices ();

/* User visible functions declared in indices.c. */
extern void info_index_search (), info_next_index_match ();
extern void do_info_index_search ();
extern int index_intry_exists ();

#endif /* not INFO_INDICES_H */
