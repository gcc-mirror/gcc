/* man.h: Defines and external function declarations for man.c.
   $Id: man.h,v 1.1.1.2 1998/03/22 20:42:46 law Exp $

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

   Author: Brian J. Fox (bfox@ai.mit.edu) Sat May  6 16:19:13 1995. */

#ifndef INFO_MAN_H
#define INFO_MAN_H

#define MANPAGE_FILE_BUFFER_NAME "*manpages*"

extern NODE *make_manpage_node (/* char *pagename */);
extern NODE *get_manpage_node (/* FILE_BUFFER *file_buffer, char *pagename */);
extern FILE_BUFFER *create_manpage_file_buffer (/* void */);
extern long locate_manpage_xref (/* NODE *node, long start, int dir */);
extern REFERENCE **xrefs_of_manpage (/* NODE *node */);
extern REFERENCE **manpage_xrefs_in_binding (/* NODE *node, SEARCH_BINDING *binding */);

#endif /* INFO_MAN_H */
