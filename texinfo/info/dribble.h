/* dribble.h -- Functions and vars declared in dribble.c. */

/* This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993 Free Software Foundation, Inc.

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

#if !defined (_DRIBBLE_H_)
#define _DRIBBLE_H_

/* When non-zero, it is a stream to write all input characters to for the
   duration of this info session. */
extern FILE *info_dribble_file;

/* Open a dribble file named NAME, perhaps closing an already open one.
   This sets the global variable INFO_DRIBBLE_FILE to the open stream. */
extern void open_dribble_file ();

/* If there is a dribble file already open, close it. */
extern void close_dribble_file ();

/* Write some output to our existing dribble file. */
extern void dribble ();

#endif /* !_DRIBBLE_H_ */
