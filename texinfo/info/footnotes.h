/* footnotes.h -- Some functions for manipulating footnotes. */

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

#if !defined (_FOOTNOTES_H_)
#define _FOOTNOTES_H_

/* Magic string which indicates following text is footnotes. */
#define FOOTNOTE_LABEL "---------- Footnotes ----------"

#define FN_FOUND   0
#define FN_UNFOUND 1
#define FN_UNABLE  2


/* Create or delete the footnotes window depending on whether footnotes
   exist in WINDOW's node or not.  Returns FN_FOUND if footnotes were found
   and displayed.  Returns FN_UNFOUND if there were no footnotes found
   in WINDOW's node.  Returns FN_UNABLE if there were footnotes, but the
   window to show them couldn't be made. */
extern int info_get_or_remove_footnotes ();

/* Non-zero means attempt to show footnotes when displaying a new window. */
extern int auto_footnotes_p;

#endif /* !_FOOTNOTES_H_ */

