/* doc.h -- Structure associating function pointers with documentation. */

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

#if !defined (DOC_H)
#define DOC_H

#include "info.h" /* for NAMED_FUNCTIONS, VFunction, etc.  */

typedef struct {
  VFunction *func;
#if defined (NAMED_FUNCTIONS)
  char *func_name;
#endif /* NAMED_FUNCTIONS */
  char *doc;
} FUNCTION_DOC;

extern FUNCTION_DOC function_doc_array[];

extern char *function_documentation ();
extern char *key_documentation ();
extern char *pretty_keyname ();
extern char *replace_in_documentation ();
extern void info_document_key ();
extern void dump_map_to_message_buffer ();

#if defined (NAMED_FUNCTIONS)
extern char *function_name ();
extern VFunction *named_function ();
#endif /* NAMED_FUNCTIONS */
#endif /* !DOC_H */
