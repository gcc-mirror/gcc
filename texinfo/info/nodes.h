/* nodes.h -- How we represent nodes internally.
   $Id: nodes.h,v 1.1.1.2 1998/03/22 20:42:49 law Exp $

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

#if !defined (NODES_H)
#define NODES_H

#include "info.h"

/* **************************************************************** */
/*                                                                  */
/*                    User Code Interface                           */
/*                                                                  */
/* **************************************************************** */

/* Callers generally only want the node itself.  This structure is used
   to pass node information around.  None of the information in this
   structure should ever be directly freed.  The structure itself can
   be passed to free ().  Note that NODE->parent is non-null if this
   node's file is a subfile.  In that case, NODE->parent is the logical
   name of the file containing this node.  Both names are given as full
   paths, so you might have: node->filename = "/usr/gnu/info/emacs-1",
   with node->parent = "/usr/gnu/info/emacs". */
typedef struct {
  char *filename;               /* The physical file containing this node. */
  char *parent;                 /* Non-null is the logical file name. */
  char *nodename;               /* The name of this node. */
  char *contents;               /* Characters appearing in this node. */
  long nodelen;                 /* The length of the CONTENTS member. */
  int flags;                    /* See immediately below. */
} NODE;

/* Defines that can appear in NODE->flags.  All informative. */
#define N_HasTagsTable 0x01     /* This node was found through a tags table. */
#define N_TagsIndirect 0x02     /* The tags table was an indirect one. */
#define N_UpdateTags   0x04     /* The tags table is out of date. */
#define N_IsCompressed 0x08     /* The file is compressed on disk. */
#define N_IsInternal   0x10     /* This node was made by Info. */
#define N_CannotGC     0x20     /* File buffer cannot be gc'ed. */
#define N_IsManPage    0x40     /* This node is a Un*x manpage. */

/* **************************************************************** */
/*                                                                  */
/*                     Internal Data Structures                     */
/*                                                                  */
/* **************************************************************** */

/* Some defines describing details about Info file contents. */

/* String Constants. */
#define INFO_FILE_LABEL                 "File:"
#define INFO_NODE_LABEL                 "Node:"
#define INFO_PREV_LABEL                 "Prev:"
#define INFO_ALTPREV_LABEL              "Previous:"
#define INFO_NEXT_LABEL                 "Next:"
#define INFO_UP_LABEL                   "Up:"
#define INFO_MENU_LABEL                 "\n* Menu:"
#define INFO_MENU_ENTRY_LABEL           "\n* "
#define INFO_XREF_LABEL                 "*Note"
#define TAGS_TABLE_END_LABEL            "\nEnd Tag Table"
#define TAGS_TABLE_BEG_LABEL            "Tag Table:\n"
#define INDIRECT_TAGS_TABLE_LABEL       "Indirect:\n"
#define TAGS_TABLE_IS_INDIRECT_LABEL    "(Indirect)"

/* Character Constants. */
#define INFO_COOKIE '\037'
#define INFO_FF     '\014'
#define INFO_TAGSEP '\177'

/* For each logical file that we have loaded, we keep a list of the names
   of the nodes that are found in that file.  A pointer to a node in an
   info file is called a "tag".  For split files, the tag pointer is
   "indirect"; that is, the pointer also contains the name of the split
   file where the node can be found.  For non-split files, the filename
   member in the structure below simply contains the name of the current
   file.  The following structure describes a single node within a file. */
typedef struct {
  char *filename;               /* The file where this node can be found. */
  char *nodename;               /* The node pointed to by this tag. */
  long nodestart;               /* The offset of the start of this node. */
  long nodelen;                 /* The length of this node. */
} TAG;

/* The following structure is used to remember information about the contents
   of Info files that we have loaded at least once before.  The FINFO member
   is present so that we can reload the file if it has been modified since
   last being loaded.  All of the arrays appearing within this structure
   are NULL terminated, and each array which can change size has a
   corresponding SLOTS member which says how many slots have been allocated
   (with malloc ()) for this array. */
typedef struct {
  char *filename;               /* The filename used to find this file. */
  char *fullpath;               /* The full pathname of this info file. */
  struct stat finfo;            /* Information about this file. */
  char *contents;               /* The contents of this particular file. */
  long filesize;                /* The number of bytes this file expands to. */
  char **subfiles;              /* If non-null, the list of subfiles. */
  TAG **tags;                   /* If non-null, the indirect tags table. */
  int tags_slots;               /* Number of slots allocated for TAGS. */
  int flags;                    /* Various flags.  Mimics of N_* flags. */
} FILE_BUFFER;

/* **************************************************************** */
/*                                                                  */
/*                  Externally Visible Functions                    */
/*                                                                  */
/* **************************************************************** */

/* Array of FILE_BUFFER * which represents the currently loaded info files. */
extern FILE_BUFFER **info_loaded_files;

/* The number of slots currently allocated to INFO_LOADED_FILES. */
extern int info_loaded_files_slots;

/* Locate the file named by FILENAME, and return the information structure
   describing this file.  The file may appear in our list of loaded files
   already, or it may not.  If it does not already appear, find the file,
   and add it to the list of loaded files.  If the file cannot be found,
   return a NULL FILE_BUFFER *. */
extern FILE_BUFFER *info_find_file ();

/* Force load the file named FILENAME, and return the information structure
   describing this file.  Even if the file was already loaded, this loads
   a new buffer, rebuilds tags and nodes, and returns a new FILE_BUFFER *. */
extern FILE_BUFFER *info_load_file ();

/* Return a pointer to a NODE structure for the Info node (FILENAME)NODENAME.
   FILENAME can be passed as NULL, in which case the filename of "dir" is used.
   NODENAME can be passed as NULL, in which case the nodename of "Top" is used.
   If the node cannot be found, return a NULL pointer. */
extern NODE *info_get_node ();

/* Return a pointer to a NODE structure for the Info node NODENAME in
   FILE_BUFFER.  NODENAME can be passed as NULL, in which case the
   nodename of "Top" is used.  If the node cannot be found, return a
   NULL pointer. */
extern NODE *info_get_node_of_file_buffer ();

/* Grovel FILE_BUFFER->contents finding tags and nodes, and filling in the
   various slots.  This can also be used to rebuild a tag or node table. */
extern void build_tags_and_nodes ();

/* When non-zero, this is a string describing the most recent file error. */
extern char *info_recent_file_error;

/* Create a new, empty file buffer. */
extern FILE_BUFFER *make_file_buffer ();

#endif /* !NODES_H */
