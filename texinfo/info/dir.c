/* dir.c -- How to build a special "dir" node from "localdir" files.
   $Id: dir.c,v 1.1.1.2 1998/03/22 20:42:22 law Exp $

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

#include "info.h"
#include "info-utils.h"
#include "filesys.h"
#include "tilde.h"

/* The "dir" node can be built from the contents of a file called "dir",
   with the addition of the menus of every file named in the array
   dirs_to_add which are found in INFOPATH. */

static void add_menu_to_file_buffer (), insert_text_into_fb_at_binding ();

static char *dirs_to_add[] = {
  "dir", "localdir", (char *)NULL
};


/* Return zero if the file represented in the stat structure TEST has
   already been seen, nonzero else.  */

typedef struct
{
  unsigned long device;
  unsigned long inode;
} dir_file_list_entry_type;

static int
new_dir_file_p (test)
    struct stat *test;
{
  static unsigned dir_file_list_len = 0;
  static dir_file_list_entry_type *dir_file_list = NULL;
  unsigned i;
  
  for (i = 0; i < dir_file_list_len; i++)
    {
      dir_file_list_entry_type entry;
      entry = dir_file_list[i];
      if (entry.device == test->st_dev && entry.inode == test->st_ino)
        return 0;
    }
  
  dir_file_list_len++;
  dir_file_list = xrealloc (dir_file_list, 
                        dir_file_list_len * sizeof (dir_file_list_entry_type));
  dir_file_list[dir_file_list_len - 1].device = test->st_dev;
  dir_file_list[dir_file_list_len - 1].inode = test->st_ino;
  return 1;
}


void
maybe_build_dir_node (dirname)
     char *dirname;
{
  int path_index, update_tags;
  char *this_dir;
  FILE_BUFFER *dir_buffer = info_find_file (dirname);

  /* If there is no "dir" in the current info path, we cannot build one
     from nothing. */
  if (!dir_buffer)
    return;

  /* If this directory has already been built, return now. */
  if (dir_buffer->flags & N_CannotGC)
    return;

  /* Initialize the list we use to avoid reading the same dir file twice
     with the dir file just found.  */
  new_dir_file_p (&dir_buffer->finfo);
  
  path_index = update_tags = 0;

  /* Using each element of the path, check for one of the files in
     DIRS_TO_ADD.  Do not check for "localdir.info.Z" or anything else.
     Only files explictly named are eligible.  This is a design decision.
     There can be an info file name "localdir.info" which contains
     information on the setting up of "localdir" files. */
  while ((this_dir = extract_colon_unit (infopath, &path_index)))
    {
      register int da_index;
      char *from_file;

      /* Expand a leading tilde if one is present. */
      if (*this_dir == '~')
        {
          char *tilde_expanded_dirname;

          tilde_expanded_dirname = tilde_expand_word (this_dir);
          if (tilde_expanded_dirname != this_dir)
            {
              free (this_dir);
              this_dir = tilde_expanded_dirname;
            }
        }

      /* For every different file named in DIRS_TO_ADD found in the
         search path, add that file's menu to our "dir" node. */
      for (da_index = 0; (from_file = dirs_to_add[da_index]); da_index++)
        {
          struct stat finfo;
          int statable;
          int namelen = strlen (from_file);
          char *fullpath = xmalloc (3 + strlen (this_dir) + namelen);
          
          strcpy (fullpath, this_dir);
          if (fullpath[strlen (fullpath) - 1] != '/')
            strcat (fullpath, "/");
          strcat (fullpath, from_file);

          statable = (stat (fullpath, &finfo) == 0);

          /* Only add this file if we have not seen it before.  */
          if (statable && S_ISREG (finfo.st_mode) && new_dir_file_p (&finfo))
            {
              long filesize;
              char *contents = filesys_read_info_file (fullpath, &filesize,
                                                       &finfo);
              if (contents)
                {
                  update_tags++;
                  add_menu_to_file_buffer (contents, filesize, dir_buffer);
                  free (contents);
                }
            }

          free (fullpath);
        }
      free (this_dir);
    }

  if (update_tags)
    build_tags_and_nodes (dir_buffer);

  /* Flag that the dir buffer has been built. */
  dir_buffer->flags |= N_CannotGC;
}

/* Given CONTENTS and FB (a file buffer), add the menu found in CONTENTS
   to the menu found in FB->contents.  Second argument SIZE is the total
   size of CONTENTS. */
static void
add_menu_to_file_buffer (contents, size, fb)
     char *contents;
     long size;
     FILE_BUFFER *fb;
{
  SEARCH_BINDING contents_binding, fb_binding;
  long contents_offset, fb_offset;

  contents_binding.buffer = contents;
  contents_binding.start = 0;
  contents_binding.end = size;
  contents_binding.flags = S_FoldCase | S_SkipDest;

  fb_binding.buffer = fb->contents;
  fb_binding.start = 0;
  fb_binding.end = fb->filesize;
  fb_binding.flags = S_FoldCase | S_SkipDest;

  /* Move to the start of the menus in CONTENTS and FB. */
  contents_offset = search_forward (INFO_MENU_LABEL, &contents_binding);
  fb_offset = search_forward (INFO_MENU_LABEL, &fb_binding);

  /* If there is no menu in CONTENTS, quit now. */
  if (contents_offset == -1)
    return;

  /* There is a menu in CONTENTS, and contents_offset points to the first
     character following the menu starter string.  Skip all whitespace
     and newline characters. */
  contents_offset += skip_whitespace_and_newlines (contents + contents_offset);

  /* If there is no menu in FB, make one. */
  if (fb_offset == -1)
    {
      /* Find the start of the second node in this file buffer.  If there
         is only one node, we will be adding the contents to the end of
         this node. */
      fb_offset = find_node_separator (&fb_binding);

      /* If not even a single node separator, give up. */
      if (fb_offset == -1)
        return;

      fb_binding.start = fb_offset;
      fb_binding.start +=
        skip_node_separator (fb_binding.buffer + fb_binding.start);

      /* Try to find the next node separator. */
      fb_offset = find_node_separator (&fb_binding);

      /* If found one, consider that the start of the menu.  Otherwise, the
         start of this menu is the end of the file buffer (i.e., fb->size). */
      if (fb_offset != -1)
        fb_binding.start = fb_offset;
      else
        fb_binding.start = fb_binding.end;

      insert_text_into_fb_at_binding
        (fb, &fb_binding, INFO_MENU_LABEL, strlen (INFO_MENU_LABEL));

      fb_binding.buffer = fb->contents;
      fb_binding.start = 0;
      fb_binding.end = fb->filesize;
      fb_offset = search_forward (INFO_MENU_LABEL, &fb_binding);
      if (fb_offset == -1)
        abort ();
    }

  /* CONTENTS_OFFSET and FB_OFFSET point to the starts of the menus that
     appear in their respective buffers.  Add the remainder of CONTENTS
     to the end of FB's menu. */
  fb_binding.start = fb_offset;
  fb_offset = find_node_separator (&fb_binding);
  if (fb_offset != -1)
    fb_binding.start = fb_offset;
  else
    fb_binding.start = fb_binding.end;

  /* Leave exactly one blank line between directory entries. */
  {
    int num_found = 0;

    while ((fb_binding.start > 0) &&
           (whitespace_or_newline (fb_binding.buffer[fb_binding.start - 1])))
      {
        num_found++;
        fb_binding.start--;
      }

    /* Optimize if possible. */
    if (num_found >= 2)
      {
        fb_binding.buffer[fb_binding.start++] = '\n';
        fb_binding.buffer[fb_binding.start++] = '\n';
      }
    else
      {
        /* Do it the hard way. */
        insert_text_into_fb_at_binding (fb, &fb_binding, "\n\n", 2);
        fb_binding.start += 2;
      }
  }

  /* Insert the new menu. */
  insert_text_into_fb_at_binding
    (fb, &fb_binding, contents + contents_offset, size - contents_offset);
}

static void
insert_text_into_fb_at_binding (fb, binding, text, textlen)
     FILE_BUFFER *fb;
     SEARCH_BINDING *binding;
     char *text;
     int textlen;
{
  char *contents;
  long start, end;

  start = binding->start;
  end = fb->filesize;

  contents = (char *)xmalloc (fb->filesize + textlen + 1);
  memcpy (contents, fb->contents, start);
  memcpy (contents + start, text, textlen);
  memcpy (contents + start + textlen, fb->contents + start, end - start);
  free (fb->contents);
  fb->contents = contents;
  fb->filesize += textlen;
  fb->finfo.st_size = fb->filesize;
}
