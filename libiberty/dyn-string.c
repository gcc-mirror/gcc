/* An abstract string datatype.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Mark Mitchell (mark@markmitchell.com).

This file is part of GNU CC.
   
GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libiberty.h"
#include "dyn-string.h"

/* Performs in-place initialization of a dyn_string struct.  This
   function can be used with a dyn_string struct on the stack or
   embedded in another object.  The contents of of the string itself
   are still dynamically allocated.  The string initially is capable
   of holding at least SPACE characeters, including the terminating
   NUL.  If SPACE is 0, it will silently be increated to 1.  */

void
dyn_string_init (ds_struct_ptr, space)
     struct dyn_string *ds_struct_ptr;
     int space;
{
  /* We need at least one byte in which to store the terminating NUL.  */
  if (space == 0)
    space = 1;

  ds_struct_ptr->allocated = space;
  ds_struct_ptr->s = (char *) xmalloc (space);
  ds_struct_ptr->length = 0;
  ds_struct_ptr->s[0] = '\0';
}    

/* Create a new dynamic string capable of holding at least SPACE characters,
   including the terminating NUL.  If SPACE is 0, it will be silently
   increased to 1.  */

dyn_string_t 
dyn_string_new (space)
     int space;
{
  dyn_string_t result = (dyn_string_t) xmalloc (sizeof (struct dyn_string));
  dyn_string_init (result, space);
  return result;
}

/* Free the memory used by DS.  */

void 
dyn_string_delete (ds)
     dyn_string_t ds;
{
  free (ds->s);
  free (ds);
}

/* Returns the contents of DS in a buffer allocated with malloc.  It
   is the caller's responsibility to deallocate the buffer using free.
   DS is then set to the empty string.  */

char*
dyn_string_release (ds)
     dyn_string_t ds;
{
  /* Store the old buffer.  */
  char* result = ds->s;
  /* The buffer is no longer owned by DS.  */
  ds->s = NULL;
  /* Reinitialize DS to the empty string.  */
  dyn_string_init (ds, 0);
  /* Return the old buffer.  */
  return result;
}

/* Increase the capacity of DS so it can hold at least SPACE
   characters, plus the terminating NUL.  This function will not (at
   present) reduce the capacity of DS.  */

dyn_string_t 
dyn_string_resize (ds, space)
     dyn_string_t ds;
     int space;
{
  int new_allocated = ds->allocated;

  /* Increase SPACE to hold the NUL termination.  */
  ++space;

  while (space > new_allocated)
    new_allocated *= 2;
    
  if (new_allocated != ds->allocated)
    {
      /* We actually need more space.  */
      ds->allocated = new_allocated;
      ds->s = (char *) xrealloc (ds->s, ds->allocated);
    }

  return ds;
}

/* Sets the contents of DS to the empty string.  */

void
dyn_string_clear (ds)
     dyn_string_t ds;
{
  /* A dyn_string always has room for at least the NUL terminator.  */
  ds->s[0] = '\0';
  ds->length = 0;
}

/* Makes the contents of DEST the same as the contents of SRC.  DEST
   and SRC must be distinct.  */

void
dyn_string_copy (dest, src)
     dyn_string_t dest;
     dyn_string_t src;
{
  if (dest == src)
    abort ();

  /* Make room in DEST.  */
  dyn_string_resize (dest, src->length);
  /* Copy DEST into SRC.  */
  strcpy (dest->s, src->s);
  /* Update the size of DEST.  */
  dest->length = src->length;
}

/* Copies SRC, a NUL-terminated string, into DEST.  */

void
dyn_string_copy_cstr (dest, src)
     dyn_string_t dest;
     const char *src;
{
  int length = strlen (src);
  /* Make room in DEST.  */
  dyn_string_resize (dest, length);
  /* Copy DEST into SRC.  */
  strcpy (dest->s, src);
  /* Update the size of DEST.  */
  dest->length = length;
}

/* Inserts SRC at the beginning of DEST.  DEST is expanded as
   necessary.  SRC and DEST must be distinct.  */

void 
dyn_string_prepend (dest, src)
     dyn_string_t dest;
     dyn_string_t src;
{
  dyn_string_insert (dest, 0, src);
}

/* Inserts SRC, a NUL-terminated string, at the beginning of DEST.
   DEST is expanded as necessary.  */

void 
dyn_string_prepend_cstr (dest, src)
     dyn_string_t dest;
     const char *src;
{
  dyn_string_insert_cstr (dest, 0, src);
}

/* Inserts SRC into DEST starting at position POS.  DEST is expanded as
   necessary.  SRC and DEST must be distinct.  */

void 
dyn_string_insert (dest, pos, src)
     dyn_string_t dest;
     int pos;
     dyn_string_t src;
{
  int i;

  if (src == dest)
    abort ();

  dyn_string_resize (dest, dest->length + src->length);
  /* Make room for the insertion.  Be sure to copy the NUL.  */
  for (i = dest->length; i >= pos; --i)
    dest->s[i + src->length] = dest->s[i];
  /* Splice in the new stuff.  */
  strncpy (dest->s + pos, src->s, src->length);
  /* Compute the new length.  */
  dest->length += src->length;
}

/* Inserts SRC, a NUL-terminated string, into DEST starting at
   position POS.  DEST is expanded as necessary.  */

void 
dyn_string_insert_cstr (dest, pos, src)
     dyn_string_t dest;
     int pos;
     const char *src;
{
  int i;
  int length = strlen (src);

  dyn_string_resize (dest, dest->length + length);
  /* Make room for the insertion.  Be sure to copy the NUL.  */
  for (i = dest->length; i >= pos; --i)
    dest->s[i + length] = dest->s[i];
  /* Splice in the new stuff.  */
  strncpy (dest->s + pos, src, length);
  /* Compute the new length.  */
  dest->length += length;
}

/* Append S to DS, resizing DS if necessary.  Returns DS.  */

dyn_string_t
dyn_string_append (ds, s)
     dyn_string_t ds;
     dyn_string_t s;
{
  dyn_string_resize (ds, ds->length + s->length);
  strcpy (ds->s + ds->length, s->s);
  ds->length += s->length;
  return ds;
}

/* Append the NUL-terminated string S to DS, resizing DS if necessary.
   Returns DS.  */

dyn_string_t 
dyn_string_append_cstr (ds, s)
     dyn_string_t ds;
     const char *s;
{
  int len = strlen (s);

  /* The new length is the old length plus the size of our string, plus
     one for the null at the end.  */
  dyn_string_resize (ds, ds->length + len);
  strcpy (ds->s + ds->length, s);
  ds->length += len;

  return ds;
}

/* Appends C to the end of DS.  */

dyn_string_t 
dyn_string_append_char (ds, c)
     dyn_string_t ds;
     int c;
{
  /* Make room for the extra character.  */
  dyn_string_resize (ds, ds->length + 1);
  /* Append the character; it will overwrite the old NUL.  */
  ds->s[ds->length] = c;
  /* Add a new NUL at the end.  */
  ds->s[ds->length + 1] = '\0';
  /* Update the length.  */
  ++(ds->length);
  return ds;
}

/* Sets the contents of DEST to the substring of SRC starting at START
   and ending before END.  START must be less than or equal to END,
   and both must be between zero and the length of SRC, inclusive.  */

void
dyn_string_substring (dest, src, start, end)
     dyn_string_t dest;
     dyn_string_t src;
     int start;
     int end;
{
  int i;
  int length = end - start;

  if (start > end || start > src->length || end > src->length)
    abort ();

  /* Make room for the substring.  */
  dyn_string_resize (dest, length);
  /* Copy the characters in the substring,  */
  for (i = length; --i >= 0; )
    dest->s[i] = src->s[start + i];
  /* NUL-terimate the result.  */
  dest->s[length] = '\0';
  /* Record the length of the substring.  */
  dest->length = length;
}

/* Returns non-zero if DS1 and DS2 have the same contents.  */

int
dyn_string_eq (ds1, ds2)
     dyn_string_t ds1;
     dyn_string_t ds2;
{
  /* If DS1 and DS2 have different lengths, they must not be the same.  */
  if (ds1->length != ds2->length)
    return 0;
  else
    return !strcmp (ds1->s, ds2->s);
}
