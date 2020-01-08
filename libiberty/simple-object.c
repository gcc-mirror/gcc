/* simple-object.c -- simple routines to read and write object files.
   Copyright (C) 2010-2020 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Google.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "libiberty.h"
#include "simple-object.h"

#include <errno.h>
#include <fcntl.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#include "simple-object-common.h"

/* The known object file formats.  */

static const struct simple_object_functions * const format_functions[] =
{
  &simple_object_elf_functions,
  &simple_object_mach_o_functions,
  &simple_object_coff_functions,
  &simple_object_xcoff_functions
};

/* Read data from a file using the simple_object error reporting
   conventions.  */

int
simple_object_internal_read (int descriptor, off_t offset,
			     unsigned char *buffer, size_t size,
			     const char **errmsg, int *err)
{
  if (lseek (descriptor, offset, SEEK_SET) < 0)
    {
      *errmsg = "lseek";
      *err = errno;
      return 0;
    }

  do
    {
      ssize_t got = read (descriptor, buffer, size);
      if (got == 0)
	break;
      else if (got > 0)
	{
	  buffer += got;
	  size -= got;
	}
      else if (errno != EINTR)
	{
	  *errmsg = "read";
	  *err = errno;
	  return 0;
	}
    }
  while (size > 0);

  if (size > 0)
    {
      *errmsg = "file too short";
      *err = 0;
      return 0;
    }

  return 1;
}

/* Write data to a file using the simple_object error reporting
   conventions.  */

int
simple_object_internal_write (int descriptor, off_t offset,
			      const unsigned char *buffer, size_t size,
			      const char **errmsg, int *err)
{
  if (lseek (descriptor, offset, SEEK_SET) < 0)
    {
      *errmsg = "lseek";
      *err = errno;
      return 0;
    }

  do
    {
      ssize_t wrote = write (descriptor, buffer, size);
      if (wrote == 0)
	break;
      else if (wrote > 0)
	{
	  buffer += wrote;
	  size -= wrote;
	}
      else if (errno != EINTR)
	{
	  *errmsg = "write";
	  *err = errno;
	  return 0;
	}
    }
  while (size > 0);

  if (size > 0)
    {
      *errmsg = "short write";
      *err = 0;
      return 0;
    }

  return 1;
}

/* Open for read.  */

simple_object_read *
simple_object_start_read (int descriptor, off_t offset,
			  const char *segment_name, const char **errmsg,
			  int *err)
{
  unsigned char header[SIMPLE_OBJECT_MATCH_HEADER_LEN];
  size_t len, i;

  if (!simple_object_internal_read (descriptor, offset, header,
				    SIMPLE_OBJECT_MATCH_HEADER_LEN,
				    errmsg, err))
    return NULL;

  len = sizeof (format_functions) / sizeof (format_functions[0]);
  for (i = 0; i < len; ++i)
    {
      void *data;

      data = format_functions[i]->match (header, descriptor, offset,
					 segment_name, errmsg, err);
      if (data != NULL)
	{
	  simple_object_read *ret;

	  ret = XNEW (simple_object_read);
	  ret->descriptor = descriptor;
	  ret->offset = offset;
	  ret->functions = format_functions[i];
	  ret->data = data;
	  return ret;
	}
    }

  *errmsg = "file not recognized";
  *err = 0;
  return NULL;
}

/* Find all sections.  */

const char *
simple_object_find_sections (simple_object_read *sobj,
			     int (*pfn) (void *, const char *, off_t, off_t),
			     void *data,
			     int *err)
{
  return sobj->functions->find_sections (sobj, pfn, data, err);
}

/* Internal data passed to find_one_section.  */

struct find_one_section_data
{
  /* The section we are looking for.  */
  const char *name;
  /* Where to store the section offset.  */
  off_t *offset;
  /* Where to store the section length.  */
  off_t *length;
  /* Set if the name is found.  */
  int found;
};

/* Internal function passed to find_sections.  */

static int
find_one_section (void *data, const char *name, off_t offset, off_t length)
{
  struct find_one_section_data *fosd = (struct find_one_section_data *) data;

  if (strcmp (name, fosd->name) != 0)
    return 1;

  *fosd->offset = offset;
  *fosd->length = length;
  fosd->found = 1;

  /* Stop iteration.  */
  return 0;
}

/* Find a section.  */

int
simple_object_find_section (simple_object_read *sobj, const char *name,
			    off_t *offset, off_t *length,
			    const char **errmsg, int *err)
{
  struct find_one_section_data fosd;

  fosd.name = name;
  fosd.offset = offset;
  fosd.length = length;
  fosd.found = 0;

  *errmsg = simple_object_find_sections (sobj, find_one_section,
					 (void *) &fosd, err);
  if (*errmsg != NULL)
    return 0;
  if (!fosd.found)
    return 0;
  return 1;
}

/* Callback to identify and rename LTO debug sections by name.
   Returns non-NULL if NAME is a LTO debug section, NULL if not.
   If RENAME is true it will rename LTO debug sections to non-LTO
   ones.  */

static char *
handle_lto_debug_sections (const char *name, int rename)
{
  char *newname = rename ? XCNEWVEC (char, strlen (name) + 1)
	  	         : xstrdup (name);

  /* ???  So we can't use .gnu.lto_ prefixed sections as the assembler
     complains about bogus section flags.  Which means we need to arrange
     for that to be fixed or .gnu.debuglto_ marked as SHF_EXCLUDE (to make
     fat lto object tooling work for the fat part).  */
  /* Also include corresponding reloc sections.  */
  if (strncmp (name, ".rela", sizeof (".rela") - 1) == 0)
    {
      if (rename)
        strncpy (newname, name, sizeof (".rela") - 1);
      name += sizeof (".rela") - 1;
    }
  else if (strncmp (name, ".rel", sizeof (".rel") - 1) == 0)
    {
      if (rename)
        strncpy (newname, name, sizeof (".rel") - 1);
      name += sizeof (".rel") - 1;
    }
  /* ???  For now this handles both .gnu.lto_ and .gnu.debuglto_ prefixed
     sections.  */
  /* Copy LTO debug sections and rename them to their non-LTO name.  */
  if (strncmp (name, ".gnu.debuglto_", sizeof (".gnu.debuglto_") - 1) == 0)
    return rename ? strcat (newname, name + sizeof (".gnu.debuglto_") - 1) : newname;
  else if (strncmp (name, ".gnu.lto_.debug_",
		    sizeof (".gnu.lto_.debug_") -1) == 0)
    return rename ? strcat (newname, name + sizeof (".gnu.lto_") - 1) : newname;
  /* Copy over .note.GNU-stack section under the same name if present.  */
  else if (strcmp (name, ".note.GNU-stack") == 0)
    return strcpy (newname, name);
  /* Copy over .comment section under the same name if present.  Solaris
     ld uses them to relax its checking of ELF gABI access rules for
     COMDAT sections in objects produced by GCC.  */
  else if (strcmp (name, ".comment") == 0)
    return strcpy (newname, name);
  free (newname);
  return NULL;
}

/* Wrapper for handle_lto_debug_sections.  */

static char *
handle_lto_debug_sections_rename (const char *name)
{
  return handle_lto_debug_sections (name, 1);
}

/* Wrapper for handle_lto_debug_sections.  */

static char *
handle_lto_debug_sections_norename (const char *name)
{
  return handle_lto_debug_sections (name, 0);
}

/* Copy LTO debug sections.  */

const char *
simple_object_copy_lto_debug_sections (simple_object_read *sobj,
				       const char *dest, int *err, int rename)
{
  const char *errmsg;
  simple_object_write *dest_sobj;
  simple_object_attributes *attrs;
  int outfd;

  if (! sobj->functions->copy_lto_debug_sections)
    {
      *err = EINVAL;
      return "simple_object_copy_lto_debug_sections not implemented";
    }

  attrs = simple_object_fetch_attributes (sobj, &errmsg, err);
  if (! attrs)
    return errmsg;
  dest_sobj = simple_object_start_write (attrs, NULL, &errmsg, err);
  simple_object_release_attributes (attrs);
  if (! dest_sobj)
    return errmsg;

  errmsg = sobj->functions->copy_lto_debug_sections
	 	 (sobj, dest_sobj,
		  rename ? handle_lto_debug_sections_rename
			 : handle_lto_debug_sections_norename,  err);
  if (errmsg)
    {
      simple_object_release_write (dest_sobj);
      return errmsg;
    }

  outfd = open (dest, O_CREAT|O_WRONLY|O_TRUNC|O_BINARY, 00777);
  if (outfd == -1)
    {
      *err = errno;
      simple_object_release_write (dest_sobj);
      return "open failed";
    }

  errmsg = simple_object_write_to_file (dest_sobj, outfd, err);
  close (outfd);
  if (errmsg)
    {
      simple_object_release_write (dest_sobj);
      return errmsg;
    }

  simple_object_release_write (dest_sobj);
  return NULL;
}

/* Fetch attributes.  */

simple_object_attributes *
simple_object_fetch_attributes (simple_object_read *sobj, const char **errmsg,
				int *err)
{
  void *data;
  simple_object_attributes *ret;

  data = sobj->functions->fetch_attributes (sobj, errmsg, err);
  if (data == NULL)
    return NULL;
  ret = XNEW (simple_object_attributes);
  ret->functions = sobj->functions;
  ret->data = data;
  return ret;
}

/* Release an simple_object_read.  */

void
simple_object_release_read (simple_object_read *sobj)
{
  sobj->functions->release_read (sobj->data);
  XDELETE (sobj);
}

/* Merge attributes.  */

const char *
simple_object_attributes_merge (simple_object_attributes *to,
				simple_object_attributes *from,
				int *err)
{
  if (to->functions != from->functions)
    {
      *err = 0;
      return "different object file format";
    }
  return to->functions->attributes_merge (to->data, from->data, err);
}

/* Release an attributes structure.  */

void
simple_object_release_attributes (simple_object_attributes *attrs)
{
  attrs->functions->release_attributes (attrs->data);
  XDELETE (attrs);
}

/* Start creating an object file.  */

simple_object_write *
simple_object_start_write (simple_object_attributes *attrs,
			   const char *segment_name, const char **errmsg,
			   int *err)
{
  void *data;
  simple_object_write *ret;

  data = attrs->functions->start_write (attrs->data, errmsg, err);
  if (data == NULL)
    return NULL;
  ret = XNEW (simple_object_write);
  ret->functions = attrs->functions;
  ret->segment_name = segment_name ? xstrdup (segment_name) : NULL;
  ret->sections = NULL;
  ret->last_section = NULL;
  ret->data = data;
  return ret;
}

/* Start creating a section.  */

simple_object_write_section *
simple_object_write_create_section (simple_object_write *sobj, const char *name,
				    unsigned int align,
				    const char **errmsg ATTRIBUTE_UNUSED,
				    int *err ATTRIBUTE_UNUSED)
{
  simple_object_write_section *ret;

  ret = XNEW (simple_object_write_section);
  ret->next = NULL;
  ret->name = xstrdup (name);
  ret->align = align;
  ret->buffers = NULL;
  ret->last_buffer = NULL;

  if (sobj->last_section == NULL)
    {
      sobj->sections = ret;
      sobj->last_section = ret;
    }
  else
    {
      sobj->last_section->next = ret;
      sobj->last_section = ret;
    }

  return ret;
}

/* Add data to a section.  */

const char *
simple_object_write_add_data (simple_object_write *sobj ATTRIBUTE_UNUSED,
			      simple_object_write_section *section,
			      const void *buffer,
			      size_t size, int copy,
			      int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_write_section_buffer *wsb;

  wsb = XNEW (struct simple_object_write_section_buffer);
  wsb->next = NULL;
  wsb->size = size;

  if (!copy)
    {
      wsb->buffer = buffer;
      wsb->free_buffer = NULL;
    }
  else
    {
      wsb->free_buffer = (void *) XNEWVEC (char, size);
      memcpy (wsb->free_buffer, buffer, size);
      wsb->buffer = wsb->free_buffer;
    }

  if (section->last_buffer == NULL)
    {
      section->buffers = wsb;
      section->last_buffer = wsb;
    }
  else
    {
      section->last_buffer->next = wsb;
      section->last_buffer = wsb;
    }

  return NULL;
}

/* Write the complete object file.  */

const char *
simple_object_write_to_file (simple_object_write *sobj, int descriptor,
			     int *err)
{
  return sobj->functions->write_to_file (sobj, descriptor, err);
}

/* Release an simple_object_write.  */

void
simple_object_release_write (simple_object_write *sobj)
{
  simple_object_write_section *section;

  free (sobj->segment_name);

  section = sobj->sections;
  while (section != NULL)
    {
      struct simple_object_write_section_buffer *buffer;
      simple_object_write_section *next_section;

      buffer = section->buffers;
      while (buffer != NULL)
	{
	  struct simple_object_write_section_buffer *next_buffer;

	  if (buffer->free_buffer != NULL)
	    XDELETEVEC (buffer->free_buffer);
	  next_buffer = buffer->next;
	  XDELETE (buffer);
	  buffer = next_buffer;
	}

      next_section = section->next;
      free (section->name);
      XDELETE (section);
      section = next_section;
    }

  sobj->functions->release_write (sobj->data);
  XDELETE (sobj);
}
