/* simple-object.h -- simple routines to read and write object files
   Copyright 2010 Free Software Foundation, Inc.
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

#ifndef SIMPLE_OBJECT_H
#define SIMPLE_OBJECT_H

#include <stddef.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* This header file provides four types with associated functions.
   They are used to read and write object files.  This is a minimal
   interface, intended to support the needs of gcc without bringing in
   all the power and complexity of BFD.  */

/* The type simple_object_read * is used to read an existing object
   file.  */

typedef struct simple_object_read_struct simple_object_read;

/* Create an simple_object_read given DESCRIPTOR, an open file
   descriptor, and OFFSET, an offset within the file.  The offset is
   for use with archives, and should be 0 for an ordinary object file.
   The descriptor must remain open until done with the returned
   simple_object_read.  SEGMENT_NAME is used on Mach-O and is required
   on that platform: it means to only look at sections within the
   segment with that name.  It is ignored for other object file
   formats.  On error, this function returns NULL, and sets *ERRMSG to
   an error string and sets *ERR to an errno value or 0 if there is no
   relevant errno.  */

extern simple_object_read *
simple_object_start_read (int descriptor, off_t offset,
			  const char *segment_name, const char **errmsg,
			  int *err);

/* Call PFN for each section in SIMPLE_OBJECT, passing it the section
   name, offset within the file of the section contents, and length of
   the section contents.  The offset within the file is relative to
   the offset passed to simple_object_start_read.  The DATA argument
   to simple_object_find_sections is passed on to PFN.  If PFN returns
   0, the loop is stopped and simple_object_find_sections returns.  If
   PFN returns non-zero, the loop continues.  On success this returns
   NULL.  On error it returns an error string, and sets *ERR to an
   errno value or 0 if there is no relevant errno.  */

extern const char *
simple_object_find_sections (simple_object_read *simple_object,
			     int (*pfn) (void *data, const char *,
					 off_t offset, off_t length),
			     void *data,
			     int *err);

/* Look for the section NAME in SIMPLE_OBJECT.  This returns
   information for the first section NAME in SIMPLE_OBJECT.  Note that
   calling this multiple times is inefficient; use
   simple_object_find_sections instead.

   If found, return 1 and set *OFFSET to the offset in the file of the
   section contents and set *LENGTH to the length of the section
   contents.  *OFFSET will be relative to the offset passed to
   simple_object_start_read.

   If the section is not found, and no error occurs, return 0 and set
   *ERRMSG to NULL.

   If an error occurs, return 0, set *ERRMSG to an error message, and
   set *ERR to an errno value or 0 if there is no relevant errno.  */

extern int
simple_object_find_section (simple_object_read *simple_object,
			    const char *name, off_t *offset, off_t *length,
			    const char **errmsg, int *err);

/* Release all resources associated with SIMPLE_OBJECT.  This does not
   close the file descriptor.  */

extern void
simple_object_release_read (simple_object_read *);

/* The type simple_object_attributes holds the attributes of an object
   file that matter for creating a file or ensuring that two files are
   compatible.  This is a set of magic numbers.  */

typedef struct simple_object_attributes_struct simple_object_attributes;

/* Fetch the attributes of SIMPLE_OBJECT.  This information will
   persist until simple_object_attributes_release is called, even if
   SIMPLE_OBJECT is closed.  On error this returns NULL, sets *ERRMSG
   to an error message, and sets *ERR to an errno value or 0 if there
   isn't one.  */

extern simple_object_attributes *
simple_object_fetch_attributes (simple_object_read *simple_object,
				const char **errmsg, int *err);

/* Merge the FROM attributes into TO.  If two objects with these
   attributes could be linked together without error, returns NULL.
   Otherwise, returns an error message, and sets *ERR to an errno
   value or 0 if there isn't one.  */

extern const char *
simple_object_attributes_merge (simple_object_attributes *to,
				simple_object_attributes *from,
				int *err);

/* Release all resources associated with ATTRS.  */

extern void
simple_object_release_attributes (simple_object_attributes *attrs);

/* The type simple_object_write is used to create a new object file.  */

typedef struct simple_object_write_struct simple_object_write;

/* Start creating a new object file which is like ATTRS.  You must
   fetch attribute information from an existing object file before you
   can create a new one.  There is currently no support for creating
   an object file de novo.  The segment name is only used on Mach-O,
   where it is required.  It means that all sections are created
   within that segment.  It is ignored for other object file formats.
   On error this function returns NULL, sets *ERRMSG to an error
   message, and sets *ERR to an errno value or 0 if there isn't
   one.  */

extern simple_object_write *
simple_object_start_write (simple_object_attributes *attrs,
			   const char *segment_name,
			   const char **errmsg, int *err);

/* The type simple_object_write_section is a handle for a section
   which is being written.  */

typedef struct simple_object_write_section_struct simple_object_write_section;

/* Add a section to SIMPLE_OBJECT.  NAME is the name of the new
   section.  ALIGN is the required alignment expressed as the number
   of required low-order 0 bits (e.g., 2 for alignment to a 32-bit
   boundary).  The section is created as containing data, readable,
   not writable, not executable, not loaded at runtime.  On error this
   returns NULL, sets *ERRMSG to an error message, and sets *ERR to an
   errno value or 0 if there isn't one.  */

extern simple_object_write_section *
simple_object_write_create_section (simple_object_write *simple_object,
				    const char *name, unsigned int align,
				    const char **errmsg, int *err);

/* Add data BUFFER/SIZE to SECTION in SIMPLE_OBJECT.  If COPY is
   non-zero, the data will be copied into memory if necessary.  If
   COPY is zero, BUFFER must persist until SIMPLE_OBJECT is released.
   On success this returns NULL.  On error this returns an error
   message, and sets *ERR to an errno value or 0 if there isn't
   one.  */

extern const char *
simple_object_write_add_data (simple_object_write *simple_object,
			      simple_object_write_section *section,
			      const void *buffer, size_t size,
			      int copy, int *err);

/* Write the complete object file to DESCRIPTOR, an open file
   descriptor.  This returns NULL on success.  On error this returns
   an error message, and sets *ERR to an errno value or 0 if there
   isn't one.  */

extern const char *
simple_object_write_to_file (simple_object_write *simple_object,
			     int descriptor, int *err);

/* Release all resources associated with SIMPLE_OBJECT, including any
   simple_object_write_section's that may have been created.  */

extern void
simple_object_release_write (simple_object_write *);

#ifdef __cplusplus
}
#endif

#endif
