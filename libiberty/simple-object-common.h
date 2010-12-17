/* simple-object-common.h -- common structs for object file manipulation.
   Copyright (C) 2010 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Forward reference.  */
struct simple_object_functions;

/* An object file opened for reading.  */

struct simple_object_read_struct
{
  /* The file descriptor.  */
  int descriptor;
  /* The offset within the file.  */
  off_t offset;
  /* The functions which do the actual work.  */
  const struct simple_object_functions *functions;
  /* Private data for the object file format.  */
  void *data;
};

/* Object file attributes.  */

struct simple_object_attributes_struct
{
  /* The functions which do the actual work.  */
  const struct simple_object_functions *functions;
  /* Private data for the object file format.  */
  void *data;
};

/* An object file being created.  */

struct simple_object_write_struct
{
  /* The functions which do the actual work.  */
  const struct simple_object_functions *functions;
  /* The segment_name argument from the user.  */
  char *segment_name;
  /* The start of the list of sections.  */
  simple_object_write_section *sections;
  /* The last entry in the list of sections.  */
  simple_object_write_section *last_section;
  /* Private data for the object file format.  */
  void *data;
};

/* A section in an object file being created.  */

struct simple_object_write_section_struct
{
  /* Next in the list of sections attached to an
     simple_object_write.  */
  simple_object_write_section *next;
  /* The name of this section.  */
  char *name;
  /* The required alignment.  */
  unsigned int align;
  /* The first data attached to this section.  */
  struct simple_object_write_section_buffer *buffers;
  /* The last data attached to this section.  */
  struct simple_object_write_section_buffer *last_buffer;
};

/* Data attached to a section.  */

struct simple_object_write_section_buffer
{
  /* The next data for this section.  */
  struct simple_object_write_section_buffer *next;
  /* The size of the buffer.  */
  size_t size;
  /* The actual bytes.  */
  const void *buffer;
  /* A buffer to free, or NULL.  */
  void *free_buffer;
};

/* The number of bytes we read from the start of the file to pass to
   the match function.  */
#define SIMPLE_OBJECT_MATCH_HEADER_LEN (16)

/* Format-specific object file functions.  */

struct simple_object_functions
{
  /* If this file matches these functions, return a new value for the
     private data for an simple_object_read.  HEADER is the first 16
     bytes of the file.  DESCRIPTOR, OFFSET, SEGMENT_NAME, ERRMSG, and
     ERR are as for simple_object_open_read.  If this file does not
     match, this function should return NULL with *ERRMSG set to
     NULL.  */
  void *(*match) (unsigned char header[SIMPLE_OBJECT_MATCH_HEADER_LEN],
		  int descriptor, off_t offset, const char *segment_name,
		  const char **errmsg, int *err);

  /* Implement simple_object_find_sections.  */
  const char *(*find_sections) (simple_object_read *,
				int (*pfn) (void *, const char *,
					    off_t offset, off_t length),
				void *data,
				int *err);

  /* Return the private data for the attributes for SOBJ.  */
  void *(*fetch_attributes) (simple_object_read *sobj, const char **errmsg,
			     int *err);

  /* Release the private data for an simple_object_read.  */
  void (*release_read) (void *);

  /* Merge the private data for the attributes of two files.  If they
     could be linked together, return NULL.  Otherwise return an error
     message.  */
  const char *(*attributes_merge) (void *, void *, int *err);

  /* Release the private data for an simple_object_attributes.  */
  void (*release_attributes) (void *);

  /* Start creating an object file.  */
  void *(*start_write) (void *attributes_data, const char **errmsg,
			int *err);

  /* Write the complete object file.  */
  const char *(*write_to_file) (simple_object_write *sobj, int descriptor,
				int *err);

  /* Release the private data for an simple_object_write.  */
  void (*release_write) (void *);
};

/* The known object file formats.  */

extern const struct simple_object_functions simple_object_coff_functions;
extern const struct simple_object_functions simple_object_elf_functions;
extern const struct simple_object_functions simple_object_mach_o_functions;

/* Read SIZE bytes from DESCRIPTOR at file offset OFFSET into BUFFER.
   Return non-zero on success.  On failure return 0 and set *ERRMSG
   and *ERR.  */

extern int
simple_object_internal_read (int descriptor, off_t offset,
			     unsigned char *buffer, size_t size,
			     const char **errmsg, int *err);

/* Write SIZE bytes from BUFFER to DESCRIPTOR at file offset OFFSET.
   Return non-zero on success.  On failure return 0 and set *ERRMSG
   and *ERR.  */

extern int
simple_object_internal_write (int descriptor, off_t offset,
			      const unsigned char *buffer, size_t size,
			      const char **errmsg, int *err);

/* Define ulong_type as an unsigned 64-bit type if available.
   Otherwise just make it unsigned long.  */

#ifdef UNSIGNED_64BIT_TYPE
__extension__ typedef UNSIGNED_64BIT_TYPE ulong_type;
#else
typedef unsigned long ulong_type;
#endif

/* Fetch a big-endian 16-bit value.  */

static inline unsigned short
simple_object_fetch_big_16 (const unsigned char *buf)
{
  return ((unsigned short) buf[0] << 8) | (unsigned short) buf[1];
}

/* Fetch a little-endian 16-bit value.  */

static inline unsigned short
simple_object_fetch_little_16 (const unsigned char *buf)
{
  return ((unsigned short) buf[1] << 8) | (unsigned short) buf[0];
}

/* Fetch a big-endian 32-bit value.  */

static inline unsigned int
simple_object_fetch_big_32 (const unsigned char *buf)
{
  return (((unsigned int) buf[0] << 24)
	  | ((unsigned int) buf[1] << 16)
	  | ((unsigned int) buf[2] << 8)
	  | (unsigned int) buf[3]);
}

/* Fetch a little-endian 32-bit value.  */

static inline unsigned int
simple_object_fetch_little_32 (const unsigned char *buf)
{
  return (((unsigned int) buf[3] << 24)
	  | ((unsigned int) buf[2] << 16)
	  | ((unsigned int) buf[1] << 8)
	  | (unsigned int) buf[0]);
}

/* Fetch a big-endian 32-bit value as a ulong_type.  */

static inline ulong_type
simple_object_fetch_big_32_ulong (const unsigned char *buf)
{
  return (ulong_type) simple_object_fetch_big_32 (buf);
}

/* Fetch a little-endian 32-bit value as a ulong_type.  */

static inline ulong_type
simple_object_fetch_little_32_ulong (const unsigned char *buf)
{
  return (ulong_type) simple_object_fetch_little_32 (buf);
}

#ifdef UNSIGNED_64BIT_TYPE

/* Fetch a big-endian 64-bit value.  */

static inline ulong_type
simple_object_fetch_big_64 (const unsigned char *buf)
{
  return (((ulong_type) buf[0] << 56)
	  | ((ulong_type) buf[1] << 48)
	  | ((ulong_type) buf[2] << 40)
	  | ((ulong_type) buf[3] << 32)
	  | ((ulong_type) buf[4] << 24)
	  | ((ulong_type) buf[5] << 16)
	  | ((ulong_type) buf[6] << 8)
	  | (ulong_type) buf[7]);
}

/* Fetch a little-endian 64-bit value.  */

static inline ulong_type
simple_object_fetch_little_64 (const unsigned char *buf)
{
  return (((ulong_type) buf[7] << 56)
	  | ((ulong_type) buf[6] << 48)
	  | ((ulong_type) buf[5] << 40)
	  | ((ulong_type) buf[4] << 32)
	  | ((ulong_type) buf[3] << 24)
	  | ((ulong_type) buf[2] << 16)
	  | ((ulong_type) buf[1] << 8)
	  | (ulong_type) buf[0]);
}

#endif

/* Store a big-endian 16-bit value.  */

static inline void
simple_object_set_big_16 (unsigned char *buf, unsigned short val)
{
  buf[0] = (val >> 8) & 0xff;
  buf[1] = val & 0xff;
}

/* Store a little-endian 16-bit value.  */

static inline void
simple_object_set_little_16 (unsigned char *buf, unsigned short val)
{
  buf[1] = (val >> 8) & 0xff;
  buf[0] = val & 0xff;
}

/* Store a big-endian 32-bit value.  */

static inline void
simple_object_set_big_32 (unsigned char *buf, unsigned int val)
{
  buf[0] = (val >> 24) & 0xff;
  buf[1] = (val >> 16) & 0xff;
  buf[2] = (val >> 8) & 0xff;
  buf[3] = val & 0xff;
}

/* Store a little-endian 32-bit value.  */

static inline void
simple_object_set_little_32 (unsigned char *buf, unsigned int val)
{
  buf[3] = (val >> 24) & 0xff;
  buf[2] = (val >> 16) & 0xff;
  buf[1] = (val >> 8) & 0xff;
  buf[0] = val & 0xff;
}

/* Store a big-endian 32-bit value coming in as a ulong_type.  */

static inline void
simple_object_set_big_32_ulong (unsigned char *buf, ulong_type val)
{
  simple_object_set_big_32 (buf, val);
}

/* Store a little-endian 32-bit value coming in as a ulong_type.  */

static inline void
simple_object_set_little_32_ulong (unsigned char *buf, ulong_type val)
{
  simple_object_set_little_32 (buf, val);
}

#ifdef UNSIGNED_64BIT_TYPE

/* Store a big-endian 64-bit value.  */

static inline void
simple_object_set_big_64 (unsigned char *buf, ulong_type val)
{
  buf[0] = (val >> 56) & 0xff;
  buf[1] = (val >> 48) & 0xff;
  buf[2] = (val >> 40) & 0xff;
  buf[3] = (val >> 32) & 0xff;
  buf[4] = (val >> 24) & 0xff;
  buf[5] = (val >> 16) & 0xff;
  buf[6] = (val >> 8) & 0xff;
  buf[7] = val & 0xff;
}

/* Store a little-endian 64-bit value.  */

static inline void
simple_object_set_little_64 (unsigned char *buf, ulong_type val)
{
  buf[7] = (val >> 56) & 0xff;
  buf[6] = (val >> 48) & 0xff;
  buf[5] = (val >> 40) & 0xff;
  buf[4] = (val >> 32) & 0xff;
  buf[3] = (val >> 24) & 0xff;
  buf[2] = (val >> 16) & 0xff;
  buf[1] = (val >> 8) & 0xff;
  buf[0] = val & 0xff;
}

#endif
