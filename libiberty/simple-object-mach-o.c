/* simple-object-mach-o.c -- routines to manipulate Mach-O object files.
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

#include "config.h"
#include "libiberty.h"
#include "simple-object.h"

#include <stddef.h>

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

#include "simple-object-common.h"

/* Mach-O structures and constants.  */

/* Mach-O header (32-bit version).  */

struct mach_o_header_32
{
  unsigned char magic[4];	/* Magic number.  */
  unsigned char cputype[4];	/* CPU that this object is for.  */
  unsigned char cpusubtype[4];	/* CPU subtype.  */
  unsigned char filetype[4];	/* Type of file.  */
  unsigned char ncmds[4];	/* Number of load commands.  */
  unsigned char sizeofcmds[4];	/* Total size of load commands.  */
  unsigned char flags[4];	/* Flags for special featues.  */
};

/* Mach-O header (64-bit version).  */

struct mach_o_header_64
{
  unsigned char magic[4];	/* Magic number.  */
  unsigned char cputype[4];	/* CPU that this object is for.  */
  unsigned char cpusubtype[4];	/* CPU subtype.  */
  unsigned char filetype[4];	/* Type of file.  */
  unsigned char ncmds[4];	/* Number of load commands.  */
  unsigned char sizeofcmds[4];	/* Total size of load commands.  */
  unsigned char flags[4];	/* Flags for special featues.  */
  unsigned char reserved[4];	/* Reserved.  Duh.  */
};

/* For magic field in header.  */

#define MACH_O_MH_MAGIC			0xfeedface
#define MACH_O_MH_MAGIC_64		0xfeedfacf

/* For filetype field in header.  */

#define MACH_O_MH_OBJECT		0x01

/* A Mach-O file is a list of load commands.  This is the header of a
   load command.  */

struct mach_o_load_command
{
  unsigned char cmd[4];		/* The type of load command.  */
  unsigned char cmdsize[4];	/* Size in bytes of entire command.  */
};

/* For cmd field in load command.   */

#define MACH_O_LC_SEGMENT		0x01
#define MACH_O_LC_SEGMENT_64		0x19

/* LC_SEGMENT load command.  */

struct mach_o_segment_command_32
{
  unsigned char cmd[4];		/* The type of load command (LC_SEGMENT).  */
  unsigned char cmdsize[4];	/* Size in bytes of entire command.  */
  unsigned char segname[16];	/* Name of this segment.  */
  unsigned char vmaddr[4];	/* Virtual memory address of this segment.  */
  unsigned char vmsize[4];	/* Size there, in bytes.  */
  unsigned char fileoff[4];	/* Offset in bytes of the data to be mapped.  */
  unsigned char filesize[4];	/* Size in bytes on disk.  */
  unsigned char maxprot[4];	/* Maximum permitted vmem protection.  */
  unsigned char initprot[4];	/* Initial vmem protection.  */
  unsigned char nsects[4];	/* Number of sections in this segment.  */
  unsigned char flags[4];	/* Flags that affect the loading.  */
};

/* LC_SEGMENT_64 load command.  */

struct mach_o_segment_command_64
{
  unsigned char cmd[4];		/* The type of load command (LC_SEGMENT_64).  */
  unsigned char cmdsize[4];	/* Size in bytes of entire command.  */
  unsigned char segname[16];	/* Name of this segment.  */
  unsigned char vmaddr[8];	/* Virtual memory address of this segment.  */
  unsigned char vmsize[8];	/* Size there, in bytes.  */
  unsigned char fileoff[8];	/* Offset in bytes of the data to be mapped.  */
  unsigned char filesize[8];	/* Size in bytes on disk.  */
  unsigned char maxprot[4];	/* Maximum permitted vmem protection.  */
  unsigned char initprot[4];	/* Initial vmem protection.  */
  unsigned char nsects[4];	/* Number of sections in this segment.  */
  unsigned char flags[4];	/* Flags that affect the loading.  */
};

/* 32-bit section header.  */

struct mach_o_section_32
{
  unsigned char sectname[16];	/* Section name.  */
  unsigned char segname[16];	/* Segment that the section belongs to.  */
  unsigned char addr[4];	/* Address of this section in memory.  */
  unsigned char size[4];	/* Size in bytes of this section.  */
  unsigned char offset[4];	/* File offset of this section.  */
  unsigned char align[4];	/* log2 of this section's alignment.  */
  unsigned char reloff[4];	/* File offset of this section's relocs.  */
  unsigned char nreloc[4];	/* Number of relocs for this section.  */
  unsigned char flags[4];	/* Section flags/attributes.  */
  unsigned char reserved1[4];
  unsigned char reserved2[4];
};

/* 64-bit section header.  */

struct mach_o_section_64
{
  unsigned char sectname[16];	/* Section name.  */
  unsigned char segname[16];	/* Segment that the section belongs to.  */
  unsigned char addr[8];	/* Address of this section in memory.  */
  unsigned char size[8];	/* Size in bytes of this section.  */
  unsigned char offset[4];	/* File offset of this section.  */
  unsigned char align[4];	/* log2 of this section's alignment.  */
  unsigned char reloff[4];	/* File offset of this section's relocs.  */
  unsigned char nreloc[4];	/* Number of relocs for this section.  */
  unsigned char flags[4];	/* Section flags/attributes.  */
  unsigned char reserved1[4];
  unsigned char reserved2[4];
  unsigned char reserved3[4];
};

/* Flags for Mach-O sections.  */

#define MACH_O_S_ATTR_DEBUG			0x02000000

/* The length of a segment or section name.  */

#define MACH_O_NAME_LEN (16)

/* A GNU specific extension for long section names.  */

#define GNU_SECTION_NAMES "__section_names"

/* Private data for an simple_object_read.  */

struct simple_object_mach_o_read
{
  /* User specified segment name.  */
  char *segment_name;
  /* Magic number.  */
  unsigned int magic;
  /* Whether this file is big-endian.  */
  int is_big_endian;
  /* CPU type from header.  */
  unsigned int cputype;
  /* CPU subtype from header.  */
  unsigned int cpusubtype;
  /* Number of commands, from header.  */
  unsigned int ncmds;
  /* Flags from header.  */
  unsigned int flags;
  /* Reserved field from header, only used on 64-bit.  */
  unsigned int reserved;
};

/* Private data for an simple_object_attributes.  */

struct simple_object_mach_o_attributes
{
  /* Magic number.  */
  unsigned int magic;
  /* Whether this file is big-endian.  */
  int is_big_endian;
  /* CPU type from header.  */
  unsigned int cputype;
  /* CPU subtype from header.  */
  unsigned int cpusubtype;
  /* Flags from header.  */
  unsigned int flags;
  /* Reserved field from header, only used on 64-bit.  */
  unsigned int reserved;
};

/* See if we have a Mach-O file.  */

static void *
simple_object_mach_o_match (
    unsigned char header[SIMPLE_OBJECT_MATCH_HEADER_LEN],
    int descriptor,
    off_t offset,
    const char *segment_name,
    const char **errmsg,
    int *err)
{
  unsigned int magic;
  int is_big_endian;
  unsigned int (*fetch_32) (const unsigned char *);
  unsigned int filetype;
  struct simple_object_mach_o_read *omr;
  unsigned char buf[sizeof (struct mach_o_header_64)];
  unsigned char *b;

  magic = simple_object_fetch_big_32 (header);
  if (magic == MACH_O_MH_MAGIC || magic == MACH_O_MH_MAGIC_64)
    is_big_endian = 1;
  else
    {
      magic = simple_object_fetch_little_32 (header);
      if (magic == MACH_O_MH_MAGIC || magic == MACH_O_MH_MAGIC_64)
	is_big_endian = 0;
      else
	{
	  *errmsg = NULL;
	  *err = 0;
	  return NULL;
	}
    }

#ifndef UNSIGNED_64BIT_TYPE
  if (magic == MACH_O_MH_MAGIC_64)
    {
      *errmsg = "64-bit Mach-O objects not supported";
      *err = 0;
      return NULL;
    }
#endif

  /* We require the user to provide a segment name.  This is
     unfortunate but I don't see any good choices here.  */

  if (segment_name == NULL)
    {
      *errmsg = "Mach-O file found but no segment name specified";
      *err = 0;
      return NULL;
    }

  if (strlen (segment_name) > MACH_O_NAME_LEN)
    {
      *errmsg = "Mach-O segment name too long";
      *err = 0;
      return NULL;
    }

  /* The 32-bit and 64-bit headers are similar enough that we can use
     the same code.  */

  fetch_32 = (is_big_endian
	      ? simple_object_fetch_big_32
	      : simple_object_fetch_little_32);

  if (!simple_object_internal_read (descriptor, offset, buf,
				    (magic == MACH_O_MH_MAGIC
				     ? sizeof (struct mach_o_header_32)
				     : sizeof (struct mach_o_header_64)),
				    errmsg, err))
    return NULL;

  b = &buf[0];

  filetype = (*fetch_32) (b + offsetof (struct mach_o_header_32, filetype));
  if (filetype != MACH_O_MH_OBJECT)
    {
      *errmsg = "Mach-O file is not object file";
      *err = 0;
      return NULL;
    }

  omr = XNEW (struct simple_object_mach_o_read);
  omr->segment_name = xstrdup (segment_name);
  omr->magic = magic;
  omr->is_big_endian = is_big_endian;
  omr->cputype = (*fetch_32) (b + offsetof (struct mach_o_header_32, cputype));
  omr->cpusubtype = (*fetch_32) (b
				 + offsetof (struct mach_o_header_32,
					     cpusubtype));
  omr->ncmds = (*fetch_32) (b + offsetof (struct mach_o_header_32, ncmds));
  omr->flags = (*fetch_32) (b + offsetof (struct mach_o_header_32, flags));
  if (magic == MACH_O_MH_MAGIC)
    omr->reserved = 0;
  else
    omr->reserved = (*fetch_32) (b
				 + offsetof (struct mach_o_header_64,
					     reserved));

  return (void *) omr;
}

/* Get the file offset and size from a section header.  */

static void
simple_object_mach_o_section_info (int is_big_endian, int is_32,
				   const unsigned char *sechdr, off_t *offset,
				   size_t *size)
{
  unsigned int (*fetch_32) (const unsigned char *);
  ulong_type (*fetch_64) (const unsigned char *);

  fetch_32 = (is_big_endian
	      ? simple_object_fetch_big_32
	      : simple_object_fetch_little_32);

  fetch_64 = NULL;
#ifdef UNSIGNED_64BIT_TYPE
  fetch_64 = (is_big_endian
	      ? simple_object_fetch_big_64
	      : simple_object_fetch_little_64);
#endif

  if (is_32)
    {
      *offset = fetch_32 (sechdr
			  + offsetof (struct mach_o_section_32, offset));
      *size = fetch_32 (sechdr
			+ offsetof (struct mach_o_section_32, size));
    }
  else
    {
      *offset = fetch_32 (sechdr
			  + offsetof (struct mach_o_section_64, offset));
      *size = fetch_64 (sechdr
			+ offsetof (struct mach_o_section_64, size));
    }
}

/* Handle a segment in a Mach-O file.  Return 1 if we should continue,
   0 if the caller should return.  */

static int
simple_object_mach_o_segment (simple_object_read *sobj, off_t offset,
			      const unsigned char *segbuf,
			      int (*pfn) (void *, const char *, off_t offset,
					  off_t length),
			      void *data,
			      const char **errmsg, int *err)
{
  struct simple_object_mach_o_read *omr =
    (struct simple_object_mach_o_read *) sobj->data;
  unsigned int (*fetch_32) (const unsigned char *);
  int is_32;
  size_t seghdrsize;
  size_t sechdrsize;
  size_t segname_offset;
  size_t sectname_offset;
  unsigned int nsects;
  unsigned char *secdata;
  unsigned int i;
  unsigned int strtab_index;
  char *strtab;
  size_t strtab_size;

  fetch_32 = (omr->is_big_endian
	      ? simple_object_fetch_big_32
	      : simple_object_fetch_little_32);

  is_32 = omr->magic == MACH_O_MH_MAGIC;

  if (is_32)
    {
      seghdrsize = sizeof (struct mach_o_segment_command_32);
      sechdrsize = sizeof (struct mach_o_section_32);
      segname_offset = offsetof (struct mach_o_section_32, segname);
      sectname_offset = offsetof (struct mach_o_section_32, sectname);
      nsects = (*fetch_32) (segbuf
			    + offsetof (struct mach_o_segment_command_32,
					nsects));
    }
  else
    {
      seghdrsize = sizeof (struct mach_o_segment_command_64);
      sechdrsize = sizeof (struct mach_o_section_64);
      segname_offset = offsetof (struct mach_o_section_64, segname);
      sectname_offset = offsetof (struct mach_o_section_64, sectname);
      nsects = (*fetch_32) (segbuf
			    + offsetof (struct mach_o_segment_command_64,
					nsects));
    }

  secdata = XNEWVEC (unsigned char, nsects * sechdrsize);
  if (!simple_object_internal_read (sobj->descriptor, offset + seghdrsize,
				    secdata, nsects * sechdrsize, errmsg, err))
    {
      XDELETEVEC (secdata);
      return 0;
    }

  /* Scan for a __section_names section.  This is in effect a GNU
     extension that permits section names longer than 16 chars.  */

  for (i = 0; i < nsects; ++i)
    {
      size_t nameoff;

      nameoff = i * sechdrsize + segname_offset;
      if (strcmp ((char *) secdata + nameoff, omr->segment_name) != 0)
	continue;
      nameoff = i * sechdrsize + sectname_offset;
      if (strcmp ((char *) secdata + nameoff, GNU_SECTION_NAMES) == 0)
	break;
    }

  strtab_index = i;
  if (strtab_index >= nsects)
    {
      strtab = NULL;
      strtab_size = 0;
    }
  else
    {
      off_t strtab_offset;

      simple_object_mach_o_section_info (omr->is_big_endian, is_32,
					 secdata + strtab_index * sechdrsize,
					 &strtab_offset, &strtab_size);
      strtab = XNEWVEC (char, strtab_size);
      if (!simple_object_internal_read (sobj->descriptor,
					sobj->offset + strtab_offset,
					(unsigned char *) strtab, strtab_size,
					errmsg, err))
	{
	  XDELETEVEC (strtab);
	  XDELETEVEC (secdata);
	  return 0;
	}
    }

  /* Process the sections.  */

  for (i = 0; i < nsects; ++i)
    {
      const unsigned char *sechdr;
      char namebuf[MACH_O_NAME_LEN + 1];
      char *name;
      off_t secoffset;
      size_t secsize;

      if (i == strtab_index)
	continue;

      sechdr = secdata + i * sechdrsize;

      if (strcmp ((char *) sechdr + segname_offset, omr->segment_name) != 0)
	continue;

      memcpy (namebuf, sechdr + sectname_offset, MACH_O_NAME_LEN);
      namebuf[MACH_O_NAME_LEN] = '\0';

      name = &namebuf[0];
      if (strtab != NULL && name[0] == '_' && name[1] == '_')
	{
	  unsigned long stringoffset;

	  if (sscanf (name + 2, "%08lX", &stringoffset) == 1)
	    {
	      if (stringoffset >= strtab_size)
		{
		  *errmsg = "section name offset out of range";
		  *err = 0;
		  XDELETEVEC (strtab);
		  XDELETEVEC (secdata);
		  return 0;
		}

	      name = strtab + stringoffset;
	    }
	}

      simple_object_mach_o_section_info (omr->is_big_endian, is_32, sechdr,
					 &secoffset, &secsize);

      if (!(*pfn) (data, name, secoffset, secsize))
	{
	  *errmsg = NULL;
	  *err = 0;
	  XDELETEVEC (strtab);
	  XDELETEVEC (secdata);
	  return 0;
	}
    }

  XDELETEVEC (strtab);
  XDELETEVEC (secdata);

  return 1;
}

/* Find all sections in a Mach-O file.  */

static const char *
simple_object_mach_o_find_sections (simple_object_read *sobj,
				    int (*pfn) (void *, const char *,
						off_t offset, off_t length),
				    void *data,
				    int *err)
{
  struct simple_object_mach_o_read *omr =
    (struct simple_object_mach_o_read *) sobj->data;
  off_t offset;
  size_t seghdrsize;
  unsigned int (*fetch_32) (const unsigned char *);
  const char *errmsg;
  unsigned int i;

  if (omr->magic == MACH_O_MH_MAGIC)
    {
      offset = sizeof (struct mach_o_header_32);
      seghdrsize = sizeof (struct mach_o_segment_command_32);
    }
  else
    {
      offset = sizeof (struct mach_o_header_64);
      seghdrsize = sizeof (struct mach_o_segment_command_64);
    }

  fetch_32 = (omr->is_big_endian
	      ? simple_object_fetch_big_32
	      : simple_object_fetch_little_32);

  for (i = 0; i < omr->ncmds; ++i)
    {
      unsigned char loadbuf[sizeof (struct mach_o_load_command)];
      unsigned int cmd;
      unsigned int cmdsize;

      if (!simple_object_internal_read (sobj->descriptor,
					sobj->offset + offset,
					loadbuf,
					sizeof (struct mach_o_load_command),
					&errmsg, err))
	return errmsg;

      cmd = (*fetch_32) (loadbuf + offsetof (struct mach_o_load_command, cmd));
      cmdsize = (*fetch_32) (loadbuf
			     + offsetof (struct mach_o_load_command, cmdsize));

      if (cmd == MACH_O_LC_SEGMENT || cmd == MACH_O_LC_SEGMENT_64)
	{
	  unsigned char segbuf[sizeof (struct mach_o_segment_command_64)];
	  int r;

	  if (!simple_object_internal_read (sobj->descriptor,
					    sobj->offset + offset,
					    segbuf, seghdrsize, &errmsg, err))
	    return errmsg;

	  r = simple_object_mach_o_segment (sobj, offset, segbuf, pfn,
					    data, &errmsg, err);
	  if (!r)
	    return errmsg;
	}

      offset += cmdsize;
    }

  return NULL;
}

/* Fetch the attributes for an simple_object_read.  */

static void *
simple_object_mach_o_fetch_attributes (simple_object_read *sobj,
				       const char **errmsg ATTRIBUTE_UNUSED,
				       int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_mach_o_read *omr =
    (struct simple_object_mach_o_read *) sobj->data;
  struct simple_object_mach_o_attributes *ret;

  ret = XNEW (struct simple_object_mach_o_attributes);
  ret->magic = omr->magic;
  ret->is_big_endian = omr->is_big_endian;
  ret->cputype = omr->cputype;
  ret->cpusubtype = omr->cpusubtype;
  ret->flags = omr->flags;
  ret->reserved = omr->reserved;
  return ret;
}

/* Release the private data for an simple_object_read.  */

static void
simple_object_mach_o_release_read (void *data)
{
  struct simple_object_mach_o_read *omr =
    (struct simple_object_mach_o_read *) data;

  free (omr->segment_name);
  XDELETE (omr);
}

/* Compare two attributes structures.  */

static const char *
simple_object_mach_o_attributes_merge (void *todata, void *fromdata, int *err)
{
  struct simple_object_mach_o_attributes *to =
    (struct simple_object_mach_o_attributes *) todata;
  struct simple_object_mach_o_attributes *from =
    (struct simple_object_mach_o_attributes *) fromdata;

  if (to->magic != from->magic
      || to->is_big_endian != from->is_big_endian
      || to->cputype != from->cputype)
    {
      *err = 0;
      return "Mach-O object format mismatch";
    }
  return NULL;
}

/* Release the private data for an attributes structure.  */

static void
simple_object_mach_o_release_attributes (void *data)
{
  XDELETE (data);
}

/* Prepare to write out a file.  */

static void *
simple_object_mach_o_start_write (void *attributes_data,
				  const char **errmsg ATTRIBUTE_UNUSED,
				  int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_mach_o_attributes *attrs =
    (struct simple_object_mach_o_attributes *) attributes_data;
  struct simple_object_mach_o_attributes *ret;

  /* We're just going to record the attributes, but we need to make a
     copy because the user may delete them.  */
  ret = XNEW (struct simple_object_mach_o_attributes);
  *ret = *attrs;
  return ret;
}

/* Write out the header of a Mach-O file.  */

static int
simple_object_mach_o_write_header (simple_object_write *sobj, int descriptor,
				   size_t nsects, const char **errmsg,
				   int *err)
{
  struct simple_object_mach_o_attributes *attrs =
    (struct simple_object_mach_o_attributes *) sobj->data;
  void (*set_32) (unsigned char *, unsigned int);
  unsigned char hdrbuf[sizeof (struct mach_o_header_64)];
  unsigned char *hdr;
  size_t wrsize;

  set_32 = (attrs->is_big_endian
	    ? simple_object_set_big_32
	    : simple_object_set_little_32);

  memset (hdrbuf, 0, sizeof hdrbuf);

  /* The 32-bit and 64-bit headers start out the same.  */

  hdr = &hdrbuf[0];
  set_32 (hdr + offsetof (struct mach_o_header_32, magic), attrs->magic);
  set_32 (hdr + offsetof (struct mach_o_header_32, cputype), attrs->cputype);
  set_32 (hdr + offsetof (struct mach_o_header_32, cpusubtype),
	  attrs->cpusubtype);
  set_32 (hdr + offsetof (struct mach_o_header_32, filetype), MACH_O_MH_OBJECT);
  set_32 (hdr + offsetof (struct mach_o_header_32, ncmds), 1);
  set_32 (hdr + offsetof (struct mach_o_header_32, flags), attrs->flags);
  if (attrs->magic == MACH_O_MH_MAGIC)
    {
      wrsize = sizeof (struct mach_o_header_32);
      set_32 (hdr + offsetof (struct mach_o_header_32, sizeofcmds),
	      (sizeof (struct mach_o_segment_command_32)
	       + nsects * sizeof (struct mach_o_section_32)));
    }
  else
    {
      set_32 (hdr + offsetof (struct mach_o_header_64, sizeofcmds),
	      (sizeof (struct mach_o_segment_command_64)
	       + nsects * sizeof (struct mach_o_section_64)));
      set_32 (hdr + offsetof (struct mach_o_header_64, reserved),
	      attrs->reserved);
      wrsize = sizeof (struct mach_o_header_64);
    }

  return simple_object_internal_write (descriptor, 0, hdrbuf, wrsize,
				       errmsg, err);
}

/* Write a Mach-O section header.  */

static int
simple_object_mach_o_write_section_header (simple_object_write *sobj,
					   int descriptor,
					   size_t sechdr_offset,
					   const char *name, size_t secaddr,
					   size_t secsize, size_t offset,
					   unsigned int align,
					   const char **errmsg, int *err)
{
  struct simple_object_mach_o_attributes *attrs =
    (struct simple_object_mach_o_attributes *) sobj->data;
  void (*set_32) (unsigned char *, unsigned int);
  unsigned char hdrbuf[sizeof (struct mach_o_section_64)];
  unsigned char *hdr;
  size_t sechdrsize;

  set_32 = (attrs->is_big_endian
	    ? simple_object_set_big_32
	    : simple_object_set_little_32);

  memset (hdrbuf, 0, sizeof hdrbuf);

  hdr = &hdrbuf[0];
  if (attrs->magic == MACH_O_MH_MAGIC)
    {
      strncpy ((char *) hdr + offsetof (struct mach_o_section_32, sectname),
	       name, MACH_O_NAME_LEN);
      strncpy ((char *) hdr + offsetof (struct mach_o_section_32, segname),
	       sobj->segment_name, MACH_O_NAME_LEN);
      set_32 (hdr + offsetof (struct mach_o_section_32, addr), secaddr);
      set_32 (hdr + offsetof (struct mach_o_section_32, size), secsize);
      set_32 (hdr + offsetof (struct mach_o_section_32, offset), offset);
      set_32 (hdr + offsetof (struct mach_o_section_32, align), align);
      /* reloff left as zero.  */
      /* nreloc left as zero.  */
      set_32 (hdr + offsetof (struct mach_o_section_32, flags),
	      MACH_O_S_ATTR_DEBUG);
      /* reserved1 left as zero.  */
      /* reserved2 left as zero.  */
      sechdrsize = sizeof (struct mach_o_section_32);
    }
  else
    {
#ifdef UNSIGNED_64BIT_TYPE
      void (*set_64) (unsigned char *, ulong_type);

      set_64 = (attrs->is_big_endian
		? simple_object_set_big_64
		: simple_object_set_little_64);

      strncpy ((char *) hdr + offsetof (struct mach_o_section_64, sectname),
	       name, MACH_O_NAME_LEN);
      strncpy ((char *) hdr + offsetof (struct mach_o_section_64, segname),
	       sobj->segment_name, MACH_O_NAME_LEN);
      set_64 (hdr + offsetof (struct mach_o_section_64, addr), secaddr);
      set_64 (hdr + offsetof (struct mach_o_section_64, size), secsize);
      set_32 (hdr + offsetof (struct mach_o_section_64, offset), offset);
      set_32 (hdr + offsetof (struct mach_o_section_64, align), align);
      /* reloff left as zero.  */
      /* nreloc left as zero.  */
      set_32 (hdr + offsetof (struct mach_o_section_64, flags),
	      MACH_O_S_ATTR_DEBUG);
      /* reserved1 left as zero.  */
      /* reserved2 left as zero.  */
      /* reserved3 left as zero.  */
#endif
      sechdrsize = sizeof (struct mach_o_section_64);
    }

  return simple_object_internal_write (descriptor, sechdr_offset, hdr,
				       sechdrsize, errmsg, err);
}

/* Write out the single segment and the sections of a Mach-O file.  */

static int
simple_object_mach_o_write_segment (simple_object_write *sobj, int descriptor,
				    size_t nsects, const char **errmsg,
				    int *err)
{
  struct simple_object_mach_o_attributes *attrs =
    (struct simple_object_mach_o_attributes *) sobj->data;
  void (*set_32) (unsigned char *, unsigned int);
  size_t hdrsize;
  size_t seghdrsize;
  size_t sechdrsize;
  size_t cmdsize;
  size_t offset;
  size_t sechdr_offset;
  size_t secaddr;
  unsigned int name_offset;
  simple_object_write_section *section;
  unsigned char hdrbuf[sizeof (struct mach_o_segment_command_64)];
  unsigned char *hdr;

  set_32 = (attrs->is_big_endian
	    ? simple_object_set_big_32
	    : simple_object_set_little_32);

  /* Write out the sections first.  */

  if (attrs->magic == MACH_O_MH_MAGIC)
    {
      hdrsize = sizeof (struct mach_o_header_32);
      seghdrsize = sizeof (struct mach_o_segment_command_32);
      sechdrsize = sizeof (struct mach_o_section_32);
    }
  else
    {
      hdrsize = sizeof (struct mach_o_header_64);
      seghdrsize = sizeof (struct mach_o_segment_command_64);
      sechdrsize = sizeof (struct mach_o_section_64);
    }

  sechdr_offset = hdrsize + seghdrsize;
  cmdsize = seghdrsize + nsects * sechdrsize;
  offset = hdrsize + cmdsize;
  name_offset = 0;
  secaddr = 0;

  for (section = sobj->sections; section != NULL; section = section->next)
    {
      size_t mask;
      size_t new_offset;
      size_t secsize;
      struct simple_object_write_section_buffer *buffer;
      char namebuf[MACH_O_NAME_LEN + 1];

      mask = (1U << section->align) - 1;
      new_offset = offset + mask;
      new_offset &= ~ mask;
      while (new_offset > offset)
	{
	  unsigned char zeroes[16];
	  size_t write;

	  memset (zeroes, 0, sizeof zeroes);
	  write = new_offset - offset;
	  if (write > sizeof zeroes)
	    write = sizeof zeroes;
	  if (!simple_object_internal_write (descriptor, offset, zeroes, write,
					     errmsg, err))
	    return 0;
	  offset += write;
	}

      secsize = 0;
      for (buffer = section->buffers; buffer != NULL; buffer = buffer->next)
	{
	  if (!simple_object_internal_write (descriptor, offset + secsize,
					     ((const unsigned char *)
					      buffer->buffer),
					     buffer->size, errmsg, err))
	    return 0;
	  secsize += buffer->size;
	}

      snprintf (namebuf, sizeof namebuf, "__%08X", name_offset);
      if (!simple_object_mach_o_write_section_header (sobj, descriptor,
						      sechdr_offset, namebuf,
						      secaddr, secsize, offset,
						      section->align,
						      errmsg, err))
	return 0;

      sechdr_offset += sechdrsize;
      offset += secsize;
      name_offset += strlen (section->name) + 1;
      secaddr += secsize;
    }

  /* Write out the section names.  */

  if (!simple_object_mach_o_write_section_header (sobj, descriptor,
						  sechdr_offset,
						  GNU_SECTION_NAMES, secaddr,
						  name_offset, offset, 0,
						  errmsg, err))
    return 0;

  for (section = sobj->sections; section != NULL; section = section->next)
    {
      size_t namelen;

      namelen = strlen (section->name) + 1;
      if (!simple_object_internal_write (descriptor, offset,
					 (const unsigned char *) section->name,
					 namelen, errmsg, err))
	return 0;
      offset += namelen;
    }

  /* Write out the segment header.  */

  memset (hdrbuf, 0, sizeof hdrbuf);

  hdr = &hdrbuf[0];
  if (attrs->magic == MACH_O_MH_MAGIC)
    {
      set_32 (hdr + offsetof (struct mach_o_segment_command_32, cmd),
	      MACH_O_LC_SEGMENT);
      set_32 (hdr + offsetof (struct mach_o_segment_command_32, cmdsize),
	      cmdsize);
      strncpy (((char *) hdr
		+ offsetof (struct mach_o_segment_command_32, segname)),
	       sobj->segment_name, MACH_O_NAME_LEN);
      /* vmaddr left as zero.  */
      /* vmsize left as zero.  */
      set_32 (hdr + offsetof (struct mach_o_segment_command_32, fileoff),
	      hdrsize + cmdsize);
      set_32 (hdr + offsetof (struct mach_o_segment_command_32, filesize),
	      offset - (hdrsize + cmdsize));
      /* maxprot left as zero.  */
      /* initprot left as zero.  */
      set_32 (hdr + offsetof (struct mach_o_segment_command_32, nsects),
	      nsects);
      /* flags left as zero.  */
    }
  else
    {
#ifdef UNSIGNED_64BIT_TYPE
      void (*set_64) (unsigned char *, ulong_type);

      set_64 = (attrs->is_big_endian
		? simple_object_set_big_64
		: simple_object_set_little_64);

      set_32 (hdr + offsetof (struct mach_o_segment_command_64, cmd),
	      MACH_O_LC_SEGMENT);
      set_32 (hdr + offsetof (struct mach_o_segment_command_64, cmdsize),
	      cmdsize);
      strncpy (((char *) hdr
		+ offsetof (struct mach_o_segment_command_64, segname)),
	       sobj->segment_name, MACH_O_NAME_LEN);
      /* vmaddr left as zero.  */
      /* vmsize left as zero.  */
      set_64 (hdr + offsetof (struct mach_o_segment_command_64, fileoff),
	      hdrsize + cmdsize);
      set_64 (hdr + offsetof (struct mach_o_segment_command_64, filesize),
	      offset - (hdrsize + cmdsize));
      /* maxprot left as zero.  */
      /* initprot left as zero.  */
      set_32 (hdr + offsetof (struct mach_o_segment_command_64, nsects),
	      nsects);
      /* flags left as zero.  */
#endif
    }

  return simple_object_internal_write (descriptor, hdrsize, hdr, seghdrsize,
				       errmsg, err);
}

/* Write out a complete Mach-O file.  */

static const char *
simple_object_mach_o_write_to_file (simple_object_write *sobj, int descriptor,
				    int *err)
{
  size_t nsects;
  simple_object_write_section *section;
  const char *errmsg;

  /* Start at 1 for symbol_names section.  */
  nsects = 1;
  for (section = sobj->sections; section != NULL; section = section->next)
    ++nsects;

  if (!simple_object_mach_o_write_header (sobj, descriptor, nsects,
					  &errmsg, err))
    return errmsg;

  if (!simple_object_mach_o_write_segment (sobj, descriptor, nsects,
					   &errmsg, err))
    return errmsg;

  return NULL;
}

/* Release the private data for an simple_object_write structure.  */

static void
simple_object_mach_o_release_write (void *data)
{
  XDELETE (data);
}

/* The Mach-O functions.  */

const struct simple_object_functions simple_object_mach_o_functions =
{
  simple_object_mach_o_match,
  simple_object_mach_o_find_sections,
  simple_object_mach_o_fetch_attributes,
  simple_object_mach_o_release_read,
  simple_object_mach_o_attributes_merge,
  simple_object_mach_o_release_attributes,
  simple_object_mach_o_start_write,
  simple_object_mach_o_write_to_file,
  simple_object_mach_o_release_write
};
