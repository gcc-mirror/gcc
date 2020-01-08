/* simple-object-coff.c -- routines to manipulate COFF object files.
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

/* COFF structures and constants.  */

/* COFF file header.  */

struct external_filehdr
{
  unsigned char f_magic[2];	/* magic number			*/
  unsigned char f_nscns[2];	/* number of sections		*/
  unsigned char f_timdat[4];	/* time & date stamp		*/
  unsigned char f_symptr[4];	/* file pointer to symtab	*/
  unsigned char f_nsyms[4];	/* number of symtab entries	*/
  unsigned char f_opthdr[2];	/* sizeof(optional hdr)		*/
  unsigned char f_flags[2];	/* flags			*/
};

/* Bits for filehdr f_flags field.  */

#define F_EXEC			(0x0002)
#define IMAGE_FILE_SYSTEM	(0x1000)
#define IMAGE_FILE_DLL		(0x2000)

/* COFF section header.  */

struct external_scnhdr
{
  unsigned char s_name[8];	/* section name				*/
  unsigned char s_paddr[4];	/* physical address, aliased s_nlib 	*/
  unsigned char s_vaddr[4];	/* virtual address			*/
  unsigned char s_size[4];	/* section size				*/
  unsigned char s_scnptr[4];	/* file ptr to raw data for section 	*/
  unsigned char s_relptr[4];	/* file ptr to relocation		*/
  unsigned char s_lnnoptr[4];	/* file ptr to line numbers		*/
  unsigned char s_nreloc[2];	/* number of relocation entries		*/
  unsigned char s_nlnno[2];	/* number of line number entries	*/
  unsigned char s_flags[4];	/* flags				*/
};

/* The length of the s_name field in struct external_scnhdr.  */

#define SCNNMLEN (8)

/* Bits for scnhdr s_flags field.  This includes some bits defined
   only for PE.  This may need to be moved into coff_magic.  */

#define STYP_DATA			(1 << 6)
#define IMAGE_SCN_MEM_DISCARDABLE	(1 << 25)
#define IMAGE_SCN_MEM_SHARED		(1 << 28)
#define IMAGE_SCN_MEM_READ		(1 << 30)

#define IMAGE_SCN_ALIGN_POWER_BIT_POS	     20
#define IMAGE_SCN_ALIGN_POWER_CONST(val)     \
  (((val) + 1) << IMAGE_SCN_ALIGN_POWER_BIT_POS)

/* COFF symbol table entry.  */

#define E_SYMNMLEN	8	/* # characters in a symbol name	*/

struct external_syment
{
  union
  {
    unsigned char e_name[E_SYMNMLEN];

    struct
    {
      unsigned char e_zeroes[4];
      unsigned char e_offset[4];
    } e;
  } e;

  unsigned char e_value[4];
  unsigned char e_scnum[2];
  unsigned char e_type[2];
  unsigned char e_sclass[1];
  unsigned char e_numaux[1];
};

/* Length allowed for filename in aux sym format 4.  */

#define E_FILNMLEN	18

/* Omits x_sym and other unused variants.  */

union external_auxent
{
  /* Aux sym format 4: file.  */
  union
  {
    char x_fname[E_FILNMLEN];
    struct
    {
      unsigned char x_zeroes[4];
      unsigned char x_offset[4];
    } x_n;
  } x_file;
  /* Aux sym format 5: section.  */
  struct
  {
    unsigned char x_scnlen[4];		/* section length		*/
    unsigned char x_nreloc[2];		/* # relocation entries		*/
    unsigned char x_nlinno[2];		/* # line numbers		*/
    unsigned char x_checksum[4];	/* section COMDAT checksum	*/
    unsigned char x_associated[2];	/* COMDAT assoc section index	*/
    unsigned char x_comdat[1];		/* COMDAT selection number	*/
  } x_scn;
};

/* Symbol-related constants.  */

#define IMAGE_SYM_DEBUG		(-2)
#define IMAGE_SYM_TYPE_NULL	(0)
#define IMAGE_SYM_DTYPE_NULL	(0)
#define IMAGE_SYM_CLASS_STATIC	(3)
#define IMAGE_SYM_CLASS_FILE	(103)

#define IMAGE_SYM_TYPE \
  ((IMAGE_SYM_DTYPE_NULL << 4) | IMAGE_SYM_TYPE_NULL)

/* Private data for an simple_object_read.  */

struct simple_object_coff_read
{
  /* Magic number.  */
  unsigned short magic;
  /* Whether the file is big-endian.  */
  unsigned char is_big_endian;
  /* Number of sections.  */
  unsigned short nscns;
  /* File offset of symbol table.  */
  off_t symptr;
  /* Number of symbol table entries.  */
  unsigned int nsyms;
  /* Flags.  */
  unsigned short flags;
  /* Offset of section headers in file.  */
  off_t scnhdr_offset;
};

/* Private data for an simple_object_attributes.  */

struct simple_object_coff_attributes
{
  /* Magic number.  */
  unsigned short magic;
  /* Whether the file is big-endian.  */
  unsigned char is_big_endian;
  /* Flags.  */
  unsigned short flags;
};

/* There is no magic number which indicates a COFF file as opposed to
   any other sort of file.  Instead, each COFF file starts with a
   two-byte magic number which also indicates the type of the target.
   This struct holds a magic number as well as characteristics of that
   COFF format.  */

struct coff_magic_struct
{
  /* Magic number.  */
  unsigned short magic;
  /* Whether this magic number is for a big-endian file.  */
  unsigned char is_big_endian;
  /* Flag bits, in the f_flags fields, which indicates that this file
     is not a relocatable object file.  There is no flag which
     specifically indicates a relocatable object file, it is only
     implied by the absence of these flags.  */
  unsigned short non_object_flags;
};

/* This is a list of the COFF magic numbers which we recognize, namely
   the ones used on Windows.  More can be added as needed.  */

static const struct coff_magic_struct coff_magic[] =
{
  /* i386.  */
  { 0x14c, 0, F_EXEC | IMAGE_FILE_SYSTEM | IMAGE_FILE_DLL },
  /* x86_64.  */
  { 0x8664, 0, F_EXEC | IMAGE_FILE_SYSTEM | IMAGE_FILE_DLL }
};

/* See if we have a COFF file.  */

static void *
simple_object_coff_match (unsigned char header[SIMPLE_OBJECT_MATCH_HEADER_LEN],
			  int descriptor, off_t offset,
			  const char *segment_name ATTRIBUTE_UNUSED,
			  const char **errmsg, int *err)
{
  size_t c;
  unsigned short magic_big;
  unsigned short magic_little;
  unsigned short magic;
  size_t i;
  int is_big_endian;
  unsigned short (*fetch_16) (const unsigned char *);
  unsigned int (*fetch_32) (const unsigned char *);
  unsigned char hdrbuf[sizeof (struct external_filehdr)];
  unsigned short flags;
  struct simple_object_coff_read *ocr;

  c = sizeof (coff_magic) / sizeof (coff_magic[0]);
  magic_big = simple_object_fetch_big_16 (header);
  magic_little = simple_object_fetch_little_16 (header);
  for (i = 0; i < c; ++i)
    {
      if (coff_magic[i].is_big_endian
	  ? coff_magic[i].magic == magic_big
	  : coff_magic[i].magic == magic_little)
	break;
    }
  if (i >= c)
    {
      *errmsg = NULL;
      *err = 0;
      return NULL;
    }
  is_big_endian = coff_magic[i].is_big_endian;

  magic = is_big_endian ? magic_big : magic_little;
  fetch_16 = (is_big_endian
	      ? simple_object_fetch_big_16
	      : simple_object_fetch_little_16);
  fetch_32 = (is_big_endian
	      ? simple_object_fetch_big_32
	      : simple_object_fetch_little_32);

  if (!simple_object_internal_read (descriptor, offset, hdrbuf, sizeof hdrbuf,
				    errmsg, err))
    return NULL;

  flags = fetch_16 (hdrbuf + offsetof (struct external_filehdr, f_flags));
  if ((flags & coff_magic[i].non_object_flags) != 0)
    {
      *errmsg = "not relocatable object file";
      *err = 0;
      return NULL;
    }

  ocr = XNEW (struct simple_object_coff_read);
  ocr->magic = magic;
  ocr->is_big_endian = is_big_endian;
  ocr->nscns = fetch_16 (hdrbuf + offsetof (struct external_filehdr, f_nscns));
  ocr->symptr = fetch_32 (hdrbuf
			  + offsetof (struct external_filehdr, f_symptr));
  ocr->nsyms = fetch_32 (hdrbuf + offsetof (struct external_filehdr, f_nsyms));
  ocr->flags = flags;
  ocr->scnhdr_offset = (sizeof (struct external_filehdr)
			+ fetch_16 (hdrbuf + offsetof (struct external_filehdr,
						       f_opthdr)));

  return (void *) ocr;
}

/* Read the string table in a COFF file.  */

static char *
simple_object_coff_read_strtab (simple_object_read *sobj, size_t *strtab_size,
				const char **errmsg, int *err)
{
  struct simple_object_coff_read *ocr =
    (struct simple_object_coff_read *) sobj->data;
  off_t strtab_offset;
  unsigned char strsizebuf[4];
  size_t strsize;
  char *strtab;

  strtab_offset = sobj->offset + ocr->symptr
		  + ocr->nsyms * sizeof (struct external_syment);
  if (!simple_object_internal_read (sobj->descriptor, strtab_offset,
				    strsizebuf, 4, errmsg, err))
    return NULL;
  strsize = (ocr->is_big_endian
	     ? simple_object_fetch_big_32 (strsizebuf)
	     : simple_object_fetch_little_32 (strsizebuf));
  strtab = XNEWVEC (char, strsize);
  if (!simple_object_internal_read (sobj->descriptor, strtab_offset,
				    (unsigned char *) strtab, strsize, errmsg,
				    err))
    {
      XDELETEVEC (strtab);
      return NULL;
    }
  *strtab_size = strsize;
  return strtab;
}

/* Find all sections in a COFF file.  */

static const char *
simple_object_coff_find_sections (simple_object_read *sobj,
				  int (*pfn) (void *, const char *,
					      off_t offset, off_t length),
				  void *data,
				  int *err)
{
  struct simple_object_coff_read *ocr =
    (struct simple_object_coff_read *) sobj->data;
  size_t scnhdr_size;
  unsigned char *scnbuf;
  const char *errmsg;
  unsigned int (*fetch_32) (const unsigned char *);
  unsigned int nscns;
  char *strtab;
  size_t strtab_size;
  unsigned int i;

  scnhdr_size = sizeof (struct external_scnhdr);
  scnbuf = XNEWVEC (unsigned char, scnhdr_size * ocr->nscns);
  if (!simple_object_internal_read (sobj->descriptor,
				    sobj->offset + ocr->scnhdr_offset,
				    scnbuf, scnhdr_size * ocr->nscns, &errmsg,
				    err))
    {
      XDELETEVEC (scnbuf);
      return errmsg;
    }

  fetch_32 = (ocr->is_big_endian
	      ? simple_object_fetch_big_32
	      : simple_object_fetch_little_32);

  nscns = ocr->nscns;
  strtab = NULL;
  strtab_size = 0;
  for (i = 0; i < nscns; ++i)
    {
      unsigned char *scnhdr;
      unsigned char *scnname;
      char namebuf[SCNNMLEN + 1];
      char *name;
      off_t scnptr;
      unsigned int size;

      scnhdr = scnbuf + i * scnhdr_size;
      scnname = scnhdr + offsetof (struct external_scnhdr, s_name);
      memcpy (namebuf, scnname, SCNNMLEN);
      namebuf[SCNNMLEN] = '\0';
      name = &namebuf[0];
      if (namebuf[0] == '/')
	{
	  size_t strindex;
	  char *end;

	  strindex = strtol (namebuf + 1, &end, 10);
	  if (*end == '\0')
	    {
	      /* The real section name is found in the string
		 table.  */
	      if (strtab == NULL)
		{
		  strtab = simple_object_coff_read_strtab (sobj,
							   &strtab_size,
							   &errmsg, err);
		  if (strtab == NULL)
		    {
		      XDELETEVEC (scnbuf);
		      return errmsg;
		    }
		}

	      if (strindex < 4 || strindex >= strtab_size)
		{
		  XDELETEVEC (strtab);
		  XDELETEVEC (scnbuf);
		  *err = 0;
		  return "section string index out of range";
		}

	      name = strtab + strindex;
	    }
	}

      scnptr = fetch_32 (scnhdr + offsetof (struct external_scnhdr, s_scnptr));
      size = fetch_32 (scnhdr + offsetof (struct external_scnhdr, s_size));

      if (!(*pfn) (data, name, scnptr, size))
	break;
    }

  if (strtab != NULL)
    XDELETEVEC (strtab);
  XDELETEVEC (scnbuf);

  return NULL;
}

/* Fetch the attributes for an simple_object_read.  */

static void *
simple_object_coff_fetch_attributes (simple_object_read *sobj,
				     const char **errmsg ATTRIBUTE_UNUSED,
				     int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_coff_read *ocr =
    (struct simple_object_coff_read *) sobj->data;
  struct simple_object_coff_attributes *ret;

  ret = XNEW (struct simple_object_coff_attributes);
  ret->magic = ocr->magic;
  ret->is_big_endian = ocr->is_big_endian;
  ret->flags = ocr->flags;
  return ret;
}

/* Release the private data for an simple_object_read.  */

static void
simple_object_coff_release_read (void *data)
{
  XDELETE (data);
}

/* Compare two attributes structures.  */

static const char *
simple_object_coff_attributes_merge (void *todata, void *fromdata, int *err)
{
  struct simple_object_coff_attributes *to =
    (struct simple_object_coff_attributes *) todata;
  struct simple_object_coff_attributes *from =
    (struct simple_object_coff_attributes *) fromdata;

  if (to->magic != from->magic || to->is_big_endian != from->is_big_endian)
    {
      *err = 0;
      return "COFF object format mismatch";
    }
  return NULL;
}

/* Release the private data for an attributes structure.  */

static void
simple_object_coff_release_attributes (void *data)
{
  XDELETE (data);
}

/* Prepare to write out a file.  */

static void *
simple_object_coff_start_write (void *attributes_data,
				const char **errmsg ATTRIBUTE_UNUSED,
				int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_coff_attributes *attrs =
    (struct simple_object_coff_attributes *) attributes_data;
  struct simple_object_coff_attributes *ret;

  /* We're just going to record the attributes, but we need to make a
     copy because the user may delete them.  */
  ret = XNEW (struct simple_object_coff_attributes);
  *ret = *attrs;
  return ret;
}

/* Write out a COFF filehdr.  */

static int
simple_object_coff_write_filehdr (simple_object_write *sobj, int descriptor,
				  unsigned int nscns, size_t symtab_offset,
				  unsigned int nsyms, const char **errmsg,
				  int *err)
{
  struct simple_object_coff_attributes *attrs =
    (struct simple_object_coff_attributes *) sobj->data;
  unsigned char hdrbuf[sizeof (struct external_filehdr)];
  unsigned char *hdr;
  void (*set_16) (unsigned char *, unsigned short);
  void (*set_32) (unsigned char *, unsigned int);

  hdr = &hdrbuf[0];

  set_16 = (attrs->is_big_endian
	    ? simple_object_set_big_16
	    : simple_object_set_little_16);
  set_32 = (attrs->is_big_endian
	    ? simple_object_set_big_32
	    : simple_object_set_little_32);

  memset (hdr, 0, sizeof (struct external_filehdr));

  set_16 (hdr + offsetof (struct external_filehdr, f_magic), attrs->magic);
  set_16 (hdr + offsetof (struct external_filehdr, f_nscns), nscns);
  /* f_timdat left as zero.  */
  set_32 (hdr + offsetof (struct external_filehdr, f_symptr), symtab_offset);
  set_32 (hdr + offsetof (struct external_filehdr, f_nsyms), nsyms);
  /* f_opthdr left as zero.  */
  set_16 (hdr + offsetof (struct external_filehdr, f_flags), attrs->flags);

  return simple_object_internal_write (descriptor, 0, hdrbuf,
				       sizeof (struct external_filehdr),
				       errmsg, err);
}

/* Write out a COFF section header.  */

static int
simple_object_coff_write_scnhdr (simple_object_write *sobj, int descriptor,
				 const char *name, size_t *name_offset,
				 off_t scnhdr_offset, size_t scnsize,
				 off_t offset, unsigned int align,
				 const char **errmsg, int *err)
{
  struct simple_object_coff_attributes *attrs =
    (struct simple_object_coff_attributes *) sobj->data;
  void (*set_32) (unsigned char *, unsigned int);
  unsigned char hdrbuf[sizeof (struct external_scnhdr)];
  unsigned char *hdr;
  size_t namelen;
  unsigned int flags;

  set_32 = (attrs->is_big_endian
	    ? simple_object_set_big_32
	    : simple_object_set_little_32);

  memset (hdrbuf, 0, sizeof hdrbuf);
  hdr = &hdrbuf[0];

  namelen = strlen (name);
  if (namelen <= SCNNMLEN)
    strncpy ((char *) hdr + offsetof (struct external_scnhdr, s_name), name,
	     SCNNMLEN);
  else
    {
      snprintf ((char *) hdr + offsetof (struct external_scnhdr, s_name),
		SCNNMLEN, "/%lu", (unsigned long) *name_offset);
      *name_offset += namelen + 1;
    }

  /* s_paddr left as zero.  */
  /* s_vaddr left as zero.  */
  set_32 (hdr + offsetof (struct external_scnhdr, s_size), scnsize);
  set_32 (hdr + offsetof (struct external_scnhdr, s_scnptr), offset);
  /* s_relptr left as zero.  */
  /* s_lnnoptr left as zero.  */
  /* s_nreloc left as zero.  */
  /* s_nlnno left as zero.  */
  flags = (STYP_DATA | IMAGE_SCN_MEM_DISCARDABLE | IMAGE_SCN_MEM_SHARED
	   | IMAGE_SCN_MEM_READ);
  /* PE can represent alignment up to 13.  */
  if (align > 13)
    align = 13;
  flags |= IMAGE_SCN_ALIGN_POWER_CONST(align);
  set_32 (hdr + offsetof (struct external_scnhdr, s_flags), flags);

  return simple_object_internal_write (descriptor, scnhdr_offset, hdrbuf,
				       sizeof (struct external_scnhdr),
				       errmsg, err);
}

/* Write out a complete COFF file.  */

static const char *
simple_object_coff_write_to_file (simple_object_write *sobj, int descriptor,
				  int *err)
{
  struct simple_object_coff_attributes *attrs =
    (struct simple_object_coff_attributes *) sobj->data;
  unsigned int nscns, secnum;
  simple_object_write_section *section;
  off_t scnhdr_offset;
  size_t symtab_offset;
  off_t secsym_offset;
  unsigned int nsyms;
  size_t offset;
  size_t name_offset;
  const char *errmsg;
  unsigned char strsizebuf[4];
  /* The interface doesn't give us access to the name of the input file
     yet.  We want to use its basename for the FILE symbol.  This is
     what 'gas' uses when told to assemble from stdin.  */
  const char *source_filename = "fake";
  size_t sflen;
  union
  {
    struct external_syment sym;
    union external_auxent aux;
  } syms[2];
  void (*set_16) (unsigned char *, unsigned short);
  void (*set_32) (unsigned char *, unsigned int);

  set_16 = (attrs->is_big_endian
	    ? simple_object_set_big_16
	    : simple_object_set_little_16);
  set_32 = (attrs->is_big_endian
	    ? simple_object_set_big_32
	    : simple_object_set_little_32);

  nscns = 0;
  for (section = sobj->sections; section != NULL; section = section->next)
    ++nscns;

  scnhdr_offset = sizeof (struct external_filehdr);
  offset = scnhdr_offset + nscns * sizeof (struct external_scnhdr);
  name_offset = 4;
  for (section = sobj->sections; section != NULL; section = section->next)
    {
      size_t mask;
      size_t new_offset;
      size_t scnsize;
      struct simple_object_write_section_buffer *buffer;

      mask = (1U << section->align) - 1;
      new_offset = offset & mask;
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
					     &errmsg, err))
	    return errmsg;
	}

      scnsize = 0;
      for (buffer = section->buffers; buffer != NULL; buffer = buffer->next)
	{
	  if (!simple_object_internal_write (descriptor, offset + scnsize,
					     ((const unsigned char *)
					      buffer->buffer),
					     buffer->size, &errmsg, err))
	    return errmsg;
	  scnsize += buffer->size;
	}

      if (!simple_object_coff_write_scnhdr (sobj, descriptor, section->name,
					    &name_offset, scnhdr_offset,
					    scnsize, offset, section->align,
					    &errmsg, err))
	return errmsg;

      scnhdr_offset += sizeof (struct external_scnhdr);
      offset += scnsize;
    }

  /* Symbol table is always half-word aligned.  */
  offset += (offset & 1);
  /* There is a file symbol and a section symbol per section,
     and each of these has a single auxiliary symbol following.  */
  nsyms = 2 * (nscns + 1);
  symtab_offset = offset;
  /* Advance across space reserved for symbol table to locate
     start of string table.  */
  offset += nsyms * sizeof (struct external_syment);

  /* Write out file symbol.  */
  memset (&syms[0], 0, sizeof (syms));
  strcpy ((char *)&syms[0].sym.e.e_name[0], ".file");
  set_16 (&syms[0].sym.e_scnum[0], IMAGE_SYM_DEBUG);
  set_16 (&syms[0].sym.e_type[0], IMAGE_SYM_TYPE);
  syms[0].sym.e_sclass[0] = IMAGE_SYM_CLASS_FILE;
  syms[0].sym.e_numaux[0] = 1;
  /* The name need not be nul-terminated if it fits into the x_fname field
     directly, but must be if it has to be placed into the string table.  */
  sflen = strlen (source_filename);
  if (sflen <= E_FILNMLEN)
    memcpy (&syms[1].aux.x_file.x_fname[0], source_filename, sflen);
  else
    {
      set_32 (&syms[1].aux.x_file.x_n.x_offset[0], name_offset);
      if (!simple_object_internal_write (descriptor, offset + name_offset,
					 ((const unsigned char *)
					  source_filename),
					 sflen + 1, &errmsg, err))
	return errmsg;
      name_offset += strlen (source_filename) + 1;
    }
  if (!simple_object_internal_write (descriptor, symtab_offset,
				     (const unsigned char *) &syms[0],
				     sizeof (syms), &errmsg, err))
    return errmsg;

  /* Write the string table length, followed by the strings and section
     symbols in step with each other.  */
  set_32 (strsizebuf, name_offset);
  if (!simple_object_internal_write (descriptor, offset, strsizebuf, 4,
				     &errmsg, err))
    return errmsg;

  name_offset = 4;
  secsym_offset = symtab_offset + sizeof (syms);
  memset (&syms[0], 0, sizeof (syms));
  set_16 (&syms[0].sym.e_type[0], IMAGE_SYM_TYPE);
  syms[0].sym.e_sclass[0] = IMAGE_SYM_CLASS_STATIC;
  syms[0].sym.e_numaux[0] = 1;
  secnum = 1;

  for (section = sobj->sections; section != NULL; section = section->next)
    {
      size_t namelen;
      size_t scnsize;
      struct simple_object_write_section_buffer *buffer;

      namelen = strlen (section->name);
      set_16 (&syms[0].sym.e_scnum[0], secnum++);
      scnsize = 0;
      for (buffer = section->buffers; buffer != NULL; buffer = buffer->next)
	scnsize += buffer->size;
      set_32 (&syms[1].aux.x_scn.x_scnlen[0], scnsize);
      if (namelen > SCNNMLEN)
	{
	  set_32 (&syms[0].sym.e.e.e_zeroes[0], 0);
	  set_32 (&syms[0].sym.e.e.e_offset[0], name_offset);
	  if (!simple_object_internal_write (descriptor, offset + name_offset,
					     ((const unsigned char *)
					      section->name),
					     namelen + 1, &errmsg, err))
	    return errmsg;
	  name_offset += namelen + 1;
	}
      else
	{
	  memcpy (&syms[0].sym.e.e_name[0], section->name,
		  strlen (section->name));
	  memset (&syms[0].sym.e.e_name[strlen (section->name)], 0,
		  E_SYMNMLEN - strlen (section->name));
	}

      if (!simple_object_internal_write (descriptor, secsym_offset,
					 (const unsigned char *) &syms[0],
					 sizeof (syms), &errmsg, err))
	return errmsg;
      secsym_offset += sizeof (syms);
    }

  if (!simple_object_coff_write_filehdr (sobj, descriptor, nscns,
					 symtab_offset, nsyms, &errmsg, err))
    return errmsg;

  return NULL;
}

/* Release the private data for an simple_object_write structure.  */

static void
simple_object_coff_release_write (void *data)
{
  XDELETE (data);
}

/* The COFF functions.  */

const struct simple_object_functions simple_object_coff_functions =
{
  simple_object_coff_match,
  simple_object_coff_find_sections,
  simple_object_coff_fetch_attributes,
  simple_object_coff_release_read,
  simple_object_coff_attributes_merge,
  simple_object_coff_release_attributes,
  simple_object_coff_start_write,
  simple_object_coff_write_to_file,
  simple_object_coff_release_write,
  NULL
};
