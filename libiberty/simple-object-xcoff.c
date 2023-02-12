/* simple-object-coff.c -- routines to manipulate XCOFF object files.
   Copyright (C) 2013-2023 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Google and David Edelsohn, IBM.

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

/* XCOFF structures and constants.  */

/* XCOFF file header.  */

struct external_filehdr
{
  unsigned char f_magic[2];	/* magic number			*/
  unsigned char f_nscns[2];	/* number of sections		*/
  unsigned char f_timdat[4];	/* time & date stamp		*/
  union
  {
    struct
    {
      unsigned char f_symptr[4];	/* file pointer to symtab	*/
      unsigned char f_nsyms[4];	/* number of symtab entries	*/
      unsigned char f_opthdr[2];	/* sizeof(optional hdr)		*/
      unsigned char f_flags[2];	/* flags			*/
    } xcoff32;
    struct
    {
      unsigned char f_symptr[8];	/* file pointer to symtab	*/
      unsigned char f_opthdr[2];	/* sizeof(optional hdr)		*/
      unsigned char f_flags[2];	/* flags			*/
      unsigned char f_nsyms[4];	/* number of symtab entries	*/
    } xcoff64;
  } u;
};

/* Bits for filehdr f_flags field.  */

#define F_EXEC			(0x0002)

/* The known values of f_magic in an XCOFF file header.  */

#define U802WRMAGIC 0730        /* Writeable text segments.  */
#define U802ROMAGIC 0735        /* Readonly sharable text segments.  */
#define U802TOCMAGIC 0737       /* Readonly text segments and TOC.  */
#define U803XTOCMAGIC 0757      /* Aix 4.3 64-bit XCOFF.  */
#define U64_TOCMAGIC 0767       /* AIX 5+ 64-bit XCOFF.  */

/* XCOFF section header.  */

struct external_scnhdr
{
  unsigned char s_name[8];	/* section name				*/
  union
  {
    struct
    {
      unsigned char s_paddr[4];	/* physical address, aliased s_nlib 	*/
      unsigned char s_vaddr[4];	/* virtual address			*/
      unsigned char s_size[4];	/* section size				*/
      unsigned char s_scnptr[4];	/* file ptr to raw data for section */
      unsigned char s_relptr[4];	/* file ptr to relocation	*/
      unsigned char s_lnnoptr[4];	/* file ptr to line numbers	*/
      unsigned char s_nreloc[2];	/* number of relocation entries	*/
      unsigned char s_nlnno[2];	/* number of line number entries	*/
      unsigned char s_flags[4];	/* flags				*/
    } xcoff32;
    struct
    {
      unsigned char s_paddr[8];	/* physical address, aliased s_nlib 	*/
      unsigned char s_vaddr[8];	/* virtual address			*/
      unsigned char s_size[8];	/* section size				*/
      unsigned char s_scnptr[8];	/* file ptr to raw data for section */
      unsigned char s_relptr[8];	/* file ptr to relocation	*/
      unsigned char s_lnnoptr[8];	/* file ptr to line numbers	*/
      unsigned char s_nreloc[4];	/* number of relocation entries	*/
      unsigned char s_nlnno[4];	/* number of line number entries	*/
      unsigned char s_flags[4];	/* flags				*/
    } xcoff64;
  } u;
};

#define SCNHSZ32	(40)
#define SCNHSZ64	(68)

/* The length of the s_name field in struct external_scnhdr.  */

#define SCNNMLEN	(8)

/* Bits for scnhdr s_flags field.  */

#define STYP_DATA			0x40

/* XCOFF symbol table entry.  */


#define N_SYMNMLEN	(8)	/* # characters in a symbol name	*/

/* The format of an XCOFF symbol-table entry.  */
struct external_syment
{
  union {
    struct {
      union {
/* The name of the symbol.  There is an implicit null character
   after the end of the array.  */
	char n_name[N_SYMNMLEN];
	struct {
	  /* If n_zeroes is zero, n_offset is the offset the name from
	     the start of the string table.  */
	  unsigned char n_zeroes[4];
	  unsigned char n_offset[4];
	} n;
      } n;

      /* The symbol's value.  */
      unsigned char n_value[4];
    } xcoff32;
    struct {
      /* The symbol's value.  */
      unsigned char n_value[8];

      /* The offset of the symbol from the start of the string table.  */
      unsigned char n_offset[4];
    } xcoff64;
  } u;

  /* The number of the section to which this symbol belongs.  */
  unsigned char n_scnum[2];

  /* The type of symbol.  (It can be interpreted as an n_lang
     and an n_cpu byte, but we don't care about that here.)  */
  unsigned char n_type[2];

  /* The class of symbol (a C_* value).  */
  unsigned char n_sclass[1];

  /* The number of auxiliary symbols attached to this entry.  */
  unsigned char n_numaux[1];
};

#define SYMESZ		(18)

/* Length allowed for filename in aux sym format 4.  */

#define FILNMLEN	(14)

/* Omits x_sym and other unused variants.  */

union external_auxent
{
  /* Aux sym format 4: file.  */
  union
  {
    char x_fname[FILNMLEN];
    struct
    {
      unsigned char x_zeroes[4];
      unsigned char x_offset[4];
      unsigned char x_pad[FILNMLEN-8];
      unsigned char x_ftype;
    } _x;
  } x_file;
  /* Aux sym format 5: section.  */
  struct
  {
    unsigned char x_scnlen[4];		/* section length		*/
    unsigned char x_nreloc[2];		/* # relocation entries		*/
    unsigned char x_nlinno[2];		/* # line numbers		*/
  } x_scn;
  /* CSECT auxiliary entry.  */
  union
  {
    struct
    {
      struct
      {
	unsigned char x_scnlen[4];	/* csect length */
	unsigned char x_parmhash[4];	/* parm type hash index */
	unsigned char x_snhash[2];	/* sect num with parm hash */
	unsigned char x_smtyp;		/* symbol align and type */
	unsigned char x_smclas;		/* storage mapping class */
	unsigned char x_stab;		/* dbx stab info index */
	unsigned char x_snstab[2];	/* sect num with dbx stab */
      } x_csect;
    } xcoff32;
    struct
    {
      struct
      {
	unsigned char x_scnlen_lo[4];	/* csect length */
	unsigned char x_parmhash[4];	/* parm type hash index */
	unsigned char x_snhash[2];	/* sect num with parm hash */
	unsigned char x_smtyp;		/* symbol align and type */
	unsigned char x_smclas;		/* storage mapping class */
	unsigned char x_scnlen_hi[4];
	unsigned char pad;
	unsigned char x_auxtype;
      } x_csect;
    } xcoff64;
  } u;
  /* SECTION/DWARF auxiliary entry.  */
  struct
  {
    unsigned char x_scnlen[4];		/* section length */
    unsigned char pad1[4];
    unsigned char x_nreloc[4];		/* number RLDs */
  } x_sect;
};

/* Symbol-related constants.  */

#define N_DEBUG		(-2)
#define IMAGE_SYM_TYPE_NULL	(0)
#define IMAGE_SYM_DTYPE_NULL	(0)
#define IMAGE_SYM_CLASS_STATIC	(3)
#define IMAGE_SYM_CLASS_FILE	(103)

#define IMAGE_SYM_TYPE \
  ((IMAGE_SYM_DTYPE_NULL << 4) | IMAGE_SYM_TYPE_NULL)

#define C_EXT		(2)
#define C_STAT		(3)
#define C_FILE		(103)
#define C_HIDEXT	(107)

#define XTY_SD		(1)	/* section definition */

#define XMC_XO		(7)	/* extended operation */

/* Private data for an simple_object_read.  */

struct simple_object_xcoff_read
{
  /* Magic number.  */
  unsigned short magic;
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

struct simple_object_xcoff_attributes
{
  /* Magic number.  */
  unsigned short magic;
  /* Flags.  */
  unsigned short flags;
};

/* See if we have a XCOFF file.  */

static void *
simple_object_xcoff_match (unsigned char header[SIMPLE_OBJECT_MATCH_HEADER_LEN],
			   int descriptor, off_t offset,
			   const char *segment_name ATTRIBUTE_UNUSED,
			   const char **errmsg, int *err)
{
  unsigned short magic;
  unsigned short (*fetch_16) (const unsigned char *);
  unsigned int (*fetch_32) (const unsigned char *);
  ulong_type (*fetch_64) (const unsigned char *);
  unsigned char hdrbuf[sizeof (struct external_filehdr)];
  struct simple_object_xcoff_read *ocr;
  int u64;

  magic = simple_object_fetch_big_16 (header);

  if (magic != U802TOCMAGIC && magic != U64_TOCMAGIC)
    {
      *errmsg = NULL;
      *err = 0;
      return NULL;
    }

  fetch_16 = simple_object_fetch_big_16;
  fetch_32 = simple_object_fetch_big_32;
  fetch_64 = simple_object_fetch_big_64;

  if (!simple_object_internal_read (descriptor, offset, hdrbuf, sizeof hdrbuf,
				    errmsg, err))
    return NULL;

  u64 = magic == U64_TOCMAGIC;

  ocr = XNEW (struct simple_object_xcoff_read);
  ocr->magic = magic;
  ocr->nscns = fetch_16 (hdrbuf + offsetof (struct external_filehdr, f_nscns));
  if (u64)
    {
      ocr->symptr = fetch_64 (hdrbuf
			      + offsetof (struct external_filehdr,
					  u.xcoff64.f_symptr));
      ocr->nsyms = fetch_32 (hdrbuf + offsetof (struct external_filehdr,
						u.xcoff64.f_nsyms));
      ocr->scnhdr_offset = (sizeof (struct external_filehdr)
			    + fetch_16 (hdrbuf + offsetof (struct external_filehdr,
							   u.xcoff64.f_opthdr)));

    }
  else
    {
      ocr->symptr = fetch_32 (hdrbuf
			      + offsetof (struct external_filehdr,
					  u.xcoff32.f_symptr));
      ocr->nsyms = fetch_32 (hdrbuf + offsetof (struct external_filehdr,
						u.xcoff32.f_nsyms));
      ocr->scnhdr_offset = (sizeof (struct external_filehdr) - 4
			    + fetch_16 (hdrbuf + offsetof (struct external_filehdr,
							   u.xcoff32.f_opthdr)));

    }

  return (void *) ocr;
}

/* Read the string table in a XCOFF file.  */

static char *
simple_object_xcoff_read_strtab (simple_object_read *sobj, size_t *strtab_size,
				 const char **errmsg, int *err)
{
  struct simple_object_xcoff_read *ocr =
    (struct simple_object_xcoff_read *) sobj->data;
  off_t strtab_offset;
  unsigned char strsizebuf[4];
  size_t strsize;
  char *strtab;

  strtab_offset = sobj->offset + ocr->symptr
    + ocr->nsyms * SYMESZ;
  if (!simple_object_internal_read (sobj->descriptor, strtab_offset,
				    strsizebuf, 4, errmsg, err))
    return NULL;
  strsize = simple_object_fetch_big_32 (strsizebuf);
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

/* Find all sections in a XCOFF file.  */

static const char *
simple_object_xcoff_find_sections (simple_object_read *sobj,
				  int (*pfn) (void *, const char *,
					      off_t offset, off_t length),
				  void *data,
				  int *err)
{
  struct simple_object_xcoff_read *ocr =
    (struct simple_object_xcoff_read *) sobj->data;
  int u64 = ocr->magic == U64_TOCMAGIC;
  size_t scnhdr_size;
  unsigned char *scnbuf;
  const char *errmsg;
  unsigned short (*fetch_16) (const unsigned char *);
  unsigned int (*fetch_32) (const unsigned char *);
  ulong_type (*fetch_64) (const unsigned char *);
  unsigned int nscns;
  char *strtab;
  size_t strtab_size;
  struct external_syment *symtab = NULL;
  unsigned int i;

  scnhdr_size = u64 ? SCNHSZ64 : SCNHSZ32;
  scnbuf = XNEWVEC (unsigned char, scnhdr_size * ocr->nscns);
  if (!simple_object_internal_read (sobj->descriptor,
				    sobj->offset + ocr->scnhdr_offset,
				    scnbuf, scnhdr_size * ocr->nscns, &errmsg,
				    err))
    {
      XDELETEVEC (scnbuf);
      return errmsg;
    }

  fetch_16 = simple_object_fetch_big_16;
  fetch_32 = simple_object_fetch_big_32;
  fetch_64 = simple_object_fetch_big_64;

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
      off_t size;

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
		  strtab = simple_object_xcoff_read_strtab (sobj,
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

      if (u64)
	{
	  scnptr = fetch_64 (scnhdr + offsetof (struct external_scnhdr,
						u.xcoff64.s_scnptr));
	  size = fetch_64 (scnhdr + offsetof (struct external_scnhdr,
					      u.xcoff64.s_size));
	}
      else
	{
	  scnptr = fetch_32 (scnhdr + offsetof (struct external_scnhdr,
						u.xcoff32.s_scnptr));
	  size = fetch_32 (scnhdr + offsetof (struct external_scnhdr,
					      u.xcoff32.s_size));
	}

      if (!(*pfn) (data, name, scnptr, size))
	break;
    }

  /* Special handling for .go_export csect.  */
  if (ocr->nsyms > 0)
    {
      unsigned char *sym;
      const char *n_name;
      off_t size, n_value;
      unsigned int n_numaux, n_offset, n_zeroes;
      short n_scnum;

      /* Read symbol table.  */
      symtab = XNEWVEC (struct external_syment, ocr->nsyms * SYMESZ);
      if (!simple_object_internal_read (sobj->descriptor,
					sobj->offset + ocr->symptr,
					(unsigned char *) symtab,
					ocr->nsyms * SYMESZ,
					&errmsg, err))
	{
	  XDELETEVEC (symtab);
	  XDELETEVEC (scnbuf);
	  return NULL;
	}

      /* Search in symbol table if we have a ".go_export" symbol.  */
      for (i = 0; i < ocr->nsyms; i += n_numaux + 1)
	{
	  sym = (unsigned char *) &symtab[i];
	  n_numaux = symtab[i].n_numaux[0];

	  if (symtab[i].n_sclass[0] != C_EXT
	      && symtab[i].n_sclass[0] != C_HIDEXT)
	    continue;

	  /* Must have at least one csect auxiliary entry.  */
	  if (n_numaux < 1 || i + n_numaux >= ocr->nsyms)
	    continue;

	  n_scnum = fetch_16 (sym + offsetof (struct external_syment,
					      n_scnum));
	  if (n_scnum < 1 || (unsigned int) n_scnum > nscns)
	    continue;

	  if (u64)
	    {
	      n_value = fetch_64 (sym + offsetof (struct external_syment,
						  u.xcoff64.n_value));
	      n_offset = fetch_32 (sym + offsetof (struct external_syment,
						   u.xcoff64.n_offset));
	    }
	  else
	    {
	      /* ".go_export" is longer than N_SYMNMLEN.  */
	      n_zeroes = fetch_32 (sym + offsetof (struct external_syment,
						   u.xcoff32.n.n.n_zeroes));
	      if (n_zeroes != 0)
		continue;

	      n_value = fetch_32 (sym + offsetof (struct external_syment,
						  u.xcoff32.n_value));
	      n_offset = fetch_32 (sym + offsetof (struct external_syment,
						   u.xcoff32.n.n.n_offset));
	    }

	  /* The real symbol name is found in the string table.  */
	  if (strtab == NULL)
	    {
	      strtab = simple_object_xcoff_read_strtab (sobj,
	  						&strtab_size,
							&errmsg, err);
	      if (strtab == NULL)
		{
		  XDELETEVEC (symtab);
		  XDELETEVEC (scnbuf);
		  return errmsg;
		}
	    }

	  if (n_offset >= strtab_size)
	    {
	      XDELETEVEC (strtab);
	      XDELETEVEC (symtab);
	      XDELETEVEC (scnbuf);
	      *err = 0;
	      return "symbol string index out of range";
	    }
	  n_name = strtab + n_offset;

	  if (!strcmp (n_name, ".go_export"))
	    {
	      union external_auxent *auxent;
	      unsigned char *aux, *scnhdr;
	      off_t scnptr, x_scnlen;

	      /* Found .go_export symbol, read its csect auxiliary entry.
		 By convention, it is the last auxiliary entry.  */
	      auxent = (union external_auxent *) &symtab[i + n_numaux];
	      aux = (unsigned char *) auxent;
	      if (u64)
		{
		  /* Use an intermediate 64-bit type to avoid
		     compilation warning about 32-bit shift below on
		     hosts with 32-bit off_t which aren't supported by
		     AC_SYS_LARGEFILE.  */
		  ulong_type x_scnlen64;

		  if ((auxent->u.xcoff64.x_csect.x_smtyp & 0x7) != XTY_SD
		      || auxent->u.xcoff64.x_csect.x_smclas != XMC_XO)
		    continue;

		  x_scnlen64 = 
		    fetch_32 (aux + offsetof (union external_auxent,
					      u.xcoff64.x_csect.x_scnlen_hi));
		  x_scnlen = 
		    ((x_scnlen64 << 32)
		     | fetch_32 (aux
				 + offsetof (union external_auxent,
					     u.xcoff64.x_csect.x_scnlen_lo)));
		}
	      else
		{
		  if ((auxent->u.xcoff32.x_csect.x_smtyp & 0x7) != XTY_SD
		      || auxent->u.xcoff32.x_csect.x_smclas != XMC_XO)
		    continue;

		  x_scnlen = fetch_32 (aux + offsetof (union external_auxent,
						       u.xcoff32.x_csect.x_scnlen));
		}

	      /* Get header of containing section.  */
	      scnhdr = scnbuf + (n_scnum - 1) * scnhdr_size;
	      if (u64)
		{
		  scnptr = fetch_64 (scnhdr + offsetof (struct external_scnhdr,
							u.xcoff64.s_scnptr));
		  size = fetch_64 (scnhdr + offsetof (struct external_scnhdr,
						      u.xcoff64.s_size));
		}
	      else
		{
		  scnptr = fetch_32 (scnhdr + offsetof (struct external_scnhdr,
							u.xcoff32.s_scnptr));
		  size = fetch_32 (scnhdr + offsetof (struct external_scnhdr,
						      u.xcoff32.s_size));
		}
	      if (n_value + x_scnlen > size)
		break;

	      (*pfn) (data, ".go_export", scnptr + n_value, x_scnlen);
	      break;
	    }
	}
    }

  if (symtab != NULL)
    XDELETEVEC (symtab);
  if (strtab != NULL)
    XDELETEVEC (strtab);
  XDELETEVEC (scnbuf);

  return NULL;
}

/* Fetch the attributes for an simple_object_read.  */

static void *
simple_object_xcoff_fetch_attributes (simple_object_read *sobj,
				     const char **errmsg ATTRIBUTE_UNUSED,
				     int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_xcoff_read *ocr =
    (struct simple_object_xcoff_read *) sobj->data;
  struct simple_object_xcoff_attributes *ret;

  ret = XNEW (struct simple_object_xcoff_attributes);
  ret->magic = ocr->magic;
  ret->flags = ocr->flags;
  return ret;
}

/* Release the private data for an simple_object_read.  */

static void
simple_object_xcoff_release_read (void *data)
{
  XDELETE (data);
}

/* Compare two attributes structures.  */

static const char *
simple_object_xcoff_attributes_merge (void *todata, void *fromdata, int *err)
{
  struct simple_object_xcoff_attributes *to =
    (struct simple_object_xcoff_attributes *) todata;
  struct simple_object_xcoff_attributes *from =
    (struct simple_object_xcoff_attributes *) fromdata;

  if (to->magic != from->magic)
    {
      *err = 0;
      return "XCOFF object format mismatch";
    }
  return NULL;
}

/* Release the private data for an attributes structure.  */

static void
simple_object_xcoff_release_attributes (void *data)
{
  XDELETE (data);
}

/* Prepare to write out a file.  */

static void *
simple_object_xcoff_start_write (void *attributes_data,
				const char **errmsg ATTRIBUTE_UNUSED,
				int *err ATTRIBUTE_UNUSED)
{
  struct simple_object_xcoff_attributes *attrs =
    (struct simple_object_xcoff_attributes *) attributes_data;
  struct simple_object_xcoff_attributes *ret;

  /* We're just going to record the attributes, but we need to make a
     copy because the user may delete them.  */
  ret = XNEW (struct simple_object_xcoff_attributes);
  *ret = *attrs;
  return ret;
}

/* Write out a XCOFF filehdr.  */

static int
simple_object_xcoff_write_filehdr (simple_object_write *sobj, int descriptor,
				  unsigned int nscns, size_t symtab_offset,
				  unsigned int nsyms, const char **errmsg,
				  int *err)
{
  struct simple_object_xcoff_attributes *attrs =
    (struct simple_object_xcoff_attributes *) sobj->data;
  int u64 = attrs->magic == U64_TOCMAGIC;
  unsigned char hdrbuf[sizeof (struct external_filehdr)];
  unsigned char *hdr;
  void (*set_16) (unsigned char *, unsigned short);
  void (*set_32) (unsigned char *, unsigned int);
  void (*set_64) (unsigned char *, ulong_type);

  hdr = &hdrbuf[0];

  set_16 = simple_object_set_big_16;
  set_32 = simple_object_set_big_32;
  set_64 = simple_object_set_big_64;

  memset (hdr, 0, sizeof (struct external_filehdr));

  set_16 (hdr + offsetof (struct external_filehdr, f_magic), attrs->magic);
  set_16 (hdr + offsetof (struct external_filehdr, f_nscns), nscns);
  /* f_timdat left as zero.  */
  if (u64)
    {
      set_64 (hdr + offsetof (struct external_filehdr, u.xcoff64.f_symptr),
	      symtab_offset);
      set_32 (hdr + offsetof (struct external_filehdr, u.xcoff64.f_nsyms),
	      nsyms);
      /* f_opthdr left as zero.  */
      set_16 (hdr + offsetof (struct external_filehdr, u.xcoff64.f_flags),
	      attrs->flags);
    }
  else
    {
      set_32 (hdr + offsetof (struct external_filehdr, u.xcoff64.f_symptr),
	      symtab_offset);
      set_32 (hdr + offsetof (struct external_filehdr, u.xcoff64.f_nsyms),
	      nsyms);
      /* f_opthdr left as zero.  */
      set_16 (hdr + offsetof (struct external_filehdr, u.xcoff64.f_flags),
	      attrs->flags);
    }

  return simple_object_internal_write (descriptor, 0, hdrbuf,
				       sizeof (struct external_filehdr),
				       errmsg, err);
}

/* Write out a XCOFF section header.  */

static int
simple_object_xcoff_write_scnhdr (simple_object_write *sobj,
				  int descriptor,
				  const char *name, size_t *name_offset,
				  off_t scnhdr_offset, size_t scnsize,
				  off_t offset, unsigned int align,
				  const char **errmsg, int *err)
{
  struct simple_object_xcoff_read *ocr =
    (struct simple_object_xcoff_read *) sobj->data;
  int u64 = ocr->magic == U64_TOCMAGIC;
  void (*set_32) (unsigned char *, unsigned int);
  void (*set_64) (unsigned char *, unsigned int);
  unsigned char hdrbuf[sizeof (struct external_scnhdr)];
  unsigned char *hdr;
  size_t namelen;
  unsigned int flags;

  set_32 = simple_object_set_big_32;
  set_64 = simple_object_set_big_32;

  memset (hdrbuf, 0, sizeof hdrbuf);
  hdr = &hdrbuf[0];

  namelen = strlen (name);
  if (namelen <= SCNNMLEN)
    strncpy ((char *) hdr + offsetof (struct external_scnhdr, s_name),
	     name, SCNNMLEN);
  else
    {
      snprintf ((char *) hdr + offsetof (struct external_scnhdr, s_name),
		SCNNMLEN, "/%lu", (unsigned long) *name_offset);
      *name_offset += namelen + 1;
    }

  /* s_paddr left as zero.  */
  /* s_vaddr left as zero.  */
  if (u64)
    {
      set_64 (hdr + offsetof (struct external_scnhdr, u.xcoff64.s_size),
	      scnsize);
      set_64 (hdr + offsetof (struct external_scnhdr, u.xcoff64.s_scnptr),
	      offset);
    }
  else
    {
      set_32 (hdr + offsetof (struct external_scnhdr, u.xcoff32.s_size),
	      scnsize);
      set_32 (hdr + offsetof (struct external_scnhdr, u.xcoff32.s_scnptr),
	      offset);
    }
  /* s_relptr left as zero.  */
  /* s_lnnoptr left as zero.  */
  /* s_nreloc left as zero.  */
  /* s_nlnno left as zero.  */
  flags = STYP_DATA;
  if (align > 13)
    align = 13;
  if (u64)
    set_32 (hdr + offsetof (struct external_scnhdr, u.xcoff64.s_flags), flags);
  else
    set_32 (hdr + offsetof (struct external_scnhdr, u.xcoff32.s_flags), flags);

  return simple_object_internal_write (descriptor, scnhdr_offset, hdrbuf,
				       u64 ? SCNHSZ64 : SCNHSZ32,
				       errmsg, err);
}

/* Write out a complete XCOFF file.  */

static const char *
simple_object_xcoff_write_to_file (simple_object_write *sobj, int descriptor,
				  int *err)
{
  struct simple_object_xcoff_read *ocr =
    (struct simple_object_xcoff_read *) sobj->data;
  int u64 = ocr->magic == U64_TOCMAGIC;
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

  set_16 = simple_object_set_big_16;
  set_32 = simple_object_set_big_32;

  nscns = 0;
  for (section = sobj->sections; section != NULL; section = section->next)
    ++nscns;

  scnhdr_offset = sizeof (struct external_filehdr) - (u64 ? 4 : 0);
  offset = scnhdr_offset + nscns * (u64 ? SCNHSZ64 : SCNHSZ32);
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

      if (!simple_object_xcoff_write_scnhdr (sobj, descriptor, section->name,
					    &name_offset, scnhdr_offset,
					    scnsize, offset, section->align,
					    &errmsg, err))
	return errmsg;

      scnhdr_offset += u64 ? SCNHSZ64 : SCNHSZ32;
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
  offset += nsyms * SYMESZ;

  /* Write out file symbol.  */
  memset (&syms[0], 0, sizeof (syms));
  if (!u64)
    strcpy ((char *)&syms[0].sym.u.xcoff32.n.n_name[0], ".file");
  set_16 (&syms[0].sym.n_scnum[0], N_DEBUG);
  set_16 (&syms[0].sym.n_type[0], IMAGE_SYM_TYPE);
  syms[0].sym.n_sclass[0] = C_FILE;
  syms[0].sym.n_numaux[0] = 1;
  /* The name need not be nul-terminated if it fits into the x_fname field
     directly, but must be if it has to be placed into the string table.  */
  sflen = strlen (source_filename);
  if (sflen <= FILNMLEN)
    memcpy (&syms[1].aux.x_file.x_fname[0], source_filename, sflen);
  else
    {
      set_32 (&syms[1].aux.x_file._x.x_offset[0], name_offset);
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
  set_16 (&syms[0].sym.n_type[0], IMAGE_SYM_TYPE);
  syms[0].sym.n_sclass[0] = C_STAT;
  syms[0].sym.n_numaux[0] = 1;
  secnum = 1;

  for (section = sobj->sections; section != NULL; section = section->next)
    {
      size_t namelen;
      size_t scnsize;
      struct simple_object_write_section_buffer *buffer;

      namelen = strlen (section->name);
      set_16 (&syms[0].sym.n_scnum[0], secnum++);
      scnsize = 0;
      for (buffer = section->buffers; buffer != NULL; buffer = buffer->next)
	scnsize += buffer->size;
      set_32 (&syms[1].aux.x_scn.x_scnlen[0], scnsize);
      if (namelen > SCNNMLEN)
	{
	  set_32 (&syms[0].sym.u.xcoff32.n.n.n_zeroes[0], 0);
	  set_32 (&syms[0].sym.u.xcoff32.n.n.n_offset[0], name_offset);
	  if (!simple_object_internal_write (descriptor, offset + name_offset,
					     ((const unsigned char *)
					      section->name),
					     namelen + 1, &errmsg, err))
	    return errmsg;
	  name_offset += namelen + 1;
	}
      else
	{
	  memcpy (&syms[0].sym.u.xcoff32.n.n_name[0], section->name,
		  strlen (section->name));
	  memset (&syms[0].sym.u.xcoff32.n.n_name[strlen (section->name)], 0,
		  N_SYMNMLEN - strlen (section->name));
	}

      if (!simple_object_internal_write (descriptor, secsym_offset,
					 (const unsigned char *) &syms[0],
					 sizeof (syms), &errmsg, err))
	return errmsg;
      secsym_offset += sizeof (syms);
    }

  if (!simple_object_xcoff_write_filehdr (sobj, descriptor, nscns,
					 symtab_offset, nsyms, &errmsg, err))
    return errmsg;

  return NULL;
}

/* Release the private data for an simple_object_write structure.  */

static void
simple_object_xcoff_release_write (void *data)
{
  XDELETE (data);
}

/* The XCOFF functions.  */

const struct simple_object_functions simple_object_xcoff_functions =
{
  simple_object_xcoff_match,
  simple_object_xcoff_find_sections,
  simple_object_xcoff_fetch_attributes,
  simple_object_xcoff_release_read,
  simple_object_xcoff_attributes_merge,
  simple_object_xcoff_release_attributes,
  simple_object_xcoff_start_write,
  simple_object_xcoff_write_to_file,
  simple_object_xcoff_release_write,
  NULL
};
