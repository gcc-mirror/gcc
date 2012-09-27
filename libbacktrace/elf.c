/* elf.c -- Get debug data from an ELF file for backtraces.
   Copyright (C) 2012 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Google.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer. 

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.  
    
    (3) The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.  */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "backtrace.h"
#include "internal.h"

/* The configure script must tell us whether we are 32-bit or 64-bit
   ELF.  We could make this code test and support either possibility,
   but there is no point.  This code only works for the currently
   running executable, which means that we know the ELF mode at
   configure mode.  */

#if BACKTRACE_ELF_SIZE != 32 && BACKTRACE_ELF_SIZE != 64
#error "Unknown BACKTRACE_ELF_SIZE"
#endif

/* Basic types.  */

typedef uint16_t Elf_Half;
typedef uint32_t Elf_Word;
typedef int32_t  Elf_Sword;

#if BACKTRACE_ELF_SIZE == 32

typedef uint32_t Elf_Addr;
typedef uint32_t Elf_Off;

typedef uint32_t Elf_WXword;

#else

typedef uint64_t Elf_Addr;
typedef uint64_t Elf_Off;
typedef uint64_t Elf_Xword;
typedef int64_t  Elf_Sxword;

typedef uint64_t Elf_WXword;

#endif

/* Data structures and associated constants.  */

#define EI_NIDENT 16

typedef struct {
  unsigned char	e_ident[EI_NIDENT];	/* ELF "magic number" */
  Elf_Half	e_type;			/* Identifies object file type */
  Elf_Half	e_machine;		/* Specifies required architecture */
  Elf_Word	e_version;		/* Identifies object file version */
  Elf_Addr	e_entry;		/* Entry point virtual address */
  Elf_Off	e_phoff;		/* Program header table file offset */
  Elf_Off	e_shoff;		/* Section header table file offset */
  Elf_Word	e_flags;		/* Processor-specific flags */
  Elf_Half	e_ehsize;		/* ELF header size in bytes */
  Elf_Half	e_phentsize;		/* Program header table entry size */
  Elf_Half	e_phnum;		/* Program header table entry count */
  Elf_Half	e_shentsize;		/* Section header table entry size */
  Elf_Half	e_shnum;		/* Section header table entry count */
  Elf_Half	e_shstrndx;		/* Section header string table index */
} Elf_Ehdr;

#define EI_MAG0 0
#define EI_MAG1 1
#define EI_MAG2 2
#define EI_MAG3 3
#define EI_CLASS 4
#define EI_DATA 5
#define EI_VERSION 6

#define ELFMAG0 0x7f
#define ELFMAG1 'E'
#define ELFMAG2 'L'
#define ELFMAG3 'F'

#define ELFCLASS32 1
#define ELFCLASS64 2

#define ELFDATA2LSB 1
#define ELFDATA2MSB 2

#define EV_CURRENT 1

typedef struct {
  Elf_Word	sh_name;		/* Section name, index in string tbl */
  Elf_Word	sh_type;		/* Type of section */
  Elf_WXword	sh_flags;		/* Miscellaneous section attributes */
  Elf_Addr	sh_addr;		/* Section virtual addr at execution */
  Elf_Off	sh_offset;		/* Section file offset */
  Elf_WXword	sh_size;		/* Size of section in bytes */
  Elf_Word	sh_link;		/* Index of another section */
  Elf_Word	sh_info;		/* Additional section information */
  Elf_WXword	sh_addralign;		/* Section alignment */
  Elf_WXword	sh_entsize;		/* Entry size if section holds table */
} Elf_Shdr;

#define SHN_LORESERVE	0xFF00		/* Begin range of reserved indices */
#define SHN_XINDEX	0xFFFF		/* Section index is held elsewhere */

#define SHT_SYMTAB 2
#define SHT_STRTAB 3
#define SHT_DYNSYM 11

#if BACKTRACE_ELF_SIZE == 32

typedef struct
{
  Elf_Word	st_name;		/* Symbol name, index in string tbl */
  Elf_Addr	st_value;		/* Symbol value */
  Elf_Word	st_size;		/* Symbol size */
  unsigned char	st_info;		/* Symbol binding and type */
  unsigned char	st_other;		/* Visibility and other data */
  Elf_Half	st_shndx;		/* Symbol section index */
} Elf_Sym;

#else /* BACKTRACE_ELF_SIZE != 32 */

typedef struct
{
  Elf_Word	st_name;		/* Symbol name, index in string tbl */
  unsigned char	st_info;		/* Symbol binding and type */
  unsigned char	st_other;		/* Visibility and other data */
  Elf_Half	st_shndx;		/* Symbol section index */
  Elf_Addr	st_value;		/* Symbol value */
  Elf_Xword	st_size;		/* Symbol size */
} Elf_Sym;

#endif /* BACKTRACE_ELF_SIZE != 32 */

#define STT_FUNC 2

/* An index of ELF sections we care about.  */

enum debug_section
{
  DEBUG_INFO,
  DEBUG_LINE,
  DEBUG_ABBREV,
  DEBUG_RANGES,
  DEBUG_STR,
  DEBUG_MAX
};

/* Names of sections, indexed by enum elf_section.  */

static const char * const debug_section_names[DEBUG_MAX] =
{
  ".debug_info",
  ".debug_line",
  ".debug_abbrev",
  ".debug_ranges",
  ".debug_str"
};

/* Information we gather for the sections we care about.  */

struct debug_section_info
{
  /* Section file offset.  */
  off_t offset;
  /* Section size.  */
  size_t size;
  /* Section contents, after read from file.  */
  const unsigned char *data;
};

/* Information we keep for an ELF symbol.  */

struct elf_symbol
{
  /* The name of the symbol.  */
  const char *name;
  /* The address of the symbol.  */
  uintptr_t address;
  /* The size of the symbol.  */
  size_t size;
};

/* Information to pass to elf_syminfo.  */

struct elf_syminfo_data
{
  /* The ELF symbols, sorted by address.  */
  struct elf_symbol *symbols;
  /* The number of symbols.  */
  size_t count;
};

/* A dummy callback function used when we can't find any debug info.  */

static int
elf_nodebug (struct backtrace_state *state ATTRIBUTE_UNUSED,
	     uintptr_t pc ATTRIBUTE_UNUSED,
	     backtrace_full_callback callback ATTRIBUTE_UNUSED,
	     backtrace_error_callback error_callback, void *data)
{
  error_callback (data, "no debug info in ELF executable", -1);
  return 0;
}

/* A dummy callback function used when we can't find a symbol
   table.  */

static void
elf_nosyms (struct backtrace_state *state ATTRIBUTE_UNUSED,
	    uintptr_t pc ATTRIBUTE_UNUSED,
	    backtrace_syminfo_callback callback ATTRIBUTE_UNUSED,
	    backtrace_error_callback error_callback, void *data)
{
  error_callback (data, "no symbol table in ELF executable", -1);
}

/* Compare struct elf_symbol for qsort.  */

static int
elf_symbol_compare (const void *v1, const void *v2)
{
  const struct elf_symbol *e1 = (const struct elf_symbol *) v1;
  const struct elf_symbol *e2 = (const struct elf_symbol *) v2;

  if (e1->address < e2->address)
    return -1;
  else if (e1->address > e2->address)
    return 1;
  else
    return 0;
}

/* Compare a PC against an elf_symbol for bsearch.  We allocate one
   extra entry in the array so that this can look safely at the next
   entry.  */

static int
elf_symbol_search (const void *vkey, const void *ventry)
{
  const uintptr_t *key = (const uintptr_t *) vkey;
  const struct elf_symbol *entry = (const struct elf_symbol *) ventry;
  uintptr_t pc;

  pc = *key;
  if (pc < entry->address)
    return -1;
  else if (pc >= entry->address + entry->size)
    return 1;
  else
    return 0;
}

/* Initialize the symbol table info for elf_syminfo.  */

static int
elf_initialize_syminfo (struct backtrace_state *state,
			const unsigned char *symtab_data, size_t symtab_size,
			const unsigned char *strtab, size_t strtab_size,
			backtrace_error_callback error_callback,
			void *data, struct elf_syminfo_data *sdata)
{
  size_t sym_count;
  const Elf_Sym *sym;
  size_t elf_symbol_count;
  size_t elf_symbol_size;
  struct elf_symbol *elf_symbols;
  size_t i;
  unsigned int j;

  sym_count = symtab_size / sizeof (Elf_Sym);

  /* We only care about function symbols.  Count them.  */
  sym = (const Elf_Sym *) symtab_data;
  elf_symbol_count = 0;
  for (i = 0; i < sym_count; ++i, ++sym)
    {
      if ((sym->st_info & 0xf) == STT_FUNC)
	++elf_symbol_count;
    }

  elf_symbol_size = elf_symbol_count * sizeof (struct elf_symbol);
  elf_symbols = ((struct elf_symbol *)
		 backtrace_alloc (state, elf_symbol_size, error_callback,
				  data));
  if (elf_symbols == NULL)
    return 0;

  sym = (const Elf_Sym *) symtab_data;
  j = 0;
  for (i = 0; i < sym_count; ++i, ++sym)
    {
      if ((sym->st_info & 0xf) != STT_FUNC)
	continue;
      if (sym->st_name >= strtab_size)
	{
	  error_callback (data, "symbol string index out of range", 0);
	  backtrace_free (state, elf_symbols, elf_symbol_size, error_callback,
			  data);
	  return 0;
	}
      elf_symbols[j].name = (const char *) strtab + sym->st_name;
      elf_symbols[j].address = sym->st_value;
      elf_symbols[j].size = sym->st_size;
      ++j;
    }

  qsort (elf_symbols, elf_symbol_count, sizeof (struct elf_symbol),
	 elf_symbol_compare);

  sdata->symbols = elf_symbols;
  sdata->count = elf_symbol_count;

  return 1;
}

/* Return the symbol name and value for a PC.  */

static void
elf_syminfo (struct backtrace_state *state, uintptr_t pc,
	     backtrace_syminfo_callback callback,
	     backtrace_error_callback error_callback ATTRIBUTE_UNUSED,
	     void *data)
{
  struct elf_syminfo_data *edata;
  struct elf_symbol *sym;

  edata = (struct elf_syminfo_data *) state->syminfo_data;
  sym = ((struct elf_symbol *)
	 bsearch (&pc, edata->symbols, edata->count,
		  sizeof (struct elf_symbol), elf_symbol_search));
  if (sym == NULL)
    callback (data, pc, NULL, 0);
  else
    callback (data, pc, sym->name, sym->address);
}

/* Initialize the backtrace data we need from an ELF executable.  At
   the ELF level, all we need to do is find the debug info
   sections.  */

int
backtrace_initialize (struct backtrace_state *state, int descriptor,
		      backtrace_error_callback error_callback,
		      void *data, fileline *fileline_fn)
{
  struct backtrace_view ehdr_view;
  Elf_Ehdr ehdr;
  off_t shoff;
  unsigned int shnum;
  unsigned int shstrndx;
  struct backtrace_view shdrs_view;
  int shdrs_view_valid;
  const Elf_Shdr *shdrs;
  const Elf_Shdr *shstrhdr;
  size_t shstr_size;
  off_t shstr_off;
  struct backtrace_view names_view;
  int names_view_valid;
  const char *names;
  unsigned int symtab_shndx;
  unsigned int dynsym_shndx;
  unsigned int i;
  struct debug_section_info sections[DEBUG_MAX];
  struct backtrace_view symtab_view;
  int symtab_view_valid;
  struct backtrace_view strtab_view;
  int strtab_view_valid;
  off_t min_offset;
  off_t max_offset;
  struct backtrace_view debug_view;
  int debug_view_valid;

  shdrs_view_valid = 0;
  names_view_valid = 0;
  symtab_view_valid = 0;
  strtab_view_valid = 0;
  debug_view_valid = 0;

  if (!backtrace_get_view (state, descriptor, 0, sizeof ehdr, error_callback,
			   data, &ehdr_view))
    goto fail;

  memcpy (&ehdr, ehdr_view.data, sizeof ehdr);

  backtrace_release_view (state, &ehdr_view, error_callback, data);

  if (ehdr.e_ident[EI_MAG0] != ELFMAG0
      || ehdr.e_ident[EI_MAG1] != ELFMAG1
      || ehdr.e_ident[EI_MAG2] != ELFMAG2
      || ehdr.e_ident[EI_MAG3] != ELFMAG3)
    {
      error_callback (data, "executable file is not ELF", 0);
      goto fail;
    }
  if (ehdr.e_ident[EI_VERSION] != EV_CURRENT)
    {
      error_callback (data, "executable file is unrecognized ELF version", 0);
      goto fail;
    }

#if BACKTRACE_ELF_SIZE == 32
#define BACKTRACE_ELFCLASS ELFCLASS32
#else
#define BACKTRACE_ELFCLASS ELFCLASS64
#endif

  if (ehdr.e_ident[EI_CLASS] != BACKTRACE_ELFCLASS)
    {
      error_callback (data, "executable file is unexpected ELF class", 0);
      goto fail;
    }

  if (ehdr.e_ident[EI_DATA] != ELFDATA2LSB
      && ehdr.e_ident[EI_DATA] != ELFDATA2MSB)
    {
      error_callback (data, "executable file has unknown endianness", 0);
      goto fail;
    }

  shoff = ehdr.e_shoff;
  shnum = ehdr.e_shnum;
  shstrndx = ehdr.e_shstrndx;

  if ((shnum == 0 || shstrndx == SHN_XINDEX)
      && shoff != 0)
    {
      struct backtrace_view shdr_view;
      const Elf_Shdr *shdr;

      if (!backtrace_get_view (state, descriptor, shoff, sizeof shdr,
			       error_callback, data, &shdr_view))
	goto fail;

      shdr = (const Elf_Shdr *) shdr_view.data;

      if (shnum == 0)
	shnum = shdr->sh_size;

      if (shstrndx == SHN_XINDEX)
	{
	  shstrndx = shdr->sh_link;

	  /* Versions of the GNU binutils between 2.12 and 2.18 did
	     not handle objects with more than SHN_LORESERVE sections
	     correctly.  All large section indexes were offset by
	     0x100.  There is more information at
	     http://sourceware.org/bugzilla/show_bug.cgi?id-5900 .
	     Fortunately these object files are easy to detect, as the
	     GNU binutils always put the section header string table
	     near the end of the list of sections.  Thus if the
	     section header string table index is larger than the
	     number of sections, then we know we have to subtract
	     0x100 to get the real section index.  */
	  if (shstrndx >= shnum && shstrndx >= SHN_LORESERVE + 0x100)
	    shstrndx -= 0x100;
	}

      backtrace_release_view (state, &shdr_view, error_callback, data);
    }

  /* To translate PC to file/line when using DWARF, we need to find
     the .debug_info and .debug_line sections.  */

  /* Read the section headers, skipping the first one.  */

  if (!backtrace_get_view (state, descriptor, shoff + sizeof (Elf_Shdr),
			   (shnum - 1) * sizeof (Elf_Shdr),
			   error_callback, data, &shdrs_view))
    goto fail;
  shdrs_view_valid = 1;
  shdrs = (const Elf_Shdr *) shdrs_view.data;

  /* Read the section names.  */

  shstrhdr = &shdrs[shstrndx - 1];
  shstr_size = shstrhdr->sh_size;
  shstr_off = shstrhdr->sh_offset;

  if (!backtrace_get_view (state, descriptor, shstr_off, shstr_size,
			   error_callback, data, &names_view))
    goto fail;
  names_view_valid = 1;
  names = (const char *) names_view.data;

  symtab_shndx = 0;
  dynsym_shndx = 0;

  memset (sections, 0, sizeof sections);
  for (i = 1; i < shnum; ++i)
    {
      const Elf_Shdr *shdr;
      unsigned int sh_name;
      const char *name;
      int j;

      shdr = &shdrs[i - 1];

      if (shdr->sh_type == SHT_SYMTAB)
	symtab_shndx = i;
      else if (shdr->sh_type == SHT_DYNSYM)
	dynsym_shndx = i;

      sh_name = shdr->sh_name;
      if (sh_name >= shstr_size)
	{
	  error_callback (data, "ELF section name out of range", 0);
	  goto fail;
	}

      name = names + sh_name;

      for (j = 0; j < (int) DEBUG_MAX; ++j)
	{
	  if (strcmp (name, debug_section_names[j]) == 0)
	    {
	      sections[j].offset = shdr->sh_offset;
	      sections[j].size = shdr->sh_size;
	      break;
	    }
	}
    }

  if (symtab_shndx == 0)
    symtab_shndx = dynsym_shndx;
  if (symtab_shndx == 0)
    {
      state->syminfo_fn = elf_nosyms;
      state->syminfo_data = NULL;
    }
  else
    {
      const Elf_Shdr *symtab_shdr;
      unsigned int strtab_shndx;
      const Elf_Shdr *strtab_shdr;
      struct elf_syminfo_data *sdata;

      symtab_shdr = &shdrs[symtab_shndx - 1];
      strtab_shndx = symtab_shdr->sh_link;
      if (strtab_shndx >= shnum)
	{
	  error_callback (data,
			  "ELF symbol table strtab link out of range", 0);
	  goto fail;
	}
      strtab_shdr = &shdrs[strtab_shndx - 1];

      if (!backtrace_get_view (state, descriptor, symtab_shdr->sh_offset,
			       symtab_shdr->sh_size, error_callback, data,
			       &symtab_view))
	goto fail;
      symtab_view_valid = 1;

      if (!backtrace_get_view (state, descriptor, strtab_shdr->sh_offset,
			       strtab_shdr->sh_size, error_callback, data,
			       &strtab_view))
	goto fail;
      strtab_view_valid = 1;

      sdata = ((struct elf_syminfo_data *)
	       backtrace_alloc (state, sizeof *sdata, error_callback, data));
      if (sdata == NULL)
	goto fail;

      if (!elf_initialize_syminfo (state,
				   symtab_view.data, symtab_shdr->sh_size,
				   strtab_view.data, strtab_shdr->sh_size,
				   error_callback, data, sdata))
	{
	  backtrace_free (state, sdata, sizeof *sdata, error_callback, data);
	  goto fail;
	}

      /* We no longer need the symbol table, but we hold on to the
	 string table permanently.  */
      backtrace_release_view (state, &symtab_view, error_callback, data);

      state->syminfo_fn = elf_syminfo;
      state->syminfo_data = sdata;
    }

  /* FIXME: Need to handle compressed debug sections.  */

  backtrace_release_view (state, &shdrs_view, error_callback, data);
  shdrs_view_valid = 0;
  backtrace_release_view (state, &names_view, error_callback, data);
  names_view_valid = 0;

  /* Read all the debug sections in a single view, since they are
     probably adjacent in the file.  We never release this view.  */

  min_offset = 0;
  max_offset = 0;
  for (i = 0; i < (int) DEBUG_MAX; ++i)
    {
      off_t end;

      if (min_offset == 0 || sections[i].offset < min_offset)
	min_offset = sections[i].offset;
      end = sections[i].offset + sections[i].size;
      if (end > max_offset)
	max_offset = end;
    }
  if (min_offset == 0 || max_offset == 0)
    {
      if (!backtrace_close (descriptor, error_callback, data))
	goto fail;
      *fileline_fn = elf_nodebug;
      state->fileline_data = NULL;
      return 1;
    }

  if (!backtrace_get_view (state, descriptor, min_offset,
			   max_offset - min_offset,
			   error_callback, data, &debug_view))
    goto fail;
  debug_view_valid = 1;

  /* We've read all we need from the executable.  */
  if (!backtrace_close (descriptor, error_callback, data))
    goto fail;
  descriptor = -1;

  for (i = 0; i < (int) DEBUG_MAX; ++i)
    sections[i].data = ((const unsigned char *) debug_view.data
			+ (sections[i].offset - min_offset));

  if (!backtrace_dwarf_initialize (state,
				   sections[DEBUG_INFO].data,
				   sections[DEBUG_INFO].size,
				   sections[DEBUG_LINE].data,
				   sections[DEBUG_LINE].size,
				   sections[DEBUG_ABBREV].data,
				   sections[DEBUG_ABBREV].size,
				   sections[DEBUG_RANGES].data,
				   sections[DEBUG_RANGES].size,
				   sections[DEBUG_STR].data,
				   sections[DEBUG_STR].size,
				   ehdr.e_ident[EI_DATA] == ELFDATA2MSB,
				   error_callback, data, fileline_fn))
    goto fail;

  return 1;

 fail:
  if (shdrs_view_valid)
    backtrace_release_view (state, &shdrs_view, error_callback, data);
  if (names_view_valid)
    backtrace_release_view (state, &names_view, error_callback, data);
  if (symtab_view_valid)
    backtrace_release_view (state, &symtab_view, error_callback, data);
  if (strtab_view_valid)
    backtrace_release_view (state, &strtab_view, error_callback, data);
  if (debug_view_valid)
    backtrace_release_view (state, &debug_view, error_callback, data);
  if (descriptor != -1)
    backtrace_close (descriptor, error_callback, data);
  return 0;
}
