/* Copyright (C) 2001-2022 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Locate the FDE entry for a given address, using PT_GNU_EH_FRAME ELF
   segment and dl_iterate_phdr to avoid register/deregister calls at
   DSO load/unload.  */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include "tconfig.h"
#include "tsystem.h"
#if !defined(inhibit_libc) && !defined(__OpenBSD__)
#include <elf.h>		/* Get DT_CONFIG.  */
#endif
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "dwarf2.h"
#include "unwind.h"
#define NO_BASE_OF_ENCODED_VALUE
#include "unwind-pe.h"
#include "unwind-dw2-fde.h"
#include "unwind-compat.h"
#include "gthr.h"

#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
    && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2) \
	|| (__GLIBC__ == 2 && __GLIBC_MINOR__ == 2 && defined(DT_CONFIG)))
# define USE_PT_GNU_EH_FRAME
#endif

#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
    && defined(__BIONIC__)
# define USE_PT_GNU_EH_FRAME
#endif

#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
    && defined(TARGET_DL_ITERATE_PHDR) \
    && defined(__linux__)
# define USE_PT_GNU_EH_FRAME
#endif

#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
    && defined(TARGET_DL_ITERATE_PHDR) \
    && (defined(__DragonFly__) || defined(__FreeBSD__))
# define ElfW __ElfN
# define USE_PT_GNU_EH_FRAME
#endif

#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
    && defined(TARGET_DL_ITERATE_PHDR) \
    && (defined(__OpenBSD__) || defined(__NetBSD__))
# define ElfW(type) Elf_##type
# define USE_PT_GNU_EH_FRAME
#endif

#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
    && defined(TARGET_DL_ITERATE_PHDR) \
    && defined(__sun__) && defined(__svr4__)
# define USE_PT_GNU_EH_FRAME
#endif

#if defined(USE_PT_GNU_EH_FRAME)

#include <link.h>

#ifndef __RELOC_POINTER
# define __RELOC_POINTER(ptr, base) ((ptr) + (base))
#endif

static const fde * _Unwind_Find_registered_FDE (void *pc, struct dwarf_eh_bases *bases);

#define _Unwind_Find_FDE _Unwind_Find_registered_FDE
#include "unwind-dw2-fde.c"
#undef _Unwind_Find_FDE

#ifndef PT_GNU_EH_FRAME
#define PT_GNU_EH_FRAME (PT_LOOS + 0x474e550)
#endif

#ifdef CRT_GET_RFIB_DATA
#define NEED_DBASE_MEMBER 1
#else
#define NEED_DBASE_MEMBER 0
#endif

struct unw_eh_callback_data
{
  _Unwind_Ptr pc;
#if NEED_DBASE_MEMBER
  void *dbase;
#endif
  const struct unw_eh_frame_hdr *hdr;
  int check_cache;
};

/* Returns DATA->dbase if available, else NULL.  */
static inline _Unwind_Ptr
unw_eh_callback_data_dbase (const struct unw_eh_callback_data *data
			    __attribute__ ((unused)))
{
#if NEED_DBASE_MEMBER
  return (_Unwind_Ptr) data->dbase;
#else
  return 0;
#endif
}

struct unw_eh_frame_hdr
{
  unsigned char version;
  unsigned char eh_frame_ptr_enc;
  unsigned char fde_count_enc;
  unsigned char table_enc;
};

#define FRAME_HDR_CACHE_SIZE 8

static struct frame_hdr_cache_element
{
  _Unwind_Ptr pc_low;
  _Unwind_Ptr pc_high;
#if defined __FRV_FDPIC__ || defined __BFIN_FDPIC__
  struct elf32_fdpic_loadaddr load_base;
#else
  _Unwind_Ptr load_base;
#endif
  const ElfW(Phdr) *p_eh_frame_hdr;
  const ElfW(Phdr) *p_dynamic;
  struct frame_hdr_cache_element *link;
} frame_hdr_cache[FRAME_HDR_CACHE_SIZE];

static struct frame_hdr_cache_element *frame_hdr_cache_head;

/* Like base_of_encoded_value, but take the base from a struct
   unw_eh_callback_data instead of an _Unwind_Context.  */

static inline _Unwind_Ptr
base_from_cb_data (unsigned char encoding __attribute__ ((unused)),
		   _Unwind_Ptr dbase __attribute__ ((unused)))
{
#if NEED_DBASE_MEMBER
  if (encoding == DW_EH_PE_omit)
    return 0;

  switch (encoding & 0x70)
    {
    case DW_EH_PE_absptr:
    case DW_EH_PE_pcrel:
    case DW_EH_PE_aligned:
      return 0;

    case DW_EH_PE_textrel:
      return 0;
    case DW_EH_PE_datarel:
      return dbase;
    default:
      gcc_unreachable ();
    }
#else /* !NEED_DBASE_MEMBER */
  return 0;
#endif
}

static int
_Unwind_IteratePhdrCallback (struct dl_phdr_info *info, size_t size, void *ptr)
{
  struct unw_eh_callback_data *data = (struct unw_eh_callback_data *) ptr;
  const ElfW(Phdr) *phdr, *p_eh_frame_hdr, *p_dynamic;
  long n, match;
#if defined __FRV_FDPIC__ || defined __BFIN_FDPIC__
  struct elf32_fdpic_loadaddr load_base;
#else
  _Unwind_Ptr load_base;
#endif
  _Unwind_Ptr pc_low = 0, pc_high = 0;

  struct ext_dl_phdr_info
    {
      ElfW(Addr) dlpi_addr;
      const char *dlpi_name;
      const ElfW(Phdr) *dlpi_phdr;
      ElfW(Half) dlpi_phnum;
      unsigned long long int dlpi_adds;
      unsigned long long int dlpi_subs;
    };

  match = 0;
  phdr = info->dlpi_phdr;
  load_base = info->dlpi_addr;
  p_eh_frame_hdr = NULL;
  p_dynamic = NULL;

  struct frame_hdr_cache_element *prev_cache_entry = NULL,
    *last_cache_entry = NULL;

  if (data->check_cache && size >= sizeof (struct ext_dl_phdr_info))
    {
      static unsigned long long adds = -1ULL, subs;
      struct ext_dl_phdr_info *einfo = (struct ext_dl_phdr_info *) info;

      /* We use a least recently used cache replacement policy.  Also,
	 the most recently used cache entries are placed at the head
	 of the search chain.  */

      if (einfo->dlpi_adds == adds && einfo->dlpi_subs == subs)
	{
	  /* Find data->pc in shared library cache.
	     Set load_base, p_eh_frame_hdr and p_dynamic
	     plus match from the cache and goto
	     "Read .eh_frame_hdr header." below.  */

	  struct frame_hdr_cache_element *cache_entry;

	  for (cache_entry = frame_hdr_cache_head;
	       cache_entry;
	       cache_entry = cache_entry->link)
	    {
	      if (data->pc >= cache_entry->pc_low
		  && data->pc < cache_entry->pc_high)
		{
		  load_base = cache_entry->load_base;
		  p_eh_frame_hdr = cache_entry->p_eh_frame_hdr;
		  p_dynamic = cache_entry->p_dynamic;

		  /* And move the entry we're using to the head.  */
		  if (cache_entry != frame_hdr_cache_head)
		    {
		      prev_cache_entry->link = cache_entry->link;
		      cache_entry->link = frame_hdr_cache_head;
		      frame_hdr_cache_head = cache_entry;
		    }
		  goto found;
		}

	      last_cache_entry = cache_entry;
	      /* Exit early if we found an unused entry.  */
	      if ((cache_entry->pc_low | cache_entry->pc_high) == 0)
		break;
	      if (cache_entry->link != NULL)
		prev_cache_entry = cache_entry;
	    }
	}
      else
	{
	  adds = einfo->dlpi_adds;
	  subs = einfo->dlpi_subs;
	  /* Initialize the cache.  Create a chain of cache entries,
	     with the final one terminated by a NULL link.  */
	  int i;
	  for (i = 0; i < FRAME_HDR_CACHE_SIZE; i++)
	    {
	      frame_hdr_cache[i].pc_low = 0;
	      frame_hdr_cache[i].pc_high = 0;
	      frame_hdr_cache[i].link = &frame_hdr_cache[i+1];
	    }
	  frame_hdr_cache[i-1].link = NULL;
	  frame_hdr_cache_head = &frame_hdr_cache[0];
	  data->check_cache = 0;
	}
    }

  /* Make sure struct dl_phdr_info is at least as big as we need.  */
  if (size < offsetof (struct dl_phdr_info, dlpi_phnum)
	     + sizeof (info->dlpi_phnum))
    return -1;

  /* See if PC falls into one of the loaded segments.  Find the eh_frame
     segment at the same time.  */
  for (n = info->dlpi_phnum; --n >= 0; phdr++)
    {
      if (phdr->p_type == PT_LOAD)
	{
	  _Unwind_Ptr vaddr = (_Unwind_Ptr)
	    __RELOC_POINTER (phdr->p_vaddr, load_base);
	  if (data->pc >= vaddr && data->pc < vaddr + phdr->p_memsz)
	    {
	      match = 1;
	      pc_low = vaddr;
	      pc_high =  vaddr + phdr->p_memsz;
	    }
	}
      else if (phdr->p_type == PT_GNU_EH_FRAME)
	p_eh_frame_hdr = phdr;
#ifdef PT_SUNW_UNWIND
      /* Sun ld emits PT_SUNW_UNWIND .eh_frame_hdr sections instead of
	 PT_SUNW_EH_FRAME/PT_GNU_EH_FRAME, so accept them as well.  */
      else if (phdr->p_type == PT_SUNW_UNWIND)
	p_eh_frame_hdr = phdr;
#endif
      else if (phdr->p_type == PT_DYNAMIC)
	p_dynamic = phdr;
    }

  if (!match)
    return 0;

  if (size >= sizeof (struct ext_dl_phdr_info))
    {
      /* Move the cache entry we're about to overwrite to the head of
	 the list.  If either last_cache_entry or prev_cache_entry are
	 NULL, that cache entry is already at the head.  */
      if (last_cache_entry != NULL && prev_cache_entry != NULL)
	{
	  prev_cache_entry->link = last_cache_entry->link;
	  last_cache_entry->link = frame_hdr_cache_head;
	  frame_hdr_cache_head = last_cache_entry;
	}

      frame_hdr_cache_head->load_base = load_base;
      frame_hdr_cache_head->p_eh_frame_hdr = p_eh_frame_hdr;
      frame_hdr_cache_head->p_dynamic = p_dynamic;
      frame_hdr_cache_head->pc_low = pc_low;
      frame_hdr_cache_head->pc_high = pc_high;
    }

 found:

  if (!p_eh_frame_hdr)
    return 0;

  /* Read .eh_frame_hdr header.  */
  data->hdr = (const struct unw_eh_frame_hdr *)
    __RELOC_POINTER (p_eh_frame_hdr->p_vaddr, load_base);

#ifdef CRT_GET_RFIB_DATA
# if defined __i386__ || defined __nios2__
  data->dbase = NULL;
  if (p_dynamic)
    {
      /* For dynamically linked executables and shared libraries,
	 DT_PLTGOT is the gp value for that object.  */
      ElfW(Dyn) *dyn = (ElfW(Dyn) *)
	__RELOC_POINTER (p_dynamic->p_vaddr, load_base);
      for (; dyn->d_tag != DT_NULL ; dyn++)
	if (dyn->d_tag == DT_PLTGOT)
	  {
	    data->dbase = (void *) dyn->d_un.d_ptr;
#if defined __linux__
	    /* On IA-32 Linux, _DYNAMIC is writable and GLIBC has
	       relocated it.  */
#elif defined __sun__ && defined __svr4__
	    /* On Solaris 2/x86, we need to do this ourselves.  */
	    data->dbase += load_base;
#endif
	    break;
	  }
    }
# elif (defined __FRV_FDPIC__ || defined __BFIN_FDPIC__) && defined __linux__
  data->dbase = load_base.got_value;
# else
#  error What is DW_EH_PE_datarel base on this platform?
# endif
#endif

  return 1;
}

/* Find the FDE for the program counter PC, in a previously located
   PT_GNU_EH_FRAME data region.  *BASES is updated if an FDE to return is
   found.  */

static const fde *
find_fde_tail (_Unwind_Ptr pc,
	       const struct unw_eh_frame_hdr *hdr,
	       _Unwind_Ptr dbase,
	       struct dwarf_eh_bases *bases)
{
  const unsigned char *p = (const unsigned char *) (hdr + 1);
  _Unwind_Ptr eh_frame;
  struct object ob;

  if (hdr->version != 1)
    return NULL;

  if (__builtin_expect (hdr->eh_frame_ptr_enc == (DW_EH_PE_sdata4
						  | DW_EH_PE_pcrel), 1))
    {
      /* Specialized version of read_encoded_value_with_base, based on what
	 BFD ld generates.  */
      signed value __attribute__ ((mode (SI)));
      memcpy (&value, p, sizeof (value));
      p += sizeof (value);
      dbase = value;		/* No adjustment because pcrel has base 0.  */
    }
  else
    p = read_encoded_value_with_base (hdr->eh_frame_ptr_enc,
				      base_from_cb_data (hdr->eh_frame_ptr_enc,
							 dbase),
				      p, &eh_frame);

  /* We require here specific table encoding to speed things up.
     Also, DW_EH_PE_datarel here means using PT_GNU_EH_FRAME start
     as base, not the processor specific DW_EH_PE_datarel.  */
  if (hdr->fde_count_enc != DW_EH_PE_omit
      && hdr->table_enc == (DW_EH_PE_datarel | DW_EH_PE_sdata4))
    {
      _Unwind_Ptr fde_count;

      if (__builtin_expect (hdr->fde_count_enc == DW_EH_PE_udata4, 1))
	{
	  /* Specialized version of read_encoded_value_with_base, based on
	     what BFD ld generates.  */
	  unsigned value __attribute__ ((mode (SI)));
	  memcpy (&value, p, sizeof (value));
	  p += sizeof (value);
	  fde_count = value;
	}
      else
	p = read_encoded_value_with_base (hdr->fde_count_enc,
					  base_from_cb_data (hdr->fde_count_enc,
							     dbase),
					  p, &fde_count);
      /* Shouldn't happen.  */
      if (fde_count == 0)
	return NULL;
      if ((((_Unwind_Ptr) p) & 3) == 0)
	{
	  struct fde_table {
	    signed initial_loc __attribute__ ((mode (SI)));
	    signed fde __attribute__ ((mode (SI)));
	  };
	  const struct fde_table *table = (const struct fde_table *) p;
	  size_t lo, hi, mid;
	  _Unwind_Ptr data_base = (_Unwind_Ptr) hdr;
	  fde *f;
	  unsigned int f_enc, f_enc_size;
	  _Unwind_Ptr range;

	  mid = fde_count - 1;
	  if (pc < table[0].initial_loc + data_base)
	    return NULL;
	  else if (pc < table[mid].initial_loc + data_base)
	    {
	      lo = 0;
	      hi = mid;

	      while (lo < hi)
		{
		  mid = (lo + hi) / 2;
		  if (pc < table[mid].initial_loc + data_base)
		    hi = mid;
		  else if (pc >= table[mid + 1].initial_loc + data_base)
		    lo = mid + 1;
		  else
		    break;
		}

	      gcc_assert (lo < hi);
	    }

	  f = (fde *) (table[mid].fde + data_base);
	  f_enc = get_fde_encoding (f);
	  f_enc_size = size_of_encoded_value (f_enc);

	  /* BFD ld uses DW_EH_PE_sdata4 | DW_EH_PE_pcrel on non-FDPIC targets,
	     so optimize for that.

	     This optimization is not valid for FDPIC targets.  f_enc & 0x0f as
	     passed to read_encoded_value_with_base masks away the base flags,
	     but they are implicit for FDPIC.  */
#ifndef __FDPIC__
	  if (__builtin_expect (f_enc == (DW_EH_PE_sdata4 | DW_EH_PE_pcrel),
				1))
	    {
	      signed value __attribute__ ((mode (SI)));
	      memcpy (&value, &f->pc_begin[f_enc_size], sizeof (value));
	      range = value;
	    }
	  else
#endif
	    read_encoded_value_with_base (f_enc & 0x0f, 0,
					  &f->pc_begin[f_enc_size], &range);
	  _Unwind_Ptr func = table[mid].initial_loc + data_base;
	  if (pc < table[mid].initial_loc + data_base + range)
	    {
	      bases->tbase = NULL;
	      bases->dbase = (void *) dbase;
	      bases->func = (void *) func;
	      return f;
	    }
	  else
	    return NULL;
	}
    }

  /* We have no sorted search table, so need to go the slow way.
     As soon as GLIBC will provide API so to notify that a library has been
     removed, we could cache this (and thus use search_object).  */
  ob.pc_begin = NULL;
  ob.tbase = NULL;
  ob.dbase = (void *) dbase;
  ob.u.single = (fde *) eh_frame;
  ob.s.i = 0;
  ob.s.b.mixed_encoding = 1;  /* Need to assume worst case.  */
  const fde *entry = linear_search_fdes (&ob, (fde *) eh_frame, (void *) pc);
  if (entry != NULL)
    {
      _Unwind_Ptr func;
      unsigned int encoding = get_fde_encoding (entry);

      read_encoded_value_with_base (encoding,
				    base_from_cb_data (encoding, dbase),
				    entry->pc_begin, &func);
      bases->tbase = NULL;
      bases->dbase = (void *) dbase;
      bases->func = (void *) func;
    }
  return entry;
}

const fde *
_Unwind_Find_FDE (void *pc, struct dwarf_eh_bases *bases)
{
  struct unw_eh_callback_data data;
  const fde *ret;

  ret = _Unwind_Find_registered_FDE (pc, bases);
  if (ret != NULL)
    return ret;

  /* Use DLFO_STRUCT_HAS_EH_DBASE as a proxy for the existence of a glibc-style
     _dl_find_object function.  */
#ifdef DLFO_STRUCT_HAS_EH_DBASE
  {
    struct dl_find_object dlfo;
    if (_dl_find_object (pc, &dlfo) == 0 && dlfo.dlfo_eh_frame != NULL)
      return find_fde_tail ((_Unwind_Ptr) pc, dlfo.dlfo_eh_frame,
# if DLFO_STRUCT_HAS_EH_DBASE
			    (_Unwind_Ptr) dlfo.dlfo_eh_dbase,
# else
			    0,
# endif
			    bases);
    else
      return NULL;
    }
#endif /* DLFO_STRUCT_HAS_EH_DBASE */

  data.pc = (_Unwind_Ptr) pc;
#if NEED_DBASE_MEMBER
  data.dbase = NULL;
#endif
  data.check_cache = 1;

  if (dl_iterate_phdr (_Unwind_IteratePhdrCallback, &data) <= 0)
    return NULL;

  _Unwind_Ptr dbase = unw_eh_callback_data_dbase (&data);
  return find_fde_tail ((_Unwind_Ptr) pc, data.hdr, dbase, bases);
}

#else
/* Prevent multiple include of header files.  */
#define _Unwind_Find_FDE _Unwind_Find_FDE
#include "unwind-dw2-fde.c"
#endif

#if defined (USE_GAS_SYMVER) && defined (SHARED) && defined (USE_LIBUNWIND_EXCEPTIONS)
alias (_Unwind_Find_FDE);
#endif
