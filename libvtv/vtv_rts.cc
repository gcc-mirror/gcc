/* Copyright (C) 2012-2025 Free Software Foundation, Inc.

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

/* This file is part of the vtable security feature implementation.
   The vtable security feature is designed to detect when a virtual
   call is about to be made through an invalid vtable pointer
   (possibly due to data corruption or malicious attacks). The
   compiler finds every virtual call, and inserts a verification call
   before the virtual call.  The verification call takes the actual
   vtable pointer value in the object through which the virtual call
   is being made, and compares the vtable pointer against a set of all
   valid vtable pointers that the object could contain (this set is
   based on the declared type of the object).  If the pointer is in
   the valid set, execution is allowed to continue; otherwise the
   program is halted.

  There are several pieces needed in order to make this work: 1. For
  every virtual class in the program (i.e. a class that contains
  virtual methods), we need to build the set of all possible valid
  vtables that an object of that class could point to.  This includes
  vtables for any class(es) that inherit from the class under
  consideration.  2. For every such data set we build up, we need a
  way to find and reference the data set.  This is complicated by the
  fact that the real vtable addresses are not known until runtime,
  when the program is loaded into memory, but we need to reference the
  sets at compile time when we are inserting verification calls into
  the program.  3.  We need to find every virtual call in the program,
  and insert the verification call (with the appropriate arguments)
  before the virtual call.  4. We need some runtime library pieces:
  the code to build up the data sets at runtime; the code to actually
  perform the verification using the data sets; and some code to set
  protections on the data sets, so they themselves do not become
  hacker targets.

  To find and reference the set of valid vtable pointers for any given
  virtual class, we create a special global varible for each virtual
  class.  We refer to this as the "vtable map variable" for that
  class.  The vtable map variable has the type "void *", and is
  initialized by the compiler to NULL.  At runtime when the set of
  valid vtable pointers for a virtual class, e.g. class Foo, is built,
  the vtable map variable for class Foo is made to point to the set.
  During compile time, when the compiler is inserting verification
  calls into the program, it passes the vtable map variable for the
  appropriate class to the verification call, so that at runtime the
  verification call can find the appropriate data set.

  The actual set of valid vtable pointers for a polymorphic class,
  e.g. class Foo, cannot be built until runtime, when the vtables get
  loaded into memory and their addresses are known.  But the knowledge
  about which vtables belong in which class' hierarchy is only known
  at compile time.  Therefore at compile time we collect class
  hierarchy and vtable information about every virtual class, and we
  generate calls to build up the data sets at runtime.  To build the
  data sets, we call one of the functions we add to the runtime
  library, __VLTRegisterPair.  __VLTRegisterPair takes two arguments,
  a vtable map variable and the address of a vtable.  If the vtable
  map variable is currently NULL, it creates a new data set (hash
  table), makes the vtable map variable point to the new data set, and
  inserts the vtable address into the data set.  If the vtable map
  variable is not NULL, it just inserts the vtable address into the
  data set.  In order to make sure that our data sets are built before
  any verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __VLTRegisterPair into our special constructor initialization
  function.  */

/* This file contains the main externally visible runtime library
   functions for vtable verification: __VLTChangePermission,
   __VLTRegisterPair, and __VLTVerifyVtablePointer.  It also contains
   debug versions __VLTRegisterPairDebug and
   __VLTVerifyVtablePointerDebug, which have extra parameters in order
   to make it easier to debug verification failures.

   The final piece of functionality implemented in this file is symbol
   resolution for multiple instances of the same vtable map variable.
   If the same virtual class is used in two different compilation
   units, then each compilation unit will create a vtable map variable
   for the class.  We need all instances of the same vtable map
   variable to point to the same (single) set of valid vtable
   pointers for the class, so we wrote our own hashtable-based symbol
   resolution for vtable map variables (with a tiny optimization in
   the case where there is only one instance of the variable).

   There are two other important pieces to the runtime for vtable
   verification besides the main pieces that go into libstdc++.so: two
   special tiny shared libraries, libvtv_init.so and libvtv_stubs.so.
   libvtv_init.so is built from vtv_init.cc.  It is designed to help
   minimize the calls made to mprotect (see the comments in
   vtv_init.cc for more details).  Anything compiled with
   "-fvtable-verify=std" must be linked with libvtv_init.so (the gcc
   driver has been modified to do this).  vtv_stubs.so is built from
   vtv_stubs.cc.  It replaces the main runtime functions
   (__VLTChangePermissino, __VLTRegisterPair and
   __VLTVerifyVtablePointer) with stub functions that do nothing.  If
   a programmer has a library that was built with verification, but
   wishes to not have verification turned on, the programmer can link
   in the vtv_stubs.so library.  */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined (__CYGWIN__) || defined (__MINGW32__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winternl.h>
#include <psapi.h>
#else
#include <execinfo.h>
#endif

#include <unistd.h>
#if !defined (__CYGWIN__) && !defined (__MINGW32__)
#include <sys/mman.h>
#include <link.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <limits.h>

/* For gthreads suppport */
#include <bits/c++config.h>
#include <ext/concurrence.h>

#include "vtv_utils.h"
#include "vtv_malloc.h"
#include "vtv_set.h"
#include "vtv_map.h"
#include "vtv_rts.h"
#include "vtv_fail.h"

#include "vtv-change-permission.h"

#ifdef HAVE_GETEXECNAME
const char *program_invocation_name;
#endif

#ifdef HAVE___FORTIFY_FAIL
extern "C" {

  /* __fortify_fail is a function in glibc that calls __libc_message,
     causing it to print out a program termination error message
     (including the name of the binary being terminated), a stack
     trace where the error occurred, and a memory map dump.  Ideally
     we would have called __libc_message directly, but that function
     does not appear to be accessible to functions outside glibc,
     whereas __fortify_fail is.  We call __fortify_fail from
     __vtv_really_fail.  We looked at calling __libc_fatal, which is
     externally accessible, but it does not do the back trace and
     memory dump.  */

  extern void __fortify_fail (const char *) __attribute__((noreturn));

} /* extern "C" */
#else
#if defined (__CYGWIN__) || defined (__MINGW32__)
// porting: fix link error to libc
void __fortify_fail (const char * msg){
    OutputDebugString(msg);
    abort();
}
#else
// FIXME: Provide backtrace via libbacktrace?
void __fortify_fail (const char *msg) {
    write (2, msg, strlen (msg));
    abort ();
}
#endif
#endif

/* The following variables are used only for debugging and performance
   tuning purposes. Therefore they do not need to be "protected".
   They cannot be used to attack the vtable verification system and if
   they become corrupted it will not affect the correctness or
   security of any of the rest of the vtable verification feature.  */

unsigned int num_calls_to_regset = 0;
unsigned int num_calls_to_regpair = 0;
unsigned int num_calls_to_verify_vtable = 0;
unsigned long long regset_cycles = 0;
unsigned long long regpair_cycles = 0;
unsigned long long verify_vtable_cycles = 0;

/* Be careful about initialization of statics in this file.  Some of
   the routines below are called before any runtime initialization for
   statics in this file will be done. For example, dont try to
   initialize any of these statics with a runtime call (for ex:
   sysconf). The initialization will happen after calls to the routines
   to protect/unprotec the vtabla_map variables */

/* No need to mark the following variables with VTV_PROTECTED_VAR.
   These are either const or are only used for debugging/tracing.
   debugging/tracing will not be ON on production environments */

static const bool debug_hash = HASHTABLE_STATS;

#ifdef VTV_DEBUG
static const int debug_functions = 1;
static const int debug_init = 1;
static const int debug_verify_vtable = 1;
#else
static const int debug_functions = 0;
static const int debug_init = 0;
static const int debug_verify_vtable = 0;
#endif

/* Global file descriptor variables for logging, tracing and debugging.  */

static int init_log_fd = -1;
static int verify_vtable_log_fd = -1;

/* This holds a formatted error logging message, to be written to the
   vtable verify failures log.  */
static char debug_log_message[1024];


#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t change_permissions_lock = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t change_permissions_lock;
#endif


#ifndef VTV_STATS
#define VTV_STATS 0
#endif

#if VTV_STATS

static inline unsigned long long
get_cycle_count (void)
{
  return rdtsc();
}

static inline void
accumulate_cycle_count (unsigned long long *sum, unsigned long long start)
{
  unsigned long long end = rdtsc();
  *sum = *sum + (end - start);
}

static inline void
increment_num_calls (unsigned int *num_calls)
{
  *num_calls = *num_calls + 1;
}

#else

static inline unsigned long long
get_cycle_count (void)
{
  return (unsigned long long) 0;
}

static inline void
accumulate_cycle_count (unsigned long long *sum __attribute__((__unused__)),
                        unsigned long long start __attribute__((__unused__)))
{
  /* Do nothing.  */
}

static inline void
increment_num_calls (unsigned int *num_calls __attribute__((__unused__)))
{
  /* Do nothing.  */
}

#endif

/* Types needed by insert_only_hash_sets.  */
typedef uintptr_t int_vptr;

/* The set of valid vtable pointers for each virtual class is stored
   in a hash table.  This is the hashing function used for the hash
   table.  For more information on the implementation of the hash
   table, see the class insert_only_hash_sets in vtv_set.h.  */

struct vptr_hash
  {
    /* Hash function, used to convert vtable pointer, V, (a memory
       address) into an index into the hash table.  */
    size_t
    operator() (int_vptr v) const
      {
	const uint32_t x = 0x7a35e4d9;
	const int shift = (sizeof (v) == 8) ? 23 : 21;
	v = x * v;
	return v ^ (v >> shift);
      }
  };

/* This is the memory allocator used to create the hash table data
   sets of valid vtable pointers.  We use VTV_malloc in order to keep
   track of which pages have been allocated, so we can update the
   protections on those pages appropriately.  See the class
   insert_only_hash_sets in vtv_set.h for more information.  */

struct vptr_set_alloc
  {
    /* Memory allocator operator.  N is the number of bytes to be
       allocated.  */
    void *
    operator() (size_t n) const
      {
	return __vtv_malloc (n);
      }
  };

/* Instantiate the template classes (in vtv_set.h) for our particular
   hash table needs.  */
typedef insert_only_hash_sets<int_vptr, vptr_hash, vptr_set_alloc> vtv_sets;
typedef vtv_sets::insert_only_hash_set vtv_set;
typedef vtv_set * vtv_set_handle;
typedef vtv_set_handle * vtv_set_handle_handle; 

/* Records for caching the section header information that we have
   read out of the file(s) on disk (in dl_iterate_phdr_callback), to
   avoid having to re-open and re-read the same file multiple
   times.  */

struct sect_hdr_data
{
#if defined (__CYGWIN__) || defined (__MINGW32__)
  uintptr_t dlpi_addr;    /* The header address in the INFO record,
                            passed in from dl_iterate_phdr.  */
  uintptr_t mp_low;       /* Start address of the .vtable_map_vars
                            section in memory.  */
#else
  ElfW (Addr) dlpi_addr; /* The header address in the INFO record,
                            passed in from dl_iterate_phdr.  */
  ElfW (Addr) mp_low;    /* Start address of the .vtable_map_vars
                            section in memory.  */
#endif
  size_t mp_size;        /* Size of the .vtable_map_vars section in
                            memory.  */
};

/* Array for caching the section header information, read from file,
   to avoid re-opening and re-reading the same file over-and-over
   again.  */

#define MAX_ENTRIES 250
static struct sect_hdr_data vtv_sect_info_cache[MAX_ENTRIES] VTV_PROTECTED_VAR;

unsigned int num_cache_entries VTV_PROTECTED_VAR = 0;

/* This function takes the LOAD_ADDR for an object opened by the
   dynamic loader, and checks the array of cached file data to see if
   there is an entry with the same addres.  If it finds such an entry,
   it returns the record for that entry; otherwise it returns
   NULL.  */

#if defined (__CYGWIN__) || defined (__MINGW32__)
struct sect_hdr_data *
search_cached_file_data (uintptr_t load_addr)
#else
struct sect_hdr_data *
search_cached_file_data (ElfW (Addr) load_addr)
#endif
{
  unsigned int i;
  for (i = 0; i < num_cache_entries; ++i)
    {
      if (vtv_sect_info_cache[i].dlpi_addr == load_addr)
        return &(vtv_sect_info_cache[i]);
    }

  return NULL;
}

/* This function tries to read COUNT bytes out of the file referred to
   by FD into the buffer BUF.  It returns the actual number of bytes
   it succeeded in reading.  */

static size_t
ReadPersistent (int fd, void *buf, size_t count)
{
  char *buf0 = (char *) buf;
  size_t num_bytes = 0;
  while (num_bytes < count)
    {
      int len;
      len = read (fd, buf0 + num_bytes, count - num_bytes);
      if (len < 0)
	return -1;
      if (len == 0)
	break;
      num_bytes += len;
    }

  return num_bytes;
}

/* This function tries to read COUNT bytes, starting at OFFSET from
   the file referred to by FD, and put them into BUF.  It calls
   ReadPersistent to help it do so.  It returns the actual number of
   bytes read, or -1 if it fails altogether.  */

static size_t
ReadFromOffset (int fd, void *buf, const size_t count, const off_t offset)
{
  off_t off = lseek (fd, offset, SEEK_SET);
  if (off != (off_t) -1)
    return ReadPersistent (fd, buf, count);
  return -1;
}

/* The function takes a MESSAGE and attempts to write it to the vtable
   memory protection log (for debugging purposes).  If the file is not
   open, it attempts to open the file first.  */

static void
log_memory_protection_data (char *message)
{
  static int log_fd = -1;

  if (log_fd == -1)
    log_fd = __vtv_open_log ("vtv_memory_protection_data.log");

  __vtv_add_to_log (log_fd, "%s", message);
}

#if defined (__CYGWIN__) || defined (__MINGW32__)
static void
read_section_offset_and_length (char *name,
                                uintptr_t addr,
                                const char *sect_name,
                                int mprotect_flags,
                                off_t *sect_offset,
                                WORD *sect_len)
{
  bool found = false;
  struct sect_hdr_data *cached_data = NULL;

  /* Check to see if we already have the data for this file.  */
  cached_data = search_cached_file_data (addr);

  if (cached_data)
    {
      *sect_offset = cached_data->mp_low;
      *sect_len = cached_data->mp_size;
      return;
    }

  // check for DOS Header magic bytes
  if (*(WORD *)addr == 0x5A4D)
    {
      int name_len = strlen (sect_name);
      int fd = -1;

      /* Attempt to open the binary file on disk.  */
      if (strlen (name) == 0)
        {
          return;
        }
      else
        fd = open (name, O_RDONLY | O_BINARY);

      if (fd != -1)
        {
          /* Find the section header information in memory.  */
          PIMAGE_DOS_HEADER pDosHeader = (PIMAGE_DOS_HEADER)addr;
          PIMAGE_NT_HEADERS pNtHeaders = (PIMAGE_NT_HEADERS)((char *)addr
                                          + pDosHeader->e_lfanew);
          PIMAGE_FILE_HEADER pFileHeader = &pNtHeaders->FileHeader;

          DWORD PointerToStringTable = pFileHeader->PointerToSymbolTable
                                        + (pFileHeader->NumberOfSymbols*0x12);

          PIMAGE_SECTION_HEADER sect_hdr = 
            (PIMAGE_SECTION_HEADER)((char *)&pNtHeaders->OptionalHeader
                                       + pFileHeader->SizeOfOptionalHeader);

          /* Loop through all the section headers, looking for one whose
             name is ".vtable_map_vars".  */

          for (int i = 0; i < pFileHeader->NumberOfSections && !found; ++i)
            {
              char header_name[64];

              /* Check if we have to get the section name from the COFF string
                 table. */
              if (sect_hdr[i].Name[0] == '/')
                {
                  if (atoi((const char*)sect_hdr[i].Name+1) == 0)
                    {
                      continue;
                    }

                  off_t name_offset = PointerToStringTable
                                       + atoi((const char*)sect_hdr[i].Name+1);

                  size_t bytes_read = ReadFromOffset (fd, &header_name, 64,
                                                      name_offset);

                  VTV_ASSERT (bytes_read > 0);
                }
              else
                {
                  memcpy (&header_name, sect_hdr[i].Name,
                          sizeof (sect_hdr[i].Name));
                }

              if (memcmp (header_name, sect_name, name_len) == 0)
                {
                  /* We found the section; get its load offset and
                     size.  */
                  *sect_offset = sect_hdr[i].VirtualAddress;
      if (sect_hdr[i].Misc.VirtualSize % VTV_PAGE_SIZE != 0)
        *sect_len = sect_hdr[i].Misc.VirtualSize + VTV_PAGE_SIZE
                     - (sect_hdr[i].Misc.VirtualSize % VTV_PAGE_SIZE);
      else
        *sect_len = sect_hdr[i].Misc.VirtualSize;
                  found = true;
                }
            }
          close (fd);
        }
    }

  if (*sect_offset != 0 && *sect_len != 0)
    {
      /* Calculate the page location in memory, making sure the
         address is page-aligned.  */
      uintptr_t start_addr = addr + *sect_offset;
      *sect_offset = start_addr & ~(VTV_PAGE_SIZE - 1);
      *sect_len = *sect_len - 1;

      /* Since we got this far, we must not have found these pages in
         the cache, so add them to it.  NOTE: We could get here either
         while making everything read-only or while making everything
         read-write.  We will only update the cache if we get here on
         a read-write (to make absolutely sure the cache is writable
         -- also the read-write pass should come before the read-only
         pass).  */
      if ((mprotect_flags & PROT_WRITE)
          && num_cache_entries < MAX_ENTRIES)
        {
          vtv_sect_info_cache[num_cache_entries].dlpi_addr = addr;
          vtv_sect_info_cache[num_cache_entries].mp_low = *sect_offset;
          vtv_sect_info_cache[num_cache_entries].mp_size = *sect_len;
          num_cache_entries++;
        }
    }
}
#else
static void
read_section_offset_and_length (struct dl_phdr_info *info,
                                const char *sect_name,
                                int mprotect_flags,
                                off_t *sect_offset,
                                ElfW (Word) *sect_len)
{
  char program_name[PATH_MAX];
  char *cptr;
  bool found = false;
  struct sect_hdr_data *cached_data = NULL;
  const ElfW (Phdr) *phdr_info = info->dlpi_phdr;
  const ElfW (Ehdr) *ehdr_info =
    (const ElfW (Ehdr) *) (info->dlpi_addr + info->dlpi_phdr[0].p_vaddr
                           - info->dlpi_phdr[0].p_offset);


  /* Get the name of the main executable.  This may or may not include
     arguments passed to the program.  Find the first space, assume it
     is the start of the argument list, and change it to a '\0'. */
#ifdef HAVE_GETEXECNAME
  program_invocation_name = getexecname ();
#endif
  snprintf (program_name, sizeof (program_name), program_invocation_name);

  /* Check to see if we already have the data for this file.  */
  cached_data = search_cached_file_data (info->dlpi_addr);

  if (cached_data)
    {
      *sect_offset = cached_data->mp_low;
      *sect_len = cached_data->mp_size;
      return;
    }

  /* Find the first non-escaped space in the program name and make it
     the end of the string.  */
  cptr = strchr (program_name, ' ');
  if (cptr != NULL && cptr[-1] != '\\')
    cptr[0] = '\0';

  if ((phdr_info->p_type == PT_PHDR || phdr_info->p_type == PT_LOAD)
      && (ehdr_info->e_shoff && ehdr_info->e_shnum))
    {
      int name_len = strlen (sect_name);
      int fd = -1;

      /* Attempt to open the binary file on disk.  */
      if (strlen (info->dlpi_name) == 0)
        {
          /* If the constructor initialization function was put into
             the preinit array, then this function will get called
             while handling preinit array stuff, in which case
             program_invocation_name has not been initialized.  In
             that case we can get the filename of the executable from
             "/proc/self/exe".  */
          if (strlen (program_name) > 0)
            {
              if (phdr_info->p_type == PT_PHDR)
                fd = open (program_name, O_RDONLY);
            }
          else
            fd = open ("/proc/self/exe", O_RDONLY);
        }
      else
        fd = open (info->dlpi_name, O_RDONLY);

      if (fd != -1)
        {
          /* Find the section header information in the file.  */
          ElfW (Half) strtab_idx = ehdr_info->e_shstrndx;
          ElfW (Shdr) shstrtab;
          off_t shstrtab_offset = ehdr_info->e_shoff +
                                         (ehdr_info->e_shentsize * strtab_idx);
          size_t bytes_read = ReadFromOffset (fd, &shstrtab, sizeof (shstrtab),
                                              shstrtab_offset);
          VTV_ASSERT (bytes_read == sizeof (shstrtab));

          ElfW (Shdr) sect_hdr;

	  /* This code will be needed once we have crated libvtv.so. */
	  bool is_libvtv = false;

	  /*
	  if (strstr (info->dlpi_name, "libvtv.so"))
	    is_libvtv = true;
	  */

          /* Loop through all the section headers, looking for one whose
             name is ".vtable_map_vars".  */

          for (int i = 0; i < ehdr_info->e_shnum && !found; ++i)
            {
              off_t offset = ehdr_info->e_shoff + (ehdr_info->e_shentsize * i);

              bytes_read = ReadFromOffset (fd, &sect_hdr, sizeof (sect_hdr),
                                           offset);

              VTV_ASSERT (bytes_read == sizeof (sect_hdr));

              char header_name[64];
              off_t name_offset = shstrtab.sh_offset +  sect_hdr.sh_name;

              bytes_read = ReadFromOffset (fd, &header_name, 64, name_offset);

              VTV_ASSERT (bytes_read > 0);

              if (memcmp (header_name, sect_name, name_len) == 0)
                {
                  /* We found the section; get its load offset and
                     size.  */
                  *sect_offset = sect_hdr.sh_addr;
		  if (!is_libvtv)
		    {
		      VTV_ASSERT (sect_hdr.sh_size - VTV_PAGE_SIZE >= 0);
		      *sect_len = sect_hdr.sh_size - VTV_PAGE_SIZE;
		    }
		  else
		    *sect_len = sect_hdr.sh_size;
                  found = true;
                }
            }
          close (fd);
        }
    }

  if (*sect_offset != 0 && *sect_len != 0)
    {
      /* Calculate the page location in memory, making sure the
         address is page-aligned.  */
      ElfW (Addr) start_addr = (const ElfW (Addr)) info->dlpi_addr
                                                                 + *sect_offset;
      *sect_offset = start_addr & ~(VTV_PAGE_SIZE - 1);
      *sect_len = *sect_len - 1;

      /* Since we got this far, we must not have found these pages in
         the cache, so add them to it.  NOTE: We could get here either
         while making everything read-only or while making everything
         read-write.  We will only update the cache if we get here on
         a read-write (to make absolutely sure the cache is writable
         -- also the read-write pass should come before the read-only
         pass).  */
      if ((mprotect_flags & PROT_WRITE)
          && num_cache_entries < MAX_ENTRIES)
        {
          vtv_sect_info_cache[num_cache_entries].dlpi_addr = info->dlpi_addr;
          vtv_sect_info_cache[num_cache_entries].mp_low = *sect_offset;
          vtv_sect_info_cache[num_cache_entries].mp_size = *sect_len;
          num_cache_entries++;
        }
    }
}
#endif

#if defined (__CYGWIN__) || defined (__MINGW32__)
/* This function is used to iterate over all loaded modules and searches
   for a section called ".vtable_map_vars". The only interaction with 
   the binary file on disk of the module is to read section names in the
   COFF string table. If the module contains a ".vtable_map_vars" section,
   read section offset and size from the section header of the loaded module.
   Call 'mprotect' on those pages, setting the protection either to
   read-only or read-write, depending on what's in data.
   The calls to change the protection occur in vtv_unprotect_vtable_vars 
   and vtv_protect_vtable_vars.  */

static int
iterate_modules (void *data)
{
  int * mprotect_flags = (int *) data;
  off_t map_sect_offset = 0;
  WORD map_sect_len = 0;
  char buffer[1024];
  const char *map_sect_name = VTV_PROTECTED_VARS_SECTION;
  HMODULE hMods[1024];
  HANDLE hProcess;
  DWORD cbNeeded;

  hProcess = GetCurrentProcess ();

  if (NULL == hProcess)
    return 0;

  if (EnumProcessModules (hProcess, hMods, sizeof (hMods), &cbNeeded))
    {
      /* Iterate over all loaded modules. */
      for (unsigned int i = 0; i < (cbNeeded / sizeof (HMODULE)); i++)
        {
          char szModName[MAX_PATH];

          if (GetModuleFileNameExA (hProcess, hMods[i], szModName,
                        sizeof (szModName)))
            {
              map_sect_offset = 0;
              map_sect_len = 0;
              read_section_offset_and_length (szModName,
                                              (uintptr_t) hMods[i],
                                              map_sect_name, 
                                              *mprotect_flags,
                                              &map_sect_offset,
                                              &map_sect_len);

              if (debug_functions)
                {
                  snprintf (buffer, sizeof(buffer),
                "  Looking at load module %s to change permissions to %s\n",
                szModName,
                (*mprotect_flags & PROT_WRITE) ? "READ/WRITE" : "READ-ONLY");
                  log_memory_protection_data (buffer);
                }

              /* See if we actually found the section.  */
              if (map_sect_offset && map_sect_len)
                {
                  unsigned long long start;
                  int result;

                  if (debug_functions)
                    {
                      snprintf (buffer, sizeof (buffer),
                                "  (%s): Protecting %p to %p\n",
                                szModName,
                                (void *) map_sect_offset,
                                (void *) (map_sect_offset + map_sect_len));
                      log_memory_protection_data (buffer);
                    }

                  /* Change the protections on the pages for the section.  */

                  start = get_cycle_count ();
                  result = mprotect ((void *) map_sect_offset, map_sect_len,
                                     *mprotect_flags);
                  accumulate_cycle_count (&mprotect_cycles, start);
                  if (result == -1)
                    {
                      if (debug_functions)
                        {
                          snprintf (buffer, sizeof (buffer),
                                    "Failed call to mprotect for %s error: ",
                                    (*mprotect_flags & PROT_WRITE) ?
                                    "READ/WRITE" : "READ-ONLY");
                          log_memory_protection_data (buffer);
                          perror(NULL);
                        }
                      VTV_error();
                    }
                  else
                    {
                      if (debug_functions)
                       {
                          snprintf (buffer, sizeof (buffer),
                                    "mprotect'ed range [%p, %p]\n",
                                    (void *) map_sect_offset,
                                    (char *) map_sect_offset + map_sect_len);
                          log_memory_protection_data (buffer);
                        }
                    }
                  increment_num_calls (&num_calls_to_mprotect);
                  num_pages_protected += (map_sect_len + VTV_PAGE_SIZE - 1) 
		    / VTV_PAGE_SIZE;
                  continue;
                }
            }
        }
    }

    CloseHandle(hProcess);

  return 0;
}
#else
/* This is the callback function used by dl_iterate_phdr (which is
   called from vtv_unprotect_vtable_vars and vtv_protect_vtable_vars).
   It attempts to find the binary file on disk for the INFO record
   that dl_iterate_phdr passes in; open the binary file, and read its
   section header information.  If the file contains a
   ".vtable_map_vars" section, read the section offset and size.  Use
   the section offset and size, in conjunction with the data in INFO
   to locate the pages in memory where the section is.  Call
   'mprotect' on those pages, setting the protection either to
   read-only or read-write, depending on what's in DATA.  */

static int
dl_iterate_phdr_callback (struct dl_phdr_info *info, size_t, void *data)
{
  int * mprotect_flags = (int *) data;
  off_t map_sect_offset = 0;
  ElfW (Word) map_sect_len = 0;
  char buffer[1024];
  char program_name[1024];
  const char *map_sect_name = VTV_PROTECTED_VARS_SECTION;

  /* Check to see if this is the record for the Linux Virtual Dynamic
     Shared Object (linux-vdso.so.1), which exists only in memory (and
     therefore cannot be read from disk).  */

  if (strcmp (info->dlpi_name, "linux-vdso.so.1") == 0)
    return 0;

  if (strlen (info->dlpi_name) == 0
      && info->dlpi_addr != 0)
    return 0;

  /* Get the name of the main executable.  This may or may not include
     arguments passed to the program.  Find the first space, assume it
     is the start of the argument list, and change it to a '\0'. */
#ifdef HAVE_GETEXECNAME
  program_invocation_name = getexecname ();
#endif
  snprintf (program_name, sizeof (program_name), program_invocation_name);

  read_section_offset_and_length (info, map_sect_name, *mprotect_flags,
				  &map_sect_offset, &map_sect_len);

  if (debug_functions)
    {
      snprintf (buffer, sizeof(buffer),
		"  Looking at load module %s to change permissions to %s\n",
		((strlen (info->dlpi_name) == 0) ? program_name
                                                 : info->dlpi_name),
		(*mprotect_flags & PROT_WRITE) ? "READ/WRITE" : "READ-ONLY");
      log_memory_protection_data (buffer);
    }

  /* See if we actually found the section.  */
  if (map_sect_offset && map_sect_len)
    {
      unsigned long long start;
      int result;

      if (debug_functions)
        {
          snprintf (buffer, sizeof (buffer),
                    "  (%s): Protecting %p to %p\n",
                    ((strlen (info->dlpi_name) == 0) ? program_name
                     : info->dlpi_name),
                    (void *) map_sect_offset,
                    (void *) (map_sect_offset + map_sect_len));
          log_memory_protection_data (buffer);
        }

      /* Change the protections on the pages for the section.  */

      start = get_cycle_count ();
      result = mprotect ((void *) map_sect_offset, map_sect_len,
                         *mprotect_flags);
      accumulate_cycle_count (&mprotect_cycles, start);
      if (result == -1)
        {
          if (debug_functions)
            {
              snprintf (buffer, sizeof (buffer),
                        "Failed call to mprotect for %s error: ",
                        (*mprotect_flags & PROT_WRITE) ?
                        "READ/WRITE" : "READ-ONLY");
              log_memory_protection_data (buffer);
              perror(NULL);
            }
          VTV_error();
        }
      else
        {
          if (debug_functions)
           {
              snprintf (buffer, sizeof (buffer),
                        "mprotect'ed range [%p, %p]\n",
                        (void *) map_sect_offset,
                        (char *) map_sect_offset + map_sect_len);
              log_memory_protection_data (buffer);
            }
        }
      increment_num_calls (&num_calls_to_mprotect);
      num_pages_protected += (map_sect_len + VTV_PAGE_SIZE - 1) / VTV_PAGE_SIZE;
    }

  return 0;
}
#endif

/* This function explicitly changes the protection (read-only or read-write)
   on the vtv_sect_info_cache, which is used for speeding up look ups in the
   function dl_iterate_phdr_callback.  This data structure needs to be
   explicitly made read-write before any calls  to dl_iterate_phdr_callback,
   because otherwise it may still be read-only when dl_iterate_phdr_callback
   attempts to write to it.

   More detailed explanation:  dl_iterate_phdr_callback finds all the
   .vtable_map_vars sections in all loaded objects (including the main program)
   and (depending on where it was called from) either makes all the pages in the
   sections read-write or read-only.  The vtv_sect_info_cache should be in the
   .vtable_map_vars section for libstdc++.so, which means that normally it would
   be read-only until libstdc++.so is processed by dl_iterate_phdr_callback
   (on the read-write pass), after which it will be writable.  But if any loaded
   object gets processed before libstdc++.so, it will attempt to update the
   data cache, which will still be read-only, and cause a seg fault.  Hence
   we need a special function, called before dl_iterate_phdr_callback, that
   will make the data cache writable.  */

static void
change_protections_on_phdr_cache (int protection_flag)
{
  char * low_address = (char *) &(vtv_sect_info_cache);
  size_t cache_size = MAX_ENTRIES * sizeof (struct sect_hdr_data);

  low_address = (char *) ((uintptr_t) low_address & ~(VTV_PAGE_SIZE - 1));
  
  if (mprotect ((void *) low_address, cache_size, protection_flag) == -1)
    VTV_error ();
}

/* Unprotect all the vtable map vars and other side data that is used
   to keep the core hash_map data. All of these data have been put
   into relro sections */

static void
vtv_unprotect_vtable_vars (void)
{
  int mprotect_flags;

  mprotect_flags = PROT_READ | PROT_WRITE;
  change_protections_on_phdr_cache (mprotect_flags);
#if defined (__CYGWIN__) || defined (__MINGW32__)
  iterate_modules ((void *) &mprotect_flags);
#else
  dl_iterate_phdr (dl_iterate_phdr_callback, (void *) &mprotect_flags);
#endif
}

/* Protect all the vtable map vars and other side data that is used
   to keep the core hash_map data. All of these data have been put
   into relro sections */

static void
vtv_protect_vtable_vars (void)
{
  int mprotect_flags;

  mprotect_flags = PROT_READ;
#if defined (__CYGWIN__) || defined (__MINGW32__)
  iterate_modules ((void *) &mprotect_flags);
#else
  dl_iterate_phdr (dl_iterate_phdr_callback, (void *) &mprotect_flags);
#endif
  change_protections_on_phdr_cache (mprotect_flags);
}

#ifndef __GTHREAD_MUTEX_INIT
static void
initialize_change_permissions_mutexes ()
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&change_permissions_lock);
}
#endif

/*  Variables needed for getting the statistics about the hashtable set.  */
#if HASHTABLE_STATS
_AtomicStatCounter stat_contains = 0;
_AtomicStatCounter stat_insert = 0;
_AtomicStatCounter stat_resize = 0;
_AtomicStatCounter stat_create = 0;
_AtomicStatCounter stat_probes_in_non_trivial_set = 0;
_AtomicStatCounter stat_contains_size0 = 0;
_AtomicStatCounter stat_contains_size1 = 0;
_AtomicStatCounter stat_contains_size2 = 0;
_AtomicStatCounter stat_contains_size3 = 0;
_AtomicStatCounter stat_contains_size4 = 0;
_AtomicStatCounter stat_contains_size5 = 0;
_AtomicStatCounter stat_contains_size6 = 0;
_AtomicStatCounter stat_contains_size7 = 0;
_AtomicStatCounter stat_contains_size8 = 0;
_AtomicStatCounter stat_contains_size9 = 0;
_AtomicStatCounter stat_contains_size10 = 0;
_AtomicStatCounter stat_contains_size11 = 0;
_AtomicStatCounter stat_contains_size12 = 0;
_AtomicStatCounter stat_contains_size13_or_more = 0;
_AtomicStatCounter stat_contains_sizes = 0;
_AtomicStatCounter stat_grow_from_size0_to_1 = 0;
_AtomicStatCounter stat_grow_from_size1_to_2 = 0;
_AtomicStatCounter stat_double_the_number_of_buckets = 0;
_AtomicStatCounter stat_insert_found_hash_collision = 0;
_AtomicStatCounter stat_contains_in_non_trivial_set = 0;
_AtomicStatCounter stat_insert_key_that_was_already_present = 0;
#endif
/* Record statistics about the hash table sets, for debugging.  */

static void
log_set_stats (void)
{
#if HASHTABLE_STATS
      if (set_log_fd == -1)
	set_log_fd = __vtv_open_log ("vtv_set_stats.log");

      __vtv_add_to_log (set_log_fd, "---\n%s\n",
			insert_only_hash_tables_stats().c_str());
#endif
}

/* Change the permissions on all the pages we have allocated for the
   data sets and all the ".vtable_map_var" sections in memory (which
   contain our vtable map variables).  PERM indicates whether to make
   the permissions read-only or read-write.  */

extern "C" /* This is only being applied to __VLTChangePermission*/
void
__VLTChangePermission (int perm)
{
  if (debug_functions)
    {
      if (perm == __VLTP_READ_WRITE)
	fprintf (stdout, "Changing VLT permissions to Read-Write.\n");
      else if (perm == __VLTP_READ_ONLY)
	fprintf (stdout, "Changing VLT permissions to Read-Only.\n");

      else
	fprintf (stdout, "Unrecognized permissions value: %d\n", perm);
    }

#ifndef __GTHREAD_MUTEX_INIT
  static __gthread_once_t mutex_once VTV_PROTECTED_VAR = __GTHREAD_ONCE_INIT;

  __gthread_once (&mutex_once, initialize_change_permissions_mutexes);
#endif

  /* Ordering of these unprotect/protect calls is very important.
     You first need to unprotect all the map vars and side
     structures before you do anything with the core data
     structures (hash_maps) */

  if (perm == __VLTP_READ_WRITE)
    {
      /* TODO: Need to revisit this code for dlopen. It most probably
         is not unlocking the protected vtable vars after for load
         module that is not the first load module.  */
      __gthread_mutex_lock (&change_permissions_lock);

      vtv_unprotect_vtable_vars ();
      __vtv_malloc_init ();
      __vtv_malloc_unprotect ();

    }
  else if (perm == __VLTP_READ_ONLY)
    {
      if (debug_hash)
        log_set_stats();

      __vtv_malloc_protect ();
      vtv_protect_vtable_vars ();

      __gthread_mutex_unlock (&change_permissions_lock);
    }
}

/* This is the memory allocator used to create the hash table that
   maps from vtable map variable name to the data set that vtable map
   variable should point to.  This is part of our vtable map variable
   symbol resolution, which is necessary because the same vtable map
   variable may be created by multiple compilation units and we need a
   method to make sure that all vtable map variables for a particular
   class point to the same data set at runtime.  */

struct insert_only_hash_map_allocator
  {
    /* N is the number of bytes to allocate.  */
    void *
    alloc (size_t n) const
    {  
      return __vtv_malloc (n);
    }

    /* P points to the memory to be deallocated; N is the number of
       bytes to deallocate.  */
    void
    dealloc (void *p, size_t) const
    {
      __vtv_free (p);
    }
  };

/* Explicitly instantiate this class since this file is compiled with
   -fno-implicit-templates.  These are for the hash table that is used
   to do vtable map variable symbol resolution.  */
template class insert_only_hash_map <vtv_set_handle *, 
				     insert_only_hash_map_allocator >;
typedef insert_only_hash_map <vtv_set_handle *,
                              insert_only_hash_map_allocator > s2s;
typedef const s2s::key_type  vtv_symbol_key;

static s2s * vtv_symbol_unification_map VTV_PROTECTED_VAR = NULL;

const unsigned long SET_HANDLE_HANDLE_BIT = 0x2;

/* In the case where a vtable map variable is the only instance of the
   variable we have seen, it points directly to the set of valid
   vtable pointers.  All subsequent instances of the 'same' vtable map
   variable point to the first vtable map variable.  This function,
   given a vtable map variable PTR, checks a bit to see whether it's
   pointing directly to the data set or to the first vtable map
   variable.  */

static inline bool
is_set_handle_handle (void * ptr)
{
  return ((uintptr_t) ptr & SET_HANDLE_HANDLE_BIT)
                                                      == SET_HANDLE_HANDLE_BIT;
}

/* Returns the actual pointer value of a vtable map variable, PTR (see
   comments for is_set_handle_handle for more details).  */

static inline vtv_set_handle * 
ptr_from_set_handle_handle (void * ptr)
{
  return (vtv_set_handle *) ((uintptr_t) ptr & ~SET_HANDLE_HANDLE_BIT);
}

/* Given a vtable map variable, PTR, this function sets the bit that
   says this is the second (or later) instance of a vtable map
   variable.  */

static inline vtv_set_handle_handle
set_handle_handle (vtv_set_handle * ptr)
{
  return (vtv_set_handle_handle) ((uintptr_t) ptr | SET_HANDLE_HANDLE_BIT);
}

static inline void
register_set_common (void **set_handle_ptr, size_t num_args,
                     void **vtable_ptr_array, bool debug)
{
  /* Now figure out what pointer to use for the set pointer, for the
     inserts.  */
  vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;

  if (debug)
    VTV_DEBUG_ASSERT (vtv_symbol_unification_map != NULL);

  if (!is_set_handle_handle (*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

  /* Now we've got the set and it's initialized, add the vtable
     pointers.  */
  for (size_t index = 0; index < num_args; ++index)
    {
      int_vptr vtbl_ptr = (int_vptr) vtable_ptr_array[index];
      vtv_sets::insert (vtbl_ptr, handle_ptr);
    }
}

static inline void
register_pair_common (void **set_handle_ptr, const void *vtable_ptr,
                      const char *set_symbol_name, const char *vtable_name,
                      bool debug)
{
  /* Now we've got the set and it's initialized, add the vtable
     pointer (assuming that it's not NULL...It may be NULL, as we may
     have called this function merely to initialize the set
     pointer).  */
  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;
  if (vtbl_ptr)
    {
      vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;
      if (debug)
        VTV_DEBUG_ASSERT (vtv_symbol_unification_map != NULL);
      if (!is_set_handle_handle (*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

      vtv_sets::insert (vtbl_ptr, handle_ptr);
    }

  if (debug && debug_init)
    {
      if (init_log_fd == -1)
        init_log_fd = __vtv_open_log("vtv_init.log");

      __vtv_add_to_log(init_log_fd,
		       "Registered %s : %s (%p) 2 level deref = %s\n",
		       set_symbol_name, vtable_name, vtbl_ptr,
		       is_set_handle_handle(*set_handle_ptr) ? "yes" : "no" );
    }
}

/* This routine initializes a set handle to a vtable set. It makes
   sure that there is only one set handle for a particular set by
   using a map from set name to pointer to set handle. Since there
   will be multiple copies of the pointer to the set handle (one per
   compilation unit that uses it), it makes sure to initialize all the
   pointers to the set handle so that the set handle is unique. To
   make this a little more efficient and avoid a level of indirection
   in some cases, the first pointer to handle for a particular handle
   becomes the handle itself and the other pointers will point to the
   set handle.  This is the debug version of this function, so it
   outputs extra debugging messages and logging.  SET_HANDLE_PTR is
   the address of the vtable map variable, SET_SYMBOL_KEY is the hash
   table key (containing the name of the map variable and the hash
   value) and SIZE_HINT is a guess for the best initial size for the
   set of vtable pointers that SET_HANDLE_POINTER will point to.  */

static inline void
init_set_symbol_debug (void **set_handle_ptr, const void *set_symbol_key,
                       size_t size_hint)
{
  VTV_DEBUG_ASSERT (set_handle_ptr);

  if (vtv_symbol_unification_map == NULL)
    {
      /* TODO:  For now we have chosen 1024, but we need to come up with a
         better initial size for this.  */
      vtv_symbol_unification_map = s2s::create (1024);
      VTV_DEBUG_ASSERT(vtv_symbol_unification_map);
    }

  vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;
  vtv_symbol_key *symbol_key_ptr = (vtv_symbol_key *) set_symbol_key;

  const s2s::value_type * map_value_ptr =
                              vtv_symbol_unification_map->get (symbol_key_ptr);
  char buffer[200];
  if (map_value_ptr == NULL)
    {
      if (*handle_ptr != NULL)
        {
          snprintf (buffer, sizeof (buffer),
                    "*** Found non-NULL local set ptr %p missing for symbol"
                    " %.*s",
                    *handle_ptr, symbol_key_ptr->n, symbol_key_ptr->bytes);
          __vtv_log_verification_failure (buffer, true);
          VTV_DEBUG_ASSERT (0);
        }
    }
  else if (*handle_ptr != NULL &&
           (handle_ptr != *map_value_ptr &&
            ptr_from_set_handle_handle (*handle_ptr) != *map_value_ptr))
    {
      VTV_DEBUG_ASSERT (*map_value_ptr != NULL);
      snprintf (buffer, sizeof(buffer),
                "*** Found diffence between local set ptr %p and set ptr %p"
                "for symbol %.*s",
                *handle_ptr, *map_value_ptr,
                symbol_key_ptr->n, symbol_key_ptr->bytes);
      __vtv_log_verification_failure (buffer, true);
      VTV_DEBUG_ASSERT (0);
    }
  else if (*handle_ptr == NULL)
    {
      /* Execution should not reach this point.  */
    }

  if (*handle_ptr != NULL)
    {
      if (!is_set_handle_handle (*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);
      vtv_sets::resize (size_hint, handle_ptr);
      return;
    }

  VTV_DEBUG_ASSERT (*handle_ptr == NULL);
  if (map_value_ptr != NULL)
    {
      if (*map_value_ptr == handle_ptr)
        vtv_sets::resize (size_hint, *map_value_ptr);
      else
        {
          /* The one level handle to the set already exists. So, we
             are adding one level of indirection here and we will
             store a pointer to the one level handle here.  */

          vtv_set_handle_handle * handle_handle_ptr =
                                           (vtv_set_handle_handle *)handle_ptr;
          *handle_handle_ptr = set_handle_handle(*map_value_ptr);
          VTV_DEBUG_ASSERT(*handle_handle_ptr != NULL);

          /* The handle can itself be NULL if the set has only
             been initiazlied with size hint == 1. */
          vtv_sets::resize (size_hint, *map_value_ptr);
        }
    }
  else
    {
      /* We will create a new set. So, in this case handle_ptr is the
         one level pointer to the set handle.  Create copy of map name
         in case the memory where this comes from gets unmapped by
         dlclose.  */
      size_t map_key_len = symbol_key_ptr->n + sizeof (vtv_symbol_key);
      void *map_key = __vtv_malloc (map_key_len);

      memcpy (map_key, symbol_key_ptr, map_key_len);

      s2s::value_type *value_ptr;
      vtv_symbol_unification_map =
        vtv_symbol_unification_map->find_or_add_key ((vtv_symbol_key *)map_key,
                                                     &value_ptr);
      *value_ptr = handle_ptr;

      /*  TODO: We should verify the return value. */
      vtv_sets::create (size_hint, handle_ptr);
      VTV_DEBUG_ASSERT (size_hint <= 1 || *handle_ptr != NULL);
    }

  if (debug_init)
    {
      if (init_log_fd == -1)
        init_log_fd = __vtv_open_log ("vtv_init.log");

      __vtv_add_to_log (init_log_fd,
			"Init handle:%p for symbol:%.*s hash:%u size_hint:%lu"
			"number of symbols:%lu \n",
			set_handle_ptr, symbol_key_ptr->n,
			symbol_key_ptr->bytes, symbol_key_ptr->hash, size_hint,
			vtv_symbol_unification_map->size ());
    }
}


/* This routine initializes a set handle to a vtable set. It makes
   sure that there is only one set handle for a particular set by
   using a map from set name to pointer to set handle. Since there
   will be multiple copies of the pointer to the set handle (one per
   compilation unit that uses it), it makes sure to initialize all the
   pointers to the set handle so that the set handle is unique. To
   make this a little more efficient and avoid a level of indirection
   in some cases, the first pointer to handle for a particular handle
   becomes the handle itself and the other pointers will point to the
   set handle.  This is the debug version of this function, so it
   outputs extra debugging messages and logging.  SET_HANDLE_PTR is
   the address of the vtable map variable, SET_SYMBOL_KEY is the hash
   table key (containing the name of the map variable and the hash
   value) and SIZE_HINT is a guess for the best initial size for the
   set of vtable pointers that SET_HANDLE_POINTER will point to.  */

void
__VLTRegisterSetDebug (void **set_handle_ptr, const void *set_symbol_key,
                       size_t size_hint, size_t num_args,
                       void **vtable_ptr_array)
{
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&num_calls_to_regset);

  VTV_DEBUG_ASSERT(set_handle_ptr != NULL);
  init_set_symbol_debug (set_handle_ptr, set_symbol_key, size_hint);

  register_set_common (set_handle_ptr, num_args, vtable_ptr_array, true);

  accumulate_cycle_count (&regset_cycles, start);
}

/* This function takes a the address of a vtable map variable
   (SET_HANDLE_PTR), a VTABLE_PTR to add to the data set, the name of
   the vtable map variable (SET_SYMBOL_NAME) and the name of the
   vtable (VTABLE_NAME) being pointed to.  If the vtable map variable
   is NULL it creates a new data set and initializes the variable,
   otherwise it uses our symbol unification to find the right data
   set; in either case it then adds the vtable pointer to the set.
   The other two parameters are used for debugging information.  */

void
__VLTRegisterPairDebug (void **set_handle_ptr, const  void *set_symbol_key,
                        size_t size_hint, const void *vtable_ptr,
                        const char *set_symbol_name, const char *vtable_name)
{
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&num_calls_to_regpair);

  VTV_DEBUG_ASSERT(set_handle_ptr != NULL);
  init_set_symbol_debug (set_handle_ptr, set_symbol_key, size_hint);

  register_pair_common (set_handle_ptr, vtable_ptr, set_symbol_name, vtable_name,
                        true);

  accumulate_cycle_count (&regpair_cycles, start);
}


/* This is the debug version of the verification function.  It takes
   the address of a vtable map variable (SET_HANDLE_PTR) and a
   VTABLE_PTR to validate, as well as the name of the vtable map
   variable (SET_SYMBOL_NAME) and VTABLE_NAME, which are used for
   debugging messages.  It checks to see if VTABLE_PTR is in the set
   pointed to by SET_HANDLE_PTR.  If so, it returns VTABLE_PTR,
   otherwise it calls __vtv_verify_fail, which usually logs error
   messages and calls abort.  */

const void *
__VLTVerifyVtablePointerDebug (void **set_handle_ptr, const void *vtable_ptr,
                               const char *set_symbol_name,
			       const char *vtable_name)
{
  unsigned long long start = get_cycle_count ();
  VTV_DEBUG_ASSERT (set_handle_ptr != NULL && *set_handle_ptr != NULL);
  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;

  increment_num_calls (&num_calls_to_verify_vtable);
  vtv_set_handle *handle_ptr;
  if (!is_set_handle_handle (*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

  if (vtv_sets::contains (vtbl_ptr, handle_ptr))
    {
      if (debug_verify_vtable)
        {
          if (verify_vtable_log_fd == -1)
            __vtv_open_log ("vtv_verify_vtable.log");
          __vtv_add_to_log (verify_vtable_log_fd,
			    "Verified %s %s value = %p\n",
			    set_symbol_name, vtable_name, vtable_ptr);
        }
    }
  else
    {
      /* We failed to find the vtable pointer in the set of valid
	 pointers.  Log the error data and call the failure
	 function.  */
      snprintf (debug_log_message, sizeof (debug_log_message),
		"Looking for %s in %s\n", vtable_name, set_symbol_name);
      __vtv_verify_fail_debug (set_handle_ptr, vtable_ptr, debug_log_message);

      /* Normally __vtv_verify_fail_debug will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replaced __vtv_verify_fail_debug
         with some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }
  accumulate_cycle_count (&verify_vtable_cycles, start);

  return vtable_ptr;
}

/* This routine initializes a set handle to a vtable set. It makes
   sure that there is only one set handle for a particular set by
   using a map from set name to pointer to set handle. Since there
   will be multiple copies of the pointer to the set handle (one per
   compilation unit that uses it), it makes sure to initialize all the
   pointers to the set handle so that the set handle is unique. To
   make this a little more efficient and avoid a level of indirection
   in some cases, the first pointer to handle for a particular handle
   becomes the handle itself and the other pointers will point to the
   set handle.  SET_HANDLE_PTR is the address of the vtable map
   variable, SET_SYMBOL_KEY is the hash table key (containing the name
   of the map variable and the hash value) and SIZE_HINT is a guess
   for the best initial size for the set of vtable pointers that
   SET_HANDLE_POINTER will point to.*/

static inline void
init_set_symbol (void **set_handle_ptr, const void *set_symbol_key,
                 size_t size_hint)
{
  vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;

  if (*handle_ptr != NULL)
    {
      if (!is_set_handle_handle (*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);
      vtv_sets::resize (size_hint, handle_ptr);
      return;
    }

  if (vtv_symbol_unification_map == NULL)
    vtv_symbol_unification_map = s2s::create (1024);

  vtv_symbol_key *symbol_key_ptr = (vtv_symbol_key *) set_symbol_key;
  const s2s::value_type *map_value_ptr =
                              vtv_symbol_unification_map->get (symbol_key_ptr);

  if (map_value_ptr != NULL)
    {
      if (*map_value_ptr == handle_ptr)
        vtv_sets::resize (size_hint, *map_value_ptr);
      else
        {
          /* The one level handle to the set already exists. So, we
             are adding one level of indirection here and we will
             store a pointer to the one level pointer here.  */
          vtv_set_handle_handle *handle_handle_ptr =
                                          (vtv_set_handle_handle *) handle_ptr;
          *handle_handle_ptr = set_handle_handle (*map_value_ptr);
          vtv_sets::resize (size_hint, *map_value_ptr);
        }
    }
  else
    {
      /* We will create a new set. So, in this case handle_ptr is the
         one level pointer to the set handle.  Create copy of map name
         in case the memory where this comes from gets unmapped by
         dlclose.  */
      size_t map_key_len = symbol_key_ptr->n + sizeof (vtv_symbol_key);
      void * map_key = __vtv_malloc (map_key_len);
      memcpy (map_key, symbol_key_ptr, map_key_len);

      s2s::value_type * value_ptr;
      vtv_symbol_unification_map =
        vtv_symbol_unification_map->find_or_add_key ((vtv_symbol_key *)map_key,
                                                     &value_ptr);

      *value_ptr = handle_ptr;

      /* TODO: We should verify the return value.  */
      vtv_sets::create (size_hint, handle_ptr);
    }
}

/* This routine initializes a set handle to a vtable set. It makes
   sure that there is only one set handle for a particular set by
   using a map from set name to pointer to set handle. Since there
   will be multiple copies of the pointer to the set handle (one per
   compilation unit that uses it), it makes sure to initialize all the
   pointers to the set handle so that the set handle is unique. To
   make this a little more efficient and avoid a level of indirection
   in some cases, the first pointer to handle for a particular handle
   becomes the handle itself and the other pointers will point to the
   set handle.  SET_HANDLE_PTR is the address of the vtable map
   variable, SET_SYMBOL_KEY is the hash table key (containing the name
   of the map variable and the hash value) and SIZE_HINT is a guess
   for the best initial size for the set of vtable pointers that
   SET_HANDLE_POINTER will point to.*/


void
__VLTRegisterSet (void **set_handle_ptr, const void *set_symbol_key,
                  size_t size_hint, size_t num_args, void **vtable_ptr_array)
{
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&num_calls_to_regset);

  init_set_symbol (set_handle_ptr, set_symbol_key, size_hint);
  register_set_common (set_handle_ptr, num_args, vtable_ptr_array, false);

  accumulate_cycle_count (&regset_cycles, start);
}



/* This function takes a the address of a vtable map variable
   (SET_HANDLE_PTR) and a VTABLE_PTR.  If the vtable map variable is
   NULL it creates a new data set and initializes the variable,
   otherwise it uses our symbol unification to find the right data
   set; in either case it then adds the vtable pointer to the set.  */

void
__VLTRegisterPair (void **set_handle_ptr, const  void *set_symbol_key,
                   size_t size_hint, const void *vtable_ptr)
{
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&num_calls_to_regpair);

  init_set_symbol (set_handle_ptr, set_symbol_key, size_hint);
  register_pair_common (set_handle_ptr, vtable_ptr, NULL, NULL,  false);

  accumulate_cycle_count (&regpair_cycles, start);
}

/* This is the main verification function.  It takes the address of a
   vtable map variable (SET_HANDLE_PTR) and a VTABLE_PTR to validate.
   It checks to see if VTABLE_PTR is in the set pointed to by
   SET_HANDLE_PTR.  If so, it returns VTABLE_PTR, otherwise it calls
   __vtv_verify_fail, which usually logs error messages and calls
   abort.  Since this function gets called VERY frequently, it is
   important for it to be as efficient as possible.  */

const void *
__VLTVerifyVtablePointer (void ** set_handle_ptr, const void * vtable_ptr)
{
  unsigned long long start = get_cycle_count ();
  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;

  vtv_set_handle *handle_ptr;
  increment_num_calls (&num_calls_to_verify_vtable);
  if (!is_set_handle_handle (*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

  if (!vtv_sets::contains (vtbl_ptr, handle_ptr))
    {
      __vtv_verify_fail ((void **) handle_ptr, vtable_ptr);
      /* Normally __vtv_verify_fail will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replaced __vtv_verify_fail with
         some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }
  accumulate_cycle_count (&verify_vtable_cycles, start);

  return vtable_ptr;
}

static int page_count_2 = 0;

#if !defined (__CYGWIN__) && !defined (__MINGW32__)
static int
dl_iterate_phdr_count_pages (struct dl_phdr_info *info,
                             size_t unused __attribute__ ((__unused__)),
                             void *data)
{
  int *mprotect_flags = (int *) data;
  off_t map_sect_offset = 0;
  ElfW (Word) map_sect_len = 0;
  const char *map_sect_name = VTV_PROTECTED_VARS_SECTION;

  /* Check to see if this is the record for the Linux Virtual Dynamic
     Shared Object (linux-vdso.so.1), which exists only in memory (and
     therefore cannot be read from disk).  */

  if (strcmp (info->dlpi_name, "linux-vdso.so.1") == 0)
    return 0;

  if (strlen (info->dlpi_name) == 0
      && info->dlpi_addr != 0)
    return 0;

  read_section_offset_and_length (info, map_sect_name, *mprotect_flags,
                                 &map_sect_offset, &map_sect_len);

  /* See if we actually found the section.  */
  if (map_sect_len)
    page_count_2 += (map_sect_len + VTV_PAGE_SIZE - 1) / VTV_PAGE_SIZE;

  return 0;
}
#endif

static void
count_all_pages (void)
{
  int mprotect_flags;

  mprotect_flags = PROT_READ;
  page_count_2 = 0;

#if defined (__CYGWIN__) || defined (__MINGW32__)
  iterate_modules ((void *) &mprotect_flags);
#else
  dl_iterate_phdr (dl_iterate_phdr_count_pages, (void *) &mprotect_flags);
#endif
  page_count_2 += __vtv_count_mmapped_pages ();
}

void
__VLTDumpStats (void)
{
  int log_fd = __vtv_open_log ("vtv-runtime-stats.log");

  if (log_fd != -1)
    {
      count_all_pages ();
      __vtv_add_to_log (log_fd,
			"Calls: mprotect (%d)  regset (%d) regpair (%d)"
			" verify_vtable (%d)\n",
			num_calls_to_mprotect, num_calls_to_regset,
			num_calls_to_regpair, num_calls_to_verify_vtable);
      __vtv_add_to_log (log_fd,
			"Cycles: mprotect (%lld) regset (%lld) "
			"regpair (%lld) verify_vtable (%lld)\n",
			mprotect_cycles, regset_cycles, regpair_cycles,
			verify_vtable_cycles);
      __vtv_add_to_log (log_fd,
			"Pages protected (1): %d\n", num_pages_protected);
      __vtv_add_to_log (log_fd, "Pages protected (2): %d\n", page_count_2);

      close (log_fd);
    }
}

/* This function is called from __VLTVerifyVtablePointerDebug; it
   sends as much debugging information as it can to the error log
   file, then calls __vtv_verify_fail.  SET_HANDLE_PTR is the pointer
   to the set of valid vtable pointers, VTBL_PTR is the pointer that
   was not found in the set, and DEBUG_MSG is the message to be
   written to the log file before failing. n */

void
__vtv_verify_fail_debug (void **set_handle_ptr, const void *vtbl_ptr, 
                         const char *debug_msg)
{
  __vtv_log_verification_failure (debug_msg, false);

  /* Call the public interface in case it has been overwritten by
     user.  */
  __vtv_verify_fail (set_handle_ptr, vtbl_ptr);

  __vtv_log_verification_failure ("Returned from __vtv_verify_fail."
                     " Secondary verification succeeded.\n", false);
}

/* This function calls __fortify_fail with a FAILURE_MSG and then
   calls abort.  */

void
__vtv_really_fail (const char *failure_msg)
{
  __fortify_fail (failure_msg);

  /* We should never get this far; __fortify_fail calls __libc_message
     which prints out a back trace and a memory dump and then is
     supposed to call abort, but let's play it safe anyway and call abort
     ourselves.  */
  abort ();
}

/* This function takes an error MSG, a vtable map variable
   (DATA_SET_PTR) and a vtable pointer (VTBL_PTR).  It is called when
   an attempt to verify VTBL_PTR with the set pointed to by
   DATA_SET_PTR failed.  It outputs a failure message with the
   addresses involved, and calls __vtv_really_fail.  */

static void
vtv_fail (const char *msg, void **data_set_ptr, const void *vtbl_ptr)
{
  char buffer[128];
  int buf_len;
  const char *format_str =
                 "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";

  snprintf (buffer, sizeof (buffer), format_str, vtbl_ptr,
            is_set_handle_handle(*data_set_ptr) ?
              ptr_from_set_handle_handle (*data_set_ptr) :
	      *data_set_ptr);
  buf_len = strlen (buffer);
  /* Send this to stderr.  */
  write (2, buffer, buf_len);

#ifndef VTV_NO_ABORT
    __vtv_really_fail (msg);
#endif
}

/* Send information about what we were trying to do when verification
   failed to the error log, then call vtv_fail.  This function can be
   overwritten/replaced by the user, to implement a secondary
   verification function instead.  DATA_SET_PTR is the vtable map
   variable used for the failed verification, and VTBL_PTR is the
   vtable pointer that was not found in the set.  */

void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_ptr)
{
  char log_msg[256];
  snprintf (log_msg, sizeof (log_msg), "Looking for vtable %p in set %p.\n",
            vtbl_ptr,
            is_set_handle_handle (*data_set_ptr) ?
              ptr_from_set_handle_handle (*data_set_ptr) :
              *data_set_ptr);
  __vtv_log_verification_failure (log_msg, false);

  const char *format_str =
            "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";
  snprintf (log_msg, sizeof (log_msg), format_str, vtbl_ptr, *data_set_ptr);
  __vtv_log_verification_failure (log_msg, false);
  __vtv_log_verification_failure ("  Backtrace: \n", true);

  const char *fail_msg = "Potential vtable pointer corruption detected!!\n";
  vtv_fail (fail_msg, data_set_ptr, vtbl_ptr);
}
