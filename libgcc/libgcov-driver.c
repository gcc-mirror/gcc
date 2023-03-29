/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgcov.h"
#include "gcov-io.h"

/* Return 1, if all counter values are zero, otherwise 0. */

static inline int
are_all_counters_zero (const struct gcov_ctr_info *ci_ptr)
{
  for (unsigned i = 0; i < ci_ptr->num; i++)
    if (ci_ptr->values[i] != 0)
      return 0;

  return 1;
}

#if defined(inhibit_libc)
/* If libc and its header files are not available, provide dummy functions.  */

#if defined(L_gcov)
void __gcov_init (struct gcov_info *p __attribute__ ((unused))) {}
#endif

#else /* inhibit_libc */

#if GCOV_LOCKED
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#elif GCOV_LOCKED_WITH_LOCKING
#include <fcntl.h>
#include <sys/locking.h>
#include <sys/stat.h>
#endif

#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#endif /* inhibit_libc */

#if defined(L_gcov) && !defined(inhibit_libc)
#define NEED_L_GCOV
#endif

#if defined(L_gcov_info_to_gcda) && !IN_GCOV_TOOL
#define NEED_L_GCOV_INFO_TO_GCDA
#endif

#ifdef NEED_L_GCOV
/* A utility function for outputting errors.  */
static int gcov_error (const char *, ...);

#if !IN_GCOV_TOOL
static void gcov_error_exit (void);
#endif

#include "gcov-io.cc"

#define GCOV_PROF_PREFIX "libgcov profiling error:%s:"

struct gcov_fn_buffer
{
  struct gcov_fn_buffer *next;
  unsigned fn_ix;
  struct gcov_fn_info info;
  /* note gcov_fn_info ends in a trailing array.  */
};

struct gcov_summary_buffer
{
  struct gcov_summary_buffer *next;
  struct gcov_summary summary;
};

/* A struct that bundles all the related information about the
   gcda filename.  */

struct gcov_filename
{
  char *filename;  /* filename buffer */
  int strip; /* leading chars to strip from filename */
  char *prefix; /* prefix string */
};

static struct gcov_fn_buffer *
free_fn_data (const struct gcov_info *gi_ptr, struct gcov_fn_buffer *buffer,
              unsigned limit)
{
  struct gcov_fn_buffer *next;
  unsigned ix, n_ctr = 0;

  if (!buffer)
    return 0;
  next = buffer->next;

  for (ix = 0; ix != limit; ix++)
    if (gi_ptr->merge[ix])
      free (buffer->info.ctrs[n_ctr++].values);
  free (buffer);
  return next;
}

static struct gcov_fn_buffer **
buffer_fn_data (const char *filename, const struct gcov_info *gi_ptr,
                struct gcov_fn_buffer **end_ptr, unsigned fn_ix)
{
  unsigned n_ctrs = 0, ix = 0;
  struct gcov_fn_buffer *fn_buffer;
  unsigned len;

  for (ix = GCOV_COUNTERS; ix--;)
    if (gi_ptr->merge[ix])
      n_ctrs++;

  len = sizeof (*fn_buffer) + sizeof (fn_buffer->info.ctrs[0]) * n_ctrs;
  fn_buffer = (struct gcov_fn_buffer *) xmalloc (len);

  if (!fn_buffer)
    goto fail;

  fn_buffer->next = 0;
  fn_buffer->fn_ix = fn_ix;
  fn_buffer->info.ident = gcov_read_unsigned ();
  fn_buffer->info.lineno_checksum = gcov_read_unsigned ();
  fn_buffer->info.cfg_checksum = gcov_read_unsigned ();

  for (n_ctrs = ix = 0; ix != GCOV_COUNTERS; ix++)
    {
      gcov_unsigned_t length;
      gcov_type *values;

      if (!gi_ptr->merge[ix])
        continue;

      if (gcov_read_unsigned () != GCOV_TAG_FOR_COUNTER (ix))
        {
          len = 0;
          goto fail;
        }

      length = GCOV_TAG_COUNTER_NUM (gcov_read_unsigned ());
      len = length * sizeof (gcov_type);
      values = (gcov_type *) xmalloc (len);
      if (!values)
        goto fail;

      fn_buffer->info.ctrs[n_ctrs].num = length;
      fn_buffer->info.ctrs[n_ctrs].values = values;

      while (length--)
        *values++ = gcov_read_counter ();
      n_ctrs++;
    }

  *end_ptr = fn_buffer;
  return &fn_buffer->next;

fail:
  gcov_error (GCOV_PROF_PREFIX "Function %u %s %u \n", filename, fn_ix,
              len ? "cannot allocate" : "counter mismatch", len ? len : ix);

  return (struct gcov_fn_buffer **)free_fn_data (gi_ptr, fn_buffer, ix);
}

/* Convert VERSION into a string description and return the it.
   BUFFER is used for storage of the string.  The code should be
   aligned wit gcov-iov.c.  */

static char *
gcov_version_string (char *buffer, char version[4])
{
  if (version[0] < 'A' || version[0] > 'Z'
      || version[1] < '0' || version[1] > '9'
      || version[2] < '0' || version[2] > '9')
    sprintf (buffer, "(unknown)");
  else
    {
      unsigned major = 10 * (version[0] - 'A') + (version[1] - '0');
      unsigned minor = version[2] - '0';
      sprintf (buffer, "%u.%u (%s)", major, minor,
	       version[3] == '*' ? "release" : "experimental");
    }
  return buffer;
}

/* Check if VERSION of the info block PTR matches libgcov one.
   Return 1 on success, or zero in case of versions mismatch.
   If FILENAME is not NULL, its value used for reporting purposes
   instead of value from the info block.  */

static int
gcov_version (struct gcov_info *ptr, gcov_unsigned_t version,
              const char *filename)
{
  if (version != GCOV_VERSION)
    {
      char v[4], e[4];
      char ver_string[128], expected_string[128];

      GCOV_UNSIGNED2STRING (v, version);
      GCOV_UNSIGNED2STRING (e, GCOV_VERSION);

      gcov_error (GCOV_PROF_PREFIX "Version mismatch - expected %s (%.4s) "
		  "got %s (%.4s)\n",
		  filename? filename : ptr->filename,
		  gcov_version_string (expected_string, e), e,
		  gcov_version_string (ver_string, v), v);
      return 0;
    }
  return 1;
}

/* buffer for the fn_data from another program.  */
static struct gcov_fn_buffer *fn_buffer;

/* Including system dependent components. */
#include "libgcov-driver-system.c"

/* This function merges counters in GI_PTR to an existing gcda file.
   Return 0 on success.
   Return -1 on error. In this case, caller will goto read_fatal.  */

static int
merge_one_data (const char *filename,
		struct gcov_info *gi_ptr,
		struct gcov_summary *summary)
{
  gcov_unsigned_t tag, length;
  unsigned t_ix;
  int f_ix = -1;
  int error = 0;
  struct gcov_fn_buffer **fn_tail = &fn_buffer;

  length = gcov_read_unsigned ();
  if (!gcov_version (gi_ptr, length, filename))
    return -1;

  /* Skip timestamp.  */
  gcov_read_unsigned ();

  length = gcov_read_unsigned ();
  if (length != gi_ptr->checksum)
    {
      /* Read from a different compilation.  Overwrite the file.  */
      gcov_error (GCOV_PROF_PREFIX "overwriting an existing profile data "
		  "with a different checksum\n", filename);
      return 0;
    }

  tag = gcov_read_unsigned ();
  if (tag != GCOV_TAG_OBJECT_SUMMARY)
    goto read_mismatch;
  length = gcov_read_unsigned ();
  gcc_assert (length > 0);
  gcov_read_summary (summary);

  tag = gcov_read_unsigned ();
  /* Merge execution counts for each function.  */
  for (f_ix = 0; (unsigned)f_ix != gi_ptr->n_functions;
       f_ix++, tag = gcov_read_unsigned ())
    {
      const struct gcov_ctr_info *ci_ptr;
      const struct gcov_fn_info *gfi_ptr = gi_ptr->functions[f_ix];

      if (tag != GCOV_TAG_FUNCTION)
        goto read_mismatch;

      length = gcov_read_unsigned ();
      if (!length)
        /* This function did not appear in the other program.
           We have nothing to merge.  */
        continue;

      if (length != GCOV_TAG_FUNCTION_LENGTH)
        goto read_mismatch;

      if (!gfi_ptr || gfi_ptr->key != gi_ptr)
        {
          /* This function appears in the other program.  We
             need to buffer the information in order to write
             it back out -- we'll be inserting data before
             this point, so cannot simply keep the data in the
             file.  */
          fn_tail = buffer_fn_data (filename, gi_ptr, fn_tail, f_ix);
          if (!fn_tail)
            goto read_mismatch;
          continue;
        }

      length = gcov_read_unsigned ();
      if (length != gfi_ptr->ident)
        goto read_mismatch;

      length = gcov_read_unsigned ();
      if (length != gfi_ptr->lineno_checksum)
        goto read_mismatch;

      length = gcov_read_unsigned ();
      if (length != gfi_ptr->cfg_checksum)
        goto read_mismatch;

      ci_ptr = gfi_ptr->ctrs;
      for (t_ix = 0; t_ix < GCOV_COUNTERS; t_ix++)
        {
          gcov_merge_fn merge = gi_ptr->merge[t_ix];

          if (!merge)
            continue;

	  tag = gcov_read_unsigned ();
	  int read_length = (int)gcov_read_unsigned ();
	  length = abs (read_length);
	  if (tag != GCOV_TAG_FOR_COUNTER (t_ix)
	      || (length != GCOV_TAG_COUNTER_LENGTH (ci_ptr->num)
		  && t_ix != GCOV_COUNTER_V_TOPN
		  && t_ix != GCOV_COUNTER_V_INDIR))
	    goto read_mismatch;
	  /* Merging with all zero counters does not make sense.  */
	  if (read_length > 0)
	    (*merge) (ci_ptr->values, ci_ptr->num);
	  ci_ptr++;
	}
      if ((error = gcov_is_error ()))
	goto read_error;
    }

  if (tag)
    {
    read_mismatch:;
      gcov_error (GCOV_PROF_PREFIX "Merge mismatch for %s %u\n",
                  filename, f_ix >= 0 ? "function" : "summary",
                  f_ix < 0 ? -1 - f_ix : f_ix);
      return -1;
    }
  return 0;

read_error:
  gcov_error (GCOV_PROF_PREFIX "%s merging\n", filename,
              error < 0 ? "Overflow": "Error");
  return -1;
}

/* Write the DATA of LENGTH characters to the gcov file.  */

static void
gcov_dump_handler (const void *data,
		   unsigned length,
		   void *arg ATTRIBUTE_UNUSED)
{
  gcov_write (data, length);
}

/* Allocate SIZE characters and return the address of the allocated memory.  */

static void *
gcov_allocate_handler (unsigned size, void *arg ATTRIBUTE_UNUSED)
{
  return xmalloc (size);
}
#endif /* NEED_L_GCOV */

#if defined(NEED_L_GCOV) || defined(NEED_L_GCOV_INFO_TO_GCDA)
/* Dump the WORD using the DUMP handler called with ARG.  */

static inline void
dump_unsigned (gcov_unsigned_t word,
	       void (*dump_fn) (const void *, unsigned, void *),
	       void *arg)
{
  (*dump_fn) (&word, sizeof (word), arg);
}

/* Dump the COUNTER using the DUMP handler called with ARG.  */

static inline void
dump_counter (gcov_type counter,
	      void (*dump_fn) (const void *, unsigned, void *),
	      void *arg)
{
  dump_unsigned ((gcov_unsigned_t)counter, dump_fn, arg);

  if (sizeof (counter) > sizeof (gcov_unsigned_t))
    dump_unsigned ((gcov_unsigned_t)(counter >> 32), dump_fn, arg);
  else
    dump_unsigned (0, dump_fn, arg);
}

/* Dump the STRING using the DUMP handler called with ARG.  */

static inline void
ATTRIBUTE_UNUSED
dump_string (const char *string,
	     void (*dump_fn) (const void *, unsigned, void *),
	     void *arg)
{
  unsigned length = 0;

  if (string)
    length = strlen (string) + 1;

  dump_unsigned (length, dump_fn, arg);
  if (string)
    (*dump_fn) (string, length, arg);
}

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

/* Store all TOP N counters where each has a dynamic length.  */

static void
write_topn_counters (const struct gcov_ctr_info *ci_ptr,
		     unsigned t_ix,
		     gcov_unsigned_t n_counts,
		     void (*dump_fn) (const void *, unsigned, void *),
		     void *(*allocate_fn)(unsigned, void *),
		     void *arg)
{
  unsigned counters = n_counts / GCOV_TOPN_MEM_COUNTERS;
  gcc_assert (n_counts % GCOV_TOPN_MEM_COUNTERS == 0);

  /* It can happen in a multi-threaded environment that number of counters is
     different from the size of the corresponding linked lists.  */
#define LIST_SIZE_MIN_LENGTH 4 * 1024

  static unsigned *list_sizes = NULL;
  static unsigned list_size_length = 0;

  if (list_sizes == NULL || counters > list_size_length)
    {
      list_size_length = MAX (LIST_SIZE_MIN_LENGTH, 2 * counters);
#if !defined(inhibit_libc) && HAVE_SYS_MMAN_H
      list_sizes
	= (unsigned *)malloc_mmap (list_size_length * sizeof (unsigned));
#endif

      /* Malloc fallback.  */
      if (list_sizes == NULL)
	list_sizes =
	  (unsigned *)(*allocate_fn) (list_size_length * sizeof (unsigned),
				      arg);
    }

  unsigned pair_total = 0;

  for (unsigned i = 0; i < counters; i++)
    {
      gcov_type start = ci_ptr->values[GCOV_TOPN_MEM_COUNTERS * i + 2];
      unsigned sizes = 0;

      for (struct gcov_kvp *node = (struct gcov_kvp *)(__INTPTR_TYPE__)start;
	   node != NULL; node = node->next)
	++sizes;

      pair_total += sizes;
      list_sizes[i] = sizes;
    }

  unsigned disk_size = GCOV_TOPN_DISK_COUNTERS * counters + 2 * pair_total;
  dump_unsigned (GCOV_TAG_FOR_COUNTER (t_ix), dump_fn, arg),
  dump_unsigned (GCOV_TAG_COUNTER_LENGTH (disk_size), dump_fn, arg);

  for (unsigned i = 0; i < counters; i++)
    {
      dump_counter (ci_ptr->values[GCOV_TOPN_MEM_COUNTERS * i], dump_fn, arg);
      dump_counter (list_sizes[i], dump_fn, arg);
      gcov_type start = ci_ptr->values[GCOV_TOPN_MEM_COUNTERS * i + 2];

      unsigned j = 0;
      for (struct gcov_kvp *node = (struct gcov_kvp *)(__INTPTR_TYPE__)start;
	   j < list_sizes[i]; node = node->next, j++)
	{
	  dump_counter (node->value, dump_fn, arg);
	  dump_counter (node->count, dump_fn, arg);
	}
    }
}

/* Write counters in GI_PTR and the summary in PRG to a gcda file. In
   the case of appending to an existing file, SUMMARY_POS will be non-zero.
   We will write the file starting from SUMMAY_POS.  */

static void
write_one_data (const struct gcov_info *gi_ptr,
		const struct gcov_summary *prg_p ATTRIBUTE_UNUSED,
		void (*dump_fn) (const void *, unsigned, void *),
		void *(*allocate_fn) (unsigned, void *),
		void *arg)
{
  unsigned f_ix;

  dump_unsigned (GCOV_DATA_MAGIC, dump_fn, arg);
  dump_unsigned (GCOV_VERSION, dump_fn, arg);
  dump_unsigned (gi_ptr->stamp, dump_fn, arg);
  dump_unsigned (gi_ptr->checksum, dump_fn, arg);

#ifdef NEED_L_GCOV
  /* Generate whole program statistics.  */
  gcov_write_object_summary (prg_p);
#endif

  /* Write execution counts for each function.  */
  for (f_ix = 0; f_ix != gi_ptr->n_functions; f_ix++)
    {
#ifdef NEED_L_GCOV
      unsigned buffered = 0;
#endif
      const struct gcov_fn_info *gfi_ptr;
      const struct gcov_ctr_info *ci_ptr;
      gcov_unsigned_t length;
      unsigned t_ix;

#ifdef NEED_L_GCOV
      if (fn_buffer && fn_buffer->fn_ix == f_ix)
        {
          /* Buffered data from another program.  */
          buffered = 1;
          gfi_ptr = &fn_buffer->info;
          length = GCOV_TAG_FUNCTION_LENGTH;
        }
      else
#endif
        {
          gfi_ptr = gi_ptr->functions[f_ix];
          if (gfi_ptr && gfi_ptr->key == gi_ptr)
            length = GCOV_TAG_FUNCTION_LENGTH;
          else
                length = 0;
        }

      dump_unsigned (GCOV_TAG_FUNCTION, dump_fn, arg);
      dump_unsigned (length, dump_fn, arg);
      if (!length)
        continue;

      dump_unsigned (gfi_ptr->ident, dump_fn, arg);
      dump_unsigned (gfi_ptr->lineno_checksum, dump_fn, arg);
      dump_unsigned (gfi_ptr->cfg_checksum, dump_fn, arg);

      ci_ptr = gfi_ptr->ctrs;
      for (t_ix = 0; t_ix < GCOV_COUNTERS; t_ix++)
        {
	  gcov_position_t n_counts;

	  if (!gi_ptr->merge[t_ix])
	    continue;

	  n_counts = ci_ptr->num;

	  if (t_ix == GCOV_COUNTER_V_TOPN || t_ix == GCOV_COUNTER_V_INDIR)
	    write_topn_counters (ci_ptr, t_ix, n_counts, dump_fn, allocate_fn,
				 arg);
	  else
	    {
	      dump_unsigned (GCOV_TAG_FOR_COUNTER (t_ix), dump_fn, arg);
	      if (are_all_counters_zero (ci_ptr))
		/* Do not stream when all counters are zero.  */
		dump_unsigned (GCOV_TAG_COUNTER_LENGTH (-n_counts),
			       dump_fn, arg);
	      else
		{
		  dump_unsigned (GCOV_TAG_COUNTER_LENGTH (n_counts),
				 dump_fn, arg);
		  for (unsigned i = 0; i < n_counts; i++)
		    dump_counter (ci_ptr->values[i], dump_fn, arg);
		}
	    }

	  ci_ptr++;
	}
#ifdef NEED_L_GCOV
      if (buffered)
        fn_buffer = free_fn_data (gi_ptr, fn_buffer, GCOV_COUNTERS);
#endif
    }

  dump_unsigned (0, dump_fn, arg);
}
#endif /* NEED_L_GCOV || NEED_L_GCOV_INFO_TO_GCDA */

#ifdef NEED_L_GCOV
/* Dump the coverage counts for one gcov_info object. We merge with existing
   counts when possible, to avoid growing the .da files ad infinitum. We use
   this program's checksum to make sure we only accumulate whole program
   statistics to the correct summary. An object file might be embedded
   in two separate programs, and we must keep the two program
   summaries separate.  */

static void
dump_one_gcov (struct gcov_info *gi_ptr, struct gcov_filename *gf,
	       unsigned run_counted ATTRIBUTE_UNUSED,
	       gcov_type run_max ATTRIBUTE_UNUSED, int mode)
{
  struct gcov_summary summary = {};
  int error;
  gcov_unsigned_t tag;
  fn_buffer = 0;

  error = gcov_exit_open_gcda_file (gi_ptr, gf, mode);
  if (error == -1)
    return;

  tag = gcov_read_unsigned ();
  if (tag)
    {
      /* Merge data from file.  */
      if (tag != GCOV_DATA_MAGIC)
        {
	  gcov_error (GCOV_PROF_PREFIX "Not a gcov data file\n",
		      gf->filename);
          goto read_fatal;
        }
      error = merge_one_data (gf->filename, gi_ptr, &summary);
      if (error == -1)
        goto read_fatal;
    }

  gcov_rewrite ();

#if !IN_GCOV_TOOL
  if (!run_counted)
    {
      summary.runs++;
      summary.sum_max += run_max;
    }
#else
  summary = gi_ptr->summary;
#endif

  write_one_data (gi_ptr, &summary, gcov_dump_handler, gcov_allocate_handler,
		  NULL);
  /* fall through */

read_fatal:;
  while (fn_buffer)
    fn_buffer = free_fn_data (gi_ptr, fn_buffer, GCOV_COUNTERS);

  if ((error = gcov_close ()))
    gcov_error ((error < 0 ? GCOV_PROF_PREFIX "Overflow writing\n"
		 : GCOV_PROF_PREFIX "Error writing\n"), gf->filename);
}


/* Dump all the coverage counts for the program. It first computes program
   summary and then traverses gcov_list list and dumps the gcov_info
   objects one by one.  Use MODE to open files.  */

#if !IN_GCOV_TOOL
static
#endif
void
gcov_do_dump (struct gcov_info *list, int run_counted, int mode)
{
  struct gcov_info *gi_ptr;
  struct gcov_filename gf;

  /* Compute run_max of this program run.  */
  gcov_type run_max = 0;
  for (gi_ptr = list; gi_ptr; gi_ptr = gi_ptr->next)
    for (unsigned f_ix = 0; (unsigned)f_ix != gi_ptr->n_functions; f_ix++)
      {
	const struct gcov_ctr_info *cinfo
	  = &gi_ptr->functions[f_ix]->ctrs[GCOV_COUNTER_ARCS];

	for (unsigned i = 0; i < cinfo->num; i++)
	  if (run_max < cinfo->values[i])
	    run_max = cinfo->values[i];
      }

  allocate_filename_struct (&gf);

  /* Now merge each file.  */
  for (gi_ptr = list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      dump_one_gcov (gi_ptr, &gf, run_counted, run_max, mode);
      free (gf.filename);
    }

  free (gf.prefix);
}

#if IN_GCOV_TOOL
const char *
__attribute__ ((unused))
gcov_get_filename (struct gcov_info *list)
{
  return list->filename;
}
#endif

#if !IN_GCOV_TOOL
void
__gcov_dump_one (struct gcov_root *root)
{
  if (root->dumped)
    return;

  gcov_do_dump (root->list, root->run_counted, 0);
  
  root->dumped = 1;
  root->run_counted = 1;
}

/* Per-dynamic-object gcov state.  */
struct gcov_root __gcov_root;

/* Exactly one of these will be live in the process image.  */
struct gcov_master __gcov_master = 
  {GCOV_VERSION, 0};

/* Dynamic pool for gcov_kvp structures.  */
struct gcov_kvp *__gcov_kvp_dynamic_pool;

/* Index into __gcov_kvp_dynamic_pool array.  */
unsigned __gcov_kvp_dynamic_pool_index;

/* Size of _gcov_kvp_dynamic_pool array.  */
unsigned __gcov_kvp_dynamic_pool_size;

void
__gcov_exit (void)
{
  __gcov_dump_one (&__gcov_root);
  if (__gcov_root.next)
    __gcov_root.next->prev = __gcov_root.prev;
  if (__gcov_root.prev)
    __gcov_root.prev->next = __gcov_root.next;
  else
    __gcov_master.root = __gcov_root.next;

  gcov_error_exit ();
}

/* Add a new object file onto the bb chain.  Invoked automatically
  when running an object file's global ctors.  */

void
__gcov_init (struct gcov_info *info)
{
  if (!info->version || !info->n_functions)
    return;
  if (gcov_version (info, info->version, 0))
    {
      if (!__gcov_root.list)
	{
	  /* Add to master list and at exit function.  */
	  if (gcov_version (NULL, __gcov_master.version, "<master>"))
	    {
	      __gcov_root.next = __gcov_master.root;
	      if (__gcov_master.root)
		__gcov_master.root->prev = &__gcov_root;
	      __gcov_master.root = &__gcov_root;
	    }
	}

      info->next = __gcov_root.list;
      __gcov_root.list = info;
    }
}
#endif /* !IN_GCOV_TOOL */
#endif /* NEED_L_GCOV */

#ifdef NEED_L_GCOV_INFO_TO_GCDA
/* Convert the gcov info to a gcda data stream.  It is intended for
   freestanding environments which do not support the C library file I/O.  */

void
__gcov_info_to_gcda (const struct gcov_info *gi_ptr,
		     void (*filename_fn) (const char *, void *),
		     void (*dump_fn) (const void *, unsigned, void *),
		     void *(*allocate_fn) (unsigned, void *),
		     void *arg)
{
  (*filename_fn) (gi_ptr->filename, arg);
  write_one_data (gi_ptr, NULL, dump_fn, allocate_fn, arg);
}

/* Convert the filename to a gcfn data stream.  It is intended for
   freestanding environments which do not support the C library file I/O.  */

void
__gcov_filename_to_gcfn (const char *filename,
			 void (*dump_fn) (const void *, unsigned, void *),
			 void *arg)
{
  dump_unsigned (GCOV_FILENAME_MAGIC, dump_fn, arg);
  dump_unsigned (GCOV_VERSION, dump_fn, arg);
  dump_string (filename, dump_fn, arg);
}
#endif /* NEED_L_GCOV_INFO_TO_GCDA */
