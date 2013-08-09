/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2013 Free Software Foundation, Inc.

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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "gthr.h"

#if defined(inhibit_libc)
#define IN_LIBGCOV (-1)
#else
#define IN_LIBGCOV 1
#if defined(L_gcov)
#define GCOV_LINKAGE /* nothing */
#endif
#endif
#include "gcov-io.h"

#if defined(inhibit_libc)
/* If libc and its header files are not available, provide dummy functions.  */

#ifdef L_gcov
void __gcov_init (struct gcov_info *p __attribute__ ((unused))) {}
void __gcov_flush (void) {}
#endif

#ifdef L_gcov_reset
void __gcov_reset (void) {}
#endif

#ifdef L_gcov_dump
void __gcov_dump (void) {}
#endif

#ifdef L_gcov_merge_add
void __gcov_merge_add (gcov_type *counters  __attribute__ ((unused)),
		       unsigned n_counters __attribute__ ((unused))) {}
#endif

#ifdef L_gcov_merge_single
void __gcov_merge_single (gcov_type *counters  __attribute__ ((unused)),
			  unsigned n_counters __attribute__ ((unused))) {}
#endif

#ifdef L_gcov_merge_delta
void __gcov_merge_delta (gcov_type *counters  __attribute__ ((unused)),
			 unsigned n_counters __attribute__ ((unused))) {}
#endif

#else

#include <string.h>
#if GCOV_LOCKED
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#endif

extern void gcov_clear (void) ATTRIBUTE_HIDDEN;
extern void gcov_exit (void) ATTRIBUTE_HIDDEN;
extern int gcov_dump_complete ATTRIBUTE_HIDDEN;

#ifdef L_gcov
#include "gcov-io.c"

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

/* Chain of per-object gcov structures.  */
static struct gcov_info *gcov_list;

/* Size of the longest file name. */
static size_t gcov_max_filename = 0;

/* Flag when the profile has already been dumped via __gcov_dump().  */
int gcov_dump_complete = 0;

/* Make sure path component of the given FILENAME exists, create
   missing directories. FILENAME must be writable.
   Returns zero on success, or -1 if an error occurred.  */

static int
create_file_directory (char *filename)
{
#if !defined(TARGET_POSIX_IO) && !defined(_WIN32)
  (void) filename;
  return -1;
#else
  char *s;

  s = filename;

  if (HAS_DRIVE_SPEC(s))
    s += 2;
  if (IS_DIR_SEPARATOR(*s))
    ++s;
  for (; *s != '\0'; s++)
    if (IS_DIR_SEPARATOR(*s))
      {
        char sep = *s;
	*s  = '\0';

        /* Try to make directory if it doesn't already exist.  */
        if (access (filename, F_OK) == -1
#ifdef TARGET_POSIX_IO
            && mkdir (filename, 0755) == -1
#else
            && mkdir (filename) == -1
#endif
            /* The directory might have been made by another process.  */
	    && errno != EEXIST)
	  {
            fprintf (stderr, "profiling:%s:Cannot create directory\n",
		     filename);
            *s = sep;
	    return -1;
	  };

	*s = sep;
      };
  return 0;
#endif
}

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
  fn_buffer = (struct gcov_fn_buffer *)malloc (len);

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
      values = (gcov_type *)malloc (len);
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
  fprintf (stderr, "profiling:%s:Function %u %s %u \n", filename, fn_ix,
	   len ? "cannot allocate" : "counter mismatch", len ? len : ix);

  return (struct gcov_fn_buffer **)free_fn_data (gi_ptr, fn_buffer, ix);
}

/* Add an unsigned value to the current crc */

static gcov_unsigned_t
crc32_unsigned (gcov_unsigned_t crc32, gcov_unsigned_t value)
{
  unsigned ix;

  for (ix = 32; ix--; value <<= 1)
    {
      unsigned feedback;

      feedback = (value ^ crc32) & 0x80000000 ? 0x04c11db7 : 0;
      crc32 <<= 1;
      crc32 ^= feedback;
    }

  return crc32;
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

      GCOV_UNSIGNED2STRING (v, version);
      GCOV_UNSIGNED2STRING (e, GCOV_VERSION);

      fprintf (stderr,
	       "profiling:%s:Version mismatch - expected %.4s got %.4s\n",
	       filename? filename : ptr->filename, e, v);
      return 0;
    }
  return 1;
}

/* Insert counter VALUE into HISTOGRAM.  */

static void
gcov_histogram_insert(gcov_bucket_type *histogram, gcov_type value)
{
  unsigned i;

  i = gcov_histo_index(value);
  histogram[i].num_counters++;
  histogram[i].cum_value += value;
  if (value < histogram[i].min_value)
    histogram[i].min_value = value;
}

/* Computes a histogram of the arc counters to place in the summary SUM.  */

static void
gcov_compute_histogram (struct gcov_summary *sum)
{
  struct gcov_info *gi_ptr;
  const struct gcov_fn_info *gfi_ptr;
  const struct gcov_ctr_info *ci_ptr;
  struct gcov_ctr_summary *cs_ptr;
  unsigned t_ix, f_ix, ctr_info_ix, ix;
  int h_ix;

  /* This currently only applies to arc counters.  */
  t_ix = GCOV_COUNTER_ARCS;

  /* First check if there are any counts recorded for this counter.  */
  cs_ptr = &(sum->ctrs[t_ix]);
  if (!cs_ptr->num)
    return;

  for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
    {
      cs_ptr->histogram[h_ix].num_counters = 0;
      cs_ptr->histogram[h_ix].min_value = cs_ptr->run_max;
      cs_ptr->histogram[h_ix].cum_value = 0;
    }

  /* Walk through all the per-object structures and record each of
     the count values in histogram.  */
  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      if (!gi_ptr->merge[t_ix])
        continue;

      /* Find the appropriate index into the gcov_ctr_info array
         for the counter we are currently working on based on the
         existence of the merge function pointer for this object.  */
      for (ix = 0, ctr_info_ix = 0; ix < t_ix; ix++)
        {
          if (gi_ptr->merge[ix])
            ctr_info_ix++;
        }
      for (f_ix = 0; f_ix != gi_ptr->n_functions; f_ix++)
        {
          gfi_ptr = gi_ptr->functions[f_ix];

          if (!gfi_ptr || gfi_ptr->key != gi_ptr)
            continue;

          ci_ptr = &gfi_ptr->ctrs[ctr_info_ix];
          for (ix = 0; ix < ci_ptr->num; ix++)
            gcov_histogram_insert (cs_ptr->histogram, ci_ptr->values[ix]);
        }
    }
}

/* Dump the coverage counts. We merge with existing counts when
   possible, to avoid growing the .da files ad infinitum. We use this
   program's checksum to make sure we only accumulate whole program
   statistics to the correct summary. An object file might be embedded
   in two separate programs, and we must keep the two program
   summaries separate.  */

void
gcov_exit (void)
{
  struct gcov_info *gi_ptr;
  const struct gcov_fn_info *gfi_ptr;
  struct gcov_summary this_prg; /* summary for program.  */
#if !GCOV_LOCKED
  struct gcov_summary all_prg;  /* summary for all instances of program.  */
#endif
  struct gcov_ctr_summary *cs_ptr;
  const struct gcov_ctr_info *ci_ptr;
  unsigned t_ix;
  int f_ix;
  gcov_unsigned_t c_num;
  const char *gcov_prefix;
  int gcov_prefix_strip = 0;
  size_t prefix_length;
  char *gi_filename, *gi_filename_up;
  gcov_unsigned_t crc32 = 0;

  /* Prevent the counters from being dumped a second time on exit when the
     application already wrote out the profile using __gcov_dump().  */
  if (gcov_dump_complete)
    return;

#if !GCOV_LOCKED
  memset (&all_prg, 0, sizeof (all_prg));
#endif
  /* Find the totals for this execution.  */
  memset (&this_prg, 0, sizeof (this_prg));
  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      crc32 = crc32_unsigned (crc32, gi_ptr->stamp);
      crc32 = crc32_unsigned (crc32, gi_ptr->n_functions);
      
      for (f_ix = 0; (unsigned)f_ix != gi_ptr->n_functions; f_ix++)
	{
	  gfi_ptr = gi_ptr->functions[f_ix];

	  if (gfi_ptr && gfi_ptr->key != gi_ptr)
	    gfi_ptr = 0;
	  
	  crc32 = crc32_unsigned (crc32, gfi_ptr ? gfi_ptr->cfg_checksum : 0);
	  crc32 = crc32_unsigned (crc32,
				  gfi_ptr ? gfi_ptr->lineno_checksum : 0);
	  if (!gfi_ptr)
	    continue;

	  ci_ptr = gfi_ptr->ctrs;
	  for (t_ix = 0; t_ix != GCOV_COUNTERS_SUMMABLE; t_ix++)
	    {
	      if (!gi_ptr->merge[t_ix])
		continue;

	      cs_ptr = &this_prg.ctrs[t_ix];
	      cs_ptr->num += ci_ptr->num;
	      crc32 = crc32_unsigned (crc32, ci_ptr->num);
	      
	      for (c_num = 0; c_num < ci_ptr->num; c_num++)
		{
		  cs_ptr->sum_all += ci_ptr->values[c_num];
		  if (cs_ptr->run_max < ci_ptr->values[c_num])
		    cs_ptr->run_max = ci_ptr->values[c_num];
		}
	      ci_ptr++;
	    }
	}
    }
  gcov_compute_histogram (&this_prg);

  {
    /* Check if the level of dirs to strip off specified. */
    char *tmp = getenv("GCOV_PREFIX_STRIP");
    if (tmp)
      {
	gcov_prefix_strip = atoi (tmp);
	/* Do not consider negative values. */
	if (gcov_prefix_strip < 0)
	  gcov_prefix_strip = 0;
      }
  }

  /* Get file name relocation prefix.  Non-absolute values are ignored. */
  gcov_prefix = getenv("GCOV_PREFIX");
  if (gcov_prefix)
    {
      prefix_length = strlen(gcov_prefix);

      /* Remove an unnecessary trailing '/' */
      if (IS_DIR_SEPARATOR (gcov_prefix[prefix_length - 1]))
	prefix_length--;
    }
  else
    prefix_length = 0;

  /* If no prefix was specified and a prefix stip, then we assume
     relative.  */
  if (gcov_prefix_strip != 0 && prefix_length == 0)
    {
      gcov_prefix = ".";
      prefix_length = 1;
    }
  /* Allocate and initialize the filename scratch space plus one.  */
  gi_filename = (char *) alloca (prefix_length + gcov_max_filename + 2);
  if (prefix_length)
    memcpy (gi_filename, gcov_prefix, prefix_length);
  gi_filename_up = gi_filename + prefix_length;

  /* Now merge each file.  */
  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      unsigned n_counts;
      struct gcov_summary prg; /* summary for this object over all
				  program.  */
      struct gcov_ctr_summary *cs_prg, *cs_tprg;
#if !GCOV_LOCKED
      struct gcov_ctr_summary *cs_all;
#endif
      int error = 0;
      gcov_unsigned_t tag, length;
      gcov_position_t summary_pos = 0;
      gcov_position_t eof_pos = 0;
      const char *fname, *s;
      struct gcov_fn_buffer *fn_buffer = 0;
      struct gcov_fn_buffer **fn_tail = &fn_buffer;
      struct gcov_summary_buffer *next_sum_buffer, *sum_buffer = 0;
      struct gcov_summary_buffer **sum_tail = &sum_buffer;

      fname = gi_ptr->filename;

      /* Avoid to add multiple drive letters into combined path.  */
      if (prefix_length != 0 && HAS_DRIVE_SPEC(fname))
        fname += 2;

      /* Build relocated filename, stripping off leading
         directories from the initial filename if requested. */
      if (gcov_prefix_strip > 0)
        {
          int level = 0;
          s = fname;
          if (IS_DIR_SEPARATOR(*s))
            ++s;

          /* Skip selected directory levels. */
	  for (; (*s != '\0') && (level < gcov_prefix_strip); s++)
	    if (IS_DIR_SEPARATOR(*s))
	      {
		fname = s;
		level++;
	      }
        }

      /* Update complete filename with stripped original. */
      if (prefix_length != 0 && !IS_DIR_SEPARATOR (*fname))
        {
          /* If prefix is given, add directory separator.  */
	  strcpy (gi_filename_up, "/");
	  strcpy (gi_filename_up + 1, fname);
	}
      else
        strcpy (gi_filename_up, fname);

      if (!gcov_open (gi_filename))
	{
	  /* Open failed likely due to missed directory.
	     Create directory and retry to open file. */
          if (create_file_directory (gi_filename))
	    {
	      fprintf (stderr, "profiling:%s:Skip\n", gi_filename);
	      continue;
	    }
	  if (!gcov_open (gi_filename))
	    {
              fprintf (stderr, "profiling:%s:Cannot open\n", gi_filename);
	      continue;
	    }
	}

      tag = gcov_read_unsigned ();
      if (tag)
	{
	  /* Merge data from file.  */
	  if (tag != GCOV_DATA_MAGIC)
	    {
	      fprintf (stderr, "profiling:%s:Not a gcov data file\n",
		       gi_filename);
	      goto read_fatal;
	    }
	  length = gcov_read_unsigned ();
	  if (!gcov_version (gi_ptr, length, gi_filename))
	    goto read_fatal;

	  length = gcov_read_unsigned ();
	  if (length != gi_ptr->stamp)
	    /* Read from a different compilation. Overwrite the file.  */
	    goto rewrite;

	  /* Look for program summary.  */
	  for (f_ix = 0;;)
	    {
	      struct gcov_summary tmp;
	      
	      eof_pos = gcov_position ();
	      tag = gcov_read_unsigned ();
	      if (tag != GCOV_TAG_PROGRAM_SUMMARY)
		break;

	      f_ix--;
	      length = gcov_read_unsigned ();
	      gcov_read_summary (&tmp);
	      if ((error = gcov_is_error ()))
		goto read_error;
	      if (summary_pos)
                {
                  /* Save all summaries after the one that will be
                     merged into below. These will need to be rewritten
                     as histogram merging may change the number of non-zero
                     histogram entries that will be emitted, and thus the
                     size of the merged summary.  */
                  (*sum_tail) = (struct gcov_summary_buffer *)
                      malloc (sizeof(struct gcov_summary_buffer));
                  (*sum_tail)->summary = tmp;
                  (*sum_tail)->next = 0;
                  sum_tail = &((*sum_tail)->next);
                  goto next_summary;
                }
	      if (tmp.checksum != crc32)
                goto next_summary;
	      
	      for (t_ix = 0; t_ix != GCOV_COUNTERS_SUMMABLE; t_ix++)
		if (tmp.ctrs[t_ix].num != this_prg.ctrs[t_ix].num)
                  goto next_summary;
	      prg = tmp;
	      summary_pos = eof_pos;

	    next_summary:;
	    }
	  
	  /* Merge execution counts for each function.  */
	  for (f_ix = 0; (unsigned)f_ix != gi_ptr->n_functions;
	       f_ix++, tag = gcov_read_unsigned ())
	    {
	      gfi_ptr = gi_ptr->functions[f_ix];

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
		  fn_tail = buffer_fn_data (gi_filename,
					    gi_ptr, fn_tail, f_ix);
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
		  length = gcov_read_unsigned ();
		  if (tag != GCOV_TAG_FOR_COUNTER (t_ix)
		      || length != GCOV_TAG_COUNTER_LENGTH (ci_ptr->num))
		    goto read_mismatch;
		  (*merge) (ci_ptr->values, ci_ptr->num);
		  ci_ptr++;
		}
	      if ((error = gcov_is_error ()))
		goto read_error;
	    }

	  if (tag)
	    {
	    read_mismatch:;
	      fprintf (stderr, "profiling:%s:Merge mismatch for %s %u\n",
		       gi_filename, f_ix >= 0 ? "function" : "summary",
		       f_ix < 0 ? -1 - f_ix : f_ix);
	      goto read_fatal;
	    }
	}
      goto rewrite;

    read_error:;
      fprintf (stderr, "profiling:%s:%s merging\n", gi_filename,
	       error < 0 ? "Overflow": "Error");

      goto read_fatal;

    rewrite:;
      gcov_rewrite ();
      if (!summary_pos)
	{
	  memset (&prg, 0, sizeof (prg));
	  summary_pos = eof_pos;
	}

      /* Merge the summaries.  */
      for (t_ix = 0; t_ix < GCOV_COUNTERS_SUMMABLE; t_ix++)
	{
	  cs_prg = &prg.ctrs[t_ix];
	  cs_tprg = &this_prg.ctrs[t_ix];

	  if (gi_ptr->merge[t_ix])
	    {
	      if (!cs_prg->runs++)
	        cs_prg->num = cs_tprg->num;
	      cs_prg->sum_all += cs_tprg->sum_all;
	      if (cs_prg->run_max < cs_tprg->run_max)
		cs_prg->run_max = cs_tprg->run_max;
	      cs_prg->sum_max += cs_tprg->run_max;
              if (cs_prg->runs == 1)
                memcpy (cs_prg->histogram, cs_tprg->histogram,
                        sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);
              else
                gcov_histogram_merge (cs_prg->histogram, cs_tprg->histogram);
	    }
	  else if (cs_prg->runs)
	    goto read_mismatch;

#if !GCOV_LOCKED
	  cs_all = &all_prg.ctrs[t_ix];
	  if (!cs_all->runs && cs_prg->runs)
            {
              cs_all->num = cs_prg->num;
              cs_all->runs = cs_prg->runs;
              cs_all->sum_all = cs_prg->sum_all;
              cs_all->run_max = cs_prg->run_max;
              cs_all->sum_max = cs_prg->sum_max;
            }
	  else if (!all_prg.checksum
                   /* Don't compare the histograms, which may have slight
                      variations depending on the order they were updated
                      due to the truncating integer divides used in the
                      merge.  */
                   && (cs_all->num != cs_prg->num
                       || cs_all->runs != cs_prg->runs
                       || cs_all->sum_all != cs_prg->sum_all
                       || cs_all->run_max != cs_prg->run_max
                       || cs_all->sum_max != cs_prg->sum_max))
	    {
	      fprintf (stderr,
                       "profiling:%s:Data file mismatch - some data files may "
                       "have been concurrently updated without locking support\n",
		       gi_filename);
	      all_prg.checksum = ~0u;
	    }
#endif
	}

      prg.checksum = crc32;

      /* Write out the data.  */
      if (!eof_pos)
	{
	  gcov_write_tag_length (GCOV_DATA_MAGIC, GCOV_VERSION);
	  gcov_write_unsigned (gi_ptr->stamp);
	}

      if (summary_pos)
	gcov_seek (summary_pos);

      /* Generate whole program statistics.  */
      gcov_write_summary (GCOV_TAG_PROGRAM_SUMMARY, &prg);

      /* Rewrite all the summaries that were after the summary we merged
         into. This is necessary as the merged summary may have a different
         size due to the number of non-zero histogram entries changing after
         merging.  */
      
      while (sum_buffer)
        {
          gcov_write_summary (GCOV_TAG_PROGRAM_SUMMARY, &sum_buffer->summary);
          next_sum_buffer = sum_buffer->next;
          free (sum_buffer);
          sum_buffer = next_sum_buffer;
        }

      /* Write execution counts for each function.  */
      for (f_ix = 0; (unsigned)f_ix != gi_ptr->n_functions; f_ix++)
	{
	  unsigned buffered = 0;

	  if (fn_buffer && fn_buffer->fn_ix == (unsigned)f_ix)
	    {
	      /* Buffered data from another program.  */
	      buffered = 1;
	      gfi_ptr = &fn_buffer->info;
	      length = GCOV_TAG_FUNCTION_LENGTH;
	    }
	  else
	    {
	      gfi_ptr = gi_ptr->functions[f_ix];
	      if (gfi_ptr && gfi_ptr->key == gi_ptr)
		length = GCOV_TAG_FUNCTION_LENGTH;
	      else
		length = 0;
	    }
	  
	  gcov_write_tag_length (GCOV_TAG_FUNCTION, length);
	  if (!length)
	    continue;
	  
	  gcov_write_unsigned (gfi_ptr->ident);
	  gcov_write_unsigned (gfi_ptr->lineno_checksum);
	  gcov_write_unsigned (gfi_ptr->cfg_checksum);

	  ci_ptr = gfi_ptr->ctrs;
	  for (t_ix = 0; t_ix < GCOV_COUNTERS; t_ix++)
	    {
	      if (!gi_ptr->merge[t_ix])
		continue;

	      n_counts = ci_ptr->num;
	      gcov_write_tag_length (GCOV_TAG_FOR_COUNTER (t_ix),
				     GCOV_TAG_COUNTER_LENGTH (n_counts));
	      gcov_type *c_ptr = ci_ptr->values;
	      while (n_counts--)
		gcov_write_counter (*c_ptr++);
	      ci_ptr++;
	    }
	  if (buffered)
	    fn_buffer = free_fn_data (gi_ptr, fn_buffer, GCOV_COUNTERS);
	}

      gcov_write_unsigned (0);

    read_fatal:;
      while (fn_buffer)
	fn_buffer = free_fn_data (gi_ptr, fn_buffer, GCOV_COUNTERS);

      if ((error = gcov_close ()))
	  fprintf (stderr, error  < 0 ?
		   "profiling:%s:Overflow writing\n" :
		   "profiling:%s:Error writing\n",
		   gi_filename);
    }
}

/* Reset all counters to zero.  */

void
gcov_clear (void)
{
  const struct gcov_info *gi_ptr;

  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      unsigned f_ix;

      for (f_ix = 0; f_ix < gi_ptr->n_functions; f_ix++)
	{
	  unsigned t_ix;
	  const struct gcov_fn_info *gfi_ptr = gi_ptr->functions[f_ix];

	  if (!gfi_ptr || gfi_ptr->key != gi_ptr)
	    continue;
	  const struct gcov_ctr_info *ci_ptr = gfi_ptr->ctrs;
	  for (t_ix = 0; t_ix != GCOV_COUNTERS; t_ix++)
	    {
	      if (!gi_ptr->merge[t_ix])
		continue;
	      
	      memset (ci_ptr->values, 0, sizeof (gcov_type) * ci_ptr->num);
	      ci_ptr++;
	    }
	}
    }
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
      size_t filename_length = strlen(info->filename);

      /* Refresh the longest file name information */
      if (filename_length > gcov_max_filename)
        gcov_max_filename = filename_length;

      if (!gcov_list)
	atexit (gcov_exit);

      info->next = gcov_list;
      gcov_list = info;
    }
  info->version = 0;
}

#ifdef __GTHREAD_MUTEX_INIT
ATTRIBUTE_HIDDEN __gthread_mutex_t __gcov_flush_mx = __GTHREAD_MUTEX_INIT;
#define init_mx_once()
#else
__gthread_mutex_t __gcov_flush_mx ATTRIBUTE_HIDDEN;

static void
init_mx (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&__gcov_flush_mx);
}
static void
init_mx_once (void)
{
  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  __gthread_once (&once, init_mx);
}
#endif

/* Called before fork or exec - write out profile information gathered so
   far and reset it to zero.  This avoids duplication or loss of the
   profile information gathered so far.  */

void
__gcov_flush (void)
{
  init_mx_once ();
  __gthread_mutex_lock (&__gcov_flush_mx);

  gcov_exit ();
  gcov_clear ();

  __gthread_mutex_unlock (&__gcov_flush_mx);
}

#endif /* L_gcov */

#ifdef L_gcov_reset

/* Function that can be called from application to reset counters to zero,
   in order to collect profile in region of interest.  */

void
__gcov_reset (void)
{
  gcov_clear ();
  /* Re-enable dumping to support collecting profile in multiple regions
     of interest.  */
  gcov_dump_complete = 0;
}

#endif /* L_gcov_reset */

#ifdef L_gcov_dump

/* Function that can be called from application to write profile collected
   so far, in order to collect profile in region of interest.  */

void
__gcov_dump (void)
{
  gcov_exit ();
  /* Prevent profile from being dumped a second time on application exit.  */
  gcov_dump_complete = 1;
}

#endif /* L_gcov_dump */

#ifdef L_gcov_merge_add
/* The profile merging function that just adds the counters.  It is given
   an array COUNTERS of N_COUNTERS old counters and it reads the same number
   of counters from the gcov file.  */
void
__gcov_merge_add (gcov_type *counters, unsigned n_counters)
{
  for (; n_counters; counters++, n_counters--)
    *counters += gcov_read_counter ();
}
#endif /* L_gcov_merge_add */

#ifdef L_gcov_merge_ior
/* The profile merging function that just adds the counters.  It is given
   an array COUNTERS of N_COUNTERS old counters and it reads the same number
   of counters from the gcov file.  */
void
__gcov_merge_ior (gcov_type *counters, unsigned n_counters)
{
  for (; n_counters; counters++, n_counters--)
    *counters |= gcov_read_counter ();
}
#endif

#ifdef L_gcov_merge_single
/* The profile merging function for choosing the most common value.
   It is given an array COUNTERS of N_COUNTERS old counters and it
   reads the same number of counters from the gcov file.  The counters
   are split into 3-tuples where the members of the tuple have
   meanings:

   -- the stored candidate on the most common value of the measured entity
   -- counter
   -- total number of evaluations of the value  */
void
__gcov_merge_single (gcov_type *counters, unsigned n_counters)
{
  unsigned i, n_measures;
  gcov_type value, counter, all;

  gcc_assert (!(n_counters % 3));
  n_measures = n_counters / 3;
  for (i = 0; i < n_measures; i++, counters += 3)
    {
      value = gcov_read_counter ();
      counter = gcov_read_counter ();
      all = gcov_read_counter ();

      if (counters[0] == value)
	counters[1] += counter;
      else if (counter > counters[1])
	{
	  counters[0] = value;
	  counters[1] = counter - counters[1];
	}
      else
	counters[1] -= counter;
      counters[2] += all;
    }
}
#endif /* L_gcov_merge_single */

#ifdef L_gcov_merge_delta
/* The profile merging function for choosing the most common
   difference between two consecutive evaluations of the value.  It is
   given an array COUNTERS of N_COUNTERS old counters and it reads the
   same number of counters from the gcov file.  The counters are split
   into 4-tuples where the members of the tuple have meanings:

   -- the last value of the measured entity
   -- the stored candidate on the most common difference
   -- counter
   -- total number of evaluations of the value  */
void
__gcov_merge_delta (gcov_type *counters, unsigned n_counters)
{
  unsigned i, n_measures;
  gcov_type value, counter, all;

  gcc_assert (!(n_counters % 4));
  n_measures = n_counters / 4;
  for (i = 0; i < n_measures; i++, counters += 4)
    {
      /* last = */ gcov_read_counter ();
      value = gcov_read_counter ();
      counter = gcov_read_counter ();
      all = gcov_read_counter ();

      if (counters[1] == value)
	counters[2] += counter;
      else if (counter > counters[2])
	{
	  counters[1] = value;
	  counters[2] = counter - counters[2];
	}
      else
	counters[2] -= counter;
      counters[3] += all;
    }
}
#endif /* L_gcov_merge_delta */

#ifdef L_gcov_interval_profiler
/* If VALUE is in interval <START, START + STEPS - 1>, then increases the
   corresponding counter in COUNTERS.  If the VALUE is above or below
   the interval, COUNTERS[STEPS] or COUNTERS[STEPS + 1] is increased
   instead.  */

void
__gcov_interval_profiler (gcov_type *counters, gcov_type value,
			  int start, unsigned steps)
{
  gcov_type delta = value - start;
  if (delta < 0)
    counters[steps + 1]++;
  else if (delta >= steps)
    counters[steps]++;
  else
    counters[delta]++;
}
#endif

#ifdef L_gcov_pow2_profiler
/* If VALUE is a power of two, COUNTERS[1] is incremented.  Otherwise
   COUNTERS[0] is incremented.  */

void
__gcov_pow2_profiler (gcov_type *counters, gcov_type value)
{
  if (value & (value - 1))
    counters[0]++;
  else
    counters[1]++;
}
#endif

/* Tries to determine the most common value among its inputs.  Checks if the
   value stored in COUNTERS[0] matches VALUE.  If this is the case, COUNTERS[1]
   is incremented.  If this is not the case and COUNTERS[1] is not zero,
   COUNTERS[1] is decremented.  Otherwise COUNTERS[1] is set to one and
   VALUE is stored to COUNTERS[0].  This algorithm guarantees that if this
   function is called more than 50% of the time with one value, this value
   will be in COUNTERS[0] in the end.

   In any case, COUNTERS[2] is incremented.  */

static inline void
__gcov_one_value_profiler_body (gcov_type *counters, gcov_type value)
{
  if (value == counters[0])
    counters[1]++;
  else if (counters[1] == 0)
    {
      counters[1] = 1;
      counters[0] = value;
    }
  else
    counters[1]--;
  counters[2]++;
}

#ifdef L_gcov_one_value_profiler
void
__gcov_one_value_profiler (gcov_type *counters, gcov_type value)
{
  __gcov_one_value_profiler_body (counters, value);
}
#endif

#ifdef L_gcov_indirect_call_profiler

/* These two variables are used to actually track caller and callee.  Keep
   them in TLS memory so races are not common (they are written to often).
   The variables are set directly by GCC instrumented code, so declaration
   here must match one in tree-profile.c  */

#ifdef HAVE_CC_TLS
__thread 
#endif
void * __gcov_indirect_call_callee;
#ifdef HAVE_CC_TLS
__thread 
#endif
gcov_type * __gcov_indirect_call_counters;


/* By default, the C++ compiler will use function addresses in the
   vtable entries.  Setting TARGET_VTABLE_USES_DESCRIPTORS to nonzero
   tells the compiler to use function descriptors instead.  The value
   of this macro says how many words wide the descriptor is (normally 2),
   but it may be dependent on target flags.  Since we do not have access
   to the target flags here we just check to see if it is set and use
   that to set VTABLE_USES_DESCRIPTORS to 0 or 1.

   It is assumed that the address of a function descriptor may be treated
   as a pointer to a function.  */

#ifdef TARGET_VTABLE_USES_DESCRIPTORS
#define VTABLE_USES_DESCRIPTORS 1
#else
#define VTABLE_USES_DESCRIPTORS 0
#endif

/* Tries to determine the most common value among its inputs. */
void
__gcov_indirect_call_profiler_v2 (gcov_type value, void* cur_func)
{
  /* If the C++ virtual tables contain function descriptors then one
     function may have multiple descriptors and we need to dereference
     the descriptors to see if they point to the same function.  */
  if (cur_func == __gcov_indirect_call_callee
      || (VTABLE_USES_DESCRIPTORS && __gcov_indirect_call_callee
	  && *(void **) cur_func == *(void **) __gcov_indirect_call_callee))
    __gcov_one_value_profiler_body (__gcov_indirect_call_counters, value);
}
#endif


#ifdef L_gcov_average_profiler
/* Increase corresponding COUNTER by VALUE.  FIXME: Perhaps we want
   to saturate up.  */

void
__gcov_average_profiler (gcov_type *counters, gcov_type value)
{
  counters[0] += value;
  counters[1] ++;
}
#endif

#ifdef L_gcov_ior_profiler
/* Bitwise-OR VALUE into COUNTER.  */

void
__gcov_ior_profiler (gcov_type *counters, gcov_type value)
{
  *counters |= value;
}
#endif

#ifdef L_gcov_fork
/* A wrapper for the fork function.  Flushes the accumulated profiling data, so
   that they are not counted twice.  */

pid_t
__gcov_fork (void)
{
  pid_t pid;
  extern __gthread_mutex_t __gcov_flush_mx;
  __gcov_flush ();
  pid = fork ();
  if (pid == 0)
    __GTHREAD_MUTEX_INIT_FUNCTION (&__gcov_flush_mx);
  return pid;
}
#endif

#ifdef L_gcov_execl
/* A wrapper for the execl function.  Flushes the accumulated profiling data, so
   that they are not lost.  */

int
__gcov_execl (const char *path, char *arg, ...)
{
  va_list ap, aq;
  unsigned i, length;
  char **args;

  __gcov_flush ();

  va_start (ap, arg);
  va_copy (aq, ap);

  length = 2;
  while (va_arg (ap, char *))
    length++;
  va_end (ap);

  args = (char **) alloca (length * sizeof (void *));
  args[0] = arg;
  for (i = 1; i < length; i++)
    args[i] = va_arg (aq, char *);
  va_end (aq);

  return execv (path, args);
}
#endif

#ifdef L_gcov_execlp
/* A wrapper for the execlp function.  Flushes the accumulated profiling data, so
   that they are not lost.  */

int
__gcov_execlp (const char *path, char *arg, ...)
{
  va_list ap, aq;
  unsigned i, length;
  char **args;

  __gcov_flush ();

  va_start (ap, arg);
  va_copy (aq, ap);

  length = 2;
  while (va_arg (ap, char *))
    length++;
  va_end (ap);

  args = (char **) alloca (length * sizeof (void *));
  args[0] = arg;
  for (i = 1; i < length; i++)
    args[i] = va_arg (aq, char *);
  va_end (aq);

  return execvp (path, args);
}
#endif

#ifdef L_gcov_execle
/* A wrapper for the execle function.  Flushes the accumulated profiling data, so
   that they are not lost.  */

int
__gcov_execle (const char *path, char *arg, ...)
{
  va_list ap, aq;
  unsigned i, length;
  char **args;
  char **envp;

  __gcov_flush ();

  va_start (ap, arg);
  va_copy (aq, ap);

  length = 2;
  while (va_arg (ap, char *))
    length++;
  va_end (ap);

  args = (char **) alloca (length * sizeof (void *));
  args[0] = arg;
  for (i = 1; i < length; i++)
    args[i] = va_arg (aq, char *);
  envp = va_arg (aq, char **);
  va_end (aq);

  return execve (path, args, envp);
}
#endif

#ifdef L_gcov_execv
/* A wrapper for the execv function.  Flushes the accumulated profiling data, so
   that they are not lost.  */

int
__gcov_execv (const char *path, char *const argv[])
{
  __gcov_flush ();
  return execv (path, argv);
}
#endif

#ifdef L_gcov_execvp
/* A wrapper for the execvp function.  Flushes the accumulated profiling data, so
   that they are not lost.  */

int
__gcov_execvp (const char *path, char *const argv[])
{
  __gcov_flush ();
  return execvp (path, argv);
}
#endif

#ifdef L_gcov_execve
/* A wrapper for the execve function.  Flushes the accumulated profiling data, so
   that they are not lost.  */

int
__gcov_execve (const char *path, char *const argv[], char *const envp[])
{
  __gcov_flush ();
  return execve (path, argv, envp);
}
#endif
#endif /* inhibit_libc */
