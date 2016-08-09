/* Utility functions for reading gcda files into in-memory
   gcov_info structures and offline profile processing. */
/* Copyright (C) 2014-2016 Free Software Foundation, Inc.
   Contributed by Rong Xu <xur@google.com>.

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


#define IN_GCOV_TOOL 1

#include "libgcov.h"
#include "intl.h"
#include "diagnostic.h"
#include "version.h"
#include "demangle.h"

/* Borrowed from basic-block.h.  */
#define RDIV(X,Y) (((X) + (Y) / 2) / (Y))

extern gcov_position_t gcov_position();
extern int gcov_is_error();

/* Verbose mode for debug.  */
static int verbose;

/* Set verbose flag.  */
void gcov_set_verbose (void)
{
  verbose = 1;
}

/* The following part is to read Gcda and reconstruct GCOV_INFO.  */

#include "obstack.h"
#include <unistd.h>
#ifdef HAVE_FTW_H
#include <ftw.h>
#endif

static void tag_function (unsigned, unsigned);
static void tag_blocks (unsigned, unsigned);
static void tag_arcs (unsigned, unsigned);
static void tag_lines (unsigned, unsigned);
static void tag_counters (unsigned, unsigned);
static void tag_summary (unsigned, unsigned);

/* The gcov_info for the first module.  */
static struct gcov_info *curr_gcov_info;
/* The gcov_info being processed.  */
static struct gcov_info *gcov_info_head;
/* This variable contains all the functions in current module.  */
static struct obstack fn_info;
/* The function being processed.  */
static struct gcov_fn_info *curr_fn_info;
/* The number of functions seen so far.  */
static unsigned num_fn_info;
/* This variable contains all the counters for current module.  */
static int k_ctrs_mask[GCOV_COUNTERS];
/* The kind of counters that have been seen.  */
static struct gcov_ctr_info k_ctrs[GCOV_COUNTERS];
/* Number of kind of counters that have been seen.  */
static int k_ctrs_types;

/* Merge functions for counters.  */
#define DEF_GCOV_COUNTER(COUNTER, NAME, FN_TYPE) __gcov_merge ## FN_TYPE,
static gcov_merge_fn ctr_merge_functions[GCOV_COUNTERS] = {
#include "gcov-counter.def"
};
#undef DEF_GCOV_COUNTER

/* Set the ctrs field in gcov_fn_info object FN_INFO.  */

static void
set_fn_ctrs (struct gcov_fn_info *fn_info)
{
  int j = 0, i;

  for (i = 0; i < GCOV_COUNTERS; i++)
    {
      if (k_ctrs_mask[i] == 0)
        continue;
      fn_info->ctrs[j].num = k_ctrs[i].num;
      fn_info->ctrs[j].values = k_ctrs[i].values;
      j++;
    }
  if (k_ctrs_types == 0)
    k_ctrs_types = j;
  else
    gcc_assert (j == k_ctrs_types);
}

/* For each tag in gcda file, we have an entry here.
   TAG is the tag value; NAME is the tag name; and
   PROC is the handler function.  */

typedef struct tag_format
{
    unsigned tag;
    char const *name;
    void (*proc) (unsigned, unsigned);
} tag_format_t;

/* Handler table for various Tags.  */

static const tag_format_t tag_table[] =
{
  {0, "NOP", NULL},
  {0, "UNKNOWN", NULL},
  {0, "COUNTERS", tag_counters},
  {GCOV_TAG_FUNCTION, "FUNCTION", tag_function},
  {GCOV_TAG_BLOCKS, "BLOCKS", tag_blocks},
  {GCOV_TAG_ARCS, "ARCS", tag_arcs},
  {GCOV_TAG_LINES, "LINES", tag_lines},
  {GCOV_TAG_OBJECT_SUMMARY, "OBJECT_SUMMARY", tag_summary},
  {GCOV_TAG_PROGRAM_SUMMARY, "PROGRAM_SUMMARY", tag_summary},
  {0, NULL, NULL}
};

/* Handler for reading function tag.  */

static void
tag_function (unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  int i;

  /* write out previous fn_info.  */
  if (num_fn_info)
    {
      set_fn_ctrs (curr_fn_info);
      obstack_ptr_grow (&fn_info, curr_fn_info);
    }

  /* Here we over allocate a bit, using GCOV_COUNTERS instead of the actual active
     counter types.  */
  curr_fn_info = (struct gcov_fn_info *) xcalloc (sizeof (struct gcov_fn_info)
                   + GCOV_COUNTERS * sizeof (struct gcov_ctr_info), 1);

  for (i = 0; i < GCOV_COUNTERS; i++)
     k_ctrs[i].num = 0;
  k_ctrs_types = 0;

  curr_fn_info->key = curr_gcov_info;
  curr_fn_info->ident = gcov_read_unsigned ();
  curr_fn_info->lineno_checksum = gcov_read_unsigned ();
  curr_fn_info->cfg_checksum = gcov_read_unsigned ();
  num_fn_info++;

  if (verbose)
    fnotice (stdout, "tag one function id=%d\n", curr_fn_info->ident);
}

/* Handler for reading block tag.  */

static void
tag_blocks (unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  /* TBD: gcov-tool currently does not handle gcno files. Assert here.  */
  gcc_unreachable ();
}

/* Handler for reading flow arc tag.  */

static void
tag_arcs (unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  /* TBD: gcov-tool currently does not handle gcno files. Assert here.  */
  gcc_unreachable ();
}

/* Handler for reading line tag.  */

static void
tag_lines (unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  /* TBD: gcov-tool currently does not handle gcno files. Assert here.  */
  gcc_unreachable ();
}

/* Handler for reading counters array tag with value as TAG and length of LENGTH.  */

static void
tag_counters (unsigned tag, unsigned length)
{
  unsigned n_counts = GCOV_TAG_COUNTER_NUM (length);
  gcov_type *values;
  unsigned ix;
  unsigned tag_ix;

  tag_ix = GCOV_COUNTER_FOR_TAG (tag);
  gcc_assert (tag_ix < GCOV_COUNTERS);
  k_ctrs_mask [tag_ix] = 1;
  gcc_assert (k_ctrs[tag_ix].num == 0);
  k_ctrs[tag_ix].num = n_counts;

  k_ctrs[tag_ix].values = values = (gcov_type *) xmalloc (n_counts * sizeof (gcov_type));
  gcc_assert (values);

  for (ix = 0; ix != n_counts; ix++)
    values[ix] = gcov_read_counter ();
}

/* Handler for reading summary tag.  */

static void
tag_summary (unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  struct gcov_summary summary;

  gcov_read_summary (&summary);
}

/* This function is called at the end of reading a gcda file.
   It flushes the contents in curr_fn_info to gcov_info object OBJ_INFO.  */

static void
read_gcda_finalize (struct gcov_info *obj_info)
{
  int i;

  set_fn_ctrs (curr_fn_info);
  obstack_ptr_grow (&fn_info, curr_fn_info);

  /* We set the following fields: merge, n_functions, and functions.  */
  obj_info->n_functions = num_fn_info;
  obj_info->functions = (const struct gcov_fn_info**) obstack_finish (&fn_info);

  /* wrap all the counter array.  */
  for (i=0; i< GCOV_COUNTERS; i++)
    {
      if (k_ctrs_mask[i])
        obj_info->merge[i] = ctr_merge_functions[i];
    }
}

/* Read the content of a gcda file FILENAME, and return a gcov_info data structure.
   Program level summary CURRENT_SUMMARY will also be updated.  */

static struct gcov_info *
read_gcda_file (const char *filename)
{
  unsigned tags[4];
  unsigned depth = 0;
  unsigned magic, version;
  struct gcov_info *obj_info;
  int i;

  for (i=0; i< GCOV_COUNTERS; i++)
    k_ctrs_mask[i] = 0;
  k_ctrs_types = 0;

  if (!gcov_open (filename))
    {
      fnotice (stderr, "%s:cannot open\n", filename);
      return NULL;
    }

  /* Read magic.  */
  magic = gcov_read_unsigned ();
  if (magic != GCOV_DATA_MAGIC)
    {
      fnotice (stderr, "%s:not a gcov data file\n", filename);
      gcov_close ();
      return NULL;
    }

  /* Read version.  */
  version = gcov_read_unsigned ();
  if (version != GCOV_VERSION)
    {
      fnotice (stderr, "%s:incorrect gcov version %d vs %d \n", filename, version, GCOV_VERSION);
      gcov_close ();
      return NULL;
    }

  /* Instantiate a gcov_info object.  */
  curr_gcov_info = obj_info = (struct gcov_info *) xcalloc (sizeof (struct gcov_info) +
             sizeof (struct gcov_ctr_info) * GCOV_COUNTERS, 1);

  obj_info->version = version;
  obstack_init (&fn_info);
  num_fn_info = 0;
  curr_fn_info = 0;
  {
    size_t len = strlen (filename) + 1;
    char *str_dup = (char*) xmalloc (len);

    memcpy (str_dup, filename, len);
    obj_info->filename = str_dup;
  }

  /* Read stamp.  */
  obj_info->stamp = gcov_read_unsigned ();

  while (1)
    {
      gcov_position_t base;
      unsigned tag, length;
      tag_format_t const *format;
      unsigned tag_depth;
      int error;
      unsigned mask;

      tag = gcov_read_unsigned ();
      if (!tag)
        break;
      length = gcov_read_unsigned ();
      base = gcov_position ();
      mask = GCOV_TAG_MASK (tag) >> 1;
      for (tag_depth = 4; mask; mask >>= 8)
        {
          if (((mask & 0xff) != 0xff))
            {
              warning (0, "%s:tag `%x' is invalid\n", filename, tag);
              break;
            }
          tag_depth--;
        }
      for (format = tag_table; format->name; format++)
        if (format->tag == tag)
          goto found;
      format = &tag_table[GCOV_TAG_IS_COUNTER (tag) ? 2 : 1];
    found:;
      if (tag)
        {
          if (depth && depth < tag_depth)
            {
              if (!GCOV_TAG_IS_SUBTAG (tags[depth - 1], tag))
                warning (0, "%s:tag `%x' is incorrectly nested\n",
                         filename, tag);
            }
          depth = tag_depth;
          tags[depth - 1] = tag;
        }

      if (format->proc)
        {
          unsigned long actual_length;

          (*format->proc) (tag, length);

          actual_length = gcov_position () - base;
          if (actual_length > length)
            warning (0, "%s:record size mismatch %lu bytes overread\n",
                     filename, actual_length - length);
          else if (length > actual_length)
            warning (0, "%s:record size mismatch %lu bytes unread\n",
                     filename, length - actual_length);
       }

      gcov_sync (base, length);
      if ((error = gcov_is_error ()))
        {
          warning (0, error < 0 ? "%s:counter overflow at %lu\n" :
                                  "%s:read error at %lu\n", filename,
                   (long unsigned) gcov_position ());
          break;
        }
    }

  read_gcda_finalize (obj_info);
  gcov_close ();

  return obj_info;
}

#ifdef HAVE_FTW_H
/* This will be called by ftw(). It opens and read a gcda file FILENAME.
   Return a non-zero value to stop the tree walk.  */

static int
ftw_read_file (const char *filename,
               const struct stat *status ATTRIBUTE_UNUSED,
               int type)
{
  int filename_len;
  int suffix_len;
  struct gcov_info *obj_info;

  /* Only read regular files.  */
  if (type != FTW_F)
    return 0;

  filename_len = strlen (filename);
  suffix_len = strlen (GCOV_DATA_SUFFIX);

  if (filename_len <= suffix_len)
    return 0;

  if (strcmp(filename + filename_len - suffix_len, GCOV_DATA_SUFFIX))
    return 0;

  if (verbose)
    fnotice (stderr, "reading file: %s\n", filename);

  obj_info = read_gcda_file (filename);
  if (!obj_info)
    return 0;

  obj_info->next = gcov_info_head;
  gcov_info_head = obj_info;

  return 0;
}
#endif

/* Initializer for reading a profile dir.  */

static inline void
read_profile_dir_init (void)
{
  gcov_info_head = 0;
}

/* Driver for read a profile directory and convert into gcov_info list in memory.
   Return NULL on error,
   Return the head of gcov_info list on success.  */

struct gcov_info *
gcov_read_profile_dir (const char* dir_name, int recompute_summary ATTRIBUTE_UNUSED)
{
  char *pwd;
  int ret;

  read_profile_dir_init ();

  if (access (dir_name, R_OK) != 0)
    {
      fnotice (stderr, "cannot access directory %s\n", dir_name);
      return NULL;
    }
  pwd = getcwd (NULL, 0);
  gcc_assert (pwd);
  ret = chdir (dir_name);
  if (ret !=0)
    {
      fnotice (stderr, "%s is not a directory\n", dir_name);
      return NULL;
    }
#ifdef HAVE_FTW_H
  ftw (".", ftw_read_file, 50);
#endif
  ret = chdir (pwd);
  free (pwd);


  return gcov_info_head;;
}

/* This part of the code is to merge profile counters. These
   variables are set in merge_wrapper and to be used by
   global function gcov_read_counter_mem() and gcov_get_merge_weight.  */

/* We save the counter value address to this variable.  */
static gcov_type *gcov_value_buf;

/* The number of counter values to be read by current merging.  */
static gcov_unsigned_t gcov_value_buf_size;

/* The index of counter values being read.  */
static gcov_unsigned_t gcov_value_buf_pos;

/* The weight of current merging.  */
static unsigned gcov_merge_weight;

/* Read a counter value from gcov_value_buf array.  */

gcov_type
gcov_read_counter_mem (void)
{
  gcov_type ret;
  gcc_assert (gcov_value_buf_pos < gcov_value_buf_size);
  ret = *(gcov_value_buf + gcov_value_buf_pos);
  ++gcov_value_buf_pos;
  return ret;
}

/* Return the recorded merge weight.  */

unsigned
gcov_get_merge_weight (void)
{
  return gcov_merge_weight;
}

/* A wrapper function for merge functions. It sets up the
   value buffer and weights and then calls the merge function.  */

static void
merge_wrapper (gcov_merge_fn f, gcov_type *v1, gcov_unsigned_t n,
               gcov_type *v2, unsigned w)
{
  gcov_value_buf = v2;
  gcov_value_buf_pos = 0;
  gcov_value_buf_size = n;
  gcov_merge_weight = w;
  (*f) (v1, n);
}

/* Offline tool to manipulate profile data.
   This tool targets on matched profiles. But it has some tolerance on
   unmatched profiles.
   When merging p1 to p2 (p2 is the dst),
   * m.gcda in p1 but not in p2: append m.gcda to p2 with specified weight;
     emit warning
   * m.gcda in p2 but not in p1: keep m.gcda in p2 and multiply by
     specified weight; emit warning.
   * m.gcda in both p1 and p2:
   ** p1->m.gcda->f checksum matches p2->m.gcda->f: simple merge.
   ** p1->m.gcda->f checksum does not matches p2->m.gcda->f: keep
      p2->m.gcda->f and
      drop p1->m.gcda->f. A warning is emitted.  */

/* Add INFO2's counter to INFO1, multiplying by weight W.  */

static int
gcov_merge (struct gcov_info *info1, struct gcov_info *info2, int w)
{
  unsigned f_ix;
  unsigned n_functions = info1->n_functions;
  int has_mismatch = 0;

  gcc_assert (info2->n_functions == n_functions);
  for (f_ix = 0; f_ix < n_functions; f_ix++)
    {
      unsigned t_ix;
      const struct gcov_fn_info *gfi_ptr1 = info1->functions[f_ix];
      const struct gcov_fn_info *gfi_ptr2 = info2->functions[f_ix];
      const struct gcov_ctr_info *ci_ptr1, *ci_ptr2;

      if (!gfi_ptr1 || gfi_ptr1->key != info1)
        continue;
      if (!gfi_ptr2 || gfi_ptr2->key != info2)
        continue;

      if (gfi_ptr1->cfg_checksum != gfi_ptr2->cfg_checksum)
        {
          fnotice (stderr, "in %s, cfg_checksum mismatch, skipping\n",
                  info1->filename);
          has_mismatch = 1;
          continue;
        }
      ci_ptr1 = gfi_ptr1->ctrs;
      ci_ptr2 = gfi_ptr2->ctrs;
      for (t_ix = 0; t_ix != GCOV_COUNTERS; t_ix++)
        {
          gcov_merge_fn merge1 = info1->merge[t_ix];
          gcov_merge_fn merge2 = info2->merge[t_ix];

          gcc_assert (merge1 == merge2);
          if (!merge1)
            continue;
          gcc_assert (ci_ptr1->num == ci_ptr2->num);
          merge_wrapper (merge1, ci_ptr1->values, ci_ptr1->num, ci_ptr2->values, w);
          ci_ptr1++;
          ci_ptr2++;
        }
    }

  return has_mismatch;
}

/* Find and return the match gcov_info object for INFO from ARRAY.
   SIZE is the length of ARRAY.
   Return NULL if there is no match.  */

static struct gcov_info *
find_match_gcov_info (struct gcov_info **array, int size,
		      struct gcov_info *info)
{
  struct gcov_info *gi_ptr;
  struct gcov_info *ret = NULL;
  int i;

  for (i = 0; i < size; i++)
    {
      gi_ptr = array[i];
      if (gi_ptr == 0)
        continue;
      if (!strcmp (gi_ptr->filename, info->filename))
        {
          ret = gi_ptr;
          array[i] = 0;
          break;
        }
    }

  if (ret && ret->n_functions != info->n_functions)
    {
      fnotice (stderr, "mismatched profiles in %s (%d functions"
                       " vs %d functions)\n",
                       ret->filename,
                       ret->n_functions,
                       info->n_functions);
      ret = NULL;
    }
  return ret;
}

/* Merge the list of gcov_info objects from SRC_PROFILE to TGT_PROFILE.
   Return 0 on success: without mismatch.
   Reutrn 1 on error.  */

int
gcov_profile_merge (struct gcov_info *tgt_profile, struct gcov_info *src_profile,
                    int w1, int w2)
{
  struct gcov_info *gi_ptr;
  struct gcov_info **tgt_infos;
  struct gcov_info *tgt_tail;
  struct gcov_info **in_src_not_tgt;
  unsigned tgt_cnt = 0, src_cnt = 0;
  unsigned unmatch_info_cnt = 0;
  unsigned int i;

  for (gi_ptr = tgt_profile; gi_ptr; gi_ptr = gi_ptr->next)
    tgt_cnt++;
  for (gi_ptr = src_profile; gi_ptr; gi_ptr = gi_ptr->next)
    src_cnt++;
  tgt_infos = (struct gcov_info **) xmalloc (sizeof (struct gcov_info *)
                 * tgt_cnt);
  gcc_assert (tgt_infos);
  in_src_not_tgt = (struct gcov_info **) xmalloc (sizeof (struct gcov_info *)
                     * src_cnt);
  gcc_assert (in_src_not_tgt);

  for (gi_ptr = tgt_profile, i = 0; gi_ptr; gi_ptr = gi_ptr->next, i++)
    tgt_infos[i] = gi_ptr;

  tgt_tail = tgt_infos[tgt_cnt - 1];

  /* First pass on tgt_profile, we multiply w1 to all counters.  */
  if (w1 > 1)
    {
       for (i = 0; i < tgt_cnt; i++)
         gcov_merge (tgt_infos[i], tgt_infos[i], w1-1);
    }

  /* Second pass, add src_profile to the tgt_profile.  */
  for (gi_ptr = src_profile; gi_ptr; gi_ptr = gi_ptr->next)
    {
      struct gcov_info *gi_ptr1;

      gi_ptr1 = find_match_gcov_info (tgt_infos, tgt_cnt, gi_ptr);
      if (gi_ptr1 == NULL)
        {
          in_src_not_tgt[unmatch_info_cnt++] = gi_ptr;
          continue;
        }
      gcov_merge (gi_ptr1, gi_ptr, w2);
    }

  /* For modules in src but not in tgt. We adjust the counter and append.  */
  for (i = 0; i < unmatch_info_cnt; i++)
    {
      gi_ptr = in_src_not_tgt[i];
      gcov_merge (gi_ptr, gi_ptr, w2 - 1);
      tgt_tail->next = gi_ptr;
      tgt_tail = gi_ptr;
    }

  return 0;
}

typedef gcov_type (*counter_op_fn) (gcov_type, void*, void*);

/* Performing FN upon arc counters.  */

static void
__gcov_add_counter_op (gcov_type *counters, unsigned n_counters,
                       counter_op_fn fn, void *data1, void *data2)
{
  for (; n_counters; counters++, n_counters--)
    {
      gcov_type val = *counters;
      *counters = fn(val, data1, data2);
    }
}

/* Performing FN upon ior counters.  */

static void
__gcov_ior_counter_op (gcov_type *counters ATTRIBUTE_UNUSED,
                       unsigned n_counters ATTRIBUTE_UNUSED,
                       counter_op_fn fn ATTRIBUTE_UNUSED,
                       void *data1 ATTRIBUTE_UNUSED,
                       void *data2 ATTRIBUTE_UNUSED)
{
  /* Do nothing.  */
}

/* Performing FN upon time-profile counters.  */

static void
__gcov_time_profile_counter_op (gcov_type *counters ATTRIBUTE_UNUSED,
                                unsigned n_counters ATTRIBUTE_UNUSED,
                                counter_op_fn fn ATTRIBUTE_UNUSED,
                                void *data1 ATTRIBUTE_UNUSED,
                                void *data2 ATTRIBUTE_UNUSED)
{
  /* Do nothing.  */
}

/* Performaing FN upon delta counters.  */

static void
__gcov_delta_counter_op (gcov_type *counters, unsigned n_counters,
                         counter_op_fn fn, void *data1, void *data2)
{
  unsigned i, n_measures;

  gcc_assert (!(n_counters % 4));
  n_measures = n_counters / 4;
  for (i = 0; i < n_measures; i++, counters += 4)
    {
      counters[2] = fn (counters[2], data1, data2);
      counters[3] = fn (counters[3], data1, data2);
    }
}

/* Performing FN upon single counters.  */

static void
__gcov_single_counter_op (gcov_type *counters, unsigned n_counters,
                          counter_op_fn fn, void *data1, void *data2)
{
  unsigned i, n_measures;

  gcc_assert (!(n_counters % 3));
  n_measures = n_counters / 3;
  for (i = 0; i < n_measures; i++, counters += 3)
    {
      counters[1] = fn (counters[1], data1, data2);
      counters[2] = fn (counters[2], data1, data2);
    }
}

/* Performing FN upon indirect-call profile counters.  */

static void
__gcov_icall_topn_counter_op (gcov_type *counters, unsigned n_counters,
                              counter_op_fn fn, void *data1, void *data2)
{
  unsigned i;

  gcc_assert (!(n_counters % GCOV_ICALL_TOPN_NCOUNTS));
  for (i = 0; i < n_counters; i += GCOV_ICALL_TOPN_NCOUNTS)
    {
      unsigned j;
      gcov_type *value_array = &counters[i + 1];

      for (j = 0; j < GCOV_ICALL_TOPN_NCOUNTS - 1; j += 2)
        value_array[j + 1] = fn (value_array[j + 1], data1, data2);
    }
}

/* Scaling the counter value V by multiplying *(float*) DATA1.  */

static gcov_type
fp_scale (gcov_type v, void *data1, void *data2 ATTRIBUTE_UNUSED)
{
  float f = *(float *) data1;
  return (gcov_type) (v * f);
}

/* Scaling the counter value V by multiplying DATA2/DATA1.  */

static gcov_type
int_scale (gcov_type v, void *data1, void *data2)
{
  int n = *(int *) data1;
  int d = *(int *) data2;
  return (gcov_type) ( RDIV (v,d) * n);
}

/* Type of function used to process counters.  */
typedef void (*gcov_counter_fn) (gcov_type *, gcov_unsigned_t,
                          counter_op_fn, void *, void *);

/* Function array to process profile counters.  */
#define DEF_GCOV_COUNTER(COUNTER, NAME, FN_TYPE) \
  __gcov ## FN_TYPE ## _counter_op,
static gcov_counter_fn ctr_functions[GCOV_COUNTERS] = {
#include "gcov-counter.def"
};
#undef DEF_GCOV_COUNTER

/* Driver for scaling profile counters.  */

int
gcov_profile_scale (struct gcov_info *profile, float scale_factor, int n, int d)
{
  struct gcov_info *gi_ptr;
  unsigned f_ix;

  if (verbose)
    fnotice (stdout, "scale_factor is %f or %d/%d\n", scale_factor, n, d);

  /* Scaling the counters.  */
  for (gi_ptr = profile; gi_ptr; gi_ptr = gi_ptr->next)
    for (f_ix = 0; f_ix < gi_ptr->n_functions; f_ix++)
      {
        unsigned t_ix;
        const struct gcov_fn_info *gfi_ptr = gi_ptr->functions[f_ix];
        const struct gcov_ctr_info *ci_ptr;

        if (!gfi_ptr || gfi_ptr->key != gi_ptr)
          continue;

        ci_ptr = gfi_ptr->ctrs;
        for (t_ix = 0; t_ix != GCOV_COUNTERS; t_ix++)
          {
            gcov_merge_fn merge = gi_ptr->merge[t_ix];

            if (!merge)
              continue;
            if (d == 0)
              (*ctr_functions[t_ix]) (ci_ptr->values, ci_ptr->num,
                                      fp_scale, &scale_factor, NULL);
            else
              (*ctr_functions[t_ix]) (ci_ptr->values, ci_ptr->num,
                                      int_scale, &n, &d);
            ci_ptr++;
          }
      }

  return 0;
}

/* Driver to normalize profile counters.  */

int
gcov_profile_normalize (struct gcov_info *profile, gcov_type max_val)
{
  struct gcov_info *gi_ptr;
  gcov_type curr_max_val = 0;
  unsigned f_ix;
  unsigned int i;
  float scale_factor;

  /* Find the largest count value.  */
  for (gi_ptr = profile; gi_ptr; gi_ptr = gi_ptr->next)
    for (f_ix = 0; f_ix < gi_ptr->n_functions; f_ix++)
      {
        unsigned t_ix;
        const struct gcov_fn_info *gfi_ptr = gi_ptr->functions[f_ix];
        const struct gcov_ctr_info *ci_ptr;

        if (!gfi_ptr || gfi_ptr->key != gi_ptr)
          continue;

        ci_ptr = gfi_ptr->ctrs;
        for (t_ix = 0; t_ix < 1; t_ix++)
          {
            for (i = 0; i < ci_ptr->num; i++)
              if (ci_ptr->values[i] > curr_max_val)
                curr_max_val = ci_ptr->values[i];
            ci_ptr++;
          }
      }

  scale_factor = (float)max_val / curr_max_val;
  if (verbose)
    fnotice (stdout, "max_val is %" PRId64 "\n", curr_max_val);

  return gcov_profile_scale (profile, scale_factor, 0, 0);
}

/* The following variables are defined in gcc/gcov-tool.c.  */
extern int overlap_func_level;
extern int overlap_obj_level;
extern int overlap_hot_only;
extern int overlap_use_fullname;
extern double overlap_hot_threshold;

/* Compute the overlap score of two values. The score is defined as:
    min (V1/SUM_1, V2/SUM_2)  */

static double
calculate_2_entries (const unsigned long v1, const unsigned long v2,
                     const double sum_1, const double sum_2)
{
  double val1 = (sum_1 == 0.0 ? 0.0 : v1/sum_1);
  double val2 = (sum_2 == 0.0 ? 0.0 : v2/sum_2);

  if (val2 < val1)
    val1 = val2;

  return val1;
}

/*  Compute the overlap score between GCOV_INFO1 and GCOV_INFO2.
    SUM_1 is the sum_all for profile1 where GCOV_INFO1 belongs.
    SUM_2 is the sum_all for profile2 where GCOV_INFO2 belongs.
    This function also updates cumulative score CUM_1_RESULT and
    CUM_2_RESULT.  */

static double
compute_one_gcov (const struct gcov_info *gcov_info1,
                  const struct gcov_info *gcov_info2,
                  const double sum_1, const double sum_2,
                  double *cum_1_result, double *cum_2_result)
{
  unsigned f_ix;
  double ret = 0;
  double cum_1 = 0, cum_2 = 0;
  const struct gcov_info *gcov_info = 0;
  double *cum_p;
  double sum;

  gcc_assert (gcov_info1 || gcov_info2);
  if (!gcov_info1)
    {
      gcov_info = gcov_info2;
      cum_p = cum_2_result;
      sum = sum_2;
      *cum_1_result = 0;
    } else
  if (!gcov_info2)
    {
      gcov_info = gcov_info1;
      cum_p = cum_1_result;
      sum = sum_1;
      *cum_2_result = 0;
    }

  if (gcov_info)
  {
    for (f_ix = 0; f_ix < gcov_info->n_functions; f_ix++)
      {
        unsigned t_ix;
        const struct gcov_fn_info *gfi_ptr = gcov_info->functions[f_ix];
        if (!gfi_ptr || gfi_ptr->key != gcov_info)
          continue;
        const struct gcov_ctr_info *ci_ptr = gfi_ptr->ctrs;
        for (t_ix = 0; t_ix < GCOV_COUNTERS_SUMMABLE; t_ix++)
          {
            unsigned c_num;

            if (!gcov_info->merge[t_ix])
              continue;

            for (c_num = 0; c_num < ci_ptr->num; c_num++)
              {
                cum_1 += ci_ptr->values[c_num] / sum;
              }
            ci_ptr++;
          }
      }
    *cum_p = cum_1;
    return 0.0;
  }

  for (f_ix = 0; f_ix < gcov_info1->n_functions; f_ix++)
    {
      unsigned t_ix;
      double func_cum_1 = 0.0;
      double func_cum_2 = 0.0;
      double func_val = 0.0;
      int nonzero = 0;
      int hot = 0;
      const struct gcov_fn_info *gfi_ptr1 = gcov_info1->functions[f_ix];
      const struct gcov_fn_info *gfi_ptr2 = gcov_info2->functions[f_ix];

      if (!gfi_ptr1 || gfi_ptr1->key != gcov_info1)
        continue;
      if (!gfi_ptr2 || gfi_ptr2->key != gcov_info2)
        continue;

      const struct gcov_ctr_info *ci_ptr1 = gfi_ptr1->ctrs;
      const struct gcov_ctr_info *ci_ptr2 = gfi_ptr2->ctrs;
      for (t_ix = 0; t_ix < GCOV_COUNTERS_SUMMABLE; t_ix++)
        {
          unsigned c_num;

          if (!gcov_info1->merge[t_ix])
            continue;

          for (c_num = 0; c_num < ci_ptr1->num; c_num++)
            {
              if (ci_ptr1->values[c_num] | ci_ptr2->values[c_num])
                {
                  func_val += calculate_2_entries (ci_ptr1->values[c_num],
                                          ci_ptr2->values[c_num],
                                          sum_1, sum_2);

                  func_cum_1 += ci_ptr1->values[c_num] / sum_1;
                  func_cum_2 += ci_ptr2->values[c_num] / sum_2;
                  nonzero = 1;
                  if (ci_ptr1->values[c_num] / sum_1 >= overlap_hot_threshold ||
                      ci_ptr2->values[c_num] / sum_2 >= overlap_hot_threshold)
                    hot = 1;
                }
            }
          ci_ptr1++;
          ci_ptr2++;
        }
      ret += func_val;
      cum_1 += func_cum_1;
      cum_2 += func_cum_2;
      if (overlap_func_level && nonzero && (!overlap_hot_only || hot))
        {
          printf("   \tfunc_id=%10d \toverlap =%6.5f%% (%5.5f%% %5.5f%%)\n",
                 gfi_ptr1->ident, func_val*100, func_cum_1*100, func_cum_2*100);
        }
    }
  *cum_1_result = cum_1;
  *cum_2_result = cum_2;
  return ret;
}

/* Test if all counter values in this GCOV_INFO are cold.
   "Cold" is defined as the counter value being less than
   or equal to THRESHOLD.  */

static bool
gcov_info_count_all_cold (const struct gcov_info *gcov_info,
                          gcov_type threshold)
{
  unsigned f_ix;

  for (f_ix = 0; f_ix < gcov_info->n_functions; f_ix++)
    {
      unsigned t_ix;
      const struct gcov_fn_info *gfi_ptr = gcov_info->functions[f_ix];

      if (!gfi_ptr || gfi_ptr->key != gcov_info)
        continue;
      const struct gcov_ctr_info *ci_ptr = gfi_ptr->ctrs;
      for (t_ix = 0; t_ix < GCOV_COUNTERS_SUMMABLE; t_ix++)
        {
          unsigned c_num;

          if (!gcov_info->merge[t_ix])
            continue;

          for (c_num = 0; c_num < ci_ptr->num; c_num++)
            {
              if (ci_ptr->values[c_num] > threshold)
                return false;
            }
          ci_ptr++;
        }
    }

  return true;
}

/* Test if all counter values in this GCOV_INFO are 0.  */

static bool
gcov_info_count_all_zero (const struct gcov_info *gcov_info)
{
  return gcov_info_count_all_cold (gcov_info, 0);
}

/* A pair of matched GCOV_INFO.
   The flag is a bitvector:
     b0: obj1's all counts are 0;
     b1: obj1's all counts are cold (but no 0);
     b2: obj1 is hot;
     b3: no obj1 to match obj2;
     b4: obj2's all counts are 0;
     b5: obj2's all counts are cold (but no 0);
     b6: obj2 is hot;
     b7: no obj2 to match obj1;
 */
struct overlap_t {
   const struct gcov_info *obj1;
   const struct gcov_info *obj2;
   char flag;
};

#define FLAG_BOTH_ZERO(flag) ((flag & 0x1) && (flag & 0x10))
#define FLAG_BOTH_COLD(flag) ((flag & 0x2) && (flag & 0x20))
#define FLAG_ONE_HOT(flag) ((flag & 0x4) || (flag & 0x40))

/* Cumlative overlap dscore for profile1 and profile2.  */
static double overlap_sum_1, overlap_sum_2;

/* sum_all for profile1 and profile2.  */
static gcov_type p1_sum_all, p2_sum_all;

/* run_max for profile1 and profile2.  */
static gcov_type p1_run_max, p2_run_max;

/* The number of gcda files in the profiles.  */
static unsigned gcda_files[2];

/* The number of unique gcda files in the profiles
   (not existing in the other profile).  */
static unsigned unique_gcda_files[2];

/* The number of gcda files that all counter values are 0.  */
static unsigned zero_gcda_files[2];

/* The number of gcda files that all counter values are cold (but not 0).  */
static unsigned cold_gcda_files[2];

/* The number of gcda files that includes hot counter values.  */
static unsigned hot_gcda_files[2];

/* The number of gcda files with hot count value in either profiles.  */
static unsigned both_hot_cnt;

/* The number of gcda files with all counts cold (but not 0) in
   both profiles. */
static unsigned both_cold_cnt;

/* The number of gcda files with all counts 0 in both profiles.  */
static unsigned both_zero_cnt;

/* Extract the basename of the filename NAME.  */

static char *
extract_file_basename (const char *name)
{
  char *str;
  int len = 0;
  char *path = xstrdup (name);
  char sep_str[2];

  sep_str[0] = DIR_SEPARATOR;
  sep_str[1] = 0;
  str = strstr(path, sep_str);
  do{
      len = strlen(str) + 1;
      path = &path[strlen(path) - len + 2];
      str = strstr(path, sep_str);
  } while(str);

  return path;
}

/* Utility function to get the filename.  */

static const char *
get_file_basename (const char *name)
{
  if (overlap_use_fullname)
    return name;
  return extract_file_basename (name);
}

/* A utility function to set the flag for the gcda files.  */

static void
set_flag (struct overlap_t *e)
{
  char flag = 0;

  if (!e->obj1)
    {
      unique_gcda_files[1]++;
      flag = 0x8;
    }
  else
    {
      gcda_files[0]++;
      if (gcov_info_count_all_zero (e->obj1))
        {
          zero_gcda_files[0]++;
          flag = 0x1;
        }
      else
      if (gcov_info_count_all_cold (e->obj1, overlap_sum_1
			      * overlap_hot_threshold))
        {
          cold_gcda_files[0]++;
          flag = 0x2;
        }
      else
        {
          hot_gcda_files[0]++;
          flag = 0x4;
        }
    }

  if (!e->obj2)
    {
      unique_gcda_files[0]++;
      flag |= (0x8 << 4);
    }
  else
    {
      gcda_files[1]++;
      if (gcov_info_count_all_zero (e->obj2))
        {
          zero_gcda_files[1]++;
          flag |= (0x1 << 4);
        }
      else
      if (gcov_info_count_all_cold (e->obj2, overlap_sum_2
			      * overlap_hot_threshold))
        {
          cold_gcda_files[1]++;
          flag |= (0x2 << 4);
        }
      else
        {
          hot_gcda_files[1]++;
          flag |= (0x4 << 4);
        }
    }

  gcc_assert (flag);
  e->flag = flag;
}

/* Test if INFO1 and INFO2 are from the matched source file.
   Return 1 if they match; return 0 otherwise.  */

static int
matched_gcov_info (const struct gcov_info *info1, const struct gcov_info *info2)
{
  /* For FDO, we have to match the name. This can be expensive.
     Maybe we should use hash here.  */
  if (strcmp (info1->filename, info2->filename))
    return 0;

  if (info1->n_functions != info2->n_functions)
    {
      fnotice (stderr, "mismatched profiles in %s (%d functions"
                       " vs %d functions)\n",
                       info1->filename,
                       info1->n_functions,
                       info2->n_functions);
      return 0;
    }
  return 1;
}

/* Defined in libgcov-driver.c.  */
extern gcov_unsigned_t compute_summary (struct gcov_info *,
                 struct gcov_summary *, size_t *);

/* Compute the overlap score of two profiles with the head of GCOV_LIST1 and
   GCOV_LIST1. Return a number ranging from [0.0, 1.0], with 0.0 meaning no
   match and 1.0 meaning a perfect match.  */

static double
calculate_overlap (struct gcov_info *gcov_list1,
                   struct gcov_info *gcov_list2)
{
  struct gcov_summary this_prg;
  unsigned list1_cnt = 0, list2_cnt= 0, all_cnt;
  unsigned int i, j;
  size_t max_length;
  const struct gcov_info *gi_ptr;
  struct overlap_t *all_infos;

  compute_summary (gcov_list1, &this_prg, &max_length);
  overlap_sum_1 = (double) (this_prg.ctrs[0].sum_all);
  p1_sum_all = this_prg.ctrs[0].sum_all;
  p1_run_max = this_prg.ctrs[0].run_max;
  compute_summary (gcov_list2, &this_prg, &max_length);
  overlap_sum_2 = (double) (this_prg.ctrs[0].sum_all);
  p2_sum_all = this_prg.ctrs[0].sum_all;
  p2_run_max = this_prg.ctrs[0].run_max;

  for (gi_ptr = gcov_list1; gi_ptr; gi_ptr = gi_ptr->next)
    list1_cnt++;
  for (gi_ptr = gcov_list2; gi_ptr; gi_ptr = gi_ptr->next)
    list2_cnt++;
  all_cnt = list1_cnt + list2_cnt;
  all_infos = (struct overlap_t *) xmalloc (sizeof (struct overlap_t)
               * all_cnt * 2);
  gcc_assert (all_infos);

  i = 0;
  for (gi_ptr = gcov_list1; gi_ptr; gi_ptr = gi_ptr->next, i++)
    {
      all_infos[i].obj1 = gi_ptr;
      all_infos[i].obj2 = 0;
    }

  for (gi_ptr = gcov_list2; gi_ptr; gi_ptr = gi_ptr->next, i++)
    {
      all_infos[i].obj1 = 0;
      all_infos[i].obj2 = gi_ptr;
    }

  for (i = list1_cnt; i < all_cnt; i++)
    {
      if (all_infos[i].obj2 == 0)
        continue;
      for (j = 0; j < list1_cnt; j++)
        {
          if (all_infos[j].obj2 != 0)
            continue;
          if (matched_gcov_info (all_infos[i].obj2, all_infos[j].obj1))
            {
              all_infos[j].obj2 = all_infos[i].obj2;
              all_infos[i].obj2 = 0;
              break;
            }
        }
    }

  for (i = 0; i < all_cnt; i++)
    if (all_infos[i].obj1 || all_infos[i].obj2)
      {
        set_flag (all_infos + i);
        if (FLAG_ONE_HOT (all_infos[i].flag))
            both_hot_cnt++;
        if (FLAG_BOTH_COLD(all_infos[i].flag))
            both_cold_cnt++;
        if (FLAG_BOTH_ZERO(all_infos[i].flag))
            both_zero_cnt++;
      }

  double prg_val = 0;
  double sum_val = 0;
  double sum_cum_1 = 0;
  double sum_cum_2 = 0;

  for (i = 0; i < all_cnt; i++)
    {
      double val;
      double cum_1, cum_2;
      const char *filename;

      if (all_infos[i].obj1 == 0 && all_infos[i].obj2 == 0)
        continue;
      if (FLAG_BOTH_ZERO (all_infos[i].flag))
          continue;

      if (all_infos[i].obj1)
        filename = get_file_basename (all_infos[i].obj1->filename);
      else
        filename = get_file_basename (all_infos[i].obj2->filename);

      if (overlap_func_level)
        printf("\n   processing %36s:\n", filename);

      val = compute_one_gcov (all_infos[i].obj1, all_infos[i].obj2,
          overlap_sum_1, overlap_sum_2, &cum_1, &cum_2);

      if (overlap_obj_level && (!overlap_hot_only || FLAG_ONE_HOT (all_infos[i].flag)))
        {
          printf("   obj=%36s  overlap = %6.2f%% (%5.2f%% %5.2f%%)\n",
                  filename, val*100, cum_1*100, cum_2*100);
          sum_val += val;
          sum_cum_1 += cum_1;
          sum_cum_2 += cum_2;
        }

      prg_val += val;

    }

  if (overlap_obj_level)
    printf("   SUM:%36s  overlap = %6.2f%% (%5.2f%% %5.2f%%)\n",
           "", sum_val*100, sum_cum_1*100, sum_cum_2*100);

  printf ("  Statistics:\n"
          "                    profile1_#     profile2_#       overlap_#\n");
  printf ("    gcda files:  %12u\t%12u\t%12u\n", gcda_files[0], gcda_files[1],
	  gcda_files[0]-unique_gcda_files[0]);
  printf ("  unique files:  %12u\t%12u\n", unique_gcda_files[0],
	  unique_gcda_files[1]);
  printf ("     hot files:  %12u\t%12u\t%12u\n", hot_gcda_files[0],
	  hot_gcda_files[1], both_hot_cnt);
  printf ("    cold files:  %12u\t%12u\t%12u\n", cold_gcda_files[0],
	  cold_gcda_files[1], both_cold_cnt);
  printf ("    zero files:  %12u\t%12u\t%12u\n", zero_gcda_files[0],
	  zero_gcda_files[1], both_zero_cnt);
  printf ("       sum_all:  %12" PRId64 "\t%12" PRId64 "\n",
	  p1_sum_all, p2_sum_all);
  printf ("       run_max:  %12" PRId64 "\t%12" PRId64 "\n",
	  p1_run_max, p2_run_max);

  return prg_val;
}

/* Compute the overlap score of two lists of gcov_info objects PROFILE1 and
   PROFILE2.
   Return 0 on success: without mismatch. Reutrn 1 on error.  */

int
gcov_profile_overlap (struct gcov_info *profile1, struct gcov_info *profile2)
{
  double result;

  result = calculate_overlap (profile1, profile2);

  if (result > 0)
    {
      printf("\nProgram level overlap result is %3.2f%%\n\n", result*100);
      return 0;
    }
  return 1;
}
