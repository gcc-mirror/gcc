/* Map (unsigned int) keys to (source file, line, column) triples.
   Copyright (C) 2001-2016 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "line-map.h"
#include "cpplib.h"
#include "internal.h"
#include "hashtab.h"

/* Do not track column numbers higher than this one.  As a result, the
   range of column_bits is [12, 18] (or 0 if column numbers are
   disabled).  */
const unsigned int LINE_MAP_MAX_COLUMN_NUMBER = (1U << 12);

/* Highest possible source location encoded within an ordinary or
   macro map.  */
const source_location LINE_MAP_MAX_SOURCE_LOCATION = 0x70000000;

static void trace_include (const struct line_maps *, const line_map_ordinary *);
static const line_map_ordinary * linemap_ordinary_map_lookup (struct line_maps *,
							      source_location);
static const line_map_macro* linemap_macro_map_lookup (struct line_maps *,
						       source_location);
static source_location linemap_macro_map_loc_to_def_point
(const line_map_macro *, source_location);
static source_location linemap_macro_map_loc_to_exp_point
(const line_map_macro *, source_location);
static source_location linemap_macro_loc_to_spelling_point
(struct line_maps *, source_location, const line_map_ordinary **);
static source_location linemap_macro_loc_to_def_point (struct line_maps *,
						       source_location,
						       const line_map_ordinary **);
static source_location linemap_macro_loc_to_exp_point (struct line_maps *,
						       source_location,
						       const line_map_ordinary **);

/* Counters defined in macro.c.  */
extern unsigned num_expanded_macros_counter;
extern unsigned num_macro_tokens_counter;

/* Hash function for location_adhoc_data hashtable.  */

static hashval_t
location_adhoc_data_hash (const void *l)
{
  const struct location_adhoc_data *lb =
      (const struct location_adhoc_data *) l;
  return ((hashval_t) lb->locus
	  + (hashval_t) lb->src_range.m_start
	  + (hashval_t) lb->src_range.m_finish
	  + (size_t) lb->data);
}

/* Compare function for location_adhoc_data hashtable.  */

static int
location_adhoc_data_eq (const void *l1, const void *l2)
{
  const struct location_adhoc_data *lb1 =
      (const struct location_adhoc_data *) l1;
  const struct location_adhoc_data *lb2 =
      (const struct location_adhoc_data *) l2;
  return (lb1->locus == lb2->locus
	  && lb1->src_range.m_start == lb2->src_range.m_start
	  && lb1->src_range.m_finish == lb2->src_range.m_finish
	  && lb1->data == lb2->data);
}

/* Update the hashtable when location_adhoc_data is reallocated.  */

static int
location_adhoc_data_update (void **slot, void *data)
{
  *((char **) slot) += *((int64_t *) data);
  return 1;
}

/* Rebuild the hash table from the location adhoc data.  */

void
rebuild_location_adhoc_htab (struct line_maps *set)
{
  unsigned i;
  set->location_adhoc_data_map.htab =
      htab_create (100, location_adhoc_data_hash, location_adhoc_data_eq, NULL);
  for (i = 0; i < set->location_adhoc_data_map.curr_loc; i++)
    htab_find_slot (set->location_adhoc_data_map.htab,
		    set->location_adhoc_data_map.data + i, INSERT);
}

/* Helper function for get_combined_adhoc_loc.
   Can the given LOCUS + SRC_RANGE and DATA pointer be stored compactly
   within a source_location, without needing to use an ad-hoc location.  */

static bool
can_be_stored_compactly_p (struct line_maps *set,
			   source_location locus,
			   source_range src_range,
			   void *data)
{
  /* If there's an ad-hoc pointer, we can't store it directly in the
     source_location, we need the lookaside.  */
  if (data)
    return false;

  /* We only store ranges that begin at the locus and that are sufficiently
     "sane".  */
  if (src_range.m_start != locus)
    return false;

  if (src_range.m_finish < src_range.m_start)
    return false;

  if (src_range.m_start < RESERVED_LOCATION_COUNT)
    return false;

  if (locus >= LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES)
    return false;

  /* All 3 locations must be within ordinary maps, typically, the same
     ordinary map.  */
  source_location lowest_macro_loc = LINEMAPS_MACRO_LOWEST_LOCATION (set);
  if (locus >= lowest_macro_loc)
    return false;
  if (src_range.m_start >= lowest_macro_loc)
    return false;
  if (src_range.m_finish >= lowest_macro_loc)
    return false;

  /* Passed all tests.  */
  return true;
}

/* Combine LOCUS and DATA to a combined adhoc loc.  */

source_location
get_combined_adhoc_loc (struct line_maps *set,
			source_location locus,
			source_range src_range,
			void *data)
{
  struct location_adhoc_data lb;
  struct location_adhoc_data **slot;

  if (IS_ADHOC_LOC (locus))
    locus
      = set->location_adhoc_data_map.data[locus & MAX_SOURCE_LOCATION].locus;
  if (locus == 0 && data == NULL)
    return 0;

  /* Any ordinary locations ought to be "pure" at this point: no
     compressed ranges.  */
  linemap_assert (locus < RESERVED_LOCATION_COUNT
		  || locus >= LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES
		  || locus >= LINEMAPS_MACRO_LOWEST_LOCATION (set)
		  || pure_location_p (set, locus));

  /* Consider short-range optimization.  */
  if (can_be_stored_compactly_p (set, locus, src_range, data))
    {
      /* The low bits ought to be clear.  */
      linemap_assert (pure_location_p (set, locus));
      const line_map *map = linemap_lookup (set, locus);
      const line_map_ordinary *ordmap = linemap_check_ordinary (map);
      unsigned int int_diff = src_range.m_finish - src_range.m_start;
      unsigned int col_diff = (int_diff >> ordmap->m_range_bits);
      if (col_diff < (1U << ordmap->m_range_bits))
	{
	  source_location packed = locus | col_diff;
	  set->num_optimized_ranges++;
	  return packed;
	}
    }

  /* We can also compactly store locations
     when locus == start == finish (and data is NULL).  */
  if (locus == src_range.m_start
      && locus == src_range.m_finish
      && !data)
    return locus;

  if (!data)
    set->num_unoptimized_ranges++;

  lb.locus = locus;
  lb.src_range = src_range;
  lb.data = data;
  slot = (struct location_adhoc_data **)
      htab_find_slot (set->location_adhoc_data_map.htab, &lb, INSERT);
  if (*slot == NULL)
    {
      if (set->location_adhoc_data_map.curr_loc >=
	  set->location_adhoc_data_map.allocated)
	{
	  char *orig_data = (char *) set->location_adhoc_data_map.data;
	  int64_t offset;
	  /* Cast away extern "C" from the type of xrealloc.  */
	  line_map_realloc reallocator = (set->reallocator
					  ? set->reallocator
					  : (line_map_realloc) xrealloc);

	  if (set->location_adhoc_data_map.allocated == 0)
	    set->location_adhoc_data_map.allocated = 128;
	  else
	    set->location_adhoc_data_map.allocated *= 2;
	  set->location_adhoc_data_map.data = (struct location_adhoc_data *)
	      reallocator (set->location_adhoc_data_map.data,
			   set->location_adhoc_data_map.allocated
			   * sizeof (struct location_adhoc_data));
	  offset = (char *) (set->location_adhoc_data_map.data) - orig_data;
	  if (set->location_adhoc_data_map.allocated > 128)
	    htab_traverse (set->location_adhoc_data_map.htab,
			   location_adhoc_data_update, &offset);
	}
      *slot = set->location_adhoc_data_map.data
	      + set->location_adhoc_data_map.curr_loc;
      set->location_adhoc_data_map.data[set->location_adhoc_data_map.curr_loc++]
	= lb;
    }
  return ((*slot) - set->location_adhoc_data_map.data) | 0x80000000;
}

/* Return the data for the adhoc loc.  */

void *
get_data_from_adhoc_loc (struct line_maps *set, source_location loc)
{
  linemap_assert (IS_ADHOC_LOC (loc));
  return set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].data;
}

/* Return the location for the adhoc loc.  */

source_location
get_location_from_adhoc_loc (struct line_maps *set, source_location loc)
{
  linemap_assert (IS_ADHOC_LOC (loc));
  return set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;
}

/* Return the source_range for adhoc location LOC.  */

static source_range
get_range_from_adhoc_loc (struct line_maps *set, source_location loc)
{
  linemap_assert (IS_ADHOC_LOC (loc));
  return set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].src_range;
}

/* Get the source_range of location LOC, either from the ad-hoc
   lookaside table, or embedded inside LOC itself.  */

source_range
get_range_from_loc (struct line_maps *set,
		    source_location loc)
{
  if (IS_ADHOC_LOC (loc))
    return get_range_from_adhoc_loc (set, loc);

  /* For ordinary maps, extract packed range.  */
  if (loc >= RESERVED_LOCATION_COUNT
      && loc < LINEMAPS_MACRO_LOWEST_LOCATION (set)
      && loc <= LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES)
    {
      const line_map *map = linemap_lookup (set, loc);
      const line_map_ordinary *ordmap = linemap_check_ordinary (map);
      source_range result;
      int offset = loc & ((1 << ordmap->m_range_bits) - 1);
      result.m_start = loc - offset;
      result.m_finish = result.m_start + (offset << ordmap->m_range_bits);
      return result;
    }

  return source_range::from_location (loc);
}

/* Get whether location LOC is a "pure" location, or
   whether it is an ad-hoc location, or embeds range information.  */

bool
pure_location_p (line_maps *set, source_location loc)
{
  if (IS_ADHOC_LOC (loc))
    return false;

  const line_map *map = linemap_lookup (set, loc);
  const line_map_ordinary *ordmap = linemap_check_ordinary (map);

  if (loc & ((1U << ordmap->m_range_bits) - 1))
    return false;

  return true;
}

/* Given location LOC within SET, strip away any packed range information
   or ad-hoc information.  */

source_location
get_pure_location (line_maps *set, source_location loc)
{
  if (IS_ADHOC_LOC (loc))
    loc
      = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  if (loc >= LINEMAPS_MACRO_LOWEST_LOCATION (set))
    return loc;

  if (loc < RESERVED_LOCATION_COUNT)
    return loc;

  const line_map *map = linemap_lookup (set, loc);
  const line_map_ordinary *ordmap = linemap_check_ordinary (map);

  return loc & ~((1 << ordmap->m_range_bits) - 1);
}

/* Finalize the location_adhoc_data structure.  */
void
location_adhoc_data_fini (struct line_maps *set)
{
  htab_delete (set->location_adhoc_data_map.htab);
}

/* Initialize a line map set.  */

void
linemap_init (struct line_maps *set,
	      source_location builtin_location)
{
  memset (set, 0, sizeof (struct line_maps));
  set->highest_location = RESERVED_LOCATION_COUNT - 1;
  set->highest_line = RESERVED_LOCATION_COUNT - 1;
  set->location_adhoc_data_map.htab =
      htab_create (100, location_adhoc_data_hash, location_adhoc_data_eq, NULL);
  set->builtin_location = builtin_location;
}

/* Check for and warn about line_maps entered but not exited.  */

void
linemap_check_files_exited (struct line_maps *set)
{
  const line_map_ordinary *map;
  /* Depending upon whether we are handling preprocessed input or
     not, this can be a user error or an ICE.  */
  for (map = LINEMAPS_LAST_ORDINARY_MAP (set);
       ! MAIN_FILE_P (map);
       map = INCLUDED_FROM (set, map))
    fprintf (stderr, "line-map.c: file \"%s\" entered but not left\n",
	     ORDINARY_MAP_FILE_NAME (map));
}

/* Create a new line map in the line map set SET, and return it.
   REASON is the reason of creating the map. It determines the type
   of map created (ordinary or macro map). Note that ordinary maps and
   macro maps are allocated in different memory location.  */

static struct line_map *
new_linemap (struct line_maps *set,
	     enum lc_reason reason)
{
  /* Depending on this variable, a macro map would be allocated in a
     different memory location than an ordinary map.  */
  bool macro_map_p = (reason == LC_ENTER_MACRO);
  struct line_map *result;

  if (LINEMAPS_USED (set, macro_map_p) == LINEMAPS_ALLOCATED (set, macro_map_p))
    {
      /* We ran out of allocated line maps. Let's allocate more.  */
      size_t alloc_size;

      /* Cast away extern "C" from the type of xrealloc.  */
      line_map_realloc reallocator = (set->reallocator
				      ? set->reallocator
				      : (line_map_realloc) xrealloc);
      line_map_round_alloc_size_func round_alloc_size =
	set->round_alloc_size;

      size_t map_size = (macro_map_p
			 ? sizeof (line_map_macro)
			 : sizeof (line_map_ordinary));

      /* We are going to execute some dance to try to reduce the
	 overhead of the memory allocator, in case we are using the
	 ggc-page.c one.
	 
	 The actual size of memory we are going to get back from the
	 allocator is the smallest power of 2 that is greater than the
	 size we requested.  So let's consider that size then.  */

      alloc_size =
	(2 * LINEMAPS_ALLOCATED (set, macro_map_p) +  256)
	* map_size;

      /* Get the actual size of memory that is going to be allocated
	 by the allocator.  */
      alloc_size = round_alloc_size (alloc_size);

      /* Now alloc_size contains the exact memory size we would get if
	 we have asked for the initial alloc_size amount of memory.
	 Let's get back to the number of macro map that amounts
	 to.  */
      LINEMAPS_ALLOCATED (set, macro_map_p) =
	alloc_size / map_size;

      /* And now let's really do the re-allocation.  */
      if (macro_map_p)
	{
	  set->info_macro.maps
	    = (line_map_macro *) (*reallocator) (set->info_macro.maps,
						 (LINEMAPS_ALLOCATED (set, macro_map_p)
						  * map_size));
	  result = &set->info_macro.maps[LINEMAPS_USED (set, macro_map_p)];
	}
      else
	{
	  set->info_ordinary.maps =
	    (line_map_ordinary *) (*reallocator) (set->info_ordinary.maps,
						  (LINEMAPS_ALLOCATED (set, macro_map_p)
						   * map_size));
	  result = &set->info_ordinary.maps[LINEMAPS_USED (set, macro_map_p)];
	}
      memset (result, 0,
	      ((LINEMAPS_ALLOCATED (set, macro_map_p)
		- LINEMAPS_USED (set, macro_map_p))
	       * map_size));
    }
  else
    {
      if (macro_map_p)
	result = &set->info_macro.maps[LINEMAPS_USED (set, macro_map_p)];
      else
	result = &set->info_ordinary.maps[LINEMAPS_USED (set, macro_map_p)];
    }

  LINEMAPS_USED (set, macro_map_p)++;

  result->reason = reason;
  return result;
}

/* Add a mapping of logical source line to physical source file and
   line number.

   The text pointed to by TO_FILE must have a lifetime
   at least as long as the final call to lookup_line ().  An empty
   TO_FILE means standard input.  If reason is LC_LEAVE, and
   TO_FILE is NULL, then TO_FILE, TO_LINE and SYSP are given their
   natural values considering the file we are returning to.

   FROM_LINE should be monotonic increasing across calls to this
   function.  A call to this function can relocate the previous set of
   maps, so any stored line_map pointers should not be used.  */

const struct line_map *
linemap_add (struct line_maps *set, enum lc_reason reason,
	     unsigned int sysp, const char *to_file, linenum_type to_line)
{
  /* Generate a start_location above the current highest_location.
     If possible, make the low range bits be zero.  */
  source_location start_location;
  if (set->highest_location < LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      start_location = set->highest_location + (1 << set->default_range_bits);
      if (set->default_range_bits)
	start_location &= ~((1 << set->default_range_bits) - 1);
      linemap_assert (0 == (start_location
			    & ((1 << set->default_range_bits) - 1)));
    }
  else
    start_location = set->highest_location + 1;

  linemap_assert (!(LINEMAPS_ORDINARY_USED (set)
		    && (start_location
			< MAP_START_LOCATION (LINEMAPS_LAST_ORDINARY_MAP (set)))));

  /* When we enter the file for the first time reason cannot be
     LC_RENAME.  */
  linemap_assert (!(set->depth == 0 && reason == LC_RENAME));

  /* If we are leaving the main file, return a NULL map.  */
  if (reason == LC_LEAVE
      && MAIN_FILE_P (LINEMAPS_LAST_ORDINARY_MAP (set))
      && to_file == NULL)
    {
      set->depth--;
      return NULL;
    }

  linemap_assert (reason != LC_ENTER_MACRO);
  line_map_ordinary *map = linemap_check_ordinary (new_linemap (set, reason));

  if (to_file && *to_file == '\0' && reason != LC_RENAME_VERBATIM)
    to_file = "<stdin>";

  if (reason == LC_RENAME_VERBATIM)
    reason = LC_RENAME;

  if (reason == LC_LEAVE)
    {
      /* When we are just leaving an "included" file, and jump to the next
	 location inside the "includer" right after the #include
	 "included", this variable points the map in use right before the
	 #include "included", inside the same "includer" file.  */
      line_map_ordinary *from;

      linemap_assert (!MAIN_FILE_P (map - 1));
      /* (MAP - 1) points to the map we are leaving. The
	 map from which (MAP - 1) got included should be the map
	 that comes right before MAP in the same file.  */
      from = INCLUDED_FROM (set, map - 1);

      /* A TO_FILE of NULL is special - we use the natural values.  */
      if (to_file == NULL)
	{
	  to_file = ORDINARY_MAP_FILE_NAME (from);
	  to_line = SOURCE_LINE (from, from[1].start_location);
	  sysp = ORDINARY_MAP_IN_SYSTEM_HEADER_P (from);
	}
      else
	linemap_assert (filename_cmp (ORDINARY_MAP_FILE_NAME (from),
				      to_file) == 0);
    }

  map->sysp = sysp;
  map->start_location = start_location;
  map->to_file = to_file;
  map->to_line = to_line;
  LINEMAPS_ORDINARY_CACHE (set) = LINEMAPS_ORDINARY_USED (set) - 1;
  map->m_column_and_range_bits = 0;
  map->m_range_bits = 0;
  set->highest_location = start_location;
  set->highest_line = start_location;
  set->max_column_hint = 0;

  /* This assertion is placed after set->highest_location has
     been updated, since the latter affects
     linemap_location_from_macro_expansion_p, which ultimately affects
     pure_location_p.  */
  linemap_assert (pure_location_p (set, start_location));

  if (reason == LC_ENTER)
    {
      map->included_from =
	set->depth == 0 ? -1 : (int) (LINEMAPS_ORDINARY_USED (set) - 2);
      set->depth++;
      if (set->trace_includes)
	trace_include (set, map);
    }
  else if (reason == LC_RENAME)
    map->included_from = ORDINARY_MAP_INCLUDER_FILE_INDEX (&map[-1]);
  else if (reason == LC_LEAVE)
    {
      set->depth--;
      map->included_from =
	ORDINARY_MAP_INCLUDER_FILE_INDEX (INCLUDED_FROM (set, map - 1));
    }

  return map;
}

/* Returns TRUE if the line table set tracks token locations across
   macro expansion, FALSE otherwise.  */

bool
linemap_tracks_macro_expansion_locs_p (struct line_maps *set)
{
  return LINEMAPS_MACRO_MAPS (set) != NULL;
}

/* Create a macro map.  A macro map encodes source locations of tokens
   that are part of a macro replacement-list, at a macro expansion
   point.  See the extensive comments of struct line_map and struct
   line_map_macro, in line-map.h.

   This map shall be created when the macro is expanded.  The map
   encodes the source location of the expansion point of the macro as
   well as the "original" source location of each token that is part
   of the macro replacement-list.  If a macro is defined but never
   expanded, it has no macro map.  SET is the set of maps the macro
   map should be part of.  MACRO_NODE is the macro which the new macro
   map should encode source locations for.  EXPANSION is the location
   of the expansion point of MACRO. For function-like macros
   invocations, it's best to make it point to the closing parenthesis
   of the macro, rather than the the location of the first character
   of the macro.  NUM_TOKENS is the number of tokens that are part of
   the replacement-list of MACRO.

   Note that when we run out of the integer space available for source
   locations, this function returns NULL.  In that case, callers of
   this function cannot encode {line,column} pairs into locations of
   macro tokens anymore.  */

const line_map_macro *
linemap_enter_macro (struct line_maps *set, struct cpp_hashnode *macro_node,
		     source_location expansion, unsigned int num_tokens)
{
  line_map_macro *map;
  source_location start_location;
  /* Cast away extern "C" from the type of xrealloc.  */
  line_map_realloc reallocator = (set->reallocator
				  ? set->reallocator
				  : (line_map_realloc) xrealloc);

  start_location = LINEMAPS_MACRO_LOWEST_LOCATION (set) - num_tokens;

  if (start_location <= set->highest_line
      || start_location > LINEMAPS_MACRO_LOWEST_LOCATION (set))
    /* We ran out of macro map space.   */
    return NULL;

  map = linemap_check_macro (new_linemap (set, LC_ENTER_MACRO));

  map->start_location = start_location;
  map->macro = macro_node;
  map->n_tokens = num_tokens;
  map->macro_locations
    = (source_location*) reallocator (NULL,
				      2 * num_tokens
				      * sizeof (source_location));
  map->expansion = expansion;
  memset (MACRO_MAP_LOCATIONS (map), 0,
	  num_tokens * sizeof (source_location));

  LINEMAPS_MACRO_CACHE (set) = LINEMAPS_MACRO_USED (set) - 1;

  return map;
}

/* Create and return a virtual location for a token that is part of a
   macro expansion-list at a macro expansion point.  See the comment
   inside struct line_map_macro to see what an expansion-list exactly
   is.

   A call to this function must come after a call to
   linemap_enter_macro.

   MAP is the map into which the source location is created.  TOKEN_NO
   is the index of the token in the macro replacement-list, starting
   at number 0.

   ORIG_LOC is the location of the token outside of this macro
   expansion.  If the token comes originally from the macro
   definition, it is the locus in the macro definition; otherwise it
   is a location in the context of the caller of this macro expansion
   (which is a virtual location or a source location if the caller is
   itself a macro expansion or not).

   ORIG_PARM_REPLACEMENT_LOC is the location in the macro definition,
   either of the token itself or of a macro parameter that it
   replaces.  */

source_location
linemap_add_macro_token (const line_map_macro *map,
			 unsigned int token_no,
			 source_location orig_loc,
			 source_location orig_parm_replacement_loc)
{
  source_location result;

  linemap_assert (linemap_macro_expansion_map_p (map));
  linemap_assert (token_no < MACRO_MAP_NUM_MACRO_TOKENS (map));

  MACRO_MAP_LOCATIONS (map)[2 * token_no] = orig_loc;
  MACRO_MAP_LOCATIONS (map)[2 * token_no + 1] = orig_parm_replacement_loc;

  result = MAP_START_LOCATION (map) + token_no;
  return result;
}

/* Return a source_location for the start (i.e. column==0) of
   (physical) line TO_LINE in the current source file (as in the
   most recent linemap_add).   MAX_COLUMN_HINT is the highest column
   number we expect to use in this line (but it does not change
   the highest_location).  */

source_location
linemap_line_start (struct line_maps *set, linenum_type to_line,
		    unsigned int max_column_hint)
{
  line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (set);
  source_location highest = set->highest_location;
  source_location r;
  linenum_type last_line =
    SOURCE_LINE (map, set->highest_line);
  int line_delta = to_line - last_line;
  bool add_map = false;
  linemap_assert (map->m_column_and_range_bits >= map->m_range_bits);
  int effective_column_bits = map->m_column_and_range_bits - map->m_range_bits;

  if (line_delta < 0
      || (line_delta > 10
	  && line_delta * map->m_column_and_range_bits > 1000)
      || (max_column_hint >= (1U << effective_column_bits))
      || (max_column_hint <= 80 && effective_column_bits >= 10)
      || (highest > LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES
	  && map->m_range_bits > 0)
      || (highest > LINE_MAP_MAX_LOCATION_WITH_COLS
	  && (set->max_column_hint || highest >= LINE_MAP_MAX_SOURCE_LOCATION)))
    add_map = true;
  else
    max_column_hint = set->max_column_hint;
  if (add_map)
    {
      int column_bits;
      int range_bits;
      if (max_column_hint > LINE_MAP_MAX_COLUMN_NUMBER
	  || highest > LINE_MAP_MAX_LOCATION_WITH_COLS)
	{
	  /* If the column number is ridiculous or we've allocated a huge
	     number of source_locations, give up on column numbers
	     (and on packed ranges).  */
	  max_column_hint = 0;
	  column_bits = 0;
	  range_bits = 0;
	  if (highest > LINE_MAP_MAX_SOURCE_LOCATION)
	    return 0;
	}
      else
	{
	  column_bits = 7;
	  if (highest <= LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES)
	    range_bits = set->default_range_bits;
	  else
	    range_bits = 0;
	  while (max_column_hint >= (1U << column_bits))
	    column_bits++;
	  max_column_hint = 1U << column_bits;
	  column_bits += range_bits;
	}
      /* Allocate the new line_map.  However, if the current map only has a
	 single line we can sometimes just increase its column_bits instead. */
      if (line_delta < 0
	  || last_line != ORDINARY_MAP_STARTING_LINE_NUMBER (map)
	  || SOURCE_COLUMN (map, highest) >= (1U << column_bits)
	  || range_bits < map->m_range_bits)
	map = linemap_check_ordinary
	        (const_cast <line_map *>
		  (linemap_add (set, LC_RENAME,
				ORDINARY_MAP_IN_SYSTEM_HEADER_P (map),
				ORDINARY_MAP_FILE_NAME (map),
				to_line)));
      map->m_column_and_range_bits = column_bits;
      map->m_range_bits = range_bits;
      r = (MAP_START_LOCATION (map)
	   + ((to_line - ORDINARY_MAP_STARTING_LINE_NUMBER (map))
	      << column_bits));
    }
  else
    r = set->highest_line + (line_delta << map->m_column_and_range_bits);

  /* Locations of ordinary tokens are always lower than locations of
     macro tokens.  */
  if (r >= LINEMAPS_MACRO_LOWEST_LOCATION (set))
    return 0;

  set->highest_line = r;
  if (r > set->highest_location)
    set->highest_location = r;
  set->max_column_hint = max_column_hint;

  /* At this point, we expect one of:
     (a) the normal case: a "pure" location with 0 range bits, or
     (b) we've gone past LINE_MAP_MAX_LOCATION_WITH_COLS so can't track
        columns anymore (or ranges), or
     (c) we're in a region with a column hint exceeding
        LINE_MAP_MAX_COLUMN_NUMBER, so column-tracking is off,
	with column_bits == 0.  */
  linemap_assert (pure_location_p (set, r)
		  || r >= LINE_MAP_MAX_LOCATION_WITH_COLS
		  || map->m_column_and_range_bits == 0);
  linemap_assert (SOURCE_LINE (map, r) == to_line);
  return r;
}

/* Encode and return a source_location from a column number. The
   source line considered is the last source line used to call
   linemap_line_start, i.e, the last source line which a location was
   encoded from.  */

source_location
linemap_position_for_column (struct line_maps *set, unsigned int to_column)
{
  source_location r = set->highest_line;

  linemap_assert
    (!linemap_macro_expansion_map_p (LINEMAPS_LAST_ORDINARY_MAP (set)));

  if (to_column >= set->max_column_hint)
    {
      if (r > LINE_MAP_MAX_LOCATION_WITH_COLS
	  || to_column > LINE_MAP_MAX_COLUMN_NUMBER)
	{
	  /* Running low on source_locations - disable column numbers.  */
	  return r;
	}
      else
	{
	  line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (set);
	  r = linemap_line_start (set, SOURCE_LINE (map, r), to_column + 50);
	}
    }
  line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (set);
  r = r + (to_column << map->m_range_bits);
  if (r >= set->highest_location)
    set->highest_location = r;
  return r;
}

/* Encode and return a source location from a given line and
   column.  */

source_location
linemap_position_for_line_and_column (line_maps *set,
				      const line_map_ordinary *ord_map,
				      linenum_type line,
				      unsigned column)
{
  linemap_assert (ORDINARY_MAP_STARTING_LINE_NUMBER (ord_map) <= line);

  source_location r = MAP_START_LOCATION (ord_map);
  r += ((line - ORDINARY_MAP_STARTING_LINE_NUMBER (ord_map))
	<< ord_map->m_column_and_range_bits);
  if (r <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    r += ((column & ((1 << ord_map->m_column_and_range_bits) - 1))
	  << ord_map->m_range_bits);
  source_location upper_limit = LINEMAPS_MACRO_LOWEST_LOCATION (set);
  if (r >= upper_limit)
    r = upper_limit - 1;
  if (r > set->highest_location)
    set->highest_location = r;
  return r;
}

/* Encode and return a source_location starting from location LOC and
   shifting it by COLUMN_OFFSET columns.  This function does not support
   virtual locations.  */

source_location
linemap_position_for_loc_and_offset (struct line_maps *set,
				     source_location loc,
				     unsigned int column_offset)
{
  const line_map_ordinary * map = NULL;

  if (IS_ADHOC_LOC (loc))
    loc = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  /* This function does not support virtual locations yet.  */
  if (linemap_assert_fails
      (!linemap_location_from_macro_expansion_p (set, loc)))
    return loc;

  if (column_offset == 0
      /* Adding an offset to a reserved location (like
	 UNKNOWN_LOCATION for the C/C++ FEs) does not really make
	 sense.  So let's leave the location intact in that case.  */
      || loc < RESERVED_LOCATION_COUNT)
    return loc;

  /* We find the real location and shift it.  */
  loc = linemap_resolve_location (set, loc, LRK_SPELLING_LOCATION, &map);
  /* The new location (loc + offset) should be higher than the first
     location encoded by MAP.  This can fail if the line information
     is messed up because of line directives (see PR66415).  */
  if (MAP_START_LOCATION (map) >= loc + (column_offset << map->m_range_bits))
    return loc;

  linenum_type line = SOURCE_LINE (map, loc);
  unsigned int column = SOURCE_COLUMN (map, loc);

  /* If MAP is not the last line map of its set, then the new location
     (loc + offset) should be less than the first location encoded by
     the next line map of the set.  Otherwise, we try to encode the
     location in the next map.  */
  while (map != LINEMAPS_LAST_ORDINARY_MAP (set)
	 && (loc + (column_offset << map->m_range_bits)
	     >= MAP_START_LOCATION (&map[1])))
    {
      map = &map[1];
      /* If the next map starts in a higher line, we cannot encode the
	 location there.  */
      if (line < ORDINARY_MAP_STARTING_LINE_NUMBER (map))
	return loc;
    }

  column += column_offset;
  if (linemap_assert_fails (column < (1u << map->m_column_and_range_bits)))
    return loc;

  source_location r = 
    linemap_position_for_line_and_column (set, map, line, column);
  if (linemap_assert_fails (r <= set->highest_location)
      || linemap_assert_fails (map == linemap_lookup (set, r)))
    return loc;

  return r;
}

/* Given a virtual source location yielded by a map (either an
   ordinary or a macro map), returns that map.  */

const struct line_map*
linemap_lookup (struct line_maps *set, source_location line)
{
  if (IS_ADHOC_LOC (line))
    line = set->location_adhoc_data_map.data[line & MAX_SOURCE_LOCATION].locus;
  if (linemap_location_from_macro_expansion_p (set, line))
    return linemap_macro_map_lookup (set, line);
  return linemap_ordinary_map_lookup (set, line);
}

/* Given a source location yielded by an ordinary map, returns that
   map.  Since the set is built chronologically, the logical lines are
   monotonic increasing, and so the list is sorted and we can use a
   binary search.  */

static const line_map_ordinary *
linemap_ordinary_map_lookup (struct line_maps *set, source_location line)
{
  unsigned int md, mn, mx;
  const line_map_ordinary *cached, *result;

  if (IS_ADHOC_LOC (line))
    line = set->location_adhoc_data_map.data[line & MAX_SOURCE_LOCATION].locus;

  if (set ==  NULL || line < RESERVED_LOCATION_COUNT)
    return NULL;

  mn = LINEMAPS_ORDINARY_CACHE (set);
  mx = LINEMAPS_ORDINARY_USED (set);
  
  cached = LINEMAPS_ORDINARY_MAP_AT (set, mn);
  /* We should get a segfault if no line_maps have been added yet.  */
  if (line >= MAP_START_LOCATION (cached))
    {
      if (mn + 1 == mx || line < MAP_START_LOCATION (&cached[1]))
	return cached;
    }
  else
    {
      mx = mn;
      mn = 0;
    }

  while (mx - mn > 1)
    {
      md = (mn + mx) / 2;
      if (MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (set, md)) > line)
	mx = md;
      else
	mn = md;
    }

  LINEMAPS_ORDINARY_CACHE (set) = mn;
  result = LINEMAPS_ORDINARY_MAP_AT (set, mn);
  linemap_assert (line >= MAP_START_LOCATION (result));
  return result;
}

/* Given a source location yielded by a macro map, returns that map.
   Since the set is built chronologically, the logical lines are
   monotonic decreasing, and so the list is sorted and we can use a
   binary search.  */

static const line_map_macro *
linemap_macro_map_lookup (struct line_maps *set, source_location line)
{
  unsigned int md, mn, mx;
  const struct line_map_macro *cached, *result;

  if (IS_ADHOC_LOC (line))
    line = set->location_adhoc_data_map.data[line & MAX_SOURCE_LOCATION].locus;

  linemap_assert (line >= LINEMAPS_MACRO_LOWEST_LOCATION (set));

  if (set ==  NULL)
    return NULL;

  mn = LINEMAPS_MACRO_CACHE (set);
  mx = LINEMAPS_MACRO_USED (set);
  cached = LINEMAPS_MACRO_MAP_AT (set, mn);
  
  if (line >= MAP_START_LOCATION (cached))
    {
      if (mn == 0 || line < MAP_START_LOCATION (&cached[-1]))
	return cached;
      mx = mn - 1;
      mn = 0;
    }

  while (mn < mx)
    {
      md = (mx + mn) / 2;
      if (MAP_START_LOCATION (LINEMAPS_MACRO_MAP_AT (set, md)) > line)
	mn = md + 1;
      else
	mx = md;
    }

  LINEMAPS_MACRO_CACHE (set) = mx;
  result = LINEMAPS_MACRO_MAP_AT (set, LINEMAPS_MACRO_CACHE (set));
  linemap_assert (MAP_START_LOCATION (result) <= line);

  return result;
}

/* Return TRUE if MAP encodes locations coming from a macro
   replacement-list at macro expansion point.  */

bool
linemap_macro_expansion_map_p (const struct line_map *map)
{
  if (!map)
    return false;
  return (map->reason == LC_ENTER_MACRO);
}

/* If LOCATION is the locus of a token in a replacement-list of a
   macro expansion return the location of the macro expansion point.

   Read the comments of struct line_map and struct line_map_macro in
   line-map.h to understand what a macro expansion point is.  */

static source_location
linemap_macro_map_loc_to_exp_point (const line_map_macro *map,
				    source_location location ATTRIBUTE_UNUSED)
{
  linemap_assert (linemap_macro_expansion_map_p (map)
		  && location >= MAP_START_LOCATION (map));

  /* Make sure LOCATION is correct.  */
  linemap_assert ((location - MAP_START_LOCATION (map))
		  <  MACRO_MAP_NUM_MACRO_TOKENS (map));

  return MACRO_MAP_EXPANSION_POINT_LOCATION (map);
}

/* LOCATION is the source location of a token that belongs to a macro
   replacement-list as part of the macro expansion denoted by MAP.

   Return the location of the token at the definition point of the
   macro.  */

static source_location
linemap_macro_map_loc_to_def_point (const line_map_macro *map,
				    source_location location)
{
  unsigned token_no;

  linemap_assert (linemap_macro_expansion_map_p (map)
		  && location >= MAP_START_LOCATION (map));
  linemap_assert (location >= RESERVED_LOCATION_COUNT);

  token_no = location - MAP_START_LOCATION (map);
  linemap_assert (token_no < MACRO_MAP_NUM_MACRO_TOKENS (map));

  location = MACRO_MAP_LOCATIONS (map)[2 * token_no + 1];

  return location;
}

/* If LOCATION is the locus of a token that is an argument of a
   function-like macro M and appears in the expansion of M, return the
   locus of that argument in the context of the caller of M.

   In other words, this returns the xI location presented in the
   comments of line_map_macro above.  */
source_location
linemap_macro_map_loc_unwind_toward_spelling (line_maps *set,
					      const line_map_macro* map,
					      source_location location)
{
  unsigned token_no;

  if (IS_ADHOC_LOC (location))
    location = get_location_from_adhoc_loc (set, location);

  linemap_assert (linemap_macro_expansion_map_p (map)
		  && location >= MAP_START_LOCATION (map));
  linemap_assert (location >= RESERVED_LOCATION_COUNT);
  linemap_assert (!IS_ADHOC_LOC (location));

  token_no = location - MAP_START_LOCATION (map);
  linemap_assert (token_no < MACRO_MAP_NUM_MACRO_TOKENS (map));

  location = MACRO_MAP_LOCATIONS (map)[2 * token_no];
  
  return location;
}

/* Return the source line number corresponding to source location
   LOCATION.  SET is the line map set LOCATION comes from.  If
   LOCATION is the source location of token that is part of the
   replacement-list of a macro expansion return the line number of the
   macro expansion point.  */

int
linemap_get_expansion_line (struct line_maps *set,
			    source_location location)
{
  const line_map_ordinary *map = NULL;

  if (IS_ADHOC_LOC (location))
    location = set->location_adhoc_data_map.data[location
						 & MAX_SOURCE_LOCATION].locus;

  if (location < RESERVED_LOCATION_COUNT)
    return 0;

  location =
    linemap_macro_loc_to_exp_point (set, location, &map);

  return SOURCE_LINE (map, location);
}

/* Return the path of the file corresponding to source code location
   LOCATION.

   If LOCATION is the source location of token that is part of the
   replacement-list of a macro expansion return the file path of the
   macro expansion point.

   SET is the line map set LOCATION comes from.  */

const char*
linemap_get_expansion_filename (struct line_maps *set,
				source_location location)
{
  const struct line_map_ordinary *map = NULL;

  if (IS_ADHOC_LOC (location))
    location = set->location_adhoc_data_map.data[location
						 & MAX_SOURCE_LOCATION].locus;

  if (location < RESERVED_LOCATION_COUNT)
    return NULL;

  location =
    linemap_macro_loc_to_exp_point (set, location, &map);

  return LINEMAP_FILE (map);
}

/* Return the name of the macro associated to MACRO_MAP.  */

const char*
linemap_map_get_macro_name (const line_map_macro *macro_map)
{
  linemap_assert (macro_map && linemap_macro_expansion_map_p (macro_map));
  return (const char*) NODE_NAME (MACRO_MAP_MACRO (macro_map));
}

/* Return a positive value if LOCATION is the locus of a token that is
   located in a system header, O otherwise. It returns 1 if LOCATION
   is the locus of a token that is located in a system header, and 2
   if LOCATION is the locus of a token located in a C system header
   that therefore needs to be extern "C" protected in C++.

   Note that this function returns 1 if LOCATION belongs to a token
   that is part of a macro replacement-list defined in a system
   header, but expanded in a non-system file.  */

int
linemap_location_in_system_header_p (struct line_maps *set,
				     source_location location)
{
  const struct line_map *map = NULL;

  if (IS_ADHOC_LOC (location))
    location = set->location_adhoc_data_map.data[location
						 & MAX_SOURCE_LOCATION].locus;

  if (location < RESERVED_LOCATION_COUNT)
    return false;

  /* Let's look at where the token for LOCATION comes from.  */
  while (true)
    {
      map = linemap_lookup (set, location);
      if (map != NULL)
	{
	  if (!linemap_macro_expansion_map_p (map))
	    /* It's a normal token.  */
	    return LINEMAP_SYSP (linemap_check_ordinary (map));
	  else
	    {
	      const line_map_macro *macro_map = linemap_check_macro (map);

	      /* It's a token resulting from a macro expansion.  */
	      source_location loc =
		linemap_macro_map_loc_unwind_toward_spelling (set, macro_map, location);
	      if (loc < RESERVED_LOCATION_COUNT)
		/* This token might come from a built-in macro.  Let's
		   look at where that macro got expanded.  */
		location = linemap_macro_map_loc_to_exp_point (macro_map, location);
	      else
		location = loc;
	    }
	}
      else
	break;
    }
  return false;
}

/* Return TRUE if LOCATION is a source code location of a token coming
   from a macro replacement-list at a macro expansion point, FALSE
   otherwise.  */

bool
linemap_location_from_macro_expansion_p (const struct line_maps *set,
					 source_location location)
{
  if (IS_ADHOC_LOC (location))
    location = set->location_adhoc_data_map.data[location
						 & MAX_SOURCE_LOCATION].locus;

  linemap_assert (location <= MAX_SOURCE_LOCATION
		  && (set->highest_location
		      < LINEMAPS_MACRO_LOWEST_LOCATION (set)));
  if (set == NULL)
    return false;
  return (location > set->highest_location);
}

/* Given two virtual locations *LOC0 and *LOC1, return the first
   common macro map in their macro expansion histories.  Return NULL
   if no common macro was found.  *LOC0 (resp. *LOC1) is set to the
   virtual location of the token inside the resulting macro.  */

static const struct line_map*
first_map_in_common_1 (struct line_maps *set,
		       source_location *loc0,
		       source_location *loc1)
{
  source_location l0 = *loc0, l1 = *loc1;
  const struct line_map *map0 = linemap_lookup (set, l0),
    *map1 = linemap_lookup (set, l1);

  while (linemap_macro_expansion_map_p (map0)
	 && linemap_macro_expansion_map_p (map1)
	 && (map0 != map1))
    {
      if (MAP_START_LOCATION (map0) < MAP_START_LOCATION (map1))
	{
	  l0 = linemap_macro_map_loc_to_exp_point (linemap_check_macro (map0),
						   l0);
	  map0 = linemap_lookup (set, l0);
	}
      else
	{
	  l1 = linemap_macro_map_loc_to_exp_point (linemap_check_macro (map1),
						   l1);
	  map1 = linemap_lookup (set, l1);
	}
    }

  if (map0 == map1)
    {
      *loc0 = l0;
      *loc1 = l1;
      return map0;
    }
  return NULL;
}

/* Given two virtual locations LOC0 and LOC1, return the first common
   macro map in their macro expansion histories.  Return NULL if no
   common macro was found.  *RES_LOC0 (resp. *RES_LOC1) is set to the
   virtual location of the token inside the resulting macro, upon
   return of a non-NULL result.  */

static const struct line_map*
first_map_in_common (struct line_maps *set,
		     source_location loc0,
		     source_location loc1,
		     source_location  *res_loc0,
		     source_location  *res_loc1)
{
  *res_loc0 = loc0;
  *res_loc1 = loc1;

  return first_map_in_common_1 (set, res_loc0, res_loc1);
}

/* Return a positive value if PRE denotes the location of a token that
   comes before the token of POST, 0 if PRE denotes the location of
   the same token as the token for POST, and a negative value
   otherwise.  */

int
linemap_compare_locations (struct line_maps *set,
			   source_location  pre,
			   source_location post)
{
  bool pre_virtual_p, post_virtual_p;
  source_location l0 = pre, l1 = post;

  if (IS_ADHOC_LOC (l0))
    l0 = get_location_from_adhoc_loc (set, l0);
  if (IS_ADHOC_LOC (l1))
    l1 = get_location_from_adhoc_loc (set, l1);

  if (l0 == l1)
    return 0;

  if ((pre_virtual_p = linemap_location_from_macro_expansion_p (set, l0)))
    l0 = linemap_resolve_location (set, l0,
				   LRK_MACRO_EXPANSION_POINT,
				   NULL);

  if ((post_virtual_p = linemap_location_from_macro_expansion_p (set, l1)))
    l1 = linemap_resolve_location (set, l1,
				   LRK_MACRO_EXPANSION_POINT,
				   NULL);

  if (l0 == l1
      && pre_virtual_p
      && post_virtual_p)
    {
      /* So pre and post represent two tokens that are present in a
	 same macro expansion.  Let's see if the token for pre was
	 before the token for post in that expansion.  */
      unsigned i0, i1;
      const struct line_map *map =
	first_map_in_common (set, pre, post, &l0, &l1);

      if (map == NULL)
	/* This should not be possible.  */
	abort ();

      i0 = l0 - MAP_START_LOCATION (map);
      i1 = l1 - MAP_START_LOCATION (map);
      return i1 - i0;
    }

  if (IS_ADHOC_LOC (l0))
    l0 = get_location_from_adhoc_loc (set, l0);
  if (IS_ADHOC_LOC (l1))
    l1 = get_location_from_adhoc_loc (set, l1);

  return l1 - l0;
}

/* Print an include trace, for e.g. the -H option of the preprocessor.  */

static void
trace_include (const struct line_maps *set, const line_map_ordinary *map)
{
  unsigned int i = set->depth;

  while (--i)
    putc ('.', stderr);

  fprintf (stderr, " %s\n", ORDINARY_MAP_FILE_NAME (map));
}

/* Return the spelling location of the token wherever it comes from,
   whether part of a macro definition or not.

   This is a subroutine for linemap_resolve_location.  */

static source_location
linemap_macro_loc_to_spelling_point (struct line_maps *set,
				     source_location location,
				     const line_map_ordinary **original_map)
{
  struct line_map *map;
  linemap_assert (set && location >= RESERVED_LOCATION_COUNT);

  while (true)
    {
      map = const_cast <line_map *> (linemap_lookup (set, location));
      if (!linemap_macro_expansion_map_p (map))
	break;

      location
	= linemap_macro_map_loc_unwind_toward_spelling
	    (set, linemap_check_macro (map),
	     location);
    }

  if (original_map)
    *original_map = linemap_check_ordinary (map);
  return location;
}

/* If LOCATION is the source location of a token that belongs to a
   macro replacement-list -- as part of a macro expansion -- then
   return the location of the token at the definition point of the
   macro.  Otherwise, return LOCATION.  SET is the set of maps
   location come from.  ORIGINAL_MAP is an output parm. If non NULL,
   the function sets *ORIGINAL_MAP to the ordinary (non-macro) map the
   returned location comes from. 

   This is a subroutine of linemap_resolve_location.  */

static source_location
linemap_macro_loc_to_def_point (struct line_maps *set,
				source_location location,
				const line_map_ordinary **original_map)
{
  struct line_map *map;

  if (IS_ADHOC_LOC (location))
    location = set->location_adhoc_data_map.data[location
						 & MAX_SOURCE_LOCATION].locus;

  linemap_assert (set && location >= RESERVED_LOCATION_COUNT);

  while (true)
    {
      map = const_cast <line_map *> (linemap_lookup (set, location));
      if (!linemap_macro_expansion_map_p (map))
	break;

      location =
	linemap_macro_map_loc_to_def_point (linemap_check_macro (map),
					    location);
    }

  if (original_map)
    *original_map = linemap_check_ordinary (map);
  return location;
}

/* If LOCATION is the source location of a token that belongs to a
   macro replacement-list -- at a macro expansion point -- then return
   the location of the topmost expansion point of the macro.  We say
   topmost because if we are in the context of a nested macro
   expansion, the function returns the source location of the first
   macro expansion that triggered the nested expansions.

   Otherwise, return LOCATION.  SET is the set of maps location come
   from.  ORIGINAL_MAP is an output parm. If non NULL, the function
   sets *ORIGINAL_MAP to the ordinary (non-macro) map the returned
   location comes from.

   This is a subroutine of linemap_resolve_location.  */

static source_location
linemap_macro_loc_to_exp_point (struct line_maps *set,
				source_location location,
				const line_map_ordinary **original_map)
{
  struct line_map *map;

  if (IS_ADHOC_LOC (location))
    location = set->location_adhoc_data_map.data[location
						 & MAX_SOURCE_LOCATION].locus;

  linemap_assert (set && location >= RESERVED_LOCATION_COUNT);

  while (true)
    {
      map = const_cast <line_map *> (linemap_lookup (set, location));
      if (!linemap_macro_expansion_map_p (map))
	break;
      location = linemap_macro_map_loc_to_exp_point (linemap_check_macro (map),
						     location);
    }

  if (original_map)
    *original_map = linemap_check_ordinary (map);
  return location;
}

/* Resolve a virtual location into either a spelling location, an
   expansion point location or a token argument replacement point
   location.  Return the map that encodes the virtual location as well
   as the resolved location.

   If LOC is *NOT* the location of a token resulting from the
   expansion of a macro, then the parameter LRK (which stands for
   Location Resolution Kind) is ignored and the resulting location
   just equals the one given in argument.

   Now if LOC *IS* the location of a token resulting from the
   expansion of a macro, this is what happens.

   * If LRK is set to LRK_MACRO_EXPANSION_POINT
   -------------------------------

   The virtual location is resolved to the first macro expansion point
   that led to this macro expansion.

   * If LRK is set to LRK_SPELLING_LOCATION
   -------------------------------------

   The virtual location is resolved to the locus where the token has
   been spelled in the source.   This can follow through all the macro
   expansions that led to the token.

   * If LRK is set to LRK_MACRO_DEFINITION_LOCATION
   --------------------------------------

   The virtual location is resolved to the locus of the token in the
   context of the macro definition.

   If LOC is the locus of a token that is an argument of a
   function-like macro [replacing a parameter in the replacement list
   of the macro] the virtual location is resolved to the locus of the
   parameter that is replaced, in the context of the definition of the
   macro.

   If LOC is the locus of a token that is not an argument of a
   function-like macro, then the function behaves as if LRK was set to
   LRK_SPELLING_LOCATION.

   If MAP is not NULL, *MAP is set to the map encoding the
   returned location.  Note that if the returned location wasn't originally
   encoded by a map, then *MAP is set to NULL.  This can happen if LOC
   resolves to a location reserved for the client code, like
   UNKNOWN_LOCATION or BUILTINS_LOCATION in GCC.  */

source_location
linemap_resolve_location (struct line_maps *set,
			  source_location loc,
			  enum location_resolution_kind lrk,
			  const line_map_ordinary **map)
{
  source_location locus = loc;
  if (IS_ADHOC_LOC (loc))
    locus = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  if (locus < RESERVED_LOCATION_COUNT)
    {
      /* A reserved location wasn't encoded in a map.  Let's return a
	 NULL map here, just like what linemap_ordinary_map_lookup
	 does.  */
      if (map)
	*map = NULL;
      return loc;
    }

  switch (lrk)
    {
    case LRK_MACRO_EXPANSION_POINT:
      loc = linemap_macro_loc_to_exp_point (set, loc, map);
      break;
    case LRK_SPELLING_LOCATION:
      loc = linemap_macro_loc_to_spelling_point (set, loc, map);
      break;
    case LRK_MACRO_DEFINITION_LOCATION:
      loc = linemap_macro_loc_to_def_point (set, loc, map);
      break;
    default:
      abort ();
    }
  return loc;
}

/* 
   Suppose that LOC is the virtual location of a token T coming from
   the expansion of a macro M.  This function then steps up to get the
   location L of the point where M got expanded.  If L is a spelling
   location inside a macro expansion M', then this function returns
   the locus of the point where M' was expanded.  Said otherwise, this
   function returns the location of T in the context that triggered
   the expansion of M. 

   *LOC_MAP must be set to the map of LOC.  This function then sets it
   to the map of the returned location.  */

source_location
linemap_unwind_toward_expansion (struct line_maps *set,
				 source_location loc,
				 const struct line_map **map)
{
  source_location resolved_location;
  const line_map_macro *macro_map = linemap_check_macro (*map);
  const struct line_map *resolved_map;

  if (IS_ADHOC_LOC (loc))
    loc = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  resolved_location =
    linemap_macro_map_loc_unwind_toward_spelling (set, macro_map, loc);
  resolved_map = linemap_lookup (set, resolved_location);

  if (!linemap_macro_expansion_map_p (resolved_map))
    {
      resolved_location = linemap_macro_map_loc_to_exp_point (macro_map, loc);
      resolved_map = linemap_lookup (set, resolved_location);
    }

  *map = resolved_map;
  return resolved_location;
}

/* If LOC is the virtual location of a token coming from the expansion
   of a macro M and if its spelling location is reserved (e.g, a
   location for a built-in token), then this function unwinds (using
   linemap_unwind_toward_expansion) the location until a location that
   is not reserved and is not in a system header is reached.  In other
   words, this unwinds the reserved location until a location that is
   in real source code is reached.

   Otherwise, if the spelling location for LOC is not reserved or if
   LOC doesn't come from the expansion of a macro, the function
   returns LOC as is and *MAP is not touched.

   *MAP is set to the map of the returned location if the later is
   different from LOC.  */
source_location
linemap_unwind_to_first_non_reserved_loc (struct line_maps *set,
					  source_location loc,
					  const struct line_map **map)
{
  source_location resolved_loc;
  const struct line_map *map0 = NULL;
  const line_map_ordinary *map1 = NULL;

  if (IS_ADHOC_LOC (loc))
    loc = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  map0 = linemap_lookup (set, loc);
  if (!linemap_macro_expansion_map_p (map0))
    return loc;

  resolved_loc = linemap_resolve_location (set, loc,
					   LRK_SPELLING_LOCATION,
					   &map1);

  if (resolved_loc >= RESERVED_LOCATION_COUNT
      && !LINEMAP_SYSP (map1))
    return loc;

  while (linemap_macro_expansion_map_p (map0)
	 && (resolved_loc < RESERVED_LOCATION_COUNT
	     || LINEMAP_SYSP (map1)))
    {
      loc = linemap_unwind_toward_expansion (set, loc, &map0);
      resolved_loc = linemap_resolve_location (set, loc,
					       LRK_SPELLING_LOCATION,
					       &map1);
    }

  if (map != NULL)
    *map = map0;
  return loc;
}

/* Expand source code location LOC and return a user readable source
   code location.  LOC must be a spelling (non-virtual) location.  If
   it's a location < RESERVED_LOCATION_COUNT a zeroed expanded source
   location is returned.  */

expanded_location
linemap_expand_location (struct line_maps *set,
			 const struct line_map *map,
			 source_location loc)

{
  expanded_location xloc;

  memset (&xloc, 0, sizeof (xloc));
  if (IS_ADHOC_LOC (loc))
    {
      xloc.data
	= set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].data;
      loc = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;
    }

  if (loc < RESERVED_LOCATION_COUNT)
    /* The location for this token wasn't generated from a line map.
       It was probably a location for a builtin token, chosen by some
       client code.  Let's not try to expand the location in that
       case.  */;
  else if (map == NULL)
    /* We shouldn't be getting a NULL map with a location that is not
       reserved by the client code.  */
    abort ();
  else
    {
      /* MAP must be an ordinary map and LOC must be non-virtual,
	 encoded into this map, obviously; the accessors used on MAP
	 below ensure it is ordinary.  Let's just assert the
	 non-virtualness of LOC here.  */
      if (linemap_location_from_macro_expansion_p (set, loc))
	abort ();

      const line_map_ordinary *ord_map = linemap_check_ordinary (map);

      xloc.file = LINEMAP_FILE (ord_map);
      xloc.line = SOURCE_LINE (ord_map, loc);
      xloc.column = SOURCE_COLUMN (ord_map, loc);
      xloc.sysp = LINEMAP_SYSP (ord_map) != 0;
    }

  return xloc;
}


/* Dump line map at index IX in line table SET to STREAM.  If STREAM
   is NULL, use stderr.  IS_MACRO is true if the caller wants to
   dump a macro map, false otherwise.  */

void
linemap_dump (FILE *stream, struct line_maps *set, unsigned ix, bool is_macro)
{
  const char *lc_reasons_v[LC_ENTER_MACRO + 1]
      = { "LC_ENTER", "LC_LEAVE", "LC_RENAME", "LC_RENAME_VERBATIM",
	  "LC_ENTER_MACRO" };
  const char *reason;
  const line_map *map;

  if (stream == NULL)
    stream = stderr;

  if (!is_macro)
    map = LINEMAPS_ORDINARY_MAP_AT (set, ix);
  else
    map = LINEMAPS_MACRO_MAP_AT (set, ix);

  reason = (map->reason <= LC_ENTER_MACRO) ? lc_reasons_v[map->reason] : "???";

  fprintf (stream, "Map #%u [%p] - LOC: %u - REASON: %s - SYSP: %s\n",
	   ix, (void *) map, map->start_location, reason,
	   ((!is_macro
	     && ORDINARY_MAP_IN_SYSTEM_HEADER_P (linemap_check_ordinary (map)))
	    ? "yes" : "no"));
  if (!is_macro)
    {
      const line_map_ordinary *ord_map = linemap_check_ordinary (map);
      unsigned includer_ix;
      const line_map_ordinary *includer_map;

      includer_ix = ORDINARY_MAP_INCLUDER_FILE_INDEX (ord_map);
      includer_map = includer_ix < LINEMAPS_ORDINARY_USED (set)
		     ? LINEMAPS_ORDINARY_MAP_AT (set, includer_ix)
		     : NULL;

      fprintf (stream, "File: %s:%d\n", ORDINARY_MAP_FILE_NAME (ord_map),
	       ORDINARY_MAP_STARTING_LINE_NUMBER (ord_map));
      fprintf (stream, "Included from: [%d] %s\n", includer_ix,
	       includer_map ? ORDINARY_MAP_FILE_NAME (includer_map) : "None");
    }
  else
    {
      const line_map_macro *macro_map = linemap_check_macro (map);
      fprintf (stream, "Macro: %s (%u tokens)\n",
	       linemap_map_get_macro_name (macro_map),
	       MACRO_MAP_NUM_MACRO_TOKENS (macro_map));
    }

  fprintf (stream, "\n");
}


/* Dump debugging information about source location LOC into the file
   stream STREAM. SET is the line map set LOC comes from.  */

void
linemap_dump_location (struct line_maps *set,
		       source_location loc,
		       FILE *stream)
{
  const line_map_ordinary *map;
  source_location location;
  const char *path = "", *from = "";
  int l = -1, c = -1, s = -1, e = -1;

  if (IS_ADHOC_LOC (loc))
    loc = set->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  if (loc == 0)
    return;

  location =
    linemap_resolve_location (set, loc, LRK_MACRO_DEFINITION_LOCATION, &map);

  if (map == NULL)
    /* Only reserved locations can be tolerated in this case.  */
    linemap_assert (location < RESERVED_LOCATION_COUNT);
  else
    {
      path = LINEMAP_FILE (map);
      l = SOURCE_LINE (map, location);
      c = SOURCE_COLUMN (map, location);
      s = LINEMAP_SYSP (map) != 0;
      e = location != loc;
      if (e)
	from = "N/A";
      else
	from = (INCLUDED_FROM (set, map))
	  ? LINEMAP_FILE (INCLUDED_FROM (set, map))
	  : "<NULL>";
    }

  /* P: path, L: line, C: column, S: in-system-header, M: map address,
     E: macro expansion?, LOC: original location, R: resolved location   */
  fprintf (stream, "{P:%s;F:%s;L:%d;C:%d;S:%d;M:%p;E:%d,LOC:%d,R:%d}",
	   path, from, l, c, s, (void*)map, e, loc, location);
}

/* Return the highest location emitted for a given file for which
   there is a line map in SET.  FILE_NAME is the file name to
   consider.  If the function returns TRUE, *LOC is set to the highest
   location emitted for that file.  */

bool
linemap_get_file_highest_location (struct line_maps *set,
				   const char *file_name,
				   source_location *loc)
{
  /* If the set is empty or no ordinary map has been created then
     there is no file to look for ...  */
  if (set == NULL || set->info_ordinary.used == 0)
    return false;

  /* Now look for the last ordinary map created for FILE_NAME.  */
  int i;
  for (i = set->info_ordinary.used - 1; i >= 0; --i)
    {
      const char *fname = set->info_ordinary.maps[i].to_file;
      if (fname && !filename_cmp (fname, file_name))
	break;
    }

  if (i < 0)
    return false;

  /* The highest location for a given map is either the starting
     location of the next map minus one, or -- if the map is the
     latest one -- the highest location of the set.  */
  source_location result;
  if (i == (int) set->info_ordinary.used - 1)
    result = set->highest_location;
  else
    result = set->info_ordinary.maps[i + 1].start_location - 1;

  *loc = result;
  return true;
}

/* Compute and return statistics about the memory consumption of some
   parts of the line table SET.  */

void
linemap_get_statistics (struct line_maps *set,
			struct linemap_stats *s)
{
  long ordinary_maps_allocated_size, ordinary_maps_used_size,
    macro_maps_allocated_size, macro_maps_used_size,
    macro_maps_locations_size = 0, duplicated_macro_maps_locations_size = 0;

  const line_map_macro *cur_map;

  ordinary_maps_allocated_size =
    LINEMAPS_ORDINARY_ALLOCATED (set) * sizeof (struct line_map_ordinary);

  ordinary_maps_used_size =
    LINEMAPS_ORDINARY_USED (set) * sizeof (struct line_map_ordinary);

  macro_maps_allocated_size =
    LINEMAPS_MACRO_ALLOCATED (set) * sizeof (struct line_map_macro);

  for (cur_map = LINEMAPS_MACRO_MAPS (set);
       cur_map && cur_map <= LINEMAPS_LAST_MACRO_MAP (set);
       ++cur_map)
    {
      unsigned i;

      linemap_assert (linemap_macro_expansion_map_p (cur_map));

      macro_maps_locations_size +=
	2 * MACRO_MAP_NUM_MACRO_TOKENS (cur_map) * sizeof (source_location);

      for (i = 0; i < 2 * MACRO_MAP_NUM_MACRO_TOKENS (cur_map); i += 2)
	{
	  if (MACRO_MAP_LOCATIONS (cur_map)[i] ==
	      MACRO_MAP_LOCATIONS (cur_map)[i + 1])
	    duplicated_macro_maps_locations_size +=
	      sizeof (source_location);
	}
    }

  macro_maps_used_size =
    LINEMAPS_MACRO_USED (set) * sizeof (struct line_map_macro);

  s->num_ordinary_maps_allocated = LINEMAPS_ORDINARY_ALLOCATED (set);
  s->num_ordinary_maps_used = LINEMAPS_ORDINARY_USED (set);
  s->ordinary_maps_allocated_size = ordinary_maps_allocated_size;
  s->ordinary_maps_used_size = ordinary_maps_used_size;
  s->num_expanded_macros = num_expanded_macros_counter;
  s->num_macro_tokens = num_macro_tokens_counter;
  s->num_macro_maps_used = LINEMAPS_MACRO_USED (set);
  s->macro_maps_allocated_size = macro_maps_allocated_size;
  s->macro_maps_locations_size = macro_maps_locations_size;
  s->macro_maps_used_size = macro_maps_used_size;
  s->duplicated_macro_maps_locations_size =
    duplicated_macro_maps_locations_size;
  s->adhoc_table_size = (set->location_adhoc_data_map.allocated
			 * sizeof (struct location_adhoc_data));
  s->adhoc_table_entries_used = set->location_adhoc_data_map.curr_loc;
}


/* Dump line table SET to STREAM.  If STREAM is NULL, stderr is used.
   NUM_ORDINARY specifies how many ordinary maps to dump.  NUM_MACRO
   specifies how many macro maps to dump.  */

void
line_table_dump (FILE *stream, struct line_maps *set, unsigned int num_ordinary,
		 unsigned int num_macro)
{
  unsigned int i;

  if (set == NULL)
    return;

  if (stream == NULL)
    stream = stderr;

  fprintf (stream, "# of ordinary maps:  %d\n", LINEMAPS_ORDINARY_USED (set));
  fprintf (stream, "# of macro maps:     %d\n", LINEMAPS_MACRO_USED (set));
  fprintf (stream, "Include stack depth: %d\n", set->depth);
  fprintf (stream, "Highest location:    %u\n", set->highest_location);

  if (num_ordinary)
    {
      fprintf (stream, "\nOrdinary line maps\n");
      for (i = 0; i < num_ordinary && i < LINEMAPS_ORDINARY_USED (set); i++)
	linemap_dump (stream, set, i, false);
      fprintf (stream, "\n");
    }

  if (num_macro)
    {
      fprintf (stream, "\nMacro line maps\n");
      for (i = 0; i < num_macro && i < LINEMAPS_MACRO_USED (set); i++)
	linemap_dump (stream, set, i, true);
      fprintf (stream, "\n");
    }
}

/* struct source_range.  */

/* Is there any part of this range on the given line?  */

bool
source_range::intersects_line_p (const char *file, int line) const
{
  expanded_location exploc_start
    = linemap_client_expand_location_to_spelling_point (m_start);
  if (file != exploc_start.file)
    return false;
  if (line < exploc_start.line)
      return false;
  expanded_location exploc_finish
    = linemap_client_expand_location_to_spelling_point (m_finish);
  if (file != exploc_finish.file)
    return false;
  if (line > exploc_finish.line)
      return false;
  return true;
}

/* class rich_location.  */

/* Construct a rich_location with location LOC as its initial range.  */

rich_location::rich_location (line_maps *set, source_location loc) :
  m_line_table (set),
  m_ranges (),
  m_column_override (0),
  m_have_expanded_location (false),
  m_fixit_hints (),
  m_seen_impossible_fixit (false)
{
  add_range (loc, true);
}

/* The destructor for class rich_location.  */

rich_location::~rich_location ()
{
  for (unsigned int i = 0; i < m_fixit_hints.count (); i++)
    delete get_fixit_hint (i);
}

/* Get location IDX within this rich_location.  */

source_location
rich_location::get_loc (unsigned int idx) const
{
  const location_range *locrange = get_range (idx);
  return locrange->m_loc;
}

/* Get range IDX within this rich_location.  */

const location_range *
rich_location::get_range (unsigned int idx) const
{
  return &m_ranges[idx];
}

/* Mutable access to range IDX within this rich_location.  */

location_range *
rich_location::get_range (unsigned int idx)
{
  return &m_ranges[idx];
}

/* Expand location IDX within this rich_location.  */
/* Get an expanded_location for this rich_location's primary
   location.  */

expanded_location
rich_location::get_expanded_location (unsigned int idx)
{
  if (idx == 0)
   {
     /* Cache the expansion of the primary location.  */
     if (!m_have_expanded_location)
       {
	  m_expanded_location
	    = linemap_client_expand_location_to_spelling_point (get_loc (0));
	  if (m_column_override)
	    m_expanded_location.column = m_column_override;
	  m_have_expanded_location = true;
       }

     return m_expanded_location;
   }
  else
    return linemap_client_expand_location_to_spelling_point (get_loc (idx));
}

/* Set the column of the primary location, with 0 meaning
   "don't override it".  */

void
rich_location::override_column (int column)
{
  m_column_override = column;
  m_have_expanded_location = false;
}

/* Add the given range.  */

void
rich_location::add_range (source_location loc, bool show_caret_p)
{
  location_range range;
  range.m_loc = loc;
  range.m_show_caret_p = show_caret_p;
  m_ranges.push (range);
}

/* Add or overwrite the location given by IDX, setting its location to LOC,
   and setting its "should my caret be printed" flag to SHOW_CARET_P.

   It must either overwrite an existing location, or add one *exactly* on
   the end of the array.

   This is primarily for use by gcc when implementing diagnostic format
   decoders e.g.
   - the "+" in the C/C++ frontends, for handling format codes like "%q+D"
     (which writes the source location of a tree back into location 0 of
     the rich_location), and
   - the "%C" and "%L" format codes in the Fortran frontend.  */

void
rich_location::set_range (line_maps * /*set*/, unsigned int idx,
			  source_location loc, bool show_caret_p)
{
  /* We can either overwrite an existing range, or add one exactly
     on the end of the array.  */
  linemap_assert (idx <= m_ranges.count ());

  if (idx == m_ranges.count ())
    add_range (loc,  show_caret_p);
  else
    {
      location_range *locrange = get_range (idx);
      locrange->m_loc = loc;
      locrange->m_show_caret_p = show_caret_p;
    }

  if (idx == 0)
    /* Mark any cached value here as dirty.  */
    m_have_expanded_location = false;
}

/* Methods for adding insertion fix-it hints.  */

/* Add a fixit-hint, suggesting insertion of NEW_CONTENT
   immediately before the primary range's start location.  */

void
rich_location::add_fixit_insert_before (const char *new_content)
{
  add_fixit_insert_before (get_loc (), new_content);
}

/* Add a fixit-hint, suggesting insertion of NEW_CONTENT
   immediately before the start of WHERE.  */

void
rich_location::add_fixit_insert_before (source_location where,
					const char *new_content)
{
  source_location start = get_range_from_loc (m_line_table, where).m_start;

  if (reject_impossible_fixit (start))
    return;
  /* We do not yet support newlines within fix-it hints.  */
  if (strchr (new_content, '\n'))
    {
      stop_supporting_fixits ();
      return;
    }
  add_fixit (new fixit_insert (start, new_content));
}

/* Add a fixit-hint, suggesting insertion of NEW_CONTENT
   immediately after the primary range's end-point.  */

void
rich_location::add_fixit_insert_after (const char *new_content)
{
  add_fixit_insert_after (get_loc (), new_content);
}

/* Add a fixit-hint, suggesting insertion of NEW_CONTENT
   immediately after the end-point of WHERE.  */

void
rich_location::add_fixit_insert_after (source_location where,
				       const char *new_content)
{
  source_location finish = get_range_from_loc (m_line_table, where).m_finish;

  if (reject_impossible_fixit (finish))
    return;

  source_location next_loc
    = linemap_position_for_loc_and_offset (m_line_table, finish, 1);

  /* linemap_position_for_loc_and_offset can fail, if so, it returns
     its input value.  */
  if (next_loc == finish)
    {
      stop_supporting_fixits ();
      return;
    }

  add_fixit (new fixit_insert (next_loc, new_content));
}

/* Methods for adding removal fix-it hints.  */

/* Add a fixit-hint, suggesting removal of the content covered
   by range 0.  */

void
rich_location::add_fixit_remove ()
{
  add_fixit_remove (get_loc ());
}

/* Add a fixit-hint, suggesting removal of the content between
   the start and finish of WHERE.  */

void
rich_location::add_fixit_remove (source_location where)
{
  source_range range = get_range_from_loc (m_line_table, where);
  add_fixit_remove (range);
}

/* Add a fixit-hint, suggesting removal of the content at
   SRC_RANGE.  */

void
rich_location::add_fixit_remove (source_range src_range)
{
  add_fixit_replace (src_range, "");
}

/* Return true iff A is in the column directly before B, on the
   same line of the same source file.  */

static bool
column_before_p (line_maps *set, source_location a, source_location b)
{
  if (IS_ADHOC_LOC (a))
    a = get_location_from_adhoc_loc (set, a);
  if (IS_ADHOC_LOC (b))
    b = get_location_from_adhoc_loc (set, b);

  /* They must both be in ordinary maps.  */
  const struct line_map *linemap_a = linemap_lookup (set, a);
  if (linemap_macro_expansion_map_p (linemap_a))
    return false;
  const struct line_map *linemap_b = linemap_lookup (set, b);
  if (linemap_macro_expansion_map_p (linemap_b))
    return false;

  /* To be on the same line, they must be in the same ordinary map.  */
  if (linemap_a != linemap_b)
    return false;

  linenum_type line_a
    = SOURCE_LINE (linemap_check_ordinary (linemap_a), a);
  linenum_type line_b
    = SOURCE_LINE (linemap_check_ordinary (linemap_b), b);
  if (line_a != line_b)
    return false;

  linenum_type column_a
    = SOURCE_COLUMN (linemap_check_ordinary (linemap_a), a);
  linenum_type column_b
    = SOURCE_COLUMN (linemap_check_ordinary (linemap_b), b);

  return column_b == column_a + 1;
}

/* Add a fixit-hint, suggesting replacement of the content covered
   by range 0 with NEW_CONTENT.  */

void
rich_location::add_fixit_replace (const char *new_content)
{
  add_fixit_replace (get_loc (), new_content);
}

/* Methods for adding "replace" fix-it hints.  */

/* Add a fixit-hint, suggesting replacement of the content between
   the start and finish of WHERE with NEW_CONTENT.  */

void
rich_location::add_fixit_replace (source_location where,
				  const char *new_content)
{
  source_range range = get_range_from_loc (m_line_table, where);
  add_fixit_replace (range, new_content);
}

/* Add a fixit-hint, suggesting replacement of the content at
   SRC_RANGE with NEW_CONTENT.  */

void
rich_location::add_fixit_replace (source_range src_range,
				  const char *new_content)
{
  src_range.m_start = get_pure_location (m_line_table, src_range.m_start);
  src_range.m_finish = get_pure_location (m_line_table, src_range.m_finish);

  if (reject_impossible_fixit (src_range.m_start))
    return;
  if (reject_impossible_fixit (src_range.m_finish))
    return;

  /* We do not yet support newlines within fix-it hints.  */
  if (strchr (new_content, '\n'))
    {
      stop_supporting_fixits ();
      return;
    }

  /* Consolidate neighboring fixits.  */
  fixit_hint *prev = get_last_fixit_hint ();
  if (prev)
    if (prev->maybe_append_replace (m_line_table, src_range, new_content))
      return;

  add_fixit (new fixit_replace (src_range, new_content));
}

/* Get the last fix-it hint within this rich_location, or NULL if none.  */

fixit_hint *
rich_location::get_last_fixit_hint () const
{
  if (m_fixit_hints.count () > 0)
    return get_fixit_hint (m_fixit_hints.count () - 1);
  else
    return NULL;
}

/* If WHERE is an "awkward" location, then mark this rich_location as not
   supporting fixits, purging any thay were already added, and return true.

   Otherwise (the common case), return false.  */

bool
rich_location::reject_impossible_fixit (source_location where)
{
  /* Fix-its within a rich_location should either all be suggested, or
     none of them should be suggested.
     Once we've rejected a fixit, we reject any more, even those
     with reasonable locations.  */
  if (m_seen_impossible_fixit)
    return true;

  if (where <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    /* WHERE is a reasonable location for a fix-it; don't reject it.  */
    return false;

  /* Otherwise we have an attempt to add a fix-it with an "awkward"
     location: either one that we can't obtain column information
     for (within an ordinary map), or one within a macro expansion.  */
  stop_supporting_fixits ();
  return true;
}

/* Mark this rich_location as not supporting fixits, purging any that were
   already added.  */

void
rich_location::stop_supporting_fixits ()
{
  m_seen_impossible_fixit = true;

  /* Purge the rich_location of any fix-its that were already added. */
  for (unsigned int i = 0; i < m_fixit_hints.count (); i++)
    delete get_fixit_hint (i);
  m_fixit_hints.truncate (0);
}

/* Add HINT to the fix-it hints in this rich_location.  */

void
rich_location::add_fixit (fixit_hint *hint)
{
  m_fixit_hints.push (hint);
}

/* class fixit_insert.  */

fixit_insert::fixit_insert (source_location where,
			    const char *new_content)
: m_where (where),
  m_bytes (xstrdup (new_content)),
  m_len (strlen (new_content))
{
}

fixit_insert::~fixit_insert ()
{
  free (m_bytes);
}

/* Implementation of fixit_hint::affects_line_p for fixit_insert.  */

bool
fixit_insert::affects_line_p (const char *file, int line) const
{
  expanded_location exploc
    = linemap_client_expand_location_to_spelling_point (m_where);
  if (file == exploc.file)
    if (line == exploc.line)
      return true;
  return false;
}

/* Implementation of maybe_append_replace for fixit_insert.  Reject
   the attempt to consolidate fix-its.  */

bool
fixit_insert::maybe_append_replace (line_maps *, source_range, const char *)
{
  return false;
}

/* class fixit_replace.  */

fixit_replace::fixit_replace (source_range src_range,
			      const char *new_content)
: m_src_range (src_range),
  m_bytes (xstrdup (new_content)),
  m_len (strlen (new_content))
{
}

fixit_replace::~fixit_replace ()
{
  free (m_bytes);
}

/* Implementation of fixit_hint::affects_line_p for fixit_replace.  */

bool
fixit_replace::affects_line_p (const char *file, int line) const
{
  return m_src_range.intersects_line_p (file, line);
}

/* Implementation of maybe_append_replace for fixit_replace.  If
   possible, merge the new replacement into this one and return true.
   Otherwise return false.  */

bool
fixit_replace::maybe_append_replace (line_maps *set,
				     source_range src_range,
				     const char *new_content)
{
  /* Does SRC_RANGE start immediately after this one finishes?  */
  if (!column_before_p (set, m_src_range.m_finish, src_range.m_start))
    return false;

  /* We have neighboring replacements; merge them.  */
  m_src_range.m_finish = src_range.m_finish;
  size_t extra_len = strlen (new_content);
  m_bytes = (char *)xrealloc (m_bytes, m_len + extra_len + 1);
  memcpy (m_bytes + m_len, new_content, extra_len);
  m_len += extra_len;
  m_bytes[m_len] = '\0';
  return true;
}
