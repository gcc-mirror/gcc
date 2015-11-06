/* Map (unsigned int) keys to (source file, line, column) triples.
   Copyright (C) 2001-2015 Free Software Foundation, Inc.

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

#ifndef LIBCPP_LINE_MAP_H
#define LIBCPP_LINE_MAP_H

#ifndef GTY
#define GTY(x) /* nothing */
#endif

/* Reason for creating a new line map with linemap_add.  LC_ENTER is
   when including a new file, e.g. a #include directive in C.
   LC_LEAVE is when reaching a file's end.  LC_RENAME is when a file
   name or line number changes for neither of the above reasons
   (e.g. a #line directive in C); LC_RENAME_VERBATIM is like LC_RENAME
   but a filename of "" is not specially interpreted as standard
   input. LC_ENTER_MACRO is when a macro expansion is about to start.  */
enum lc_reason
{
  LC_ENTER = 0,
  LC_LEAVE,
  LC_RENAME,
  LC_RENAME_VERBATIM,
  LC_ENTER_MACRO
  /* FIXME: add support for stringize and paste.  */
};

/* The type of line numbers.  */
typedef unsigned int linenum_type;

/* The typedef "source_location" is a key within the location database,
   identifying a source location or macro expansion.

   This key only has meaning in relation to a line_maps instance.  Within
   gcc there is a single line_maps instance: "line_table", declared in
   gcc/input.h and defined in gcc/input.c.

   The values of the keys are intended to be internal to libcpp,
   but for ease-of-understanding the implementation, they are currently
   assigned as follows:

  Actual     | Value                         | Meaning
  -----------+-------------------------------+-------------------------------
  0x00000000 | UNKNOWN_LOCATION (gcc/input.h)| Unknown/invalid location.
  -----------+-------------------------------+-------------------------------
  0x00000001 | BUILTINS_LOCATION             | The location for declarations
             |   (gcc/input.h)               | in "<built-in>"
  -----------+-------------------------------+-------------------------------
  0x00000002 | RESERVED_LOCATION_COUNT       | The first location to be
             | (also                         | handed out, and the
             |  ordmap[0]->start_location)   | first line in ordmap 0
  -----------+-------------------------------+-------------------------------
             | ordmap[1]->start_location     | First line in ordmap 1
             | ordmap[1]->start_location+1   | First column in that line
             | ordmap[1]->start_location+2   | 2nd column in that line
             |                               | Subsequent lines are offset by
             |                               | (1 << column_bits),
             |                               | e.g. 128 for 7 bits, with a
             |                               | column value of 0 representing
             |                               | "the whole line".
             | ordmap[2]->start_location-1   | Final location in ordmap 1
  -----------+-------------------------------+-------------------------------
             | ordmap[2]->start_location     | First line in ordmap 2
             | ordmap[3]->start_location-1   | Final location in ordmap 2
  -----------+-------------------------------+-------------------------------
             |                               | (etc)
  -----------+-------------------------------+-------------------------------
             | ordmap[n-1]->start_location   | First line in final ord map
             |                               | (etc)
             | set->highest_location - 1     | Final location in that ordmap
  -----------+-------------------------------+-------------------------------
             | set->highest_location         | Location of the where the next
             |                               | ordinary linemap would start
  -----------+-------------------------------+-------------------------------
             |                               |
             |                  VVVVVVVVVVVVVVVVVVVVVVVVVVV
             |                  Ordinary maps grow this way
             |
             |                    (unallocated integers)
             |
  0x60000000 | LINE_MAP_MAX_LOCATION_WITH_COLS
             |   Beyond this point, ordinary linemaps have 0 bits per column:
             |   each increment of the value corresponds to a new source line.
             |
  0x70000000 | LINE_MAP_MAX_SOURCE_LOCATION
             |   Beyond the point, we give up on ordinary maps; attempts to
             |   create locations in them lead to UNKNOWN_LOCATION (0).
             |
             |                    (unallocated integers)
             |
             |                   Macro maps grow this way
             |                   ^^^^^^^^^^^^^^^^^^^^^^^^
             |                               |
  -----------+-------------------------------+-------------------------------
             | LINEMAPS_MACRO_LOWEST_LOCATION| Locations within macro maps
             | macromap[m-1]->start_location | Start of last macro map
             |                               |
  -----------+-------------------------------+-------------------------------
             | macromap[m-2]->start_location | Start of penultimate macro map
  -----------+-------------------------------+-------------------------------
             | macromap[1]->start_location   | Start of macro map 1
  -----------+-------------------------------+-------------------------------
             | macromap[0]->start_location   | Start of macro map 0
  0x7fffffff | MAX_SOURCE_LOCATION           | Also used as a mask for
             |                               | accessing the ad-hoc data table
  -----------+-------------------------------+-------------------------------
  0x80000000 | Start of ad-hoc values; the lower 31 bits are used as an index
  ...        | into the line_table->location_adhoc_data_map.data array.
  0xffffffff | UINT_MAX                      |
  -----------+-------------------------------+-------------------------------

  To see how this works in practice, see the worked example in
  libcpp/location-example.txt.  */
typedef unsigned int source_location;

/* Memory allocation function typedef.  Works like xrealloc.  */
typedef void *(*line_map_realloc) (void *, size_t);

/* Memory allocator function that returns the actual allocated size,
   for a given requested allocation.  */
typedef size_t (*line_map_round_alloc_size_func) (size_t);

/* A line_map encodes a sequence of locations.
   There are two kinds of maps. Ordinary maps and macro expansion
   maps, a.k.a macro maps.

   A macro map encodes source locations of tokens that are part of a
   macro replacement-list, at a macro expansion point. E.g, in:

            #define PLUS(A,B) A + B

   No macro map is going to be created there, because we are not at a
   macro expansion point. We are at a macro /definition/ point. So the
   locations of the tokens of the macro replacement-list (i.e, A + B)
   will be locations in an ordinary map, not a macro map.

   On the other hand, if we later do:

        int a = PLUS (1,2);

   The invocation of PLUS here is a macro expansion. So we are at a
   macro expansion point. The preprocessor expands PLUS (1,2) and
   replaces it with the tokens of its replacement-list: 1 + 2. A macro
   map is going to be created to hold (or rather to map, haha ...) the
   locations of the tokens 1, + and 2. The macro map also records the
   location of the expansion point of PLUS. That location is mapped in
   the map that is active right before the location of the invocation
   of PLUS.  */
struct GTY((tag ("0"), desc ("%h.reason == LC_ENTER_MACRO ? 2 : 1"))) line_map {
  source_location start_location;

  /* The reason for creation of this line map.  */
  ENUM_BITFIELD (lc_reason) reason : CHAR_BIT;
};

/* An ordinary line map encodes physical source locations. Those
   physical source locations are called "spelling locations".
   
   Physical source file TO_FILE at line TO_LINE at column 0 is represented
   by the logical START_LOCATION.  TO_LINE+L at column C is represented by
   START_LOCATION+(L*(1<<column_bits))+C, as long as C<(1<<column_bits),
   and the result_location is less than the next line_map's start_location.
   (The top line is line 1 and the leftmost column is column 1; line/column 0
   means "entire file/line" or "unknown line/column" or "not applicable".)

   The highest possible source location is MAX_SOURCE_LOCATION.  */
struct GTY((tag ("1"))) line_map_ordinary : public line_map {
  const char *to_file;
  linenum_type to_line;

  /* An index into the set that gives the line mapping at whose end
     the current one was included.  File(s) at the bottom of the
     include stack have this set to -1.  */
  int included_from;

  /* SYSP is one for a system header, two for a C system header file
     that therefore needs to be extern "C" protected in C++, and zero
     otherwise.  This field isn't really needed now that it's in
     cpp_buffer.  */
  unsigned char sysp;

  /* Number of the low-order source_location bits used for a column number.  */
  unsigned int column_bits : 8;
};

/* This is the highest possible source location encoded within an
   ordinary or macro map.  */
const source_location MAX_SOURCE_LOCATION = 0x7FFFFFFF;

struct cpp_hashnode;

/* A macro line map encodes location of tokens coming from a macro
   expansion.
   
   The offset from START_LOCATION is used to index into
   MACRO_LOCATIONS; this holds the original location of the token.  */
struct GTY((tag ("2"))) line_map_macro : public line_map {
  /* The cpp macro which expansion gave birth to this macro map.  */
  struct cpp_hashnode * GTY ((nested_ptr (union tree_node,
				   "%h ? CPP_HASHNODE (GCC_IDENT_TO_HT_IDENT (%h)) : NULL",
				   "%h ? HT_IDENT_TO_GCC_IDENT (HT_NODE (%h)) : NULL")))
    macro;

  /* The number of tokens inside the replacement-list of MACRO.  */
  unsigned int n_tokens;

  /* This array of location is actually an array of pairs of
     locations. The elements inside it thus look like:

           x0,y0, x1,y1, x2,y2, ...., xn,yn.

     where n == n_tokens;

     Remember that these xI,yI are collected when libcpp is about to
     expand a given macro.

     yI is the location in the macro definition, either of the token
     itself or of a macro parameter that it replaces.

     Imagine this:

	#define PLUS(A, B) A + B  <--- #1

	int a = PLUS (1,2); <--- #2

     There is a macro map for the expansion of PLUS in #2.  PLUS is
     expanded into its expansion-list.  The expansion-list is the
     replacement-list of PLUS where the macro parameters are replaced
     with their arguments.  So the replacement-list of PLUS is made of
     the tokens:

        A, +, B

     and the expansion-list is made of the tokens:

        1, +, 2

     Let's consider the case of token "+".  Its y1 [yI for I == 1] is
     its spelling location in #1.

     y0 (thus for token "1") is the spelling location of A in #1.

     And y2 (of token "2") is the spelling location of B in #1.

     When the token is /not/ an argument for a macro, xI is the same
     location as yI.  Otherwise, xI is the location of the token
     outside this macro expansion.  If this macro was expanded from
     another macro expansion, xI is a virtual location representing
     the token in that macro expansion; otherwise, it is the spelling
     location of the token.

     Note that a virtual location is a location returned by
     linemap_add_macro_token.  It encodes the relevant locations (x,y
     pairs) of that token across the macro expansions from which it
     (the token) might come from.

     In the example above x1 (for token "+") is going to be the same
     as y1.  x0 is the spelling location for the argument token "1",
     and x2 is the spelling location for the argument token "2".  */
  source_location * GTY((atomic)) macro_locations;

  /* This is the location of the expansion point of the current macro
     map.  It's the location of the macro name.  That location is held
     by the map that was current right before the current one. It
     could have been either a macro or an ordinary map, depending on
     if we are in a nested expansion context not.  */
  source_location expansion;
};

#if CHECKING_P && (GCC_VERSION >= 2007)

/* Assertion macro to be used in line-map code.  */
#define linemap_assert(EXPR)                  \
  do {                                                \
    if (! (EXPR))                             \
      abort ();                                       \
  } while (0)

/* Assert that becomes a conditional expression when checking is disabled at
   compilation time.  Use this for conditions that should not happen but if
   they happen, it is better to handle them gracefully rather than crash
   randomly later.
   Usage:

   if (linemap_assert_fails(EXPR)) handle_error(); */
#define linemap_assert_fails(EXPR) __extension__ \
  ({linemap_assert (EXPR); false;})

#else
/* Include EXPR, so that unused variable warnings do not occur.  */
#define linemap_assert(EXPR) ((void)(0 && (EXPR)))
#define linemap_assert_fails(EXPR) (! (EXPR))
#endif

/* Return TRUE if MAP encodes locations coming from a macro
   replacement-list at macro expansion point.  */
bool
linemap_macro_expansion_map_p (const struct line_map *);

/* Assert that MAP encodes locations of tokens that are not part of
   the replacement-list of a macro expansion, downcasting from
   line_map * to line_map_ordinary *.  */

inline line_map_ordinary *
linemap_check_ordinary (struct line_map *map)
{
  linemap_assert (!linemap_macro_expansion_map_p (map));
  return (line_map_ordinary *)map;
}

/* Assert that MAP encodes locations of tokens that are not part of
   the replacement-list of a macro expansion, downcasting from
   const line_map * to const line_map_ordinary *.  */

inline const line_map_ordinary *
linemap_check_ordinary (const struct line_map *map)
{
  linemap_assert (!linemap_macro_expansion_map_p (map));
  return (const line_map_ordinary *)map;
}

/* Assert that MAP is a macro expansion and downcast to the appropriate
   subclass.  */

inline line_map_macro *linemap_check_macro (line_map *map)
{
  linemap_assert (linemap_macro_expansion_map_p (map));
  return (line_map_macro *)map;
}

/* Assert that MAP is a macro expansion and downcast to the appropriate
   subclass.  */

inline const line_map_macro *
linemap_check_macro (const line_map *map)
{
  linemap_assert (linemap_macro_expansion_map_p (map));
  return (const line_map_macro *)map;
}

/* Read the start location of MAP.  */

inline source_location
MAP_START_LOCATION (const line_map *map)
{
  return map->start_location;
}

/* Get the starting line number of ordinary map MAP.  */

inline linenum_type
ORDINARY_MAP_STARTING_LINE_NUMBER (const line_map_ordinary *ord_map)
{
  return ord_map->to_line;
}

/* Get the index of the ordinary map at whose end
   ordinary map MAP was included.

   File(s) at the bottom of the include stack have this set.  */

inline int
ORDINARY_MAP_INCLUDER_FILE_INDEX (const line_map_ordinary *ord_map)
{
  return ord_map->included_from;
}

/* Return a positive value if map encodes locations from a system
   header, 0 otherwise. Returns 1 if ordinary map MAP encodes locations
   in a system header and 2 if it encodes locations in a C system header
   that therefore needs to be extern "C" protected in C++.  */

inline unsigned char
ORDINARY_MAP_IN_SYSTEM_HEADER_P (const line_map_ordinary *ord_map)
{
  return ord_map->sysp;
}

/* Get the number of the low-order source_location bits used for a
   column number within ordinary map MAP.  */

inline unsigned char
ORDINARY_MAP_NUMBER_OF_COLUMN_BITS (const line_map_ordinary *ord_map)
{
  return ord_map->column_bits;
}

/* Get the filename of ordinary map MAP.  */

inline const char *
ORDINARY_MAP_FILE_NAME (const line_map_ordinary *ord_map)
{
  return ord_map->to_file;
}

/* Get the cpp macro whose expansion gave birth to macro map MAP.  */

inline cpp_hashnode *
MACRO_MAP_MACRO (const line_map_macro *macro_map)
{
  return macro_map->macro;
}

/* Get the number of tokens inside the replacement-list of the macro
   that led to macro map MAP.  */

inline unsigned int
MACRO_MAP_NUM_MACRO_TOKENS (const line_map_macro *macro_map)
{
  return macro_map->n_tokens;
}

/* Get the array of pairs of locations within macro map MAP.
   See the declaration of line_map_macro for more information.  */

inline source_location *
MACRO_MAP_LOCATIONS (const line_map_macro *macro_map)
{
  return macro_map->macro_locations;
}

/* Get the location of the expansion point of the macro map MAP.  */

inline source_location
MACRO_MAP_EXPANSION_POINT_LOCATION (const line_map_macro *macro_map)
{
  return macro_map->expansion;
}

/* The abstraction of a set of location maps. There can be several
   types of location maps. This abstraction contains the attributes
   that are independent from the type of the map.

   Essentially this is just a vector of T_linemap_subclass,
   which can only ever grow in size.  */

struct GTY(()) maps_info_ordinary {
  /* This array contains the "ordinary" line maps, for all
     events other than macro expansion
     (e.g. when a new preprocessing unit starts or ends).  */
  line_map_ordinary * GTY ((length ("%h.used"))) maps;

  /* The total number of allocated maps.  */
  unsigned int allocated;

  /* The number of elements used in maps. This number is smaller
     or equal to ALLOCATED.  */
  unsigned int used;

  unsigned int cache;
};

struct GTY(()) maps_info_macro {
  /* This array contains the macro line maps.
     A macro line map is created whenever a macro expansion occurs.  */
  line_map_macro * GTY ((length ("%h.used"))) maps;

  /* The total number of allocated maps.  */
  unsigned int allocated;

  /* The number of elements used in maps. This number is smaller
     or equal to ALLOCATED.  */
  unsigned int used;

  unsigned int cache;
};

/* Data structure to associate an arbitrary data to a source location.  */
struct GTY(()) location_adhoc_data {
  source_location locus;
  void * GTY((skip)) data;
};

struct htab;

/* The following data structure encodes a location with some adhoc data
   and maps it to a new unsigned integer (called an adhoc location)
   that replaces the original location to represent the mapping.

   The new adhoc_loc uses the highest bit as the enabling bit, i.e. if the
   highest bit is 1, then the number is adhoc_loc. Otherwise, it serves as
   the original location. Once identified as the adhoc_loc, the lower 31
   bits of the integer is used to index the location_adhoc_data array,
   in which the locus and associated data is stored.  */

struct GTY(()) location_adhoc_data_map {
  struct htab * GTY((skip)) htab;
  source_location curr_loc;
  unsigned int allocated;
  struct location_adhoc_data GTY((length ("%h.allocated"))) *data;
};

/* A set of chronological line_map structures.  */
struct GTY(()) line_maps {
  
  maps_info_ordinary info_ordinary;

  maps_info_macro info_macro;

  /* Depth of the include stack, including the current file.  */
  unsigned int depth;

  /* If true, prints an include trace a la -H.  */
  bool trace_includes;

  /* Highest source_location "given out".  */
  source_location highest_location;

  /* Start of line of highest source_location "given out".  */
  source_location highest_line;

  /* The maximum column number we can quickly allocate.  Higher numbers
     may require allocating a new line_map.  */
  unsigned int max_column_hint;

  /* If non-null, the allocator to use when resizing 'maps'.  If null,
     xrealloc is used.  */
  line_map_realloc reallocator;

  /* The allocators' function used to know the actual size it
     allocated, for a certain allocation size requested.  */
  line_map_round_alloc_size_func round_alloc_size;

  struct location_adhoc_data_map location_adhoc_data_map;

  /* The special location value that is used as spelling location for
     built-in tokens.  */
  source_location builtin_location;

  /* True if we've seen a #line or # 44 "file" directive.  */
  bool seen_line_directive;
};

/* Returns the number of allocated maps so far. MAP_KIND shall be TRUE
   if we are interested in macro maps, FALSE otherwise.  */
inline unsigned int
LINEMAPS_ALLOCATED (const line_maps *set, bool map_kind)
{
  if (map_kind)
    return set->info_macro.allocated;
  else
    return set->info_ordinary.allocated;
}

/* As above, but by reference (e.g. as an lvalue).  */

inline unsigned int &
LINEMAPS_ALLOCATED (line_maps *set, bool map_kind)
{
  if (map_kind)
    return set->info_macro.allocated;
  else
    return set->info_ordinary.allocated;
}

/* Returns the number of used maps so far. MAP_KIND shall be TRUE if
   we are interested in macro maps, FALSE otherwise.*/
inline unsigned int
LINEMAPS_USED (const line_maps *set, bool map_kind)
{
  if (map_kind)
    return set->info_macro.used;
  else
    return set->info_ordinary.used;
}

/* As above, but by reference (e.g. as an lvalue).  */

inline unsigned int &
LINEMAPS_USED (line_maps *set, bool map_kind)
{
  if (map_kind)
    return set->info_macro.used;
  else
    return set->info_ordinary.used;
}

/* Returns the index of the last map that was looked up with
   linemap_lookup. MAP_KIND shall be TRUE if we are interested in
   macro maps, FALSE otherwise.  */
inline unsigned int
LINEMAPS_CACHE (const line_maps *set, bool map_kind)
{
  if (map_kind)
    return set->info_macro.cache;
  else
    return set->info_ordinary.cache;
}

/* As above, but by reference (e.g. as an lvalue).  */

inline unsigned int &
LINEMAPS_CACHE (line_maps *set, bool map_kind)
{
  if (map_kind)
    return set->info_macro.cache;
  else
    return set->info_ordinary.cache;
}

/* Return the map at a given index.  */
inline line_map *
LINEMAPS_MAP_AT (const line_maps *set, bool map_kind, int index)
{
  if (map_kind)
    return &set->info_macro.maps[index];
  else
    return &set->info_ordinary.maps[index];
}

/* Returns the last map used in the line table SET. MAP_KIND
   shall be TRUE if we are interested in macro maps, FALSE
   otherwise.*/
inline line_map *
LINEMAPS_LAST_MAP (const line_maps *set, bool map_kind)
{
  return LINEMAPS_MAP_AT (set, map_kind,
			  LINEMAPS_USED (set, map_kind) - 1);
}

/* Returns the last map that was allocated in the line table SET.
   MAP_KIND shall be TRUE if we are interested in macro maps, FALSE
   otherwise.*/
inline line_map *
LINEMAPS_LAST_ALLOCATED_MAP (const line_maps *set, bool map_kind)
{
  return LINEMAPS_MAP_AT (set, map_kind,
			  LINEMAPS_ALLOCATED (set, map_kind) - 1);
}

/* Returns a pointer to the memory region where ordinary maps are
   allocated in the line table SET.  */
inline line_map_ordinary *
LINEMAPS_ORDINARY_MAPS (const line_maps *set)
{
  return set->info_ordinary.maps;
}

/* Returns the INDEXth ordinary map.  */
inline line_map_ordinary *
LINEMAPS_ORDINARY_MAP_AT (const line_maps *set, int index)
{
  linemap_assert (index >= 0);
  linemap_assert ((unsigned int)index < set->info_ordinary.used);
  return &set->info_ordinary.maps[index];
}

/* Return the number of ordinary maps allocated in the line table
   SET.  */
inline unsigned int
LINEMAPS_ORDINARY_ALLOCATED (const line_maps *set)
{
  return LINEMAPS_ALLOCATED (set, false);
}

/* Return the number of ordinary maps used in the line table SET.  */
inline unsigned int
LINEMAPS_ORDINARY_USED (const line_maps *set)
{
  return LINEMAPS_USED (set, false);
}

/* Return the index of the last ordinary map that was looked up with
   linemap_lookup.  */
inline unsigned int
LINEMAPS_ORDINARY_CACHE (const line_maps *set)
{
  return LINEMAPS_CACHE (set, false);
}

/* As above, but by reference (e.g. as an lvalue).  */

inline unsigned int &
LINEMAPS_ORDINARY_CACHE (line_maps *set)
{
  return LINEMAPS_CACHE (set, false);
}

/* Returns a pointer to the last ordinary map used in the line table
   SET.  */
inline line_map_ordinary *
LINEMAPS_LAST_ORDINARY_MAP (const line_maps *set)
{
  return (line_map_ordinary *)LINEMAPS_LAST_MAP (set, false);
}

/* Returns a pointer to the last ordinary map allocated the line table
   SET.  */
inline line_map_ordinary *
LINEMAPS_LAST_ALLOCATED_ORDINARY_MAP (const line_maps *set)
{
  return (line_map_ordinary *)LINEMAPS_LAST_ALLOCATED_MAP (set, false);
}

/* Returns a pointer to the beginning of the region where macro maps
   are allcoated.  */
inline line_map_macro *
LINEMAPS_MACRO_MAPS (const line_maps *set)
{
  return set->info_macro.maps;
}

/* Returns the INDEXth macro map.  */
inline line_map_macro *
LINEMAPS_MACRO_MAP_AT (const line_maps *set, int index)
{
  linemap_assert (index >= 0);
  linemap_assert ((unsigned int)index < set->info_macro.used);
  return &set->info_macro.maps[index];
}

/* Returns the number of macro maps that were allocated in the line
   table SET.  */
inline unsigned int
LINEMAPS_MACRO_ALLOCATED (const line_maps *set)
{
  return LINEMAPS_ALLOCATED (set, true);
}

/* Returns the number of macro maps used in the line table SET.  */
inline unsigned int
LINEMAPS_MACRO_USED (const line_maps *set)
{
  return LINEMAPS_USED (set, true);
}

/* Returns the index of the last macro map looked up with
   linemap_lookup.  */
inline unsigned int
LINEMAPS_MACRO_CACHE (const line_maps *set)
{
  return LINEMAPS_CACHE (set, true);
}

/* As above, but by reference (e.g. as an lvalue).  */

inline unsigned int &
LINEMAPS_MACRO_CACHE (line_maps *set)
{
  return LINEMAPS_CACHE (set, true);
}

/* Returns the last macro map used in the line table SET.  */
inline line_map_macro *
LINEMAPS_LAST_MACRO_MAP (const line_maps *set)
{
  return (line_map_macro *)LINEMAPS_LAST_MAP (set, true);
}

/* Returns the lowest location [of a token resulting from macro
   expansion] encoded in this line table.  */
inline source_location
LINEMAPS_MACRO_LOWEST_LOCATION (const line_maps *set)
{
  return LINEMAPS_MACRO_USED (set)
         ? MAP_START_LOCATION (LINEMAPS_LAST_MACRO_MAP (set))
         : MAX_SOURCE_LOCATION;
}

/* Returns the last macro map allocated in the line table SET.  */
inline line_map_macro *
LINEMAPS_LAST_ALLOCATED_MACRO_MAP (const line_maps *set)
{
  return (line_map_macro *)LINEMAPS_LAST_ALLOCATED_MAP (set, true);
}

extern void location_adhoc_data_fini (struct line_maps *);
extern source_location get_combined_adhoc_loc (struct line_maps *,
					       source_location, void *);
extern void *get_data_from_adhoc_loc (struct line_maps *, source_location);
extern source_location get_location_from_adhoc_loc (struct line_maps *,
						    source_location);

/* Get whether location LOC is an ad-hoc location.  */

inline bool
IS_ADHOC_LOC (source_location loc)
{
  return (loc & MAX_SOURCE_LOCATION) != loc;
}

/* Combine LOC and BLOCK, giving a combined adhoc location.  */

inline source_location
COMBINE_LOCATION_DATA (struct line_maps *set,
		       source_location loc,
		       void *block)
{
  return get_combined_adhoc_loc (set, loc, block);
}

extern void rebuild_location_adhoc_htab (struct line_maps *);

/* Initialize a line map set.  SET is the line map set to initialize
   and BUILTIN_LOCATION is the special location value to be used as
   spelling location for built-in tokens.  This BUILTIN_LOCATION has
   to be strictly less than RESERVED_LOCATION_COUNT.  */
extern void linemap_init (struct line_maps *set,
			  source_location builtin_location);

/* Check for and warn about line_maps entered but not exited.  */

extern void linemap_check_files_exited (struct line_maps *);

/* Return a source_location for the start (i.e. column==0) of
   (physical) line TO_LINE in the current source file (as in the
   most recent linemap_add).   MAX_COLUMN_HINT is the highest column
   number we expect to use in this line (but it does not change
   the highest_location).  */

extern source_location linemap_line_start
(struct line_maps *set, linenum_type to_line,  unsigned int max_column_hint);

/* Add a mapping of logical source line to physical source file and
   line number. This function creates an "ordinary map", which is a
   map that records locations of tokens that are not part of macro
   replacement-lists present at a macro expansion point.

   The text pointed to by TO_FILE must have a lifetime
   at least as long as the lifetime of SET.  An empty
   TO_FILE means standard input.  If reason is LC_LEAVE, and
   TO_FILE is NULL, then TO_FILE, TO_LINE and SYSP are given their
   natural values considering the file we are returning to.

   A call to this function can relocate the previous set of
   maps, so any stored line_map pointers should not be used.  */
extern const struct line_map *linemap_add
  (struct line_maps *, enum lc_reason, unsigned int sysp,
   const char *to_file, linenum_type to_line);

/* Given a logical source location, returns the map which the
   corresponding (source file, line, column) triplet can be deduced
   from. Since the set is built chronologically, the logical lines are
   monotonic increasing, and so the list is sorted and we can use a
   binary search. If no line map have been allocated yet, this
   function returns NULL.  */
extern const struct line_map *linemap_lookup
  (struct line_maps *, source_location);

/* Returns TRUE if the line table set tracks token locations across
   macro expansion, FALSE otherwise.  */
bool linemap_tracks_macro_expansion_locs_p (struct line_maps *);

/* Return the name of the macro associated to MACRO_MAP.  */
const char* linemap_map_get_macro_name (const line_map_macro *);

/* Return a positive value if LOCATION is the locus of a token that is
   located in a system header, O otherwise. It returns 1 if LOCATION
   is the locus of a token that is located in a system header, and 2
   if LOCATION is the locus of a token located in a C system header
   that therefore needs to be extern "C" protected in C++.

   Note that this function returns 1 if LOCATION belongs to a token
   that is part of a macro replacement-list defined in a system
   header, but expanded in a non-system file.  */
int linemap_location_in_system_header_p (struct line_maps *,
					 source_location);

/* Return TRUE if LOCATION is a source code location of a token coming
   from a macro replacement-list at a macro expansion point, FALSE
   otherwise.  */
bool linemap_location_from_macro_expansion_p (const struct line_maps *,
					      source_location);

/* source_location values from 0 to RESERVED_LOCATION_COUNT-1 will
   be reserved for libcpp user as special values, no token from libcpp
   will contain any of those locations.  */
const source_location RESERVED_LOCATION_COUNT = 2;

/* Converts a map and a source_location to source line.  */
inline linenum_type
SOURCE_LINE (const line_map_ordinary *ord_map, source_location loc)
{
  return ((loc - ord_map->start_location)
	  >> ord_map->column_bits) + ord_map->to_line;
}

/* Convert a map and source_location to source column number.  */
inline linenum_type
SOURCE_COLUMN (const line_map_ordinary *ord_map, source_location loc)
{
  return ((loc - ord_map->start_location)
	  & ((1 << ord_map->column_bits) - 1));
}

/* Return the location of the last source line within an ordinary
   map.  */
inline source_location
LAST_SOURCE_LINE_LOCATION (const line_map_ordinary *map)
{
  return (((map[1].start_location - 1
	    - map->start_location)
	   & ~((1 << map->column_bits) - 1))
	  + map->start_location);
}

/* Returns the last source line number within an ordinary map.  This
   is the (last) line of the #include, or other directive, that caused
   a map change.  */
inline linenum_type
LAST_SOURCE_LINE (const line_map_ordinary *map)
{
  return SOURCE_LINE (map, LAST_SOURCE_LINE_LOCATION (map));
}

/* Return the last column number within an ordinary map.  */

inline linenum_type
LAST_SOURCE_COLUMN (const line_map_ordinary *map)
{
  return SOURCE_COLUMN (map, LAST_SOURCE_LINE_LOCATION (map));
}

/* Returns the map a given map was included from, or NULL if the map
   belongs to the main file, i.e, a file that wasn't included by
   another one.  */
inline line_map_ordinary *
INCLUDED_FROM (struct line_maps *set, const line_map_ordinary *ord_map)
{
  return ((ord_map->included_from == -1)
	  ? NULL
	  : LINEMAPS_ORDINARY_MAP_AT (set, ord_map->included_from));
}

/* True if the map is at the bottom of the include stack.  */

inline bool
MAIN_FILE_P (const line_map_ordinary *ord_map)
{
  return ord_map->included_from < 0;
}

/* Encode and return a source_location from a column number. The
   source line considered is the last source line used to call
   linemap_line_start, i.e, the last source line which a location was
   encoded from.  */
extern source_location
linemap_position_for_column (struct line_maps *, unsigned int);

/* Encode and return a source location from a given line and
   column.  */
source_location
linemap_position_for_line_and_column (const line_map_ordinary *,
				      linenum_type, unsigned int);

/* Encode and return a source_location starting from location LOC and
   shifting it by OFFSET columns.  This function does not support
   virtual locations.  */
source_location
linemap_position_for_loc_and_offset (struct line_maps *set,
				     source_location loc,
				     unsigned int offset);

/* Return the file this map is for.  */
inline const char *
LINEMAP_FILE (const line_map_ordinary *ord_map)
{
  return ord_map->to_file;
}

/* Return the line number this map started encoding location from.  */
inline linenum_type
LINEMAP_LINE (const line_map_ordinary *ord_map)
{
  return ord_map->to_line;
}

/* Return a positive value if map encodes locations from a system
   header, 0 otherwise. Returns 1 if MAP encodes locations in a
   system header and 2 if it encodes locations in a C system header
   that therefore needs to be extern "C" protected in C++.  */
inline unsigned char
LINEMAP_SYSP (const line_map_ordinary *ord_map)
{
  return ord_map->sysp;
}

/* Return a positive value if PRE denotes the location of a token that
   comes before the token of POST, 0 if PRE denotes the location of
   the same token as the token for POST, and a negative value
   otherwise.  */
int linemap_compare_locations (struct line_maps *set,
			       source_location   pre,
			       source_location   post);

/* Return TRUE if LOC_A denotes the location a token that comes
   topogically before the token denoted by location LOC_B, or if they
   are equal.  */
inline bool
linemap_location_before_p (struct line_maps *set,
			   source_location loc_a,
			   source_location loc_b)
{
  return linemap_compare_locations (set, loc_a, loc_b) >= 0;
}

typedef struct
{
  /* The name of the source file involved.  */
  const char *file;

  /* The line-location in the source file.  */
  int line;

  int column;

  void *data;

  /* In a system header?. */
  bool sysp;
} expanded_location;

/* This is enum is used by the function linemap_resolve_location
   below.  The meaning of the values is explained in the comment of
   that function.  */
enum location_resolution_kind
{
  LRK_MACRO_EXPANSION_POINT,
  LRK_SPELLING_LOCATION,
  LRK_MACRO_DEFINITION_LOCATION
};

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

   If LOC_MAP is not NULL, *LOC_MAP is set to the map encoding the
   returned location.  Note that if the returned location wasn't originally
   encoded by a map, the *MAP is set to NULL.  This can happen if LOC
   resolves to a location reserved for the client code, like
   UNKNOWN_LOCATION or BUILTINS_LOCATION in GCC.  */

source_location linemap_resolve_location (struct line_maps *,
					  source_location loc,
					  enum location_resolution_kind lrk,
					  const line_map_ordinary **loc_map);

/* Suppose that LOC is the virtual location of a token coming from the
   expansion of a macro M.  This function then steps up to get the
   location L of the point where M got expanded.  If L is a spelling
   location inside a macro expansion M', then this function returns
   the point where M' was expanded.  LOC_MAP is an output parameter.
   When non-NULL, *LOC_MAP is set to the map of the returned
   location.  */
source_location linemap_unwind_toward_expansion (struct line_maps *,
						 source_location loc,
						 const struct line_map **loc_map);

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
source_location linemap_unwind_to_first_non_reserved_loc (struct line_maps *,
							  source_location loc,
							  const struct line_map **map);

/* Expand source code location LOC and return a user readable source
   code location.  LOC must be a spelling (non-virtual) location.  If
   it's a location < RESERVED_LOCATION_COUNT a zeroed expanded source
   location is returned.  */
expanded_location linemap_expand_location (struct line_maps *,
					   const struct line_map *,
					   source_location loc);

/* Statistics about maps allocation and usage as returned by
   linemap_get_statistics.  */
struct linemap_stats
{
  long num_ordinary_maps_allocated;
  long num_ordinary_maps_used;
  long ordinary_maps_allocated_size;
  long ordinary_maps_used_size;
  long num_expanded_macros;
  long num_macro_tokens;
  long num_macro_maps_used;
  long macro_maps_allocated_size;
  long macro_maps_used_size;
  long macro_maps_locations_size;
  long duplicated_macro_maps_locations_size;
  long adhoc_table_size;
  long adhoc_table_entries_used;
};

/* Return the highest location emitted for a given file for which
   there is a line map in SET.  FILE_NAME is the file name to
   consider.  If the function returns TRUE, *LOC is set to the highest
   location emitted for that file.  */
bool linemap_get_file_highest_location (struct line_maps * set,
					const char *file_name,
					source_location *loc);

/* Compute and return statistics about the memory consumption of some
   parts of the line table SET.  */
void linemap_get_statistics (struct line_maps *, struct linemap_stats *);

/* Dump debugging information about source location LOC into the file
   stream STREAM. SET is the line map set LOC comes from.  */
void linemap_dump_location (struct line_maps *, source_location, FILE *);

/* Dump line map at index IX in line table SET to STREAM.  If STREAM
   is NULL, use stderr.  IS_MACRO is true if the caller wants to
   dump a macro map, false otherwise.  */
void linemap_dump (FILE *, struct line_maps *, unsigned, bool);

/* Dump line table SET to STREAM.  If STREAM is NULL, stderr is used.
   NUM_ORDINARY specifies how many ordinary maps to dump.  NUM_MACRO
   specifies how many macro maps to dump.  */
void line_table_dump (FILE *, struct line_maps *, unsigned int, unsigned int);

#endif /* !LIBCPP_LINE_MAP_H  */
