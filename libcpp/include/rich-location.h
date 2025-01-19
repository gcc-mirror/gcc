/* Bundles of location information used when printing diagnostics.
   Copyright (C) 2015-2025 Free Software Foundation, Inc.

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

#ifndef LIBCPP_RICH_LOCATION_H
#define LIBCPP_RICH_LOCATION_H

#include "label-text.h"

class range_label;
class label_effects;

/* A hint to diagnostic_show_locus on how to print a source range within a
   rich_location.

   Typically this is SHOW_RANGE_WITH_CARET for the 0th range, and
   SHOW_RANGE_WITHOUT_CARET for subsequent ranges,
   but the Fortran frontend uses SHOW_RANGE_WITH_CARET repeatedly for
   printing things like:

       x = x + y
           1   2
       Error: Shapes for operands at (1) and (2) are not conformable

   where "1" and "2" are notionally carets.  */

enum range_display_kind
{
  /* Show the pertinent source line(s), the caret, and underline(s).  */
  SHOW_RANGE_WITH_CARET,

  /* Show the pertinent source line(s) and underline(s), but don't
     show the caret (just an underline).  */
  SHOW_RANGE_WITHOUT_CARET,

  /* Just show the source lines; don't show the range itself.
     This is for use when displaying some line-insertion fix-it hints (for
     showing the user context on the change, for when it doesn't make sense
     to highlight the first column on the next line).  */
  SHOW_LINES_WITHOUT_RANGE
};

/* A location within a rich_location: a caret&range, with
   the caret potentially flagged for display, and an optional
   label.  */

struct location_range
{
  location_t m_loc;

  enum range_display_kind m_range_display_kind;

  /* If non-NULL, the label for this range.  */
  const range_label *m_label;

  /* If non-null, the name of the color to use for this range.  */
  const char *m_highlight_color;
};

/* A partially-embedded vec for use within rich_location for storing
   ranges and fix-it hints.

   Elements [0..NUM_EMBEDDED) are allocated within m_embed, after
   that they are within the dynamically-allocated m_extra.

   This allows for static allocation in the common case, whilst
   supporting the rarer case of an arbitrary number of elements.

   Dynamic allocation is not performed unless it's needed.  */

template <typename T, int NUM_EMBEDDED>
class semi_embedded_vec
{
 public:
  semi_embedded_vec ();
  ~semi_embedded_vec ();
  semi_embedded_vec (const semi_embedded_vec &other);

  unsigned int count () const { return m_num; }
  T& operator[] (int idx);
  const T& operator[] (int idx) const;

  void push (const T&);
  void truncate (int len);

 private:
  int m_num;
  T m_embedded[NUM_EMBEDDED];
  int m_alloc;
  T *m_extra;
};

/* Constructor for semi_embedded_vec.  In particular, no dynamic allocation
   is done.  */

template <typename T, int NUM_EMBEDDED>
semi_embedded_vec<T, NUM_EMBEDDED>::semi_embedded_vec ()
: m_num (0), m_alloc (0), m_extra (NULL)
{
}

/* Copy constructor for semi_embedded_vec.  */

template <typename T, int NUM_EMBEDDED>
semi_embedded_vec<T, NUM_EMBEDDED>::semi_embedded_vec (const semi_embedded_vec &other)
: m_num (0),
  m_alloc (other.m_alloc),
  m_extra (nullptr)
{
  if (other.m_extra)
    m_extra = XNEWVEC (T, m_alloc);

  for (int i = 0; i < other.m_num; i++)
    push (other[i]);
}

/* semi_embedded_vec's dtor.  Release any dynamically-allocated memory.  */

template <typename T, int NUM_EMBEDDED>
semi_embedded_vec<T, NUM_EMBEDDED>::~semi_embedded_vec ()
{
  XDELETEVEC (m_extra);
}

/* Look up element IDX, mutably.  */

template <typename T, int NUM_EMBEDDED>
T&
semi_embedded_vec<T, NUM_EMBEDDED>::operator[] (int idx)
{
  linemap_assert (idx < m_num);
  if (idx < NUM_EMBEDDED)
    return m_embedded[idx];
  else
    {
      linemap_assert (m_extra != NULL);
      return m_extra[idx - NUM_EMBEDDED];
    }
}

/* Look up element IDX (const).  */

template <typename T, int NUM_EMBEDDED>
const T&
semi_embedded_vec<T, NUM_EMBEDDED>::operator[] (int idx) const
{
  linemap_assert (idx < m_num);
  if (idx < NUM_EMBEDDED)
    return m_embedded[idx];
  else
    {
      linemap_assert (m_extra != NULL);
      return m_extra[idx - NUM_EMBEDDED];
    }
}

/* Append VALUE to the end of the semi_embedded_vec.  */

template <typename T, int NUM_EMBEDDED>
void
semi_embedded_vec<T, NUM_EMBEDDED>::push (const T& value)
{
  int idx = m_num++;
  if (idx < NUM_EMBEDDED)
    m_embedded[idx] = value;
  else
    {
      /* Offset "idx" to be an index within m_extra.  */
      idx -= NUM_EMBEDDED;
      if (NULL == m_extra)
	{
	  linemap_assert (m_alloc == 0);
	  m_alloc = 16;
	  m_extra = XNEWVEC (T, m_alloc);
	}
      else if (idx >= m_alloc)
	{
	  linemap_assert (m_alloc > 0);
	  m_alloc *= 2;
	  m_extra = XRESIZEVEC (T, m_extra, m_alloc);
	}
      linemap_assert (m_extra);
      linemap_assert (idx < m_alloc);
      m_extra[idx] = value;
    }
}

/* Truncate to length LEN.  No deallocation is performed.  */

template <typename T, int NUM_EMBEDDED>
void
semi_embedded_vec<T, NUM_EMBEDDED>::truncate (int len)
{
  linemap_assert (len <= m_num);
  m_num = len;
}

class fixit_hint;
class diagnostic_path;

/* A "rich" source code location, for use when printing diagnostics.
   A rich_location has one or more carets&ranges, where the carets
   are optional.  These are referred to as "ranges" from here.
   Typically the zeroth range has a caret; other ranges sometimes
   have carets.

   The "primary" location of a rich_location is the caret of range 0,
   used for determining the line/column when printing diagnostic
   text, such as:

      some-file.c:3:1: error: ...etc...

   Additional ranges may be added to help the user identify other
   pertinent clauses in a diagnostic.

   Ranges can (optionally) be given labels via class range_label.

   rich_location instances are intended to be allocated on the stack
   when generating diagnostics, and to be short-lived.

   Examples of rich locations
   --------------------------

   Example A
   *********
      int i = "foo";
              ^
   This "rich" location is simply a single range (range 0), with
   caret = start = finish at the given point.

   Example B
   *********
      a = (foo && bar)
          ~~~~~^~~~~~~
   This rich location has a single range (range 0), with the caret
   at the first "&", and the start/finish at the parentheses.
   Compare with example C below.

   Example C
   *********
      a = (foo && bar)
           ~~~ ^~ ~~~
   This rich location has three ranges:
   - Range 0 has its caret and start location at the first "&" and
     end at the second "&.
   - Range 1 has its start and finish at the "f" and "o" of "foo";
     the caret is not flagged for display, but is perhaps at the "f"
     of "foo".
   - Similarly, range 2 has its start and finish at the "b" and "r" of
     "bar"; the caret is not flagged for display, but is perhaps at the
     "b" of "bar".
   Compare with example B above.

   Example D (Fortran frontend)
   ****************************
       x = x + y
           1   2
   This rich location has range 0 at "1", and range 1 at "2".
   Both are flagged for caret display.  Both ranges have start/finish
   equal to their caret point.  The frontend overrides the diagnostic
   context's default caret character for these ranges.

   Example E (range labels)
   ************************
      printf ("arg0: %i  arg1: %s arg2: %i",
                               ^~
                               |
                               const char *
              100, 101, 102);
                   ~~~
                   |
                   int
   This rich location has two ranges:
   - range 0 is at the "%s" with start = caret = "%" and finish at
     the "s".  It has a range_label ("const char *").
   - range 1 has start/finish covering the "101" and is not flagged for
     caret printing.  The caret is at the start of "101", where its
     range_label is printed ("int").

   Fix-it hints
   ------------

   Rich locations can also contain "fix-it hints", giving suggestions
   for the user on how to edit their code to fix a problem.  These
   can be expressed as insertions, replacements, and removals of text.
   The edits by default are relative to the zeroth range within the
   rich_location, but optionally they can be expressed relative to
   other locations (using various overloaded methods of the form
   rich_location::add_fixit_*).

   For example:

   Example F: fix-it hint: insert_before
   *************************************
      ptr = arr[0];
	    ^~~~~~
	    &
   This rich location has a single range (range 0) covering "arr[0]",
   with the caret at the start.  The rich location has a single
   insertion fix-it hint, inserted before range 0, added via
     richloc.add_fixit_insert_before ("&");

   Example G: multiple fix-it hints: insert_before and insert_after
   ****************************************************************
      #define FN(ARG0, ARG1, ARG2) fn(ARG0, ARG1, ARG2)
				      ^~~~  ^~~~  ^~~~
				      (   ) (   ) (   )
   This rich location has three ranges, covering "arg0", "arg1",
   and "arg2", all with caret-printing enabled.
   The rich location has 6 insertion fix-it hints: each arg
   has a pair of insertion fix-it hints, suggesting wrapping
   them with parentheses: one a '(' inserted before,
   the other a ')' inserted after, added via
     richloc.add_fixit_insert_before (LOC, "(");
   and
     richloc.add_fixit_insert_after (LOC, ")");

   Example H: fix-it hint: removal
   *******************************
     struct s {int i};;
		      ^
		      -
   This rich location has a single range at the stray trailing
   semicolon, along with a single removal fix-it hint, covering
   the same range, added via:
     richloc.add_fixit_remove ();

   Example I: fix-it hint: replace
   *******************************
      c = s.colour;
	    ^~~~~~
	    color
   This rich location has a single range (range 0) covering "colour",
   and a single "replace" fix-it hint, covering the same range,
   added via
     richloc.add_fixit_replace ("color");

   Example J: fix-it hint: line insertion
   **************************************

     3 | #include <stddef.h>
     + |+#include <stdio.h>
     4 | int the_next_line;

   This rich location has a single range at line 4 column 1, marked
   with SHOW_LINES_WITHOUT_RANGE (to avoid printing a meaningless caret
   on the "i" of int).  It has a insertion fix-it hint of the string
   "#include <stdio.h>\n".

   Adding a fix-it hint can fail: for example, attempts to insert content
   at the transition between two line maps may fail due to there being no
   location_t value to express the new location.

   Attempts to add a fix-it hint within a macro expansion will fail.

   There is only limited support for newline characters in fix-it hints:
   only hints with newlines which insert an entire new line are permitted,
   inserting at the start of a line, and finishing with a newline
   (with no interior newline characters).  Other attempts to add
   fix-it hints containing newline characters will fail.
   Similarly, attempts to delete or replace a range *affecting* multiple
   lines will fail.

   The rich_location API handles these failures gracefully, so that
   diagnostics can attempt to add fix-it hints without each needing
   extensive checking.

   Fix-it hints within a rich_location are "atomic": if any hints can't
   be applied, none of them will be (tracked by the m_seen_impossible_fixit
   flag), and no fix-its hints will be displayed for that rich_location.
   This implies that diagnostic messages need to be worded in such a way
   that they make sense whether or not the fix-it hints are displayed,
   or that richloc.seen_impossible_fixit_p () should be checked before
   issuing the diagnostics.  */

class rich_location
{
 public:
  /* Constructors.  */

  /* Constructing from a location.  */
  rich_location (line_maps *set, location_t loc,
		 const range_label *label = nullptr,
		 const char *label_highlight_color = nullptr);

  /* Destructor.  */
  ~rich_location ();

  rich_location (const rich_location &);
  rich_location (rich_location &&) = delete;
  rich_location &operator= (const rich_location &) = delete;
  rich_location &operator= (rich_location &&) = delete;

  /* Accessors.  */
  location_t get_loc () const { return get_loc (0); }
  location_t get_loc (unsigned int idx) const;

  void set_highlight_color (const char *highlight_color);

  void
  add_range (location_t loc,
	     enum range_display_kind range_display_kind
	       = SHOW_RANGE_WITHOUT_CARET,
	     const range_label *label = nullptr,
	     const char *highlight_color = nullptr);

  void
  set_range (unsigned int idx, location_t loc,
	     enum range_display_kind range_display_kind,
	     const char *highlight_color = nullptr);

  unsigned int get_num_locations () const { return m_ranges.count (); }

  const location_range *get_range (unsigned int idx) const;
  location_range *get_range (unsigned int idx);

  expanded_location get_expanded_location (unsigned int idx) const;

  void
  override_column (int column);

  /* Fix-it hints.  */

  /* Methods for adding insertion fix-it hints.  */

  /* Suggest inserting NEW_CONTENT immediately before the primary
     range's start.  */
  void
  add_fixit_insert_before (const char *new_content);

  /* Suggest inserting NEW_CONTENT immediately before the start of WHERE.  */
  void
  add_fixit_insert_before (location_t where,
			   const char *new_content);

  /* Suggest inserting NEW_CONTENT immediately after the end of the primary
     range.  */
  void
  add_fixit_insert_after (const char *new_content);

  /* Suggest inserting NEW_CONTENT immediately after the end of WHERE.  */
  void
  add_fixit_insert_after (location_t where,
			  const char *new_content);

  /* Methods for adding removal fix-it hints.  */

  /* Suggest removing the content covered by range 0.  */
  void
  add_fixit_remove ();

  /* Suggest removing the content covered between the start and finish
     of WHERE.  */
  void
  add_fixit_remove (location_t where);

  /* Suggest removing the content covered by SRC_RANGE.  */
  void
  add_fixit_remove (source_range src_range);

  /* Methods for adding "replace" fix-it hints.  */

  /* Suggest replacing the content covered by range 0 with NEW_CONTENT.  */
  void
  add_fixit_replace (const char *new_content);

  /* Suggest replacing the content between the start and finish of
     WHERE with NEW_CONTENT.  */
  void
  add_fixit_replace (location_t where,
		     const char *new_content);

  /* Suggest replacing the content covered by SRC_RANGE with
     NEW_CONTENT.  */
  void
  add_fixit_replace (source_range src_range,
		     const char *new_content);

  unsigned int get_num_fixit_hints () const { return m_fixit_hints.count (); }
  fixit_hint *get_fixit_hint (int idx) const { return m_fixit_hints[idx]; }
  fixit_hint *get_last_fixit_hint () const;
  bool seen_impossible_fixit_p () const { return m_seen_impossible_fixit; }

  /* Set this if the fix-it hints are not suitable to be
     automatically applied.

     For example, if you are suggesting more than one
     mutually exclusive solution to a problem, then
     it doesn't make sense to apply all of the solutions;
     manual intervention is required.

     If set, then the fix-it hints in the rich_location will
     be printed, but will not be added to generated patches,
     or affect the modified version of the file.  */
  void fixits_cannot_be_auto_applied ()
  {
    m_fixits_cannot_be_auto_applied = true;
  }

  bool fixits_can_be_auto_applied_p () const
  {
    return !m_fixits_cannot_be_auto_applied;
  }

  /* An optional path through the code.  */
  const diagnostic_path *get_path () const { return m_path; }
  void set_path (const diagnostic_path *path) { m_path = path; }

  /* A flag for hinting that the diagnostic involves character encoding
     issues, and thus that it will be helpful to the user if we show some
     representation of how the characters in the pertinent source lines
     are encoded.
     The default is false (i.e. do not escape).
     When set to true, non-ASCII bytes in the pertinent source lines will
     be escaped in a manner controlled by the user-supplied option
     -fdiagnostics-escape-format=, so that the user can better understand
     what's going on with the encoding in their source file.  */
  bool escape_on_output_p () const { return m_escape_on_output; }
  void set_escape_on_output (bool flag) { m_escape_on_output = flag; }

  const line_maps *get_line_table () const { return m_line_table; }

  int get_column_override () const { return m_column_override; }

private:
  bool reject_impossible_fixit (location_t where);
  void stop_supporting_fixits ();
  void maybe_add_fixit (location_t start,
			location_t next_loc,
			const char *new_content);

public:
  static const int STATICALLY_ALLOCATED_RANGES = 3;

protected:
  line_maps * const m_line_table;
  semi_embedded_vec <location_range, STATICALLY_ALLOCATED_RANGES> m_ranges;

  int m_column_override;

  mutable bool m_have_expanded_location;
  bool m_seen_impossible_fixit;
  bool m_fixits_cannot_be_auto_applied;
  bool m_escape_on_output;

  mutable expanded_location m_expanded_location;

  /* The class manages the memory pointed to by the elements of
     the m_fixit_hints vector.  */
  static const int MAX_STATIC_FIXIT_HINTS = 2;
  semi_embedded_vec <fixit_hint *, MAX_STATIC_FIXIT_HINTS> m_fixit_hints;

  const diagnostic_path *m_path;
};

/* Abstract base class for labelling a range within a rich_location
   (e.g. for labelling expressions with their type).

   Generating the text could require non-trivial work, so this work
   is delayed (via the "get_text" virtual function) until the diagnostic
   printing code "knows" it needs it, thus avoiding doing it e.g. for
   warnings that are filtered by command-line flags.  This virtual
   function also isolates libcpp and the diagnostics subsystem from
   the front-end and middle-end-specific code for generating the text
   for the labels.

   Like the rich_location instances they annotate, range_label instances
   are intended to be allocated on the stack when generating diagnostics,
   and to be short-lived.  */

class range_label
{
 public:
  virtual ~range_label () {}

  /* Get localized text for the label.
     The RANGE_IDX is provided, allowing for range_label instances to be
     shared by multiple ranges if need be (the "flyweight" design pattern).  */
  virtual label_text get_text (unsigned range_idx) const = 0;

  /* Get any special effects for the label (e.g. links to other labels).  */
  virtual const label_effects *get_effects (unsigned /*range_idx*/) const
  {
    return nullptr;
  }
};

/* A fix-it hint: a suggested insertion, replacement, or deletion of text.
   We handle these three types of edit with one class, by representing
   them as replacement of a half-open range:
       [start, next_loc)
   Insertions have start == next_loc: "replace" the empty string at the
   start location with the new string.
   Deletions are replacement with the empty string.

   There is only limited support for newline characters in fix-it hints
   as noted above in the comment for class rich_location.
   A fixit_hint instance can have at most one newline character; if
   present, the newline character must be the final character of
   the content (preventing e.g. fix-its that split a pre-existing line).  */

class fixit_hint
{
 public:
  fixit_hint (location_t start,
	      location_t next_loc,
	      const char *new_content);
  fixit_hint (const fixit_hint &other);
  fixit_hint (fixit_hint &&other) = delete;
  ~fixit_hint () { free (m_bytes); }
  fixit_hint &operator= (const fixit_hint &) = delete;
  fixit_hint &operator= (fixit_hint &&) = delete;

  bool affects_line_p (const line_maps *set,
		       const char *file,
		       int line) const;
  location_t get_start_loc () const { return m_start; }
  location_t get_next_loc () const { return m_next_loc; }
  bool maybe_append (location_t start,
		     location_t next_loc,
		     const char *new_content);

  const char *get_string () const { return m_bytes; }
  size_t get_length () const { return m_len; }

  bool insertion_p () const { return m_start == m_next_loc; }

  bool ends_with_newline_p () const;

 private:
  /* We don't use source_range here since, unlike most places,
     this is a half-open/half-closed range:
       [start, next_loc)
     so that we can support insertion via start == next_loc.  */
  location_t m_start;
  location_t m_next_loc;
  char *m_bytes;
  size_t m_len;
};

#endif /* !LIBCPP_RICH_LOCATION_H  */
