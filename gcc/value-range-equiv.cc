/* Support routines for value ranges with equivalences.
   Copyright (C) 2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "value-range-equiv.h"

value_range_equiv::value_range_equiv (tree min, tree max, bitmap equiv,
				      value_range_kind kind)
{
  m_equiv = NULL;
  set (min, max, equiv, kind);
}

value_range_equiv::value_range_equiv (const value_range &other)
{
  m_equiv = NULL;
  set (other.min(), other.max (), NULL, other.kind ());
}

void
value_range_equiv::set (tree min, tree max, bitmap equiv,
			value_range_kind kind)
{
  value_range::set (min, max, kind);
  set_equiv (equiv);
  if (flag_checking)
    check ();
}

void
value_range_equiv::set (tree val)
{
  gcc_assert (TREE_CODE (val) == SSA_NAME || is_gimple_min_invariant (val));
  if (TREE_OVERFLOW_P (val))
    val = drop_tree_overflow (val);
  set (val, val);
}

void
value_range_equiv::set_undefined ()
{
  set (NULL, NULL, NULL, VR_UNDEFINED);
}

void
value_range_equiv::set_varying (tree type)
{
  value_range::set_varying (type);
  equiv_clear ();
}

/* Like set, but keep the equivalences in place.  */

void
value_range_equiv::update (tree min, tree max, value_range_kind kind)
{
  set (min, max,
       (kind != VR_UNDEFINED && kind != VR_VARYING) ? m_equiv : NULL, kind);
}

/* Copy value_range in FROM into THIS while avoiding bitmap sharing.

   Note: The code that avoids the bitmap sharing looks at the existing
   this->m_equiv, so this function cannot be used to initalize an
   object.  Use the constructors for initialization.  */

void
value_range_equiv::deep_copy (const value_range_equiv *from)
{
  set (from->min (), from->max (), from->m_equiv, from->m_kind);
}

void
value_range_equiv::move (value_range_equiv *from)
{
  set (from->min (), from->max (), NULL, from->m_kind);
  m_equiv = from->m_equiv;
  from->m_equiv = NULL;
}

void
value_range_equiv::set_equiv (bitmap equiv)
{
  if (undefined_p () || varying_p ())
    equiv = NULL;
  /* Since updating the equivalence set involves deep copying the
     bitmaps, only do it if absolutely necessary.

     All equivalence bitmaps are allocated from the same obstack.  So
     we can use the obstack associated with EQUIV to allocate vr->equiv.  */
  if (m_equiv == NULL
      && equiv != NULL)
    m_equiv = BITMAP_ALLOC (equiv->obstack);

  if (equiv != m_equiv)
    {
      if (equiv && !bitmap_empty_p (equiv))
	bitmap_copy (m_equiv, equiv);
      else
	bitmap_clear (m_equiv);
    }
}

void
value_range_equiv::check ()
{
  value_range::check ();
  switch (m_kind)
    {
    case VR_UNDEFINED:
    case VR_VARYING:
      gcc_assert (!m_equiv || bitmap_empty_p (m_equiv));
    default:;
    }
}

/* Return true if the bitmaps B1 and B2 are equal.  */

static bool
vr_bitmap_equal_p (const_bitmap b1, const_bitmap b2)
{
  return (b1 == b2
	  || ((!b1 || bitmap_empty_p (b1))
	      && (!b2 || bitmap_empty_p (b2)))
	  || (b1 && b2
	      && bitmap_equal_p (b1, b2)));
}

/* Returns TRUE if THIS == OTHER.  Ignores the equivalence bitmap if
   IGNORE_EQUIVS is TRUE.  */

bool
value_range_equiv::equal_p (const value_range_equiv &other,
			    bool ignore_equivs) const
{
  return (value_range::equal_p (other)
	  && (ignore_equivs || vr_bitmap_equal_p (m_equiv, other.m_equiv)));
}

void
value_range_equiv::equiv_clear ()
{
  if (m_equiv)
    bitmap_clear (m_equiv);
}

/* Add VAR and VAR's equivalence set (VAR_VR) to the equivalence
   bitmap.  If no equivalence table has been created, OBSTACK is the
   obstack to use (NULL for the default obstack).

   This is the central point where equivalence processing can be
   turned on/off.  */

void
value_range_equiv::equiv_add (const_tree var,
			      const value_range_equiv *var_vr,
			      bitmap_obstack *obstack)
{
  if (!m_equiv)
    m_equiv = BITMAP_ALLOC (obstack);
  unsigned ver = SSA_NAME_VERSION (var);
  bitmap_set_bit (m_equiv, ver);
  if (var_vr && var_vr->m_equiv)
    bitmap_ior_into (m_equiv, var_vr->m_equiv);
}

void
value_range_equiv::intersect (const value_range_equiv *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Intersecting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }

  /* If THIS is varying we want to pick up equivalences from OTHER.
     Just special-case this here rather than trying to fixup after the
     fact.  */
  if (this->varying_p ())
    this->deep_copy (other);
  else
    {
      value_range tem = intersect_helper (this, other);
      this->update (tem.min (), tem.max (), tem.kind ());

      /* If the result is VR_UNDEFINED there is no need to mess with
	 equivalencies.  */
      if (!undefined_p ())
	{
	  /* The resulting set of equivalences for range intersection
	     is the union of the two sets.  */
	  if (m_equiv && other->m_equiv && m_equiv != other->m_equiv)
	    bitmap_ior_into (m_equiv, other->m_equiv);
	  else if (other->m_equiv && !m_equiv)
	    {
	      /* All equivalence bitmaps are allocated from the same
		 obstack.  So we can use the obstack associated with
		 VR to allocate this->m_equiv.  */
	      m_equiv = BITMAP_ALLOC (other->m_equiv->obstack);
	      bitmap_copy (m_equiv, other->m_equiv);
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

void
value_range_equiv::union_ (const value_range_equiv *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Meeting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }

  /* If THIS is undefined we want to pick up equivalences from OTHER.
     Just special-case this here rather than trying to fixup after the fact.  */
  if (this->undefined_p ())
    this->deep_copy (other);
  else
    {
      value_range tem = union_helper (this, other);
      this->update (tem.min (), tem.max (), tem.kind ());

      /* The resulting set of equivalences is always the intersection of
	 the two sets.  */
      if (this->m_equiv && other->m_equiv && this->m_equiv != other->m_equiv)
	bitmap_and_into (this->m_equiv, other->m_equiv);
      else if (this->m_equiv && !other->m_equiv)
	bitmap_clear (this->m_equiv);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

void
value_range_equiv::dump (FILE *file) const
{
  value_range::dump (file);
  if ((m_kind == VR_RANGE || m_kind == VR_ANTI_RANGE)
      && m_equiv)
    {
      bitmap_iterator bi;
      unsigned i, c = 0;

      fprintf (file, "  EQUIVALENCES: { ");
      EXECUTE_IF_SET_IN_BITMAP (m_equiv, 0, i, bi)
	{
	  print_generic_expr (file, ssa_name (i));
	  fprintf (file, " ");
	  c++;
	}
      fprintf (file, "} (%u elements)", c);
    }
}

void
value_range_equiv::dump () const
{
  dump (stderr);
}

void
dump_value_range (FILE *file, const value_range_equiv *vr)
{
  if (!vr)
    fprintf (file, "[]");
  else
    vr->dump (file);
}

DEBUG_FUNCTION void
debug (const value_range_equiv *vr)
{
  dump_value_range (stderr, vr);
}

DEBUG_FUNCTION void
debug (const value_range_equiv &vr)
{
  dump_value_range (stderr, &vr);
}
