/* String length optimization
   Copyright (C) 2011-2018 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa-warn-restrict.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "expr.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "domwalk.h"
#include "tree-ssa-alias.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-strlen.h"
#include "params.h"
#include "tree-hash-traits.h"
#include "tree-object-size.h"
#include "builtins.h"
#include "target.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "intl.h"
#include "attribs.h"
#include "calls.h"

/* A vector indexed by SSA_NAME_VERSION.  0 means unknown, positive value
   is an index into strinfo vector, negative value stands for
   string length of a string literal (~strlen).  */
static vec<int> ssa_ver_to_stridx;

/* Number of currently active string indexes plus one.  */
static int max_stridx;

/* String information record.  */
struct strinfo
{
  /* Number of leading characters that are known to be nonzero.  This is
     also the length of the string if FULL_STRING_P.

     The values in a list of related string pointers must be consistent;
     that is, if strinfo B comes X bytes after strinfo A, it must be
     the case that A->nonzero_chars == X + B->nonzero_chars.  */
  tree nonzero_chars;
  /* Any of the corresponding pointers for querying alias oracle.  */
  tree ptr;
  /* This is used for two things:

     - To record the statement that should be used for delayed length
       computations.  We maintain the invariant that all related strinfos
       have delayed lengths or none do.

     - To record the malloc or calloc call that produced this result.  */
  gimple *stmt;
  /* Pointer to '\0' if known, if NULL, it can be computed as
     ptr + length.  */
  tree endptr;
  /* Reference count.  Any changes to strinfo entry possibly shared
     with dominating basic blocks need unshare_strinfo first, except
     for dont_invalidate which affects only the immediately next
     maybe_invalidate.  */
  int refcount;
  /* Copy of index.  get_strinfo (si->idx) should return si;  */
  int idx;
  /* These 3 fields are for chaining related string pointers together.
     E.g. for
     bl = strlen (b); dl = strlen (d); strcpy (a, b); c = a + bl;
     strcpy (c, d); e = c + dl;
     strinfo(a) -> strinfo(c) -> strinfo(e)
     All have ->first field equal to strinfo(a)->idx and are doubly
     chained through prev/next fields.  The later strinfos are required
     to point into the same string with zero or more bytes after
     the previous pointer and all bytes in between the two pointers
     must be non-zero.  Functions like strcpy or memcpy are supposed
     to adjust all previous strinfo lengths, but not following strinfo
     lengths (those are uncertain, usually invalidated during
     maybe_invalidate, except when the alias oracle knows better).
     Functions like strcat on the other side adjust the whole
     related strinfo chain.
     They are updated lazily, so to use the chain the same first fields
     and si->prev->next == si->idx needs to be verified.  */
  int first;
  int next;
  int prev;
  /* A flag whether the string is known to be written in the current
     function.  */
  bool writable;
  /* A flag for the next maybe_invalidate that this strinfo shouldn't
     be invalidated.  Always cleared by maybe_invalidate.  */
  bool dont_invalidate;
  /* True if the string is known to be nul-terminated after NONZERO_CHARS
     characters.  False is useful when detecting strings that are built
     up via successive memcpys.  */
  bool full_string_p;
};

/* Pool for allocating strinfo_struct entries.  */
static object_allocator<strinfo> strinfo_pool ("strinfo pool");

/* Vector mapping positive string indexes to strinfo, for the
   current basic block.  The first pointer in the vector is special,
   it is either NULL, meaning the vector isn't shared, or it is
   a basic block pointer to the owner basic_block if shared.
   If some other bb wants to modify the vector, the vector needs
   to be unshared first, and only the owner bb is supposed to free it.  */
static vec<strinfo *, va_heap, vl_embed> *stridx_to_strinfo;

/* One OFFSET->IDX mapping.  */
struct stridxlist
{
  struct stridxlist *next;
  HOST_WIDE_INT offset;
  int idx;
};

/* Hash table entry, mapping a DECL to a chain of OFFSET->IDX mappings.  */
struct decl_stridxlist_map
{
  struct tree_map_base base;
  struct stridxlist list;
};

/* Hash table for mapping decls to a chained list of offset -> idx
   mappings.  */
static hash_map<tree_decl_hash, stridxlist> *decl_to_stridxlist_htab;

/* Hash table mapping strlen calls to stridx instances describing
   the calls' arguments.  Non-null only when warn_stringop_truncation
   is non-zero.  */
typedef std::pair<int, location_t> stridx_strlenloc;
static hash_map<tree, stridx_strlenloc> *strlen_to_stridx;

/* Obstack for struct stridxlist and struct decl_stridxlist_map.  */
static struct obstack stridx_obstack;

/* Last memcpy statement if it could be adjusted if the trailing
   '\0' written is immediately overwritten, or
   *x = '\0' store that could be removed if it is immediately overwritten.  */
struct laststmt_struct
{
  gimple *stmt;
  tree len;
  int stridx;
} laststmt;

static int get_stridx_plus_constant (strinfo *, unsigned HOST_WIDE_INT, tree);
static void handle_builtin_stxncpy (built_in_function, gimple_stmt_iterator *);

/* Return:

   - 1 if SI is known to start with more than OFF nonzero characters.

   - 0 if SI is known to start with OFF nonzero characters,
     but is not known to start with more.

   - -1 if SI might not start with OFF nonzero characters.  */

static inline int
compare_nonzero_chars (strinfo *si, unsigned HOST_WIDE_INT off)
{
  if (si->nonzero_chars
      && TREE_CODE (si->nonzero_chars) == INTEGER_CST)
    return compare_tree_int (si->nonzero_chars, off);
  else
    return -1;
}

/* Return true if SI is known to be a zero-length string.  */

static inline bool
zero_length_string_p (strinfo *si)
{
  return si->full_string_p && integer_zerop (si->nonzero_chars);
}

/* Return strinfo vector entry IDX.  */

static inline strinfo *
get_strinfo (int idx)
{
  if (vec_safe_length (stridx_to_strinfo) <= (unsigned int) idx)
    return NULL;
  return (*stridx_to_strinfo)[idx];
}

/* Get the next strinfo in the chain after SI, or null if none.  */

static inline strinfo *
get_next_strinfo (strinfo *si)
{
  if (si->next == 0)
    return NULL;
  strinfo *nextsi = get_strinfo (si->next);
  if (nextsi == NULL || nextsi->first != si->first || nextsi->prev != si->idx)
    return NULL;
  return nextsi;
}

/* Helper function for get_stridx.  Return the strinfo index of the address
   of EXP, which is available in PTR if nonnull.  If OFFSET_OUT, it is
   OK to return the index for some X <= &EXP and store &EXP - X in
   *OFFSET_OUT.  */

static int
get_addr_stridx (tree exp, tree ptr, unsigned HOST_WIDE_INT *offset_out)
{
  HOST_WIDE_INT off;
  struct stridxlist *list, *last = NULL;
  tree base;

  if (!decl_to_stridxlist_htab)
    return 0;

  poly_int64 poff;
  base = get_addr_base_and_unit_offset (exp, &poff);
  if (base == NULL || !DECL_P (base) || !poff.is_constant (&off))
    return 0;

  list = decl_to_stridxlist_htab->get (base);
  if (list == NULL)
    return 0;

  do
    {
      if (list->offset == off)
	{
	  if (offset_out)
	    *offset_out = 0;
	  return list->idx;
	}
      if (list->offset > off)
	return 0;
      last = list;
      list = list->next;
    }
  while (list);

  if ((offset_out || ptr) && last && last->idx > 0)
    {
      unsigned HOST_WIDE_INT rel_off
	= (unsigned HOST_WIDE_INT) off - last->offset;
      strinfo *si = get_strinfo (last->idx);
      if (si && compare_nonzero_chars (si, rel_off) >= 0)
	{
	  if (offset_out)
	    {
	      *offset_out = rel_off;
	      return last->idx;
	    }
	  else
	    return get_stridx_plus_constant (si, rel_off, ptr);
	}
    }
  return 0;
}

/* Return string index for EXP.  */

static int
get_stridx (tree exp)
{
  tree s, o;

  if (TREE_CODE (exp) == SSA_NAME)
    {
      if (ssa_ver_to_stridx[SSA_NAME_VERSION (exp)])
	return ssa_ver_to_stridx[SSA_NAME_VERSION (exp)];
      int i;
      tree e = exp;
      HOST_WIDE_INT off = 0;
      for (i = 0; i < 5; i++)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (e);
	  if (!is_gimple_assign (def_stmt)
	      || gimple_assign_rhs_code (def_stmt) != POINTER_PLUS_EXPR)
	    return 0;
	  tree rhs1 = gimple_assign_rhs1 (def_stmt);
	  tree rhs2 = gimple_assign_rhs2 (def_stmt);
	  if (TREE_CODE (rhs1) != SSA_NAME
	      || !tree_fits_shwi_p (rhs2))
	    return 0;
	  HOST_WIDE_INT this_off = tree_to_shwi (rhs2);
	  if (this_off < 0)
	    return 0;
	  off = (unsigned HOST_WIDE_INT) off + this_off;
	  if (off < 0)
	    return 0;
	  if (ssa_ver_to_stridx[SSA_NAME_VERSION (rhs1)])
	    {
	      strinfo *si
		= get_strinfo (ssa_ver_to_stridx[SSA_NAME_VERSION (rhs1)]);
	      if (si && compare_nonzero_chars (si, off) >= 0)
		return get_stridx_plus_constant (si, off, exp);
	    }
	  e = rhs1;
	}
      return 0;
    }

  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      int idx = get_addr_stridx (TREE_OPERAND (exp, 0), exp, NULL);
      if (idx != 0)
	return idx;
    }

  s = string_constant (exp, &o);
  if (s != NULL_TREE
      && (o == NULL_TREE || tree_fits_shwi_p (o))
      && TREE_STRING_LENGTH (s) > 0)
    {
      HOST_WIDE_INT offset = o ? tree_to_shwi (o) : 0;
      const char *p = TREE_STRING_POINTER (s);
      int max = TREE_STRING_LENGTH (s) - 1;

      if (p[max] == '\0' && offset >= 0 && offset <= max)
	return ~(int) strlen (p + offset);
    }
  return 0;
}

/* Return true if strinfo vector is shared with the immediate dominator.  */

static inline bool
strinfo_shared (void)
{
  return vec_safe_length (stridx_to_strinfo)
	 && (*stridx_to_strinfo)[0] != NULL;
}

/* Unshare strinfo vector that is shared with the immediate dominator.  */

static void
unshare_strinfo_vec (void)
{
  strinfo *si;
  unsigned int i = 0;

  gcc_assert (strinfo_shared ());
  stridx_to_strinfo = vec_safe_copy (stridx_to_strinfo);
  for (i = 1; vec_safe_iterate (stridx_to_strinfo, i, &si); ++i)
    if (si != NULL)
      si->refcount++;
  (*stridx_to_strinfo)[0] = NULL;
}

/* Attempt to create a string index for exp, ADDR_EXPR's operand.
   Return a pointer to the location where the string index can
   be stored (if 0) or is stored, or NULL if this can't be tracked.  */

static int *
addr_stridxptr (tree exp)
{
  HOST_WIDE_INT off;

  poly_int64 poff;
  tree base = get_addr_base_and_unit_offset (exp, &poff);
  if (base == NULL_TREE || !DECL_P (base) || !poff.is_constant (&off))
    return NULL;

  if (!decl_to_stridxlist_htab)
    {
      decl_to_stridxlist_htab
       	= new hash_map<tree_decl_hash, stridxlist> (64);
      gcc_obstack_init (&stridx_obstack);
    }

  bool existed;
  stridxlist *list = &decl_to_stridxlist_htab->get_or_insert (base, &existed);
  if (existed)
    {
      int i;
      stridxlist *before = NULL;
      for (i = 0; i < 32; i++)
	{
	  if (list->offset == off)
	    return &list->idx;
	  if (list->offset > off && before == NULL)
	    before = list;
	  if (list->next == NULL)
	    break;
	  list = list->next;
	}
      if (i == 32)
	return NULL;
      if (before)
	{
	  list = before;
	  before = XOBNEW (&stridx_obstack, struct stridxlist);
	  *before = *list;
	  list->next = before;
	  list->offset = off;
	  list->idx = 0;
	  return &list->idx;
	}
      list->next = XOBNEW (&stridx_obstack, struct stridxlist);
      list = list->next;
    }

  list->next = NULL;
  list->offset = off;
  list->idx = 0;
  return &list->idx;
}

/* Create a new string index, or return 0 if reached limit.  */

static int
new_stridx (tree exp)
{
  int idx;
  if (max_stridx >= PARAM_VALUE (PARAM_MAX_TRACKED_STRLENS))
    return 0;
  if (TREE_CODE (exp) == SSA_NAME)
    {
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (exp))
	return 0;
      idx = max_stridx++;
      ssa_ver_to_stridx[SSA_NAME_VERSION (exp)] = idx;
      return idx;
    }
  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      int *pidx = addr_stridxptr (TREE_OPERAND (exp, 0));
      if (pidx != NULL)
	{
	  gcc_assert (*pidx == 0);
	  *pidx = max_stridx++;
	  return *pidx;
	}
    }
  return 0;
}

/* Like new_stridx, but for ADDR_EXPR's operand instead.  */

static int
new_addr_stridx (tree exp)
{
  int *pidx;
  if (max_stridx >= PARAM_VALUE (PARAM_MAX_TRACKED_STRLENS))
    return 0;
  pidx = addr_stridxptr (exp);
  if (pidx != NULL)
    {
      gcc_assert (*pidx == 0);
      *pidx = max_stridx++;
      return *pidx;
    }
  return 0;
}

/* Create a new strinfo.  */

static strinfo *
new_strinfo (tree ptr, int idx, tree nonzero_chars, bool full_string_p)
{
  strinfo *si = strinfo_pool.allocate ();
  si->nonzero_chars = nonzero_chars;
  si->ptr = ptr;
  si->stmt = NULL;
  si->endptr = NULL_TREE;
  si->refcount = 1;
  si->idx = idx;
  si->first = 0;
  si->prev = 0;
  si->next = 0;
  si->writable = false;
  si->dont_invalidate = false;
  si->full_string_p = full_string_p;
  return si;
}

/* Decrease strinfo refcount and free it if not referenced anymore.  */

static inline void
free_strinfo (strinfo *si)
{
  if (si && --si->refcount == 0)
    strinfo_pool.remove (si);
}

/* Set strinfo in the vector entry IDX to SI.  */

static inline void
set_strinfo (int idx, strinfo *si)
{
  if (vec_safe_length (stridx_to_strinfo) && (*stridx_to_strinfo)[0])
    unshare_strinfo_vec ();
  if (vec_safe_length (stridx_to_strinfo) <= (unsigned int) idx)
    vec_safe_grow_cleared (stridx_to_strinfo, idx + 1);
  (*stridx_to_strinfo)[idx] = si;
}

/* Return the first strinfo in the related strinfo chain
   if all strinfos in between belong to the chain, otherwise NULL.  */

static strinfo *
verify_related_strinfos (strinfo *origsi)
{
  strinfo *si = origsi, *psi;

  if (origsi->first == 0)
    return NULL;
  for (; si->prev; si = psi)
    {
      if (si->first != origsi->first)
	return NULL;
      psi = get_strinfo (si->prev);
      if (psi == NULL)
	return NULL;
      if (psi->next != si->idx)
	return NULL;
    }
  if (si->idx != si->first)
    return NULL;
  return si;
}

/* Set SI's endptr to ENDPTR and compute its length based on SI->ptr.
   Use LOC for folding.  */

static void
set_endptr_and_length (location_t loc, strinfo *si, tree endptr)
{
  si->endptr = endptr;
  si->stmt = NULL;
  tree start_as_size = fold_convert_loc (loc, size_type_node, si->ptr);
  tree end_as_size = fold_convert_loc (loc, size_type_node, endptr);
  si->nonzero_chars = fold_build2_loc (loc, MINUS_EXPR, size_type_node,
				       end_as_size, start_as_size);
  si->full_string_p = true;
}

/* Return string length, or NULL if it can't be computed.  */

static tree
get_string_length (strinfo *si)
{
  if (si->nonzero_chars)
    return si->full_string_p ? si->nonzero_chars : NULL;

  if (si->stmt)
    {
      gimple *stmt = si->stmt, *lenstmt;
      tree callee, lhs, fn, tem;
      location_t loc;
      gimple_stmt_iterator gsi;

      gcc_assert (is_gimple_call (stmt));
      callee = gimple_call_fndecl (stmt);
      gcc_assert (callee && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL);
      lhs = gimple_call_lhs (stmt);
      /* unshare_strinfo is intentionally not called here.  The (delayed)
	 transformation of strcpy or strcat into stpcpy is done at the place
	 of the former strcpy/strcat call and so can affect all the strinfos
	 with the same stmt.  If they were unshared before and transformation
	 has been already done, the handling of BUILT_IN_STPCPY{,_CHK} should
	 just compute the right length.  */
      switch (DECL_FUNCTION_CODE (callee))
	{
	case BUILT_IN_STRCAT:
	case BUILT_IN_STRCAT_CHK:
	  gsi = gsi_for_stmt (stmt);
	  fn = builtin_decl_implicit (BUILT_IN_STRLEN);
	  gcc_assert (lhs == NULL_TREE);
	  tem = unshare_expr (gimple_call_arg (stmt, 0));
	  lenstmt = gimple_build_call (fn, 1, tem);
	  lhs = make_ssa_name (TREE_TYPE (TREE_TYPE (fn)), lenstmt);
	  gimple_call_set_lhs (lenstmt, lhs);
	  gimple_set_vuse (lenstmt, gimple_vuse (stmt));
	  gsi_insert_before (&gsi, lenstmt, GSI_SAME_STMT);
	  tem = gimple_call_arg (stmt, 0);
          if (!ptrofftype_p (TREE_TYPE (lhs)))
            {
              lhs = convert_to_ptrofftype (lhs);
              lhs = force_gimple_operand_gsi (&gsi, lhs, true, NULL_TREE,
                                              true, GSI_SAME_STMT);
            }
	  lenstmt = gimple_build_assign
			(make_ssa_name (TREE_TYPE (gimple_call_arg (stmt, 0))),
			 POINTER_PLUS_EXPR,tem, lhs);
	  gsi_insert_before (&gsi, lenstmt, GSI_SAME_STMT);
	  gimple_call_set_arg (stmt, 0, gimple_assign_lhs (lenstmt));
	  lhs = NULL_TREE;
	  /* FALLTHRU */
	case BUILT_IN_STRCPY:
	case BUILT_IN_STRCPY_CHK:
	  gcc_assert (builtin_decl_implicit_p (BUILT_IN_STPCPY));
	  if (gimple_call_num_args (stmt) == 2)
	    fn = builtin_decl_implicit (BUILT_IN_STPCPY);
	  else
	    fn = builtin_decl_explicit (BUILT_IN_STPCPY_CHK);
	  gcc_assert (lhs == NULL_TREE);
	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "Optimizing: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  gimple_call_set_fndecl (stmt, fn);
	  lhs = make_ssa_name (TREE_TYPE (TREE_TYPE (fn)), stmt);
	  gimple_call_set_lhs (stmt, lhs);
	  update_stmt (stmt);
	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "into: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  /* FALLTHRU */
	case BUILT_IN_STPCPY:
	case BUILT_IN_STPCPY_CHK:
	  gcc_assert (lhs != NULL_TREE);
	  loc = gimple_location (stmt);
	  set_endptr_and_length (loc, si, lhs);
	  for (strinfo *chainsi = verify_related_strinfos (si);
	       chainsi != NULL;
	       chainsi = get_next_strinfo (chainsi))
	    if (chainsi->nonzero_chars == NULL)
	      set_endptr_and_length (loc, chainsi, lhs);
	  break;
	case BUILT_IN_MALLOC:
	  break;
	/* BUILT_IN_CALLOC always has si->nonzero_chars set.  */
	default:
	  gcc_unreachable ();
	  break;
	}
    }

  return si->nonzero_chars;
}

/* Invalidate string length information for strings whose length
   might change due to stores in stmt.  */

static bool
maybe_invalidate (gimple *stmt)
{
  strinfo *si;
  unsigned int i;
  bool nonempty = false;

  for (i = 1; vec_safe_iterate (stridx_to_strinfo, i, &si); ++i)
    if (si != NULL)
      {
	if (!si->dont_invalidate)
	  {
	    ao_ref r;
	    /* Do not use si->nonzero_chars.  */
	    ao_ref_init_from_ptr_and_size (&r, si->ptr, NULL_TREE);
	    if (stmt_may_clobber_ref_p_1 (stmt, &r))
	      {
		set_strinfo (i, NULL);
		free_strinfo (si);
		continue;
	      }
	  }
	si->dont_invalidate = false;
	nonempty = true;
      }
  return nonempty;
}

/* Unshare strinfo record SI, if it has refcount > 1 or
   if stridx_to_strinfo vector is shared with some other
   bbs.  */

static strinfo *
unshare_strinfo (strinfo *si)
{
  strinfo *nsi;

  if (si->refcount == 1 && !strinfo_shared ())
    return si;

  nsi = new_strinfo (si->ptr, si->idx, si->nonzero_chars, si->full_string_p);
  nsi->stmt = si->stmt;
  nsi->endptr = si->endptr;
  nsi->first = si->first;
  nsi->prev = si->prev;
  nsi->next = si->next;
  nsi->writable = si->writable;
  set_strinfo (si->idx, nsi);
  free_strinfo (si);
  return nsi;
}

/* Attempt to create a new strinfo for BASESI + OFF, or find existing
   strinfo if there is any.  Return it's idx, or 0 if no strinfo has
   been created.  */

static int
get_stridx_plus_constant (strinfo *basesi, unsigned HOST_WIDE_INT off,
			  tree ptr)
{
  if (TREE_CODE (ptr) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ptr))
    return 0;

  if (compare_nonzero_chars (basesi, off) < 0
      || !tree_fits_uhwi_p (basesi->nonzero_chars))
    return 0;

  unsigned HOST_WIDE_INT nonzero_chars
    = tree_to_uhwi (basesi->nonzero_chars) - off;
  strinfo *si = basesi, *chainsi;
  if (si->first || si->prev || si->next)
    si = verify_related_strinfos (basesi);
  if (si == NULL
      || si->nonzero_chars == NULL_TREE
      || TREE_CODE (si->nonzero_chars) != INTEGER_CST)
    return 0;

  if (TREE_CODE (ptr) == SSA_NAME
      && ssa_ver_to_stridx.length () <= SSA_NAME_VERSION (ptr))
    ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names);

  gcc_checking_assert (compare_tree_int (si->nonzero_chars, off) != -1);
  for (chainsi = si; chainsi->next; chainsi = si)
    {
      si = get_next_strinfo (chainsi);
      if (si == NULL
	  || si->nonzero_chars == NULL_TREE
	  || TREE_CODE (si->nonzero_chars) != INTEGER_CST)
	break;
      int r = compare_tree_int (si->nonzero_chars, nonzero_chars);
      if (r != 1)
	{
	  if (r == 0)
	    {
	      if (TREE_CODE (ptr) == SSA_NAME)
		ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)] = si->idx;
	      else
		{
		  int *pidx = addr_stridxptr (TREE_OPERAND (ptr, 0));
		  if (pidx != NULL && *pidx == 0)
		    *pidx = si->idx;
		}
	      return si->idx;
	    }
	  break;
	}
    }

  int idx = new_stridx (ptr);
  if (idx == 0)
    return 0;
  si = new_strinfo (ptr, idx, build_int_cst (size_type_node, nonzero_chars),
		    basesi->full_string_p);
  set_strinfo (idx, si);
  if (strinfo *nextsi = get_strinfo (chainsi->next))
    {
      nextsi = unshare_strinfo (nextsi);
      si->next = nextsi->idx;
      nextsi->prev = idx;
    }
  chainsi = unshare_strinfo (chainsi);
  if (chainsi->first == 0)
    chainsi->first = chainsi->idx;
  chainsi->next = idx;
  if (chainsi->endptr == NULL_TREE && zero_length_string_p (si))
    chainsi->endptr = ptr;
  si->endptr = chainsi->endptr;
  si->prev = chainsi->idx;
  si->first = chainsi->first;
  si->writable = chainsi->writable;
  return si->idx;
}

/* Note that PTR, a pointer SSA_NAME initialized in the current stmt, points
   to a zero-length string and if possible chain it to a related strinfo
   chain whose part is or might be CHAINSI.  */

static strinfo *
zero_length_string (tree ptr, strinfo *chainsi)
{
  strinfo *si;
  int idx;
  if (ssa_ver_to_stridx.length () <= SSA_NAME_VERSION (ptr))
    ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names);
  gcc_checking_assert (TREE_CODE (ptr) == SSA_NAME
		       && ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)] == 0);

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ptr))
    return NULL;
  if (chainsi != NULL)
    {
      si = verify_related_strinfos (chainsi);
      if (si)
	{
	  do
	    {
	      /* We shouldn't mix delayed and non-delayed lengths.  */
	      gcc_assert (si->full_string_p);
	      if (si->endptr == NULL_TREE)
		{
		  si = unshare_strinfo (si);
		  si->endptr = ptr;
		}
	      chainsi = si;
	      si = get_next_strinfo (si);
	    }
	  while (si != NULL);
	  if (zero_length_string_p (chainsi))
	    {
	      if (chainsi->next)
		{
		  chainsi = unshare_strinfo (chainsi);
		  chainsi->next = 0;
		}
	      ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)] = chainsi->idx;
	      return chainsi;
	    }
	}
      else
	{
	  /* We shouldn't mix delayed and non-delayed lengths.  */
	  gcc_assert (chainsi->full_string_p);
	  if (chainsi->first || chainsi->prev || chainsi->next)
	    {
	      chainsi = unshare_strinfo (chainsi);
	      chainsi->first = 0;
	      chainsi->prev = 0;
	      chainsi->next = 0;
	    }
	}
    }
  idx = new_stridx (ptr);
  if (idx == 0)
    return NULL;
  si = new_strinfo (ptr, idx, build_int_cst (size_type_node, 0), true);
  set_strinfo (idx, si);
  si->endptr = ptr;
  if (chainsi != NULL)
    {
      chainsi = unshare_strinfo (chainsi);
      if (chainsi->first == 0)
	chainsi->first = chainsi->idx;
      chainsi->next = idx;
      if (chainsi->endptr == NULL_TREE)
	chainsi->endptr = ptr;
      si->prev = chainsi->idx;
      si->first = chainsi->first;
      si->writable = chainsi->writable;
    }
  return si;
}

/* For strinfo ORIGSI whose length has been just updated, adjust other
   related strinfos so that they match the new ORIGSI.  This involves:

   - adding ADJ to the nonzero_chars fields
   - copying full_string_p from the new ORIGSI.  */

static void
adjust_related_strinfos (location_t loc, strinfo *origsi, tree adj)
{
  strinfo *si = verify_related_strinfos (origsi);

  if (si == NULL)
    return;

  while (1)
    {
      strinfo *nsi;

      if (si != origsi)
	{
	  tree tem;

	  si = unshare_strinfo (si);
	  /* We shouldn't see delayed lengths here; the caller must have
	     calculated the old length in order to calculate the
	     adjustment.  */
	  gcc_assert (si->nonzero_chars);
	  tem = fold_convert_loc (loc, TREE_TYPE (si->nonzero_chars), adj);
	  si->nonzero_chars = fold_build2_loc (loc, PLUS_EXPR,
					       TREE_TYPE (si->nonzero_chars),
					       si->nonzero_chars, tem);
	  si->full_string_p = origsi->full_string_p;

	  si->endptr = NULL_TREE;
	  si->dont_invalidate = true;
	}
      nsi = get_next_strinfo (si);
      if (nsi == NULL)
	return;
      si = nsi;
    }
}

/* Find if there are other SSA_NAME pointers equal to PTR
   for which we don't track their string lengths yet.  If so, use
   IDX for them.  */

static void
find_equal_ptrs (tree ptr, int idx)
{
  if (TREE_CODE (ptr) != SSA_NAME)
    return;
  while (1)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (ptr);
      if (!is_gimple_assign (stmt))
	return;
      ptr = gimple_assign_rhs1 (stmt);
      switch (gimple_assign_rhs_code (stmt))
	{
	case SSA_NAME:
	  break;
	CASE_CONVERT:
	  if (!POINTER_TYPE_P (TREE_TYPE (ptr)))
	    return;
	  if (TREE_CODE (ptr) == SSA_NAME)
	    break;
	  if (TREE_CODE (ptr) != ADDR_EXPR)
	    return;
	  /* FALLTHRU */
	case ADDR_EXPR:
	  {
	    int *pidx = addr_stridxptr (TREE_OPERAND (ptr, 0));
	    if (pidx != NULL && *pidx == 0)
	      *pidx = idx;
	    return;
	  }
	default:
	  return;
	}

      /* We might find an endptr created in this pass.  Grow the
	 vector in that case.  */
      if (ssa_ver_to_stridx.length () <= SSA_NAME_VERSION (ptr))
	ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names);

      if (ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)] != 0)
	return;
      ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)] = idx;
    }
}

/* Return true if STMT is a call to a builtin function with the right
   arguments and attributes that should be considered for optimization
   by this pass.  */

static bool
valid_builtin_call (gimple *stmt)
{
  if (!gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    return false;

  tree callee = gimple_call_fndecl (stmt);
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_MEMCMP:
    case BUILT_IN_MEMCMP_EQ:
    case BUILT_IN_STRCHR:
    case BUILT_IN_STRLEN:
      /* The above functions should be pure.  Punt if they aren't.  */
      if (gimple_vdef (stmt) || gimple_vuse (stmt) == NULL_TREE)
	return false;
      break;

    case BUILT_IN_CALLOC:
    case BUILT_IN_MALLOC:
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMSET:
    case BUILT_IN_STPCPY:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCPY_CHK:
      /* The above functions should be neither const nor pure.  Punt if they
	 aren't.  */
      if (gimple_vdef (stmt) == NULL_TREE || gimple_vuse (stmt) == NULL_TREE)
	return false;
      break;

    default:
      break;
    }

  return true;
}

/* If the last .MEM setter statement before STMT is
   memcpy (x, y, strlen (y) + 1), the only .MEM use of it is STMT
   and STMT is known to overwrite x[strlen (x)], adjust the last memcpy to
   just memcpy (x, y, strlen (y)).  SI must be the zero length
   strinfo.  */

static void
adjust_last_stmt (strinfo *si, gimple *stmt, bool is_strcat)
{
  tree vuse, callee, len;
  struct laststmt_struct last = laststmt;
  strinfo *lastsi, *firstsi;
  unsigned len_arg_no = 2;

  laststmt.stmt = NULL;
  laststmt.len = NULL_TREE;
  laststmt.stridx = 0;

  if (last.stmt == NULL)
    return;

  vuse = gimple_vuse (stmt);
  if (vuse == NULL_TREE
      || SSA_NAME_DEF_STMT (vuse) != last.stmt
      || !has_single_use (vuse))
    return;

  gcc_assert (last.stridx > 0);
  lastsi = get_strinfo (last.stridx);
  if (lastsi == NULL)
    return;

  if (lastsi != si)
    {
      if (lastsi->first == 0 || lastsi->first != si->first)
	return;

      firstsi = verify_related_strinfos (si);
      if (firstsi == NULL)
	return;
      while (firstsi != lastsi)
	{
	  firstsi = get_next_strinfo (firstsi);
	  if (firstsi == NULL)
	    return;
	}
    }

  if (!is_strcat && !zero_length_string_p (si))
    return;

  if (is_gimple_assign (last.stmt))
    {
      gimple_stmt_iterator gsi;

      if (!integer_zerop (gimple_assign_rhs1 (last.stmt)))
	return;
      if (stmt_could_throw_p (last.stmt))
	return;
      gsi = gsi_for_stmt (last.stmt);
      unlink_stmt_vdef (last.stmt);
      release_defs (last.stmt);
      gsi_remove (&gsi, true);
      return;
    }

  if (!valid_builtin_call (last.stmt))
    return;

  callee = gimple_call_fndecl (last.stmt);
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
      break;
    default:
      return;
    }

  len = gimple_call_arg (last.stmt, len_arg_no);
  if (tree_fits_uhwi_p (len))
    {
      if (!tree_fits_uhwi_p (last.len)
	  || integer_zerop (len)
	  || tree_to_uhwi (len) != tree_to_uhwi (last.len) + 1)
	return;
      /* Don't adjust the length if it is divisible by 4, it is more efficient
	 to store the extra '\0' in that case.  */
      if ((tree_to_uhwi (len) & 3) == 0)
	return;
    }
  else if (TREE_CODE (len) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (len);
      if (!is_gimple_assign (def_stmt)
	  || gimple_assign_rhs_code (def_stmt) != PLUS_EXPR
	  || gimple_assign_rhs1 (def_stmt) != last.len
	  || !integer_onep (gimple_assign_rhs2 (def_stmt)))
	return;
    }
  else
    return;

  gimple_call_set_arg (last.stmt, len_arg_no, last.len);
  update_stmt (last.stmt);
}

/* For an LHS that is an SSA_NAME and for strlen() or strnlen() argument
   SRC, set LHS range info to [0, min (N, BOUND)] if SRC refers to
   a character array A[N] with unknown length bounded by N, and for
   strnlen(), by min (N, BOUND).  */

static tree
maybe_set_strlen_range (tree lhs, tree src, tree bound)
{
  if (TREE_CODE (lhs) != SSA_NAME
      || !INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
    return NULL_TREE;

  if (TREE_CODE (src) == SSA_NAME)
    {
      gimple *def = SSA_NAME_DEF_STMT (src);
      if (is_gimple_assign (def)
	  && gimple_assign_rhs_code (def) == ADDR_EXPR)
	src = gimple_assign_rhs1 (def);
    }

  wide_int max = wi::to_wide (TYPE_MAX_VALUE (ptrdiff_type_node));
  wide_int min = wi::zero (max.get_precision ());

  if (TREE_CODE (src) == ADDR_EXPR)
    {
      /* The last array member of a struct can be bigger than its size
	 suggests if it's treated as a poor-man's flexible array member.  */
      src = TREE_OPERAND (src, 0);
      bool src_is_array = TREE_CODE (TREE_TYPE (src)) == ARRAY_TYPE;
      if (src_is_array && !array_at_struct_end_p (src))
	{
	  tree type = TREE_TYPE (src);
	  if (tree dom = TYPE_DOMAIN (type))
	    {
	      tree maxval = TYPE_MAX_VALUE (dom);
	      if (maxval)
		max = wi::to_wide (maxval);
	      else
		max = wi::zero (min.get_precision ());

	      /* For strlen() the upper bound above is equal to
		 the longest string that can be stored in the array
		 (i.e., it accounts for the terminating nul.  For
		 strnlen() bump up the maximum by one since the array
		 need not be nul-terminated.  */
	      if (bound)
		++max;
	    }
	}
      else
	{
	  if (TREE_CODE (src) == COMPONENT_REF && !src_is_array)
	    src = TREE_OPERAND (src, 1);
	  if (DECL_P (src))
	    {
	      /* Handle the unlikely case of strlen (&c) where c is some
		 variable.  */
	      if (tree size = DECL_SIZE_UNIT (src))
		if (TREE_CODE (size) == INTEGER_CST)
		  max = wi::to_wide (size);
	    }
	}
    }

  if (bound)
    {
      /* For strnlen, adjust MIN and MAX as necessary.  If the bound
	 is less than the size of the array set MAX to it.  It it's
	 greater than MAX and MAX is non-zero bump MAX down to account
	 for the necessary terminating nul.  Otherwise leave it alone.  */
      if (TREE_CODE (bound) == INTEGER_CST)
	{
	  wide_int wibnd = wi::to_wide (bound);
	  int cmp = wi::cmpu (wibnd, max);
	  if (cmp < 0)
	    max = wibnd;
	  else if (cmp && wi::ne_p (max, min))
	    --max;
	}
      else if (TREE_CODE (bound) == SSA_NAME)
	{
	  wide_int minbound, maxbound;
	  value_range_type rng = get_range_info (bound, &minbound, &maxbound);
	  if (rng == VR_RANGE)
	    {
	      /* For a bound in a known range, adjust the range determined
		 above as necessary.  For a bound in some anti-range or
		 in an unknown range, use the range determined above.  */
	      if (wi::ltu_p (minbound, min))
		min = minbound;
	      if (wi::ltu_p (maxbound, max))
		max = maxbound;
	    }
	}
    }

  if (min == max)
    return wide_int_to_tree (size_type_node, min);

  set_range_info (lhs, VR_RANGE, min, max);
  return lhs;
}

/* Handle a strlen call.  If strlen of the argument is known, replace
   the strlen call with the known value, otherwise remember that strlen
   of the argument is stored in the lhs SSA_NAME.  */

static void
handle_builtin_strlen (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL_TREE)
    return;

  location_t loc = gimple_location (stmt);
  tree callee = gimple_call_fndecl (stmt);
  tree src = gimple_call_arg (stmt, 0);
  tree bound = (DECL_FUNCTION_CODE (callee) == BUILT_IN_STRNLEN
		? gimple_call_arg (stmt, 1) : NULL_TREE);
  int idx = get_stridx (src);
  if (idx)
    {
      strinfo *si = NULL;
      tree rhs;

      if (idx < 0)
	rhs = build_int_cst (TREE_TYPE (lhs), ~idx);
      else
	{
	  rhs = NULL_TREE;
	  si = get_strinfo (idx);
	  if (si != NULL)
	    rhs = get_string_length (si);
	}
      if (rhs != NULL_TREE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "Optimizing: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  rhs = unshare_expr (rhs);
	  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    rhs = fold_convert_loc (loc, TREE_TYPE (lhs), rhs);
	  if (bound)
	    rhs = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (rhs), rhs, bound);
	  if (!update_call_from_tree (gsi, rhs))
	    gimplify_and_update_call_from_tree (gsi, rhs);
	  stmt = gsi_stmt (*gsi);
	  update_stmt (stmt);
	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "into: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  if (si != NULL
	      && TREE_CODE (si->nonzero_chars) != SSA_NAME
	      && TREE_CODE (si->nonzero_chars) != INTEGER_CST
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    {
	      si = unshare_strinfo (si);
	      si->nonzero_chars = lhs;
	      gcc_assert (si->full_string_p);
	    }

	  if (strlen_to_stridx)
	    strlen_to_stridx->put (lhs, stridx_strlenloc (idx, loc));

	  return;
	}
    }
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return;
  if (idx == 0)
    idx = new_stridx (src);
  else
    {
      strinfo *si = get_strinfo (idx);
      if (si != NULL)
	{
	  if (!si->full_string_p && !si->stmt)
	    {
	      /* Until now we only had a lower bound on the string length.
		 Install LHS as the actual length.  */
	      si = unshare_strinfo (si);
	      tree old = si->nonzero_chars;
	      si->nonzero_chars = lhs;
	      si->full_string_p = true;
	      if (TREE_CODE (old) == INTEGER_CST)
		{
		  old = fold_convert_loc (loc, TREE_TYPE (lhs), old);
		  tree adj = fold_build2_loc (loc, MINUS_EXPR,
					      TREE_TYPE (lhs), lhs, old);
		  adjust_related_strinfos (loc, si, adj);
		}
	      else
		{
		  si->first = 0;
		  si->prev = 0;
		  si->next = 0;
		}
	    }
	  return;
	}
    }
  if (idx)
    {
      strinfo *si = new_strinfo (src, idx, lhs, true);
      set_strinfo (idx, si);
      find_equal_ptrs (src, idx);

      /* For SRC that is an array of N elements, set LHS's range
	 to [0, min (N, BOUND)].  A constant return value means
	 the range would have consisted of a single value.  In
	 that case, fold the result into the returned constant.  */
      if (tree ret = maybe_set_strlen_range (lhs, src, bound))
	if (TREE_CODE (ret) == INTEGER_CST)
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	      {
		fprintf (dump_file, "Optimizing: ");
		print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      }
	    if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (ret)))
	      ret = fold_convert_loc (loc, TREE_TYPE (lhs), ret);
	    if (!update_call_from_tree (gsi, ret))
	      gimplify_and_update_call_from_tree (gsi, ret);
	    stmt = gsi_stmt (*gsi);
	    update_stmt (stmt);
	    if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	      {
		fprintf (dump_file, "into: ");
		print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      }
	  }

      if (strlen_to_stridx)
	strlen_to_stridx->put (lhs, stridx_strlenloc (idx, loc));
    }
}

/* Handle a strchr call.  If strlen of the first argument is known, replace
   the strchr (x, 0) call with the endptr or x + strlen, otherwise remember
   that lhs of the call is endptr and strlen of the argument is endptr - x.  */

static void
handle_builtin_strchr (gimple_stmt_iterator *gsi)
{
  int idx;
  tree src;
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL_TREE)
    return;

  if (!integer_zerop (gimple_call_arg (stmt, 1)))
    return;

  src = gimple_call_arg (stmt, 0);
  idx = get_stridx (src);
  if (idx)
    {
      strinfo *si = NULL;
      tree rhs;

      if (idx < 0)
	rhs = build_int_cst (size_type_node, ~idx);
      else
	{
	  rhs = NULL_TREE;
	  si = get_strinfo (idx);
	  if (si != NULL)
	    rhs = get_string_length (si);
	}
      if (rhs != NULL_TREE)
	{
	  location_t loc = gimple_location (stmt);

	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "Optimizing: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  if (si != NULL && si->endptr != NULL_TREE)
	    {
	      rhs = unshare_expr (si->endptr);
	      if (!useless_type_conversion_p (TREE_TYPE (lhs),
					      TREE_TYPE (rhs)))
		rhs = fold_convert_loc (loc, TREE_TYPE (lhs), rhs);
	    }
	  else
	    {
	      rhs = fold_convert_loc (loc, sizetype, unshare_expr (rhs));
	      rhs = fold_build2_loc (loc, POINTER_PLUS_EXPR,
				     TREE_TYPE (src), src, rhs);
	      if (!useless_type_conversion_p (TREE_TYPE (lhs),
					      TREE_TYPE (rhs)))
		rhs = fold_convert_loc (loc, TREE_TYPE (lhs), rhs);
	    }
	  if (!update_call_from_tree (gsi, rhs))
	    gimplify_and_update_call_from_tree (gsi, rhs);
	  stmt = gsi_stmt (*gsi);
	  update_stmt (stmt);
	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "into: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  if (si != NULL
	      && si->endptr == NULL_TREE
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    {
	      si = unshare_strinfo (si);
	      si->endptr = lhs;
	    }
	  zero_length_string (lhs, si);
	  return;
	}
    }
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return;
  if (TREE_CODE (src) != SSA_NAME || !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (src))
    {
      if (idx == 0)
	idx = new_stridx (src);
      else if (get_strinfo (idx) != NULL)
	{
	  zero_length_string (lhs, NULL);
	  return;
	}
      if (idx)
	{
	  location_t loc = gimple_location (stmt);
	  tree lhsu = fold_convert_loc (loc, size_type_node, lhs);
	  tree srcu = fold_convert_loc (loc, size_type_node, src);
	  tree length = fold_build2_loc (loc, MINUS_EXPR,
					 size_type_node, lhsu, srcu);
	  strinfo *si = new_strinfo (src, idx, length, true);
	  si->endptr = lhs;
	  set_strinfo (idx, si);
	  find_equal_ptrs (src, idx);
	  zero_length_string (lhs, si);
	}
    }
  else
    zero_length_string (lhs, NULL);
}

/* Handle a strcpy-like ({st{r,p}cpy,__st{r,p}cpy_chk}) call.
   If strlen of the second argument is known, strlen of the first argument
   is the same after this call.  Furthermore, attempt to convert it to
   memcpy.  */

static void
handle_builtin_strcpy (enum built_in_function bcode, gimple_stmt_iterator *gsi)
{
  int idx, didx;
  tree src, dst, srclen, len, lhs, type, fn, oldlen;
  bool success;
  gimple *stmt = gsi_stmt (*gsi);
  strinfo *si, *dsi, *olddsi, *zsi;
  location_t loc;

  src = gimple_call_arg (stmt, 1);
  dst = gimple_call_arg (stmt, 0);
  lhs = gimple_call_lhs (stmt);
  idx = get_stridx (src);
  si = NULL;
  if (idx > 0)
    si = get_strinfo (idx);

  didx = get_stridx (dst);
  olddsi = NULL;
  oldlen = NULL_TREE;
  if (didx > 0)
    olddsi = get_strinfo (didx);
  else if (didx < 0)
    return;

  if (olddsi != NULL)
    adjust_last_stmt (olddsi, stmt, false);

  srclen = NULL_TREE;
  if (si != NULL)
    srclen = get_string_length (si);
  else if (idx < 0)
    srclen = build_int_cst (size_type_node, ~idx);

  loc = gimple_location (stmt);
  if (srclen == NULL_TREE)
    switch (bcode)
      {
      case BUILT_IN_STRCPY:
      case BUILT_IN_STRCPY_CHK:
	if (lhs != NULL_TREE || !builtin_decl_implicit_p (BUILT_IN_STPCPY))
	  return;
	break;
      case BUILT_IN_STPCPY:
      case BUILT_IN_STPCPY_CHK:
	if (lhs == NULL_TREE)
	  return;
	else
	  {
	    tree lhsuint = fold_convert_loc (loc, size_type_node, lhs);
	    srclen = fold_convert_loc (loc, size_type_node, dst);
	    srclen = fold_build2_loc (loc, MINUS_EXPR, size_type_node,
				      lhsuint, srclen);
	  }
	break;
      default:
	gcc_unreachable ();
      }

  if (didx == 0)
    {
      didx = new_stridx (dst);
      if (didx == 0)
	return;
    }
  if (olddsi != NULL)
    {
      oldlen = olddsi->nonzero_chars;
      dsi = unshare_strinfo (olddsi);
      dsi->nonzero_chars = srclen;
      dsi->full_string_p = (srclen != NULL_TREE);
      /* Break the chain, so adjust_related_strinfo on later pointers in
	 the chain won't adjust this one anymore.  */
      dsi->next = 0;
      dsi->stmt = NULL;
      dsi->endptr = NULL_TREE;
    }
  else
    {
      dsi = new_strinfo (dst, didx, srclen, srclen != NULL_TREE);
      set_strinfo (didx, dsi);
      find_equal_ptrs (dst, didx);
    }
  dsi->writable = true;
  dsi->dont_invalidate = true;

  if (dsi->nonzero_chars == NULL_TREE)
    {
      strinfo *chainsi;

      /* If string length of src is unknown, use delayed length
	 computation.  If string lenth of dst will be needed, it
	 can be computed by transforming this strcpy call into
	 stpcpy and subtracting dst from the return value.  */

      /* Look for earlier strings whose length could be determined if
	 this strcpy is turned into an stpcpy.  */

      if (dsi->prev != 0 && (chainsi = verify_related_strinfos (dsi)) != NULL)
	{
	  for (; chainsi && chainsi != dsi; chainsi = get_strinfo (chainsi->next))
	    {
	      /* When setting a stmt for delayed length computation
		 prevent all strinfos through dsi from being
		 invalidated.  */
	      chainsi = unshare_strinfo (chainsi);
	      chainsi->stmt = stmt;
	      chainsi->nonzero_chars = NULL_TREE;
	      chainsi->full_string_p = false;
	      chainsi->endptr = NULL_TREE;
	      chainsi->dont_invalidate = true;
	    }
	}
      dsi->stmt = stmt;

      /* Try to detect overlap before returning.  This catches cases
	 like strcpy (d, d + n) where n is non-constant whose range
	 is such that (n <= strlen (d) holds).

	 OLDDSI->NONZERO_chars may have been reset by this point with
	 oldlen holding it original value.  */
      if (olddsi && oldlen)
	{
	  /* Add 1 for the terminating NUL.  */
	  tree type = TREE_TYPE (oldlen);
	  oldlen = fold_build2 (PLUS_EXPR, type, oldlen,
				build_int_cst (type, 1));
	  check_bounds_or_overlap (as_a <gcall *>(stmt), olddsi->ptr, src,
				   oldlen, NULL_TREE);
	}

      return;
    }

  if (olddsi != NULL)
    {
      tree adj = NULL_TREE;
      if (oldlen == NULL_TREE)
	;
      else if (integer_zerop (oldlen))
	adj = srclen;
      else if (TREE_CODE (oldlen) == INTEGER_CST
	       || TREE_CODE (srclen) == INTEGER_CST)
	adj = fold_build2_loc (loc, MINUS_EXPR,
			       TREE_TYPE (srclen), srclen,
			       fold_convert_loc (loc, TREE_TYPE (srclen),
						 oldlen));
      if (adj != NULL_TREE)
	adjust_related_strinfos (loc, dsi, adj);
      else
	dsi->prev = 0;
    }
  /* strcpy src may not overlap dst, so src doesn't need to be
     invalidated either.  */
  if (si != NULL)
    si->dont_invalidate = true;

  fn = NULL_TREE;
  zsi = NULL;
  switch (bcode)
    {
    case BUILT_IN_STRCPY:
      fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
      if (lhs)
	ssa_ver_to_stridx[SSA_NAME_VERSION (lhs)] = didx;
      break;
    case BUILT_IN_STRCPY_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
      if (lhs)
	ssa_ver_to_stridx[SSA_NAME_VERSION (lhs)] = didx;
      break;
    case BUILT_IN_STPCPY:
      /* This would need adjustment of the lhs (subtract one),
	 or detection that the trailing '\0' doesn't need to be
	 written, if it will be immediately overwritten.
      fn = builtin_decl_explicit (BUILT_IN_MEMPCPY);  */
      if (lhs)
	{
	  dsi->endptr = lhs;
	  zsi = zero_length_string (lhs, dsi);
	}
      break;
    case BUILT_IN_STPCPY_CHK:
      /* This would need adjustment of the lhs (subtract one),
	 or detection that the trailing '\0' doesn't need to be
	 written, if it will be immediately overwritten.
      fn = builtin_decl_explicit (BUILT_IN_MEMPCPY_CHK);  */
      if (lhs)
	{
	  dsi->endptr = lhs;
	  zsi = zero_length_string (lhs, dsi);
	}
      break;
    default:
      gcc_unreachable ();
    }
  if (zsi != NULL)
    zsi->dont_invalidate = true;

  if (fn)
    {
      tree args = TYPE_ARG_TYPES (TREE_TYPE (fn));
      type = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));
    }
  else
    type = size_type_node;

  len = fold_convert_loc (loc, type, unshare_expr (srclen));
  len = fold_build2_loc (loc, PLUS_EXPR, type, len, build_int_cst (type, 1));

  /* Set the no-warning bit on the transformed statement?  */
  bool set_no_warning = false;

  if (const strinfo *chksi = olddsi ? olddsi : dsi)
    if (si
	&& !check_bounds_or_overlap (as_a <gcall *>(stmt), chksi->ptr, si->ptr,
				     NULL_TREE, len))
      {
	gimple_set_no_warning (stmt, true);
	set_no_warning = true;
      }

  if (fn == NULL_TREE)
    return;

  len = force_gimple_operand_gsi (gsi, len, true, NULL_TREE, true,
				  GSI_SAME_STMT);
  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
    {
      fprintf (dump_file, "Optimizing: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  if (gimple_call_num_args (stmt) == 2)
    success = update_gimple_call (gsi, fn, 3, dst, src, len);
  else
    success = update_gimple_call (gsi, fn, 4, dst, src, len,
				  gimple_call_arg (stmt, 2));
  if (success)
    {
      stmt = gsi_stmt (*gsi);
      update_stmt (stmt);
      if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	{
	  fprintf (dump_file, "into: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      /* Allow adjust_last_stmt to decrease this memcpy's size.  */
      laststmt.stmt = stmt;
      laststmt.len = srclen;
      laststmt.stridx = dsi->idx;
    }
  else if (dump_file && (dump_flags & TDF_DETAILS) != 0)
    fprintf (dump_file, "not possible.\n");

  if (set_no_warning)
    gimple_set_no_warning (stmt, true);
}

/* Check the size argument to the built-in forms of stpncpy and strncpy
   for out-of-bounds offsets or overlapping access, and to see if the
   size argument is derived from a call to strlen() on the source argument,
   and if so, issue an appropriate warning.  */

static void
handle_builtin_strncat (built_in_function bcode, gimple_stmt_iterator *gsi)
{
  /* Same as stxncpy().  */
  handle_builtin_stxncpy (bcode, gsi);
}

/* Return true if LEN depends on a call to strlen(SRC) in an interesting
   way.  LEN can either be an integer expression, or a pointer (to char).
   When it is the latter (such as in recursive calls to self) is is
   assumed to be the argument in some call to strlen() whose relationship
   to SRC is being ascertained.  */

bool
is_strlen_related_p (tree src, tree len)
{
  if (TREE_CODE (TREE_TYPE (len)) == POINTER_TYPE
      && operand_equal_p (src, len, 0))
    return true;

  if (TREE_CODE (len) != SSA_NAME)
    return false;

  gimple *def_stmt = SSA_NAME_DEF_STMT (len);
  if (!def_stmt)
    return false;

  if (is_gimple_call (def_stmt))
    {
      tree func = gimple_call_fndecl (def_stmt);
      if (!valid_builtin_call (def_stmt)
	  || DECL_FUNCTION_CODE (func) != BUILT_IN_STRLEN)
	return false;

      tree arg = gimple_call_arg (def_stmt, 0);
      return is_strlen_related_p (src, arg);
    }

  if (!is_gimple_assign (def_stmt))
    return false;

  tree_code code = gimple_assign_rhs_code (def_stmt);
  tree rhs1 = gimple_assign_rhs1 (def_stmt);
  tree rhstype = TREE_TYPE (rhs1);

  if ((TREE_CODE (rhstype) == POINTER_TYPE && code == POINTER_PLUS_EXPR)
      || (INTEGRAL_TYPE_P (rhstype)
	  && (code == BIT_AND_EXPR
	      || code == NOP_EXPR)))
    {
      /* Pointer plus (an integer), and truncation are considered among
	 the (potentially) related expressions to strlen.  */
      return is_strlen_related_p (src, rhs1);
    }

  if (tree rhs2 = gimple_assign_rhs2 (def_stmt))
    {
      /* Integer subtraction is considered strlen-related when both
	 arguments are integers and second one is strlen-related.  */
      rhstype = TREE_TYPE (rhs2);
      if (INTEGRAL_TYPE_P (rhstype) && code == MINUS_EXPR)
	return is_strlen_related_p (src, rhs2);
    }

  return false;
}

/* Called by handle_builtin_stxncpy and by gimple_fold_builtin_strncpy
   in gimple-fold.c.
   Check to see if the specified bound is a) equal to the size of
   the destination DST and if so, b) if it's immediately followed by
   DST[CNT - 1] = '\0'.  If a) holds and b) does not, warn.  Otherwise,
   do nothing.  Return true if diagnostic has been issued.

   The purpose is to diagnose calls to strncpy and stpncpy that do
   not nul-terminate the copy while allowing for the idiom where
   such a call is immediately followed by setting the last element
   to nul, as in:
     char a[32];
     strncpy (a, s, sizeof a);
     a[sizeof a - 1] = '\0';
*/

bool
maybe_diag_stxncpy_trunc (gimple_stmt_iterator gsi, tree src, tree cnt)
{
  gimple *stmt = gsi_stmt (gsi);
  if (gimple_no_warning_p (stmt))
    return false;

  wide_int cntrange[2];

  if (TREE_CODE (cnt) == INTEGER_CST)
    cntrange[0] = cntrange[1] = wi::to_wide (cnt);
  else if (TREE_CODE (cnt) == SSA_NAME)
    {
      enum value_range_type rng = get_range_info (cnt, cntrange, cntrange + 1);
      if (rng == VR_RANGE)
	;
      else if (rng == VR_ANTI_RANGE)
	{
	  wide_int maxobjsize = wi::to_wide (TYPE_MAX_VALUE (ptrdiff_type_node));

	  if (wi::ltu_p (cntrange[1], maxobjsize))
	    {
	      cntrange[0] = cntrange[1] + 1;
	      cntrange[1] = maxobjsize;
	    }
	  else
	    {
	      cntrange[1] = cntrange[0] - 1;
	      cntrange[0] = wi::zero (TYPE_PRECISION (TREE_TYPE (cnt)));
	    }
	}
      else
	return false;
    }
  else
    return false;

  /* Negative value is the constant string length.  If it's less than
     the lower bound there is no truncation.  Avoid calling get_stridx()
     when ssa_ver_to_stridx is empty.  That implies the caller isn't
     running under the control of this pass and ssa_ver_to_stridx hasn't
     been created yet.  */
  int sidx = ssa_ver_to_stridx.length () ? get_stridx (src) : 0;
  if (sidx < 0 && wi::gtu_p (cntrange[0], ~sidx))
    return false;

  tree dst = gimple_call_arg (stmt, 0);
  tree dstdecl = dst;
  if (TREE_CODE (dstdecl) == ADDR_EXPR)
    dstdecl = TREE_OPERAND (dstdecl, 0);

  tree ref = NULL_TREE;

  if (!sidx)
    {
      /* If the source is a non-string return early to avoid warning
	 for possible truncation (if the truncation is certain SIDX
	 is non-zero).  */
      tree srcdecl = gimple_call_arg (stmt, 1);
      if (TREE_CODE (srcdecl) == ADDR_EXPR)
	srcdecl = TREE_OPERAND (srcdecl, 0);
      if (get_attr_nonstring_decl (srcdecl, &ref))
	return false;
    }

  /* Likewise, if the destination refers to a an array/pointer declared
     nonstring return early.  */
  if (get_attr_nonstring_decl (dstdecl, &ref))
    return false;

  /* Look for dst[i] = '\0'; after the stxncpy() call and if found
     avoid the truncation warning.  */
  gsi_next_nondebug (&gsi);
  gimple *next_stmt = gsi_stmt (gsi);
  if (!next_stmt)
    {
      /* When there is no statement in the same basic block check
	 the immediate successor block.  */
      if (basic_block bb = gimple_bb (stmt))
	{
	  if (single_succ_p (bb))
	    {
	      /* For simplicity, ignore blocks with multiple outgoing
		 edges for now and only consider successor blocks along
		 normal edges.  */
	      edge e = EDGE_SUCC (bb, 0);
	      if (!(e->flags & EDGE_ABNORMAL))
		{
		  gsi = gsi_start_bb (e->dest);
		  next_stmt = gsi_stmt (gsi);
		  if (next_stmt && is_gimple_debug (next_stmt))
		    {
		      gsi_next_nondebug (&gsi);
		      next_stmt = gsi_stmt (gsi);
		    }
		}
	    }
	}
    }

  if (next_stmt && is_gimple_assign (next_stmt))
    {
      tree lhs = gimple_assign_lhs (next_stmt);
      tree_code code = TREE_CODE (lhs);
      if (code == ARRAY_REF || code == MEM_REF)
	lhs = TREE_OPERAND (lhs, 0);

      tree func = gimple_call_fndecl (stmt);
      if (DECL_FUNCTION_CODE (func) == BUILT_IN_STPNCPY)
	{
	  tree ret = gimple_call_lhs (stmt);
	  if (ret && operand_equal_p (ret, lhs, 0))
	    return false;
	}

      /* Determine the base address and offset of the reference,
	 ignoring the innermost array index.  */
      if (TREE_CODE (ref) == ARRAY_REF)
	ref = TREE_OPERAND (ref, 0);

      poly_int64 dstoff;
      tree dstbase = get_addr_base_and_unit_offset (ref, &dstoff);

      poly_int64 lhsoff;
      tree lhsbase = get_addr_base_and_unit_offset (lhs, &lhsoff);
      if (lhsbase
	  && dstbase
	  && known_eq (dstoff, lhsoff)
	  && operand_equal_p (dstbase, lhsbase, 0))
	return false;
    }

  int prec = TYPE_PRECISION (TREE_TYPE (cnt));
  wide_int lenrange[2];
  if (strinfo *sisrc = sidx > 0 ? get_strinfo (sidx) : NULL)
    {
      lenrange[0] = (sisrc->nonzero_chars
		     && TREE_CODE (sisrc->nonzero_chars) == INTEGER_CST
		     ? wi::to_wide (sisrc->nonzero_chars)
		     : wi::zero (prec));
      lenrange[1] = lenrange[0];
    }
  else if (sidx < 0)
    lenrange[0] = lenrange[1] = wi::shwi (~sidx, prec);
  else
    {
      tree range[2];
      get_range_strlen (src, range);
      if (range[0] != NULL_TREE
	  && TREE_CODE (range[0]) == INTEGER_CST
	  && range[1] != NULL_TREE
	  && TREE_CODE (range[1]) == INTEGER_CST)
	{
	  lenrange[0] = wi::to_wide (range[0], prec);
	  lenrange[1] = wi::to_wide (range[1], prec);
	}
      else
	{
	  lenrange[0] = wi::shwi (0, prec);
	  lenrange[1] = wi::shwi (-1, prec);
	}
    }

  location_t callloc = gimple_nonartificial_location (stmt);
  callloc = expansion_point_location_if_in_system_header (callloc);

  tree func = gimple_call_fndecl (stmt);

  if (lenrange[0] != 0 || !wi::neg_p (lenrange[1]))
    {
      /* If the longest source string is shorter than the lower bound
	 of the specified count the copy is definitely nul-terminated.  */
      if (wi::ltu_p (lenrange[1], cntrange[0]))
	return false;

      if (wi::neg_p (lenrange[1]))
	{
	  /* The length of one of the strings is unknown but at least
	     one has non-zero length and that length is stored in
	     LENRANGE[1].  Swap the bounds to force a "may be truncated"
	     warning below.  */
	  lenrange[1] = lenrange[0];
	  lenrange[0] = wi::shwi (0, prec);
	}

      gcall *call = as_a <gcall *> (stmt);

      if (lenrange[0] == cntrange[1] && cntrange[0] == cntrange[1])
	return warning_n (callloc, OPT_Wstringop_truncation,
			  cntrange[0].to_uhwi (),
			  "%G%qD output truncated before terminating "
			  "nul copying %E byte from a string of the "
			  "same length",
			  "%G%qD output truncated before terminating nul "
			  "copying %E bytes from a string of the same "
			  "length",
			  call, func, cnt);
      else if (wi::geu_p (lenrange[0], cntrange[1]))
	{
	  /* The shortest string is longer than the upper bound of
	     the count so the truncation is certain.  */
	  if (cntrange[0] == cntrange[1])
	    return warning_n (callloc, OPT_Wstringop_truncation,
			      cntrange[0].to_uhwi (),
			      "%G%qD output truncated copying %E byte "
			      "from a string of length %wu",
			      "%G%qD output truncated copying %E bytes "
			      "from a string of length %wu",
			      call, func, cnt, lenrange[0].to_uhwi ());

	  return warning_at (callloc, OPT_Wstringop_truncation,
			     "%G%qD output truncated copying between %wu "
			     "and %wu bytes from a string of length %wu",
			     call, func, cntrange[0].to_uhwi (),
			     cntrange[1].to_uhwi (), lenrange[0].to_uhwi ());
	}
      else if (wi::geu_p (lenrange[1], cntrange[1]))
	{
	  /* The longest string is longer than the upper bound of
	     the count so the truncation is possible.  */
	  if (cntrange[0] == cntrange[1])
	    return warning_n (callloc, OPT_Wstringop_truncation,
			      cntrange[0].to_uhwi (),
			      "%G%qD output may be truncated copying %E "
			      "byte from a string of length %wu",
			      "%G%qD output may be truncated copying %E "
			      "bytes from a string of length %wu",
			      call, func, cnt, lenrange[1].to_uhwi ());

	  return warning_at (callloc, OPT_Wstringop_truncation,
			     "%G%qD output may be truncated copying between %wu "
			     "and %wu bytes from a string of length %wu",
			     call, func, cntrange[0].to_uhwi (),
			     cntrange[1].to_uhwi (), lenrange[1].to_uhwi ());
	}

      if (cntrange[0] != cntrange[1]
	  && wi::leu_p (cntrange[0], lenrange[0])
	  && wi::leu_p (cntrange[1], lenrange[0] + 1))
	{
	  /* If the source (including the terminating nul) is longer than
	     the lower bound of the specified count but shorter than the
	     upper bound the copy may (but need not) be truncated.  */
	  return warning_at (callloc, OPT_Wstringop_truncation,
			     "%G%qD output may be truncated copying between "
			     "%wu and %wu bytes from a string of length %wu",
			     call, func, cntrange[0].to_uhwi (),
			     cntrange[1].to_uhwi (), lenrange[0].to_uhwi ());
	}
    }

  if (tree dstsize = compute_objsize (dst, 1))
    {
      /* The source length is uknown.  Try to determine the destination
	 size and see if it matches the specified bound.  If not, bail.
	 Otherwise go on to see if it should be diagnosed for possible
	 truncation.  */
      if (!dstsize)
	return false;

      if (wi::to_wide (dstsize) != cntrange[1])
	return false;

      if (cntrange[0] == cntrange[1])
	return warning_at (callloc, OPT_Wstringop_truncation,
			   "%G%qD specified bound %E equals destination size",
			   as_a <gcall *> (stmt), func, cnt);
    }

  return false;
}

/* Check the arguments to the built-in forms of stpncpy and strncpy for
   out-of-bounds offsets or overlapping access, and to see if the size
   is derived from calling strlen() on the source argument, and if so,
   issue the appropriate warning.  */

static void
handle_builtin_stxncpy (built_in_function, gimple_stmt_iterator *gsi)
{
  if (!strlen_to_stridx)
    return;

  gimple *stmt = gsi_stmt (*gsi);

  tree dst = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);
  tree dstsize = NULL_TREE, srcsize = NULL_TREE;

  int didx = get_stridx (dst);
  if (strinfo *sidst = didx > 0 ? get_strinfo (didx) : NULL)
    {
      /* Compute the size of the destination string including the NUL.  */
      if (sidst->nonzero_chars)
	{
	  tree type = TREE_TYPE (sidst->nonzero_chars);
	  dstsize = fold_build2 (PLUS_EXPR, type, sidst->nonzero_chars,
				 build_int_cst (type, 1));
	}
      dst = sidst->ptr;
    }

  int sidx = get_stridx (src);
  strinfo *sisrc = sidx > 0 ? get_strinfo (sidx) : NULL;
  if (sisrc)
    {
      /* strncat() and strncpy() can modify the source string by writing
	 over the terminating nul so SISRC->DONT_INVALIDATE must be left
	 clear.  */

      /* Compute the size of the source string including the NUL.  */
      if (sisrc->nonzero_chars)
	{
	  tree type = TREE_TYPE (sisrc->nonzero_chars);
	  srcsize = fold_build2 (PLUS_EXPR, type, sisrc->nonzero_chars,
				 build_int_cst (type, 1));
	}

	src = sisrc->ptr;
    }
  else
    srcsize = NULL_TREE;

  if (!check_bounds_or_overlap (as_a <gcall *>(stmt), dst, src,
				dstsize, srcsize))
    {
      gimple_set_no_warning (stmt, true);
      return;
    }

  /* If the length argument was computed from strlen(S) for some string
     S retrieve the strinfo index for the string (PSS->FIRST) alonng with
     the location of the strlen() call (PSS->SECOND).  */
  stridx_strlenloc *pss = strlen_to_stridx->get (len);
  if (!pss || pss->first <= 0)
    {
      if (maybe_diag_stxncpy_trunc (*gsi, src, len))
	gimple_set_no_warning (stmt, true);

      return;
    }

  /* Retrieve the strinfo data for the string S that LEN was computed
     from as some function F of strlen (S) (i.e., LEN need not be equal
     to strlen(S)).  */
  strinfo *silen = get_strinfo (pss->first);

  location_t callloc = gimple_nonartificial_location (stmt);
  callloc = expansion_point_location_if_in_system_header (callloc);

  tree func = gimple_call_fndecl (stmt);

  bool warned = false;

  /* When -Wstringop-truncation is set, try to determine truncation
     before diagnosing possible overflow.  Truncation is implied by
     the LEN argument being equal to strlen(SRC), regardless of
     whether its value is known.  Otherwise, issue the more generic
     -Wstringop-overflow which triggers for LEN arguments that in
     any meaningful way depend on strlen(SRC).  */
  if (sisrc == silen
      && is_strlen_related_p (src, len)
      && warning_at (callloc, OPT_Wstringop_truncation,
		     "%G%qD output truncated before terminating nul "
		     "copying as many bytes from a string as its length",
		     as_a <gcall *>(stmt), func))
    warned = true;
  else if (silen && is_strlen_related_p (src, silen->ptr))
    warned = warning_at (callloc, OPT_Wstringop_overflow_,
			 "%G%qD specified bound depends on the length "
			 "of the source argument",
			 as_a <gcall *>(stmt), func);
  if (warned)
    {
      location_t strlenloc = pss->second;
      if (strlenloc != UNKNOWN_LOCATION && strlenloc != callloc)
	inform (strlenloc, "length computed here");
    }
}

/* Handle a memcpy-like ({mem{,p}cpy,__mem{,p}cpy_chk}) call.
   If strlen of the second argument is known and length of the third argument
   is that plus one, strlen of the first argument is the same after this
   call.  */

static void
handle_builtin_memcpy (enum built_in_function bcode, gimple_stmt_iterator *gsi)
{
  int idx, didx;
  tree src, dst, len, lhs, oldlen, newlen;
  gimple *stmt = gsi_stmt (*gsi);
  strinfo *si, *dsi, *olddsi;

  len = gimple_call_arg (stmt, 2);
  src = gimple_call_arg (stmt, 1);
  dst = gimple_call_arg (stmt, 0);
  idx = get_stridx (src);
  if (idx == 0)
    return;

  didx = get_stridx (dst);
  olddsi = NULL;
  if (didx > 0)
    olddsi = get_strinfo (didx);
  else if (didx < 0)
    return;

  if (olddsi != NULL
      && tree_fits_uhwi_p (len)
      && !integer_zerop (len))
    adjust_last_stmt (olddsi, stmt, false);

  bool full_string_p;
  if (idx > 0)
    {
      gimple *def_stmt;

      /* Handle memcpy (x, y, l) where l's relationship with strlen (y)
	 is known.  */
      si = get_strinfo (idx);
      if (si == NULL || si->nonzero_chars == NULL_TREE)
	return;
      if (TREE_CODE (len) == INTEGER_CST
	  && TREE_CODE (si->nonzero_chars) == INTEGER_CST)
	{
	  if (tree_int_cst_le (len, si->nonzero_chars))
	    {
	      /* Copying LEN nonzero characters, where LEN is constant.  */
	      newlen = len;
	      full_string_p = false;
	    }
	  else
	    {
	      /* Copying the whole of the analyzed part of SI.  */
	      newlen = si->nonzero_chars;
	      full_string_p = si->full_string_p;
	    }
	}
      else
	{
	  if (!si->full_string_p)
	    return;
	  if (TREE_CODE (len) != SSA_NAME)
	    return;
	  def_stmt = SSA_NAME_DEF_STMT (len);
	  if (!is_gimple_assign (def_stmt)
	      || gimple_assign_rhs_code (def_stmt) != PLUS_EXPR
	      || gimple_assign_rhs1 (def_stmt) != si->nonzero_chars
	      || !integer_onep (gimple_assign_rhs2 (def_stmt)))
	    return;
	  /* Copying variable-length string SI (and no more).  */
	  newlen = si->nonzero_chars;
	  full_string_p = true;
	}
    }
  else
    {
      si = NULL;
      /* Handle memcpy (x, "abcd", 5) or
	 memcpy (x, "abc\0uvw", 7).  */
      if (!tree_fits_uhwi_p (len))
	return;

      unsigned HOST_WIDE_INT clen = tree_to_uhwi (len);
      unsigned HOST_WIDE_INT nonzero_chars = ~idx;
      newlen = build_int_cst (size_type_node, MIN (nonzero_chars, clen));
      full_string_p = clen > nonzero_chars;
    }

  if (olddsi != NULL && TREE_CODE (len) == SSA_NAME)
    adjust_last_stmt (olddsi, stmt, false);

  if (didx == 0)
    {
      didx = new_stridx (dst);
      if (didx == 0)
	return;
    }
  oldlen = NULL_TREE;
  if (olddsi != NULL)
    {
      dsi = unshare_strinfo (olddsi);
      oldlen = olddsi->nonzero_chars;
      dsi->nonzero_chars = newlen;
      dsi->full_string_p = full_string_p;
      /* Break the chain, so adjust_related_strinfo on later pointers in
	 the chain won't adjust this one anymore.  */
      dsi->next = 0;
      dsi->stmt = NULL;
      dsi->endptr = NULL_TREE;
    }
  else
    {
      dsi = new_strinfo (dst, didx, newlen, full_string_p);
      set_strinfo (didx, dsi);
      find_equal_ptrs (dst, didx);
    }
  dsi->writable = true;
  dsi->dont_invalidate = true;
  if (olddsi != NULL)
    {
      tree adj = NULL_TREE;
      location_t loc = gimple_location (stmt);
      if (oldlen == NULL_TREE)
	;
      else if (integer_zerop (oldlen))
	adj = newlen;
      else if (TREE_CODE (oldlen) == INTEGER_CST
	       || TREE_CODE (newlen) == INTEGER_CST)
	adj = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (newlen), newlen,
			       fold_convert_loc (loc, TREE_TYPE (newlen),
						 oldlen));
      if (adj != NULL_TREE)
	adjust_related_strinfos (loc, dsi, adj);
      else
	dsi->prev = 0;
    }
  /* memcpy src may not overlap dst, so src doesn't need to be
     invalidated either.  */
  if (si != NULL)
    si->dont_invalidate = true;

  if (full_string_p)
    {
      lhs = gimple_call_lhs (stmt);
      switch (bcode)
	{
	case BUILT_IN_MEMCPY:
	case BUILT_IN_MEMCPY_CHK:
	  /* Allow adjust_last_stmt to decrease this memcpy's size.  */
	  laststmt.stmt = stmt;
	  laststmt.len = dsi->nonzero_chars;
	  laststmt.stridx = dsi->idx;
	  if (lhs)
	    ssa_ver_to_stridx[SSA_NAME_VERSION (lhs)] = didx;
	  break;
	case BUILT_IN_MEMPCPY:
	case BUILT_IN_MEMPCPY_CHK:
	  break;
	default:
	  gcc_unreachable ();
	}
    }
}

/* Handle a strcat-like ({strcat,__strcat_chk}) call.
   If strlen of the second argument is known, strlen of the first argument
   is increased by the length of the second argument.  Furthermore, attempt
   to convert it to memcpy/strcpy if the length of the first argument
   is known.  */

static void
handle_builtin_strcat (enum built_in_function bcode, gimple_stmt_iterator *gsi)
{
  int idx, didx;
  tree srclen, args, type, fn, objsz, endptr;
  bool success;
  gimple *stmt = gsi_stmt (*gsi);
  strinfo *si, *dsi;
  location_t loc = gimple_location (stmt);

  tree src = gimple_call_arg (stmt, 1);
  tree dst = gimple_call_arg (stmt, 0);

  /* Bail if the source is the same as destination.  It will be diagnosed
     elsewhere.  */
  if (operand_equal_p (src, dst, 0))
    return;

  tree lhs = gimple_call_lhs (stmt);

  didx = get_stridx (dst);
  if (didx < 0)
    return;

  dsi = NULL;
  if (didx > 0)
    dsi = get_strinfo (didx);

  srclen = NULL_TREE;
  si = NULL;
  idx = get_stridx (src);
  if (idx < 0)
    srclen = build_int_cst (size_type_node, ~idx);
  else if (idx > 0)
    {
      si = get_strinfo (idx);
      if (si != NULL)
	srclen = get_string_length (si);
    }

  /* Set the no-warning bit on the transformed statement?  */
  bool set_no_warning = false;

  if (dsi == NULL || get_string_length (dsi) == NULL_TREE)
    {
      {
	  /* The concatenation always involves copying at least one byte
	     (the terminating nul), even if the source string is empty.
	     If the source is unknown assume it's one character long and
	     used that as both sizes.  */
	tree slen = srclen;
	if (slen)
	  {
	    tree type = TREE_TYPE (slen);
	    slen = fold_build2 (PLUS_EXPR, type, slen, build_int_cst (type, 1));
	  }

	tree sptr = si && si->ptr ? si->ptr : src;

	if (!check_bounds_or_overlap (as_a <gcall *>(stmt), dst, sptr,
				      NULL_TREE, slen))
	  {
	    gimple_set_no_warning (stmt, true);
	    set_no_warning = true;
	  }
      }

      /* strcat (p, q) can be transformed into
	 tmp = p + strlen (p); endptr = stpcpy (tmp, q);
	 with length endptr - p if we need to compute the length
	 later on.  Don't do this transformation if we don't need
	 it.  */
      if (builtin_decl_implicit_p (BUILT_IN_STPCPY) && lhs == NULL_TREE)
	{
	  if (didx == 0)
	    {
	      didx = new_stridx (dst);
	      if (didx == 0)
		return;
	    }
	  if (dsi == NULL)
	    {
	      dsi = new_strinfo (dst, didx, NULL_TREE, false);
	      set_strinfo (didx, dsi);
	      find_equal_ptrs (dst, didx);
	    }
	  else
	    {
	      dsi = unshare_strinfo (dsi);
	      dsi->nonzero_chars = NULL_TREE;
	      dsi->full_string_p = false;
	      dsi->next = 0;
	      dsi->endptr = NULL_TREE;
	    }
	  dsi->writable = true;
	  dsi->stmt = stmt;
	  dsi->dont_invalidate = true;
	}
      return;
    }

  tree dstlen = dsi->nonzero_chars;
  endptr = dsi->endptr;

  dsi = unshare_strinfo (dsi);
  dsi->endptr = NULL_TREE;
  dsi->stmt = NULL;
  dsi->writable = true;

  if (srclen != NULL_TREE)
    {
      dsi->nonzero_chars = fold_build2_loc (loc, PLUS_EXPR,
					    TREE_TYPE (dsi->nonzero_chars),
					    dsi->nonzero_chars, srclen);
      gcc_assert (dsi->full_string_p);
      adjust_related_strinfos (loc, dsi, srclen);
      dsi->dont_invalidate = true;
    }
  else
    {
      dsi->nonzero_chars = NULL;
      dsi->full_string_p = false;
      if (lhs == NULL_TREE && builtin_decl_implicit_p (BUILT_IN_STPCPY))
	dsi->dont_invalidate = true;
    }

  if (si != NULL)
    /* strcat src may not overlap dst, so src doesn't need to be
       invalidated either.  */
    si->dont_invalidate = true;

  /* For now.  Could remove the lhs from the call and add
     lhs = dst; afterwards.  */
  if (lhs)
    return;

  fn = NULL_TREE;
  objsz = NULL_TREE;
  switch (bcode)
    {
    case BUILT_IN_STRCAT:
      if (srclen != NULL_TREE)
	fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
      else
	fn = builtin_decl_implicit (BUILT_IN_STRCPY);
      break;
    case BUILT_IN_STRCAT_CHK:
      if (srclen != NULL_TREE)
	fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
      else
	fn = builtin_decl_explicit (BUILT_IN_STRCPY_CHK);
      objsz = gimple_call_arg (stmt, 2);
      break;
    default:
      gcc_unreachable ();
    }

  if (fn == NULL_TREE)
    return;

  if (dsi && dstlen)
    {
      tree type = TREE_TYPE (dstlen);

      /* Compute the size of the source sequence, including the nul.  */
      tree srcsize = srclen ? srclen : size_zero_node;
      srcsize = fold_build2 (PLUS_EXPR, type, srcsize, build_int_cst (type, 1));

      tree sptr = si && si->ptr ? si->ptr : src;

      if (!check_bounds_or_overlap (as_a <gcall *>(stmt), dst, sptr,
				    dstlen, srcsize))
	{
	  gimple_set_no_warning (stmt, true);
	  set_no_warning = true;
	}
    }

  tree len = NULL_TREE;
  if (srclen != NULL_TREE)
    {
      args = TYPE_ARG_TYPES (TREE_TYPE (fn));
      type = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));

      len = fold_convert_loc (loc, type, unshare_expr (srclen));
      len = fold_build2_loc (loc, PLUS_EXPR, type, len,
			     build_int_cst (type, 1));
      len = force_gimple_operand_gsi (gsi, len, true, NULL_TREE, true,
				      GSI_SAME_STMT);
    }
  if (endptr)
    dst = fold_convert_loc (loc, TREE_TYPE (dst), unshare_expr (endptr));
  else
    dst = fold_build2_loc (loc, POINTER_PLUS_EXPR,
			   TREE_TYPE (dst), unshare_expr (dst),
			   fold_convert_loc (loc, sizetype,
					     unshare_expr (dstlen)));
  dst = force_gimple_operand_gsi (gsi, dst, true, NULL_TREE, true,
				  GSI_SAME_STMT);
  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
    {
      fprintf (dump_file, "Optimizing: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  if (srclen != NULL_TREE)
    success = update_gimple_call (gsi, fn, 3 + (objsz != NULL_TREE),
				  dst, src, len, objsz);
  else
    success = update_gimple_call (gsi, fn, 2 + (objsz != NULL_TREE),
				  dst, src, objsz);
  if (success)
    {
      stmt = gsi_stmt (*gsi);
      update_stmt (stmt);
      if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	{
	  fprintf (dump_file, "into: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      /* If srclen == NULL, note that current string length can be
	 computed by transforming this strcpy into stpcpy.  */
      if (srclen == NULL_TREE && dsi->dont_invalidate)
	dsi->stmt = stmt;
      adjust_last_stmt (dsi, stmt, true);
      if (srclen != NULL_TREE)
	{
	  laststmt.stmt = stmt;
	  laststmt.len = srclen;
	  laststmt.stridx = dsi->idx;
	}
    }
  else if (dump_file && (dump_flags & TDF_DETAILS) != 0)
    fprintf (dump_file, "not possible.\n");

  if (set_no_warning)
    gimple_set_no_warning (stmt, true);
}

/* Handle a call to malloc or calloc.  */

static void
handle_builtin_malloc (enum built_in_function bcode, gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;

  gcc_assert (get_stridx (lhs) == 0);
  int idx = new_stridx (lhs);
  tree length = NULL_TREE;
  if (bcode == BUILT_IN_CALLOC)
    length = build_int_cst (size_type_node, 0);
  strinfo *si = new_strinfo (lhs, idx, length, length != NULL_TREE);
  if (bcode == BUILT_IN_CALLOC)
    si->endptr = lhs;
  set_strinfo (idx, si);
  si->writable = true;
  si->stmt = stmt;
  si->dont_invalidate = true;
}

/* Handle a call to memset.
   After a call to calloc, memset(,0,) is unnecessary.
   memset(malloc(n),0,n) is calloc(n,1). 
   return true when the call is transfomred, false otherwise.  */

static bool
handle_builtin_memset (gimple_stmt_iterator *gsi)
{
  gimple *stmt2 = gsi_stmt (*gsi);
  if (!integer_zerop (gimple_call_arg (stmt2, 1)))
    return false;
  tree ptr = gimple_call_arg (stmt2, 0);
  int idx1 = get_stridx (ptr);
  if (idx1 <= 0)
    return false;
  strinfo *si1 = get_strinfo (idx1);
  if (!si1)
    return false;
  gimple *stmt1 = si1->stmt;
  if (!stmt1 || !is_gimple_call (stmt1))
    return false;
  tree callee1 = gimple_call_fndecl (stmt1);
  if (!valid_builtin_call (stmt1))
    return false;
  enum built_in_function code1 = DECL_FUNCTION_CODE (callee1);
  tree size = gimple_call_arg (stmt2, 2);
  if (code1 == BUILT_IN_CALLOC)
    /* Not touching stmt1 */ ;
  else if (code1 == BUILT_IN_MALLOC
	   && operand_equal_p (gimple_call_arg (stmt1, 0), size, 0))
    {
      gimple_stmt_iterator gsi1 = gsi_for_stmt (stmt1);
      update_gimple_call (&gsi1, builtin_decl_implicit (BUILT_IN_CALLOC), 2,
			  size, build_one_cst (size_type_node));
      si1->nonzero_chars = build_int_cst (size_type_node, 0);
      si1->full_string_p = true;
      si1->stmt = gsi_stmt (gsi1);
    }
  else
    return false;
  tree lhs = gimple_call_lhs (stmt2);
  unlink_stmt_vdef (stmt2);
  if (lhs)
    {
      gimple *assign = gimple_build_assign (lhs, ptr);
      gsi_replace (gsi, assign, false);
    }
  else
    {
      gsi_remove (gsi, true);
      release_defs (stmt2);
    }

  return true;
}

/* Handle a call to memcmp.  We try to handle small comparisons by
   converting them to load and compare, and replacing the call to memcmp
   with a __builtin_memcmp_eq call where possible. 
   return true when call is transformed, return false otherwise.  */

static bool
handle_builtin_memcmp (gimple_stmt_iterator *gsi)
{
  gcall *stmt2 = as_a <gcall *> (gsi_stmt (*gsi));
  tree res = gimple_call_lhs (stmt2);
  tree arg1 = gimple_call_arg (stmt2, 0);
  tree arg2 = gimple_call_arg (stmt2, 1);
  tree len = gimple_call_arg (stmt2, 2);
  unsigned HOST_WIDE_INT leni;
  use_operand_p use_p;
  imm_use_iterator iter;

  if (!res)
    return false;

  FOR_EACH_IMM_USE_FAST (use_p, iter, res)
    {
      gimple *ustmt = USE_STMT (use_p);

      if (is_gimple_debug (ustmt))
	continue;
      if (gimple_code (ustmt) == GIMPLE_ASSIGN)
	{
	  gassign *asgn = as_a <gassign *> (ustmt);
	  tree_code code = gimple_assign_rhs_code (asgn);
	  if ((code != EQ_EXPR && code != NE_EXPR)
	      || !integer_zerop (gimple_assign_rhs2 (asgn)))
	    return false;
	}
      else if (gimple_code (ustmt) == GIMPLE_COND)
	{
	  tree_code code = gimple_cond_code (ustmt);
	  if ((code != EQ_EXPR && code != NE_EXPR)
	      || !integer_zerop (gimple_cond_rhs (ustmt)))
	    return false;
	}
      else
	return false;
    }

  if (tree_fits_uhwi_p (len)
      && (leni = tree_to_uhwi (len)) <= GET_MODE_SIZE (word_mode)
      && pow2p_hwi (leni))
    {
      leni *= CHAR_TYPE_SIZE;
      unsigned align1 = get_pointer_alignment (arg1);
      unsigned align2 = get_pointer_alignment (arg2);
      unsigned align = MIN (align1, align2);
      scalar_int_mode mode;
      if (int_mode_for_size (leni, 1).exists (&mode)
	  && (align >= leni || !targetm.slow_unaligned_access (mode, align)))
	{
	  location_t loc = gimple_location (stmt2);
	  tree type, off;
	  type = build_nonstandard_integer_type (leni, 1);
	  gcc_assert (known_eq (GET_MODE_BITSIZE (TYPE_MODE (type)), leni));
	  tree ptrtype = build_pointer_type_for_mode (char_type_node,
						      ptr_mode, true);
	  off = build_int_cst (ptrtype, 0);
	  arg1 = build2_loc (loc, MEM_REF, type, arg1, off);
	  arg2 = build2_loc (loc, MEM_REF, type, arg2, off);
	  tree tem1 = fold_const_aggregate_ref (arg1);
	  if (tem1)
	    arg1 = tem1;
	  tree tem2 = fold_const_aggregate_ref (arg2);
	  if (tem2)
	    arg2 = tem2;
	  res = fold_convert_loc (loc, TREE_TYPE (res),
				  fold_build2_loc (loc, NE_EXPR,
						   boolean_type_node,
						   arg1, arg2));
	  gimplify_and_update_call_from_tree (gsi, res);
	  return true;
	}
    }

  gimple_call_set_fndecl (stmt2, builtin_decl_explicit (BUILT_IN_MEMCMP_EQ));
  return true;
}

/* Given an index to the strinfo vector, compute the string length for the
   corresponding string. Return -1 when unknown.  */
 
static HOST_WIDE_INT 
compute_string_length (int idx)
{
  HOST_WIDE_INT string_leni = -1; 
  gcc_assert (idx != 0);

  if (idx < 0)
   return ~idx;

  strinfo *si = get_strinfo (idx);
  if (si)
    {
      tree const_string_len = get_string_length (si);
      if (const_string_len && tree_fits_shwi_p (const_string_len))
	string_leni = tree_to_shwi (const_string_len);
    }

  if (string_leni < 0)
    return -1;

  return string_leni;
}

/* Determine the minimum size of the object referenced by DEST expression which
   must have a pointer type. 
   Return the minimum size of the object if successful or NULL when the size 
   cannot be determined.  */
static tree
determine_min_objsize (tree dest)
{
  unsigned HOST_WIDE_INT size = 0;

  if (compute_builtin_object_size (dest, 2, &size))
    return build_int_cst (sizetype, size);

  /* Try to determine the size of the object through the RHS of the 
     assign statement.  */
  if (TREE_CODE (dest) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (dest);
      if (!is_gimple_assign (stmt))
	return NULL_TREE;

      if (!gimple_assign_single_p (stmt)
	  && !gimple_assign_unary_nop_p (stmt))
	return NULL_TREE;

      dest = gimple_assign_rhs1 (stmt);
      return determine_min_objsize (dest);
    }

  /* Try to determine the size of the object from its type.  */
  if (TREE_CODE (dest) != ADDR_EXPR)
    return NULL_TREE;

  tree type = TREE_TYPE (dest);
  if (TREE_CODE (type) == POINTER_TYPE)
    type = TREE_TYPE (type);

  type = TYPE_MAIN_VARIANT (type);

  /* We cannot determine the size of the array if it's a flexible array, 
     which is declared at the end of a structure.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      && !array_at_struct_end_p (dest))
    {
      tree size_t = TYPE_SIZE_UNIT (type);
      if (size_t && TREE_CODE (size_t) == INTEGER_CST 
	  && !integer_zerop (size_t))
        return size_t;
    }

  return NULL_TREE;
}

/* Handle a call to strcmp or strncmp. When the result is ONLY used to do 
   equality test against zero:

   A. When the lengths of both arguments are constant and it's a strcmp:
      * if the lengths are NOT equal, we can safely fold the call
        to a non-zero value.
      * otherwise, do nothing now.
  
   B. When the length of one argument is constant, try to replace the call with
   a __builtin_str(n)cmp_eq call where possible, i.e:

   strncmp (s, STR, C) (!)= 0 in which, s is a pointer to a string, STR is a 
   string with constant length , C is a constant.
     if (C <= strlen(STR) && sizeof_array(s) > C)
       {
         replace this call with
         strncmp_eq (s, STR, C) (!)= 0
       }
     if (C > strlen(STR)
       {
         it can be safely treated as a call to strcmp (s, STR) (!)= 0
         can handled by the following strcmp.
       }

   strcmp (s, STR) (!)= 0 in which, s is a pointer to a string, STR is a 
   string with constant length.
     if  (sizeof_array(s) > strlen(STR))
       {
         replace this call with
         strcmp_eq (s, STR, strlen(STR)+1) (!)= 0
       }

   Return true when the call is transformed, return false otherwise. 
 */ 

static bool
handle_builtin_string_cmp (gimple_stmt_iterator *gsi)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree res = gimple_call_lhs (stmt);
  use_operand_p use_p;
  imm_use_iterator iter;
  tree arg1 = gimple_call_arg (stmt, 0);
  tree arg2 = gimple_call_arg (stmt, 1);
  int idx1 = get_stridx (arg1);
  int idx2 = get_stridx (arg2);
  HOST_WIDE_INT length = -1;
  bool is_ncmp = false;

  if (!res)
    return false;

  /* When both arguments are unknown, do nothing.  */
  if (idx1 == 0 && idx2 == 0)
    return false;

  /* Handle strncmp function.  */
  if (gimple_call_num_args (stmt) == 3)
    {
      tree len = gimple_call_arg (stmt, 2);
      if (tree_fits_shwi_p (len))
        length = tree_to_shwi (len);

      is_ncmp = true;
    }

  /* For strncmp, if the length argument is NOT known, do nothing.  */
  if (is_ncmp && length < 0)
    return false;

  /* When the result is ONLY used to do equality test against zero.  */
  FOR_EACH_IMM_USE_FAST (use_p, iter, res) 
    {    
      gimple *use_stmt = USE_STMT (use_p);

      if (is_gimple_debug (use_stmt))
        continue;
      if (gimple_code (use_stmt) == GIMPLE_ASSIGN)
	{    
	  tree_code code = gimple_assign_rhs_code (use_stmt);
	  if (code == COND_EXPR) 
	    {
	      tree cond_expr = gimple_assign_rhs1 (use_stmt);
	      if ((TREE_CODE (cond_expr) != EQ_EXPR 
		   && (TREE_CODE (cond_expr) != NE_EXPR))
		  || !integer_zerop (TREE_OPERAND (cond_expr, 1)))
		return false;
	    }
	  else if (code == EQ_EXPR || code == NE_EXPR)
	    {
	      if (!integer_zerop (gimple_assign_rhs2 (use_stmt)))
		return false;
            }
	  else 
	    return false;
	}
      else if (gimple_code (use_stmt) == GIMPLE_COND)
	{
	  tree_code code = gimple_cond_code (use_stmt);
	  if ((code != EQ_EXPR && code != NE_EXPR)
	      || !integer_zerop (gimple_cond_rhs (use_stmt)))
	    return false;
	}
      else
        return false;
    }
  
  /* When the lengths of both arguments are known, and they are unequal, we can 
     safely fold the call to a non-zero value for strcmp;
     othewise, do nothing now.  */
  if (idx1 != 0 && idx2 != 0)
    {
      HOST_WIDE_INT const_string_leni1 = compute_string_length (idx1);
      HOST_WIDE_INT const_string_leni2 = compute_string_length (idx2);

      if (!is_ncmp 
	  && const_string_leni1 != -1
	  && const_string_leni2 != -1
	  && const_string_leni1 != const_string_leni2) 
	{
	  replace_call_with_value (gsi, integer_one_node); 
	  return true;
	}
      return false;
    }

  /* When the length of one argument is constant.  */
  tree var_string = NULL_TREE;
  HOST_WIDE_INT const_string_leni = -1;
  
  if (idx1)
    {
      const_string_leni = compute_string_length (idx1);
      var_string = arg2;
    } 
  else 
    {
      gcc_checking_assert (idx2);
      const_string_leni = compute_string_length (idx2);
      var_string = arg1;
    } 

  if (const_string_leni < 0) 
    return false;
 
  unsigned HOST_WIDE_INT var_sizei = 0;
  /* try to determine the minimum size of the object pointed by var_string.  */
  tree size = determine_min_objsize (var_string);

  if (!size)
    return false;
 
  if (tree_fits_uhwi_p (size))
    var_sizei = tree_to_uhwi (size);

  if (var_sizei == 0)
    return false;

  /* For strncmp, if length > const_string_leni , this call can be safely 
     transformed to a strcmp.  */
  if (is_ncmp && length > const_string_leni)
    is_ncmp = false;

  unsigned HOST_WIDE_INT final_length 
    = is_ncmp ? length : const_string_leni + 1;

  /* Replace strcmp or strncmp with the corresponding str(n)cmp_eq.  */
  if (var_sizei > final_length) 
    {
      tree fn 
	= (is_ncmp 
	   ? builtin_decl_implicit (BUILT_IN_STRNCMP_EQ) 
	   : builtin_decl_implicit (BUILT_IN_STRCMP_EQ));
      if (!fn)
	return false;
      tree const_string_len = build_int_cst (size_type_node, final_length); 
      update_gimple_call (gsi, fn, 3, arg1, arg2, const_string_len);
    }
  else 
    return false;

  return true;
}

/* Handle a POINTER_PLUS_EXPR statement.
   For p = "abcd" + 2; compute associated length, or if
   p = q + off is pointing to a '\0' character of a string, call
   zero_length_string on it.  */

static void
handle_pointer_plus (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_assign_lhs (stmt), off;
  int idx = get_stridx (gimple_assign_rhs1 (stmt));
  strinfo *si, *zsi;

  if (idx == 0)
    return;

  if (idx < 0)
    {
      tree off = gimple_assign_rhs2 (stmt);
      if (tree_fits_uhwi_p (off)
	  && tree_to_uhwi (off) <= (unsigned HOST_WIDE_INT) ~idx)
	ssa_ver_to_stridx[SSA_NAME_VERSION (lhs)]
	    = ~(~idx - (int) tree_to_uhwi (off));
      return;
    }

  si = get_strinfo (idx);
  if (si == NULL || si->nonzero_chars == NULL_TREE)
    return;

  off = gimple_assign_rhs2 (stmt);
  zsi = NULL;
  if (si->full_string_p && operand_equal_p (si->nonzero_chars, off, 0))
    zsi = zero_length_string (lhs, si);
  else if (TREE_CODE (off) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (off);
      if (gimple_assign_single_p (def_stmt)
	  && si->full_string_p
	  && operand_equal_p (si->nonzero_chars,
			      gimple_assign_rhs1 (def_stmt), 0))
	zsi = zero_length_string (lhs, si);
    }
  if (zsi != NULL
      && si->endptr != NULL_TREE
      && si->endptr != lhs
      && TREE_CODE (si->endptr) == SSA_NAME)
    {
      enum tree_code rhs_code
	= useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (si->endptr))
	  ? SSA_NAME : NOP_EXPR;
      gimple_assign_set_rhs_with_ops (gsi, rhs_code, si->endptr);
      gcc_assert (gsi_stmt (*gsi) == stmt);
      update_stmt (stmt);
    }
}

/* If RHS, either directly or indirectly, refers to a string of constant
   length, return it.  Otherwise return a negative value.  */

static HOST_WIDE_INT
get_string_cst_length (tree rhs)
{
  if (TREE_CODE (rhs) == MEM_REF
      && integer_zerop (TREE_OPERAND (rhs, 1)))
    {
      rhs = TREE_OPERAND (rhs, 0);
      if (TREE_CODE (rhs) == ADDR_EXPR)
	{
	  tree rhs_addr = rhs;

	  rhs = TREE_OPERAND (rhs, 0);
	  if (TREE_CODE (rhs) != STRING_CST)
	    {
	      int idx = get_stridx (rhs_addr);
	      if (idx > 0)
		{
		  strinfo *si = get_strinfo (idx);
		  if (si
		      && si->full_string_p
		      && tree_fits_shwi_p (si->nonzero_chars))
		    return tree_to_shwi (si->nonzero_chars);
		}
	    }
	}
    }

  if (TREE_CODE (rhs) == VAR_DECL
      && TREE_READONLY (rhs))
    rhs = DECL_INITIAL (rhs);

  if (rhs && TREE_CODE (rhs) == STRING_CST)
    return strlen (TREE_STRING_POINTER (rhs));

  return -1;
}

/* Handle a single character store.  */

static bool
handle_char_store (gimple_stmt_iterator *gsi)
{
  int idx = -1;
  strinfo *si = NULL;
  gimple *stmt = gsi_stmt (*gsi);
  tree ssaname = NULL_TREE, lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);
  unsigned HOST_WIDE_INT offset = 0;

  if (TREE_CODE (lhs) == MEM_REF
      && TREE_CODE (TREE_OPERAND (lhs, 0)) == SSA_NAME)
    {
      tree mem_offset = TREE_OPERAND (lhs, 1);
      if (tree_fits_uhwi_p (mem_offset))
	{
	  /* Get the strinfo for the base, and use it if it starts with at
	     least OFFSET nonzero characters.  This is trivially true if
	     OFFSET is zero.  */
	  offset = tree_to_uhwi (mem_offset);
	  idx = get_stridx (TREE_OPERAND (lhs, 0));
	  if (idx > 0)
	    si = get_strinfo (idx);
	  if (offset == 0)
	    ssaname = TREE_OPERAND (lhs, 0);
	  else if (si == NULL || compare_nonzero_chars (si, offset) < 0)
	    return true;
	}
    }
  else
    {
      idx = get_addr_stridx (lhs, NULL_TREE, &offset);
      if (idx > 0)
	si = get_strinfo (idx);
    }

  bool storing_zero_p = initializer_zerop (rhs);
  bool storing_nonzero_p = !storing_zero_p && tree_expr_nonzero_p (rhs);
  /* Set to the length of the string being assigned if known.  */
  HOST_WIDE_INT rhslen;

  if (si != NULL)
    {
      int cmp = compare_nonzero_chars (si, offset);
      gcc_assert (offset == 0 || cmp >= 0);
      if (storing_zero_p && cmp == 0 && si->full_string_p)
	{
	  /* When overwriting a '\0' with a '\0', the store can be removed
	     if we know it has been stored in the current function.  */
	  if (!stmt_could_throw_p (stmt) && si->writable)
	    {
	      unlink_stmt_vdef (stmt);
	      release_defs (stmt);
	      gsi_remove (gsi, true);
	      return false;
	    }
	  else
	    {
	      si->writable = true;
	      gsi_next (gsi);
	      return false;
	    }
	}
      /* If si->nonzero_chars > OFFSET, we aren't overwriting '\0',
	 and if we aren't storing '\0', we know that the length of the
	 string and any other zero terminated string in memory remains
	 the same.  In that case we move to the next gimple statement and
	 return to signal the caller that it shouldn't invalidate anything.  

	 This is benefical for cases like:

	 char p[20];
	 void foo (char *q)
	 {
	   strcpy (p, "foobar");
	   size_t len = strlen (p);        // This can be optimized into 6
	   size_t len2 = strlen (q);        // This has to be computed
	   p[0] = 'X';
	   size_t len3 = strlen (p);        // This can be optimized into 6
	   size_t len4 = strlen (q);        // This can be optimized into len2
	   bar (len, len2, len3, len4);
        }
	*/ 
      else if (storing_nonzero_p && cmp > 0)
	{
	  gsi_next (gsi);
	  return false;
	}
      else if (storing_zero_p || storing_nonzero_p || (offset != 0 && cmp > 0))
	{
	  /* When storing_nonzero_p, we know that the string now starts
	     with OFFSET + 1 nonzero characters, but don't know whether
	     there's a following nul terminator.

	     When storing_zero_p, we know that the string is now OFFSET
	     characters long.

	     Otherwise, we're storing an unknown value at offset OFFSET,
	     so need to clip the nonzero_chars to OFFSET.  */
	  location_t loc = gimple_location (stmt);
	  tree oldlen = si->nonzero_chars;
	  if (cmp == 0 && si->full_string_p)
	    /* We're overwriting the nul terminator with a nonzero or
	       unknown character.  If the previous stmt was a memcpy,
	       its length may be decreased.  */
	    adjust_last_stmt (si, stmt, false);
	  si = unshare_strinfo (si);
	  if (storing_nonzero_p)
	    si->nonzero_chars = build_int_cst (size_type_node, offset + 1);
	  else
	    si->nonzero_chars = build_int_cst (size_type_node, offset);
	  si->full_string_p = storing_zero_p;
	  if (storing_zero_p
	      && ssaname
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssaname))
	    si->endptr = ssaname;
	  else
	    si->endptr = NULL;
	  si->next = 0;
	  si->stmt = NULL;
	  si->writable = true;
	  si->dont_invalidate = true;
	  if (oldlen)
	    {
	      tree adj = fold_build2_loc (loc, MINUS_EXPR, size_type_node,
					  si->nonzero_chars, oldlen);
	      adjust_related_strinfos (loc, si, adj);
	    }
	  else
	    si->prev = 0;
	}
    }
  else if (idx == 0 && (storing_zero_p || storing_nonzero_p))
    {
      if (ssaname)
	idx = new_stridx (ssaname);
      else
	idx = new_addr_stridx (lhs);
      if (idx != 0)
	{
	  tree ptr = (ssaname ? ssaname : build_fold_addr_expr (lhs));
	  tree len = storing_nonzero_p ? size_one_node : size_zero_node;
	  si = new_strinfo (ptr, idx, len, storing_zero_p);
	  set_strinfo (idx, si);
	  if (storing_zero_p
	      && ssaname
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssaname))
	    si->endptr = ssaname;
	  si->dont_invalidate = true;
	  si->writable = true;
	}
    }
  else if (idx == 0
	   && (rhslen = get_string_cst_length (gimple_assign_rhs1 (stmt))) >= 0
	   && ssaname == NULL_TREE
	   && TREE_CODE (TREE_TYPE (lhs)) == ARRAY_TYPE)
    {
      HOST_WIDE_INT a = int_size_in_bytes (TREE_TYPE (lhs));
      if (a > 0 && (unsigned HOST_WIDE_INT) a > (unsigned HOST_WIDE_INT) rhslen)
	{
	  int idx = new_addr_stridx (lhs);
	  if (idx != 0)
	    {
	      si = new_strinfo (build_fold_addr_expr (lhs), idx,
				build_int_cst (size_type_node, rhslen), true);
	      set_strinfo (idx, si);
	      si->dont_invalidate = true;
	    }
	}
    }

  if (si != NULL && offset == 0 && storing_zero_p)
    {
      /* Allow adjust_last_stmt to remove it if the stored '\0'
	 is immediately overwritten.  */
      laststmt.stmt = stmt;
      laststmt.len = build_int_cst (size_type_node, 1);
      laststmt.stridx = si->idx;
    }
  return true;
}

/* Try to fold strstr (s, t) eq/ne s to strncmp (s, t, strlen (t)) eq/ne 0.  */

static void
fold_strstr_to_strncmp (tree rhs1, tree rhs2, gimple *stmt)
{
  if (TREE_CODE (rhs1) != SSA_NAME
      || TREE_CODE (rhs2) != SSA_NAME)
    return;

  gimple *call_stmt = NULL;
  for (int pass = 0; pass < 2; pass++)
    {
      gimple *g = SSA_NAME_DEF_STMT (rhs1);
      if (gimple_call_builtin_p (g, BUILT_IN_STRSTR)
	  && has_single_use (rhs1)
	  && gimple_call_arg (g, 0) == rhs2)
	{
	  call_stmt = g;
	  break;
	}
      std::swap (rhs1, rhs2);
    }

  if (call_stmt)
    {
      tree arg0 = gimple_call_arg (call_stmt, 0);

      if (arg0 == rhs2)
	{
	  tree arg1 = gimple_call_arg (call_stmt, 1);
	  tree arg1_len = NULL_TREE;
	  int idx = get_stridx (arg1);

	  if (idx)
	    {
	      if (idx < 0)
		arg1_len = build_int_cst (size_type_node, ~idx);
	      else
		{
		  strinfo *si = get_strinfo (idx);
		  if (si)
		    arg1_len = get_string_length (si);
		}
	    }

	  if (arg1_len != NULL_TREE)
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (call_stmt);
	      tree strncmp_decl = builtin_decl_explicit (BUILT_IN_STRNCMP);

	      if (!is_gimple_val (arg1_len))
		{
		  tree arg1_len_tmp = make_ssa_name (TREE_TYPE (arg1_len));
		  gassign *arg1_stmt = gimple_build_assign (arg1_len_tmp,
							    arg1_len);
		  gsi_insert_before (&gsi, arg1_stmt, GSI_SAME_STMT);
		  arg1_len = arg1_len_tmp;
		}

	      gcall *strncmp_call = gimple_build_call (strncmp_decl, 3,
						      arg0, arg1, arg1_len);
	      tree strncmp_lhs = make_ssa_name (integer_type_node);
	      gimple_set_vuse (strncmp_call, gimple_vuse (call_stmt));
	      gimple_call_set_lhs (strncmp_call, strncmp_lhs);
	      gsi_remove (&gsi, true);
	      gsi_insert_before (&gsi, strncmp_call, GSI_SAME_STMT);
	      tree zero = build_zero_cst (TREE_TYPE (strncmp_lhs));

	      if (is_gimple_assign (stmt))
		{
		  if (gimple_assign_rhs_code (stmt) == COND_EXPR)
		    {
		      tree cond = gimple_assign_rhs1 (stmt);
		      TREE_OPERAND (cond, 0) = strncmp_lhs;
		      TREE_OPERAND (cond, 1) = zero;
		    }
		  else
		    {
		      gimple_assign_set_rhs1 (stmt, strncmp_lhs);
		      gimple_assign_set_rhs2 (stmt, zero);
		    }
		}
	      else
		{
		  gcond *cond = as_a<gcond *> (stmt);
		  gimple_cond_set_lhs (cond, strncmp_lhs);
		  gimple_cond_set_rhs (cond, zero);
		}
	      update_stmt (stmt);
	    }
	}
    }
}

/* Attempt to check for validity of the performed access a single statement
   at *GSI using string length knowledge, and to optimize it.
   If the given basic block needs clean-up of EH, CLEANUP_EH is set to
   true.  */

static bool
strlen_check_and_optimize_stmt (gimple_stmt_iterator *gsi, bool *cleanup_eh)
{
  gimple *stmt = gsi_stmt (*gsi);

  if (is_gimple_call (stmt))
    {
      tree callee = gimple_call_fndecl (stmt);
      if (valid_builtin_call (stmt))
	switch (DECL_FUNCTION_CODE (callee))
	  {
	  case BUILT_IN_STRLEN:
	  case BUILT_IN_STRNLEN:
	    handle_builtin_strlen (gsi);
	    break;
	  case BUILT_IN_STRCHR:
	    handle_builtin_strchr (gsi);
	    break;
	  case BUILT_IN_STRCPY:
	  case BUILT_IN_STRCPY_CHK:
	  case BUILT_IN_STPCPY:
	  case BUILT_IN_STPCPY_CHK:
	    handle_builtin_strcpy (DECL_FUNCTION_CODE (callee), gsi);
	    break;

	  case BUILT_IN_STRNCAT:
	  case BUILT_IN_STRNCAT_CHK:
	    handle_builtin_strncat (DECL_FUNCTION_CODE (callee), gsi);
	    break;

	  case BUILT_IN_STPNCPY:
	  case BUILT_IN_STPNCPY_CHK:
	  case BUILT_IN_STRNCPY:
	  case BUILT_IN_STRNCPY_CHK:
	    handle_builtin_stxncpy (DECL_FUNCTION_CODE (callee), gsi);
	    break;

	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMCPY_CHK:
	  case BUILT_IN_MEMPCPY:
	  case BUILT_IN_MEMPCPY_CHK:
	    handle_builtin_memcpy (DECL_FUNCTION_CODE (callee), gsi);
	    break;
	  case BUILT_IN_STRCAT:
	  case BUILT_IN_STRCAT_CHK:
	    handle_builtin_strcat (DECL_FUNCTION_CODE (callee), gsi);
	    break;
	  case BUILT_IN_MALLOC:
	  case BUILT_IN_CALLOC:
	    handle_builtin_malloc (DECL_FUNCTION_CODE (callee), gsi);
	    break;
	  case BUILT_IN_MEMSET:
	    if (handle_builtin_memset (gsi))
	      return false;
	    break;
	  case BUILT_IN_MEMCMP:
	    if (handle_builtin_memcmp (gsi))
	      return false;
	    break;
	  case BUILT_IN_STRCMP:
	  case BUILT_IN_STRNCMP:
	    if (handle_builtin_string_cmp (gsi))
	      return false;
	    break;
	  default:
	    break;
	  }
    }
  else if (is_gimple_assign (stmt) && !gimple_clobber_p (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);

      if (TREE_CODE (lhs) == SSA_NAME && POINTER_TYPE_P (TREE_TYPE (lhs)))
	{
	  if (gimple_assign_single_p (stmt)
	      || (gimple_assign_cast_p (stmt)
		  && POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (stmt)))))
	    {
	      int idx = get_stridx (gimple_assign_rhs1 (stmt));
	      ssa_ver_to_stridx[SSA_NAME_VERSION (lhs)] = idx;
	    }
	  else if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
	    handle_pointer_plus (gsi);
	}
    else if (TREE_CODE (lhs) == SSA_NAME && INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
      {
	enum tree_code code = gimple_assign_rhs_code (stmt);
	if (code == COND_EXPR)
	  {
	    tree cond = gimple_assign_rhs1 (stmt);
	    enum tree_code cond_code = TREE_CODE (cond);

	    if (cond_code == EQ_EXPR || cond_code == NE_EXPR)
	      fold_strstr_to_strncmp (TREE_OPERAND (cond, 0),
				      TREE_OPERAND (cond, 1), stmt);
	  }
	else if (code == EQ_EXPR || code == NE_EXPR)
	  fold_strstr_to_strncmp (gimple_assign_rhs1 (stmt),
				  gimple_assign_rhs2 (stmt), stmt);
	else if (gimple_assign_load_p (stmt)
		 && TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE
		 && TYPE_MODE (TREE_TYPE (lhs)) == TYPE_MODE (char_type_node)
		 && (TYPE_PRECISION (TREE_TYPE (lhs))
		     == TYPE_PRECISION (char_type_node))
		 && !gimple_has_volatile_ops (stmt))
	  {
	    tree off = integer_zero_node;
	    unsigned HOST_WIDE_INT coff = 0;
	    int idx = 0;
	    tree rhs1 = gimple_assign_rhs1 (stmt);
	    if (code == MEM_REF)
	      {
		idx = get_stridx (TREE_OPERAND (rhs1, 0));
		if (idx > 0)
		  {
		    strinfo *si = get_strinfo (idx);
		    if (si
			&& si->nonzero_chars
			&& TREE_CODE (si->nonzero_chars) == INTEGER_CST
			&& (wi::to_widest (si->nonzero_chars)
			    >= wi::to_widest (off)))
		      off = TREE_OPERAND (rhs1, 1);
		    else
		      /* This case is not useful.  See if get_addr_stridx
			 returns something usable.  */
		      idx = 0;
		  }
	      }
	    if (idx <= 0)
	      idx = get_addr_stridx (rhs1, NULL_TREE, &coff);
	    if (idx > 0)
	      {
		strinfo *si = get_strinfo (idx);
		if (si
		    && si->nonzero_chars
		    && TREE_CODE (si->nonzero_chars) == INTEGER_CST)
		  {
		    widest_int w1 = wi::to_widest (si->nonzero_chars);
		    widest_int w2 = wi::to_widest (off) + coff;
		    if (w1 == w2
			&& si->full_string_p)
		      {
			if (dump_file && (dump_flags & TDF_DETAILS) != 0)
			  {
			    fprintf (dump_file, "Optimizing: ");
			    print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
			  }

			/* Reading the final '\0' character.  */
			tree zero = build_int_cst (TREE_TYPE (lhs), 0);
			gimple_set_vuse (stmt, NULL_TREE);
			gimple_assign_set_rhs_from_tree (gsi, zero);
			*cleanup_eh
			  |= maybe_clean_or_replace_eh_stmt (stmt,
							     gsi_stmt (*gsi));
			stmt = gsi_stmt (*gsi);
			update_stmt (stmt);

			if (dump_file && (dump_flags & TDF_DETAILS) != 0)
			  {
			    fprintf (dump_file, "into: ");
			    print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
			  }
		      }
		    else if (w1 > w2)
		      {
			/* Reading a character before the final '\0'
			   character.  Just set the value range to ~[0, 0]
			   if we don't have anything better.  */
			wide_int min, max;
			tree type = TREE_TYPE (lhs);
			enum value_range_type vr
			  = get_range_info (lhs, &min, &max);
			if (vr == VR_VARYING
			    || (vr == VR_RANGE
				&& min == wi::min_value (TYPE_PRECISION (type),
							 TYPE_SIGN (type))
				&& max == wi::max_value (TYPE_PRECISION (type),
							 TYPE_SIGN (type))))
			  set_range_info (lhs, VR_ANTI_RANGE,
					  wi::zero (TYPE_PRECISION (type)),
					  wi::zero (TYPE_PRECISION (type)));
		      }
		  }
	      }
	  }

	if (strlen_to_stridx)
	  {
	    tree rhs1 = gimple_assign_rhs1 (stmt);
	    if (stridx_strlenloc *ps = strlen_to_stridx->get (rhs1))
	      strlen_to_stridx->put (lhs, stridx_strlenloc (*ps));
	  }
      }
    else if (TREE_CODE (lhs) != SSA_NAME && !TREE_SIDE_EFFECTS (lhs))
	{
	  tree type = TREE_TYPE (lhs);
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    type = TREE_TYPE (type);
	  if (TREE_CODE (type) == INTEGER_TYPE
	      && TYPE_MODE (type) == TYPE_MODE (char_type_node)
	      && TYPE_PRECISION (type) == TYPE_PRECISION (char_type_node))
	    {
	      if (! handle_char_store (gsi))
		return false;
	    }
	}
    }
  else if (gcond *cond = dyn_cast<gcond *> (stmt))
    {
      enum tree_code code = gimple_cond_code (cond);
      if (code == EQ_EXPR || code == NE_EXPR)
	fold_strstr_to_strncmp (gimple_cond_lhs (stmt),
				gimple_cond_rhs (stmt), stmt);
    }

  if (gimple_vdef (stmt))
    maybe_invalidate (stmt);
  return true;
}

/* Recursively call maybe_invalidate on stmts that might be executed
   in between dombb and current bb and that contain a vdef.  Stop when
   *count stmts are inspected, or if the whole strinfo vector has
   been invalidated.  */

static void
do_invalidate (basic_block dombb, gimple *phi, bitmap visited, int *count)
{
  unsigned int i, n = gimple_phi_num_args (phi);

  for (i = 0; i < n; i++)
    {
      tree vuse = gimple_phi_arg_def (phi, i);
      gimple *stmt = SSA_NAME_DEF_STMT (vuse);
      basic_block bb = gimple_bb (stmt);
      if (bb == NULL
	  || bb == dombb
	  || !bitmap_set_bit (visited, bb->index)
	  || !dominated_by_p (CDI_DOMINATORS, bb, dombb))
	continue;
      while (1)
	{
	  if (gimple_code (stmt) == GIMPLE_PHI)
	    {
	      do_invalidate (dombb, stmt, visited, count);
	      if (*count == 0)
		return;
	      break;
	    }
	  if (--*count == 0)
	    return;
	  if (!maybe_invalidate (stmt))
	    {
	      *count = 0;
	      return;
	    }
	  vuse = gimple_vuse (stmt);
	  stmt = SSA_NAME_DEF_STMT (vuse);
	  if (gimple_bb (stmt) != bb)
	    {
	      bb = gimple_bb (stmt);
	      if (bb == NULL
		  || bb == dombb
		  || !bitmap_set_bit (visited, bb->index)
		  || !dominated_by_p (CDI_DOMINATORS, bb, dombb))
		break;
	    }
	}
    }
}

class strlen_dom_walker : public dom_walker
{
public:
  strlen_dom_walker (cdi_direction direction)
    : dom_walker (direction), m_cleanup_cfg (false)
  {}

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

  /* Flag that will trigger TODO_cleanup_cfg to be returned in strlen
     execute function.  */
  bool m_cleanup_cfg;
};

/* Callback for walk_dominator_tree.  Attempt to optimize various
   string ops by remembering string lengths pointed by pointer SSA_NAMEs.  */

edge
strlen_dom_walker::before_dom_children (basic_block bb)
{
  basic_block dombb = get_immediate_dominator (CDI_DOMINATORS, bb);

  if (dombb == NULL)
    stridx_to_strinfo = NULL;
  else
    {
      stridx_to_strinfo = ((vec<strinfo *, va_heap, vl_embed> *) dombb->aux);
      if (stridx_to_strinfo)
	{
	  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *phi = gsi.phi ();
	      if (virtual_operand_p (gimple_phi_result (phi)))
		{
		  bitmap visited = BITMAP_ALLOC (NULL);
		  int count_vdef = 100;
		  do_invalidate (dombb, phi, visited, &count_vdef);
		  BITMAP_FREE (visited);
		  if (count_vdef == 0)
		    {
		      /* If there were too many vdefs in between immediate
			 dominator and current bb, invalidate everything.
			 If stridx_to_strinfo has been unshared, we need
			 to free it, otherwise just set it to NULL.  */
		      if (!strinfo_shared ())
			{
			  unsigned int i;
			  strinfo *si;

			  for (i = 1;
			       vec_safe_iterate (stridx_to_strinfo, i, &si);
			       ++i)
			    {
			      free_strinfo (si);
			      (*stridx_to_strinfo)[i] = NULL;
			    }
			}
		      else
			stridx_to_strinfo = NULL;
		    }
		  break;
		}
	    }
	}
    }

  /* If all PHI arguments have the same string index, the PHI result
     has it as well.  */
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree result = gimple_phi_result (phi);
      if (!virtual_operand_p (result) && POINTER_TYPE_P (TREE_TYPE (result)))
	{
	  int idx = get_stridx (gimple_phi_arg_def (phi, 0));
	  if (idx != 0)
	    {
	      unsigned int i, n = gimple_phi_num_args (phi);
	      for (i = 1; i < n; i++)
		if (idx != get_stridx (gimple_phi_arg_def (phi, i)))
		  break;
	      if (i == n)
		ssa_ver_to_stridx[SSA_NAME_VERSION (result)] = idx;
	    }
	}
    }

  bool cleanup_eh = false;

  /* Attempt to optimize individual statements.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
    if (strlen_check_and_optimize_stmt (&gsi, &cleanup_eh))
      gsi_next (&gsi);

  if (cleanup_eh && gimple_purge_dead_eh_edges (bb))
      m_cleanup_cfg = true;

  bb->aux = stridx_to_strinfo;
  if (vec_safe_length (stridx_to_strinfo) && !strinfo_shared ())
    (*stridx_to_strinfo)[0] = (strinfo *) bb;
  return NULL;
}

/* Callback for walk_dominator_tree.  Free strinfo vector if it is
   owned by the current bb, clear bb->aux.  */

void
strlen_dom_walker::after_dom_children (basic_block bb)
{
  if (bb->aux)
    {
      stridx_to_strinfo = ((vec<strinfo *, va_heap, vl_embed> *) bb->aux);
      if (vec_safe_length (stridx_to_strinfo)
	  && (*stridx_to_strinfo)[0] == (strinfo *) bb)
	{
	  unsigned int i;
	  strinfo *si;

	  for (i = 1; vec_safe_iterate (stridx_to_strinfo, i, &si); ++i)
	    free_strinfo (si);
	  vec_free (stridx_to_strinfo);
	}
      bb->aux = NULL;
    }
}

/* Main entry point.  */

namespace {

const pass_data pass_data_strlen =
{
  GIMPLE_PASS, /* type */
  "strlen", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_STRLEN, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_strlen : public gimple_opt_pass
{
public:
  pass_strlen (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_strlen, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_optimize_strlen != 0; }
  virtual unsigned int execute (function *);

}; // class pass_strlen

unsigned int
pass_strlen::execute (function *fun)
{
  gcc_assert (!strlen_to_stridx);
  if (warn_stringop_overflow || warn_stringop_truncation)
    strlen_to_stridx = new hash_map<tree, stridx_strlenloc> ();

  ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names);
  max_stridx = 1;

  calculate_dominance_info (CDI_DOMINATORS);

  /* String length optimization is implemented as a walk of the dominator
     tree and a forward walk of statements within each block.  */
  strlen_dom_walker walker (CDI_DOMINATORS);
  walker.walk (fun->cfg->x_entry_block_ptr);

  ssa_ver_to_stridx.release ();
  strinfo_pool.release ();
  if (decl_to_stridxlist_htab)
    {
      obstack_free (&stridx_obstack, NULL);
      delete decl_to_stridxlist_htab;
      decl_to_stridxlist_htab = NULL;
    }
  laststmt.stmt = NULL;
  laststmt.len = NULL_TREE;
  laststmt.stridx = 0;

  if (strlen_to_stridx)
    {
      strlen_to_stridx->empty ();
      delete strlen_to_stridx;
      strlen_to_stridx = NULL;
    }

  return walker.m_cleanup_cfg ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_strlen (gcc::context *ctxt)
{
  return new pass_strlen (ctxt);
}
