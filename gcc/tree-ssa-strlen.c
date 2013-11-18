/* String length optimization
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
#include "tree.h"
#include "hash-table.h"
#include "bitmap.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-dfa.h"
#include "tree-pass.h"
#include "domwalk.h"
#include "alloc-pool.h"
#include "tree-ssa-propagate.h"
#include "gimple-pretty-print.h"
#include "params.h"
#include "expr.h"

/* A vector indexed by SSA_NAME_VERSION.  0 means unknown, positive value
   is an index into strinfo vector, negative value stands for
   string length of a string literal (~strlen).  */
static vec<int> ssa_ver_to_stridx;

/* Number of currently active string indexes plus one.  */
static int max_stridx;

/* String information record.  */
typedef struct strinfo_struct
{
  /* String length of this string.  */
  tree length;
  /* Any of the corresponding pointers for querying alias oracle.  */
  tree ptr;
  /* Statement for delayed length computation.  */
  gimple stmt;
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
} *strinfo;

/* Pool for allocating strinfo_struct entries.  */
static alloc_pool strinfo_pool;

/* Vector mapping positive string indexes to strinfo, for the
   current basic block.  The first pointer in the vector is special,
   it is either NULL, meaning the vector isn't shared, or it is
   a basic block pointer to the owner basic_block if shared.
   If some other bb wants to modify the vector, the vector needs
   to be unshared first, and only the owner bb is supposed to free it.  */
static vec<strinfo, va_heap, vl_embed> *stridx_to_strinfo;

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

/* stridxlist hashtable helpers.  */

struct stridxlist_hasher : typed_noop_remove <decl_stridxlist_map>
{
  typedef decl_stridxlist_map value_type;
  typedef decl_stridxlist_map compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Hash a from tree in a decl_stridxlist_map.  */

inline hashval_t
stridxlist_hasher::hash (const value_type *item)
{
  return DECL_UID (item->base.from);
}

inline bool
stridxlist_hasher::equal (const value_type *v, const compare_type *c)
{
  return tree_map_base_eq (&v->base, &c->base);
}

/* Hash table for mapping decls to a chained list of offset -> idx
   mappings.  */
static hash_table <stridxlist_hasher> decl_to_stridxlist_htab;

/* Obstack for struct stridxlist and struct decl_stridxlist_map.  */
static struct obstack stridx_obstack;

/* Last memcpy statement if it could be adjusted if the trailing
   '\0' written is immediately overwritten, or
   *x = '\0' store that could be removed if it is immediately overwritten.  */
struct laststmt_struct
{
  gimple stmt;
  tree len;
  int stridx;
} laststmt;

/* Helper function for get_stridx.  */

static int
get_addr_stridx (tree exp)
{
  HOST_WIDE_INT off;
  struct decl_stridxlist_map ent, *e;
  struct stridxlist *list;
  tree base;

  if (!decl_to_stridxlist_htab.is_created ())
    return 0;

  base = get_addr_base_and_unit_offset (exp, &off);
  if (base == NULL || !DECL_P (base))
    return 0;

  ent.base.from = base;
  e = decl_to_stridxlist_htab.find_with_hash (&ent, DECL_UID (base));
  if (e == NULL)
    return 0;

  list = &e->list;
  do
    {
      if (list->offset == off)
	return list->idx;
      list = list->next;
    }
  while (list);
  return 0;
}

/* Return string index for EXP.  */

static int
get_stridx (tree exp)
{
  tree s, o;

  if (TREE_CODE (exp) == SSA_NAME)
    return ssa_ver_to_stridx[SSA_NAME_VERSION (exp)];

  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      int idx = get_addr_stridx (TREE_OPERAND (exp, 0));
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
  strinfo si;
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
  decl_stridxlist_map **slot;
  struct decl_stridxlist_map ent;
  struct stridxlist *list;
  HOST_WIDE_INT off;

  tree base = get_addr_base_and_unit_offset (exp, &off);
  if (base == NULL_TREE || !DECL_P (base))
    return NULL;

  if (!decl_to_stridxlist_htab.is_created ())
    {
      decl_to_stridxlist_htab.create (64);
      gcc_obstack_init (&stridx_obstack);
    }
  ent.base.from = base;
  slot = decl_to_stridxlist_htab.find_slot_with_hash (&ent, DECL_UID (base),
						      INSERT);
  if (*slot)
    {
      int i;
      list = &(*slot)->list;
      for (i = 0; i < 16; i++)
	{
	  if (list->offset == off)
	    return &list->idx;
	  if (list->next == NULL)
	    break;
	}
      if (i == 16)
	return NULL;
      list->next = XOBNEW (&stridx_obstack, struct stridxlist);
      list = list->next;
    }
  else
    {
      struct decl_stridxlist_map *e
	= XOBNEW (&stridx_obstack, struct decl_stridxlist_map);
      e->base.from = base;
      *slot = e;
      list = &e->list;
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

static strinfo
new_strinfo (tree ptr, int idx, tree length)
{
  strinfo si = (strinfo) pool_alloc (strinfo_pool);
  si->length = length;
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
  return si;
}

/* Decrease strinfo refcount and free it if not referenced anymore.  */

static inline void
free_strinfo (strinfo si)
{
  if (si && --si->refcount == 0)
    pool_free (strinfo_pool, si);
}

/* Return strinfo vector entry IDX.  */

static inline strinfo
get_strinfo (int idx)
{
  if (vec_safe_length (stridx_to_strinfo) <= (unsigned int) idx)
    return NULL;
  return (*stridx_to_strinfo)[idx];
}

/* Set strinfo in the vector entry IDX to SI.  */

static inline void
set_strinfo (int idx, strinfo si)
{
  if (vec_safe_length (stridx_to_strinfo) && (*stridx_to_strinfo)[0])
    unshare_strinfo_vec ();
  if (vec_safe_length (stridx_to_strinfo) <= (unsigned int) idx)
    vec_safe_grow_cleared (stridx_to_strinfo, idx + 1);
  (*stridx_to_strinfo)[idx] = si;
}

/* Return string length, or NULL if it can't be computed.  */

static tree
get_string_length (strinfo si)
{
  if (si->length)
    return si->length;

  if (si->stmt)
    {
      gimple stmt = si->stmt, lenstmt;
      tree callee, lhs, fn, tem;
      location_t loc;
      gimple_stmt_iterator gsi;

      gcc_assert (is_gimple_call (stmt));
      callee = gimple_call_fndecl (stmt);
      gcc_assert (callee && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL);
      lhs = gimple_call_lhs (stmt);
      gcc_assert (builtin_decl_implicit_p (BUILT_IN_STPCPY));
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
	  lenstmt
	    = gimple_build_assign_with_ops
	        (POINTER_PLUS_EXPR,
		 make_ssa_name (TREE_TYPE (gimple_call_arg (stmt, 0)), NULL),
		 tem, lhs);
	  gsi_insert_before (&gsi, lenstmt, GSI_SAME_STMT);
	  gimple_call_set_arg (stmt, 0, gimple_assign_lhs (lenstmt));
	  lhs = NULL_TREE;
	  /* FALLTHRU */
	case BUILT_IN_STRCPY:
	case BUILT_IN_STRCPY_CHK:
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
	  si->endptr = lhs;
	  si->stmt = NULL;
	  lhs = fold_convert_loc (loc, size_type_node, lhs);
	  si->length = fold_convert_loc (loc, size_type_node, si->ptr);
	  si->length = fold_build2_loc (loc, MINUS_EXPR, size_type_node,
					lhs, si->length);
	  break;
	default:
	  gcc_unreachable ();
	  break;
	}
    }

  return si->length;
}

/* Invalidate string length information for strings whose length
   might change due to stores in stmt.  */

static bool
maybe_invalidate (gimple stmt)
{
  strinfo si;
  unsigned int i;
  bool nonempty = false;

  for (i = 1; vec_safe_iterate (stridx_to_strinfo, i, &si); ++i)
    if (si != NULL)
      {
	if (!si->dont_invalidate)
	  {
	    ao_ref r;
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

/* Unshare strinfo record SI, if it has recount > 1 or
   if stridx_to_strinfo vector is shared with some other
   bbs.  */

static strinfo
unshare_strinfo (strinfo si)
{
  strinfo nsi;

  if (si->refcount == 1 && !strinfo_shared ())
    return si;

  nsi = new_strinfo (si->ptr, si->idx, si->length);
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

/* Return first strinfo in the related strinfo chain
   if all strinfos in between belong to the chain, otherwise
   NULL.  */

static strinfo
verify_related_strinfos (strinfo origsi)
{
  strinfo si = origsi, psi;

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

/* Note that PTR, a pointer SSA_NAME initialized in the current stmt, points
   to a zero-length string and if possible chain it to a related strinfo
   chain whose part is or might be CHAINSI.  */

static strinfo
zero_length_string (tree ptr, strinfo chainsi)
{
  strinfo si;
  int idx;
  gcc_checking_assert (TREE_CODE (ptr) == SSA_NAME
		       && get_stridx (ptr) == 0);

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ptr))
    return NULL;
  if (chainsi != NULL)
    {
      si = verify_related_strinfos (chainsi);
      if (si)
	{
	  chainsi = si;
	  for (; chainsi->next; chainsi = si)
	    {
	      if (chainsi->endptr == NULL_TREE)
		{
		  chainsi = unshare_strinfo (chainsi);
		  chainsi->endptr = ptr;
		}
	      si = get_strinfo (chainsi->next);
	      if (si == NULL
		  || si->first != chainsi->first
		  || si->prev != chainsi->idx)
		break;
	    }
	  gcc_assert (chainsi->length || chainsi->stmt);
	  if (chainsi->endptr == NULL_TREE)
	    {
	      chainsi = unshare_strinfo (chainsi);
	      chainsi->endptr = ptr;
	    }
	  if (chainsi->length && integer_zerop (chainsi->length))
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
      else if (chainsi->first || chainsi->prev || chainsi->next)
	{
	  chainsi = unshare_strinfo (chainsi);
	  chainsi->first = 0;
	  chainsi->prev = 0;
	  chainsi->next = 0;
	}
    }
  idx = new_stridx (ptr);
  if (idx == 0)
    return NULL;
  si = new_strinfo (ptr, idx, build_int_cst (size_type_node, 0));
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

/* For strinfo ORIGSI whose length has been just updated
   update also related strinfo lengths (add ADJ to each,
   but don't adjust ORIGSI).  */

static void
adjust_related_strinfos (location_t loc, strinfo origsi, tree adj)
{
  strinfo si = verify_related_strinfos (origsi);

  if (si == NULL)
    return;

  while (1)
    {
      strinfo nsi;

      if (si != origsi)
	{
	  tree tem;

	  si = unshare_strinfo (si);
	  if (si->length)
	    {
	      tem = fold_convert_loc (loc, TREE_TYPE (si->length), adj);
	      si->length = fold_build2_loc (loc, PLUS_EXPR,
					    TREE_TYPE (si->length), si->length,
					    tem);
	    }
	  else if (si->stmt != NULL)
	    /* Delayed length computation is unaffected.  */
	    ;
	  else
	    gcc_unreachable ();

	  si->endptr = NULL_TREE;
	  si->dont_invalidate = true;
	}
      if (si->next == 0)
	return;
      nsi = get_strinfo (si->next);
      if (nsi == NULL
	  || nsi->first != si->first
	  || nsi->prev != si->idx)
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
      gimple stmt = SSA_NAME_DEF_STMT (ptr);
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

/* If the last .MEM setter statement before STMT is
   memcpy (x, y, strlen (y) + 1), the only .MEM use of it is STMT
   and STMT is known to overwrite x[strlen (x)], adjust the last memcpy to
   just memcpy (x, y, strlen (y)).  SI must be the zero length
   strinfo.  */

static void
adjust_last_stmt (strinfo si, gimple stmt, bool is_strcat)
{
  tree vuse, callee, len;
  struct laststmt_struct last = laststmt;
  strinfo lastsi, firstsi;

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
	  strinfo nextsi;
	  if (firstsi->next == 0)
	    return;
	  nextsi = get_strinfo (firstsi->next);
	  if (nextsi == NULL
	      || nextsi->prev != firstsi->idx
	      || nextsi->first != si->first)
	    return;
	  firstsi = nextsi;
	}
    }

  if (!is_strcat)
    {
      if (si->length == NULL_TREE || !integer_zerop (si->length))
	return;
    }

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

  if (!gimple_call_builtin_p (last.stmt, BUILT_IN_NORMAL))
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

  len = gimple_call_arg (last.stmt, 2);
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
      gimple def_stmt = SSA_NAME_DEF_STMT (len);
      if (!is_gimple_assign (def_stmt)
	  || gimple_assign_rhs_code (def_stmt) != PLUS_EXPR
	  || gimple_assign_rhs1 (def_stmt) != last.len
	  || !integer_onep (gimple_assign_rhs2 (def_stmt)))
	return;
    }
  else
    return;

  gimple_call_set_arg (last.stmt, 2, last.len);
  update_stmt (last.stmt);
}

/* Handle a strlen call.  If strlen of the argument is known, replace
   the strlen call with the known value, otherwise remember that strlen
   of the argument is stored in the lhs SSA_NAME.  */

static void
handle_builtin_strlen (gimple_stmt_iterator *gsi)
{
  int idx;
  tree src;
  gimple stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL_TREE)
    return;

  src = gimple_call_arg (stmt, 0);
  idx = get_stridx (src);
  if (idx)
    {
      strinfo si = NULL;
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
	    rhs = fold_convert_loc (gimple_location (stmt),
				    TREE_TYPE (lhs), rhs);
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
	      && TREE_CODE (si->length) != SSA_NAME
	      && TREE_CODE (si->length) != INTEGER_CST
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    {
	      si = unshare_strinfo (si);
	      si->length = lhs;
	    }
	  return;
	}
    }
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return;
  if (idx == 0)
    idx = new_stridx (src);
  else if (get_strinfo (idx) != NULL)
    return;
  if (idx)
    {
      strinfo si = new_strinfo (src, idx, lhs);
      set_strinfo (idx, si);
      find_equal_ptrs (src, idx);
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
  gimple stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL_TREE)
    return;

  if (!integer_zerop (gimple_call_arg (stmt, 1)))
    return;

  src = gimple_call_arg (stmt, 0);
  idx = get_stridx (src);
  if (idx)
    {
      strinfo si = NULL;
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
	  strinfo si = new_strinfo (src, idx, length);
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
  tree src, dst, srclen, len, lhs, args, type, fn, oldlen;
  bool success;
  gimple stmt = gsi_stmt (*gsi);
  strinfo si, dsi, olddsi, zsi;
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
      oldlen = olddsi->length;
      dsi = unshare_strinfo (olddsi);
      dsi->length = srclen;
      /* Break the chain, so adjust_related_strinfo on later pointers in
	 the chain won't adjust this one anymore.  */
      dsi->next = 0;
      dsi->stmt = NULL;
      dsi->endptr = NULL_TREE;
    }
  else
    {
      dsi = new_strinfo (dst, didx, srclen);
      set_strinfo (didx, dsi);
      find_equal_ptrs (dst, didx);
    }
  dsi->writable = true;
  dsi->dont_invalidate = true;

  if (dsi->length == NULL_TREE)
    {
      strinfo chainsi;

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
	      chainsi->length = NULL_TREE;
	      chainsi->endptr = NULL_TREE;
	      chainsi->dont_invalidate = true;
	    }
	}
      dsi->stmt = stmt;
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

  if (fn == NULL_TREE)
    return;

  args = TYPE_ARG_TYPES (TREE_TYPE (fn));
  type = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));

  len = fold_convert_loc (loc, type, unshare_expr (srclen));
  len = fold_build2_loc (loc, PLUS_EXPR, type, len, build_int_cst (type, 1));
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
  gimple stmt = gsi_stmt (*gsi);
  strinfo si, dsi, olddsi;

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

  if (idx > 0)
    {
      gimple def_stmt;

      /* Handle memcpy (x, y, l) where l is strlen (y) + 1.  */
      si = get_strinfo (idx);
      if (si == NULL || si->length == NULL_TREE)
	return;
      if (TREE_CODE (len) != SSA_NAME)
	return;
      def_stmt = SSA_NAME_DEF_STMT (len);
      if (!is_gimple_assign (def_stmt)
	  || gimple_assign_rhs_code (def_stmt) != PLUS_EXPR
	  || gimple_assign_rhs1 (def_stmt) != si->length
	  || !integer_onep (gimple_assign_rhs2 (def_stmt)))
	return;
    }
  else
    {
      si = NULL;
      /* Handle memcpy (x, "abcd", 5) or
	 memcpy (x, "abc\0uvw", 7).  */
      if (!tree_fits_uhwi_p (len)
	  || tree_to_uhwi (len) <= (unsigned HOST_WIDE_INT) ~idx)
	return;
    }

  if (olddsi != NULL && TREE_CODE (len) == SSA_NAME)
    adjust_last_stmt (olddsi, stmt, false);

  if (didx == 0)
    {
      didx = new_stridx (dst);
      if (didx == 0)
	return;
    }
  if (si != NULL)
    newlen = si->length;
  else
    newlen = build_int_cst (size_type_node, ~idx);
  oldlen = NULL_TREE;
  if (olddsi != NULL)
    {
      dsi = unshare_strinfo (olddsi);
      oldlen = olddsi->length;
      dsi->length = newlen;
      /* Break the chain, so adjust_related_strinfo on later pointers in
	 the chain won't adjust this one anymore.  */
      dsi->next = 0;
      dsi->stmt = NULL;
      dsi->endptr = NULL_TREE;
    }
  else
    {
      dsi = new_strinfo (dst, didx, newlen);
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
	adj = dsi->length;
      else if (TREE_CODE (oldlen) == INTEGER_CST
	       || TREE_CODE (dsi->length) == INTEGER_CST)
	adj = fold_build2_loc (loc, MINUS_EXPR,
			       TREE_TYPE (dsi->length), dsi->length,
			       fold_convert_loc (loc, TREE_TYPE (dsi->length),
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

  lhs = gimple_call_lhs (stmt);
  switch (bcode)
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
      /* Allow adjust_last_stmt to decrease this memcpy's size.  */
      laststmt.stmt = stmt;
      laststmt.len = dsi->length;
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

/* Handle a strcat-like ({strcat,__strcat_chk}) call.
   If strlen of the second argument is known, strlen of the first argument
   is increased by the length of the second argument.  Furthermore, attempt
   to convert it to memcpy/strcpy if the length of the first argument
   is known.  */

static void
handle_builtin_strcat (enum built_in_function bcode, gimple_stmt_iterator *gsi)
{
  int idx, didx;
  tree src, dst, srclen, dstlen, len, lhs, args, type, fn, objsz, endptr;
  bool success;
  gimple stmt = gsi_stmt (*gsi);
  strinfo si, dsi;
  location_t loc;

  src = gimple_call_arg (stmt, 1);
  dst = gimple_call_arg (stmt, 0);
  lhs = gimple_call_lhs (stmt);

  didx = get_stridx (dst);
  if (didx < 0)
    return;

  dsi = NULL;
  if (didx > 0)
    dsi = get_strinfo (didx);
  if (dsi == NULL || get_string_length (dsi) == NULL_TREE)
    {
      /* strcat (p, q) can be transformed into
	 tmp = p + strlen (p); endptr = strpcpy (tmp, q);
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
	      dsi = new_strinfo (dst, didx, NULL_TREE);
	      set_strinfo (didx, dsi);
	      find_equal_ptrs (dst, didx);
	    }
	  else
	    {
	      dsi = unshare_strinfo (dsi);
	      dsi->length = NULL_TREE;
	      dsi->next = 0;
	      dsi->endptr = NULL_TREE;
	    }
	  dsi->writable = true;
	  dsi->stmt = stmt;
	  dsi->dont_invalidate = true;
	}
      return;
    }

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

  loc = gimple_location (stmt);
  dstlen = dsi->length;
  endptr = dsi->endptr;

  dsi = unshare_strinfo (dsi);
  dsi->endptr = NULL_TREE;
  dsi->stmt = NULL;
  dsi->writable = true;

  if (srclen != NULL_TREE)
    {
      dsi->length = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (dsi->length),
				     dsi->length, srclen);
      adjust_related_strinfos (loc, dsi, srclen);
      dsi->dont_invalidate = true;
    }
  else
    {
      dsi->length = NULL;
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

  len = NULL_TREE;
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
}

/* Handle a POINTER_PLUS_EXPR statement.
   For p = "abcd" + 2; compute associated length, or if
   p = q + off is pointing to a '\0' character of a string, call
   zero_length_string on it.  */

static void
handle_pointer_plus (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree lhs = gimple_assign_lhs (stmt), off;
  int idx = get_stridx (gimple_assign_rhs1 (stmt));
  strinfo si, zsi;

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
  if (si == NULL || si->length == NULL_TREE)
    return;

  off = gimple_assign_rhs2 (stmt);
  zsi = NULL;
  if (operand_equal_p (si->length, off, 0))
    zsi = zero_length_string (lhs, si);
  else if (TREE_CODE (off) == SSA_NAME)
    {
      gimple def_stmt = SSA_NAME_DEF_STMT (off);
      if (gimple_assign_single_p (def_stmt)
	  && operand_equal_p (si->length, gimple_assign_rhs1 (def_stmt), 0))
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
      gimple_assign_set_rhs_with_ops (gsi, rhs_code, si->endptr, NULL_TREE);
      gcc_assert (gsi_stmt (*gsi) == stmt);
      update_stmt (stmt);
    }
}

/* Handle a single character store.  */

static bool
handle_char_store (gimple_stmt_iterator *gsi)
{
  int idx = -1;
  strinfo si = NULL;
  gimple stmt = gsi_stmt (*gsi);
  tree ssaname = NULL_TREE, lhs = gimple_assign_lhs (stmt);

  if (TREE_CODE (lhs) == MEM_REF
      && TREE_CODE (TREE_OPERAND (lhs, 0)) == SSA_NAME)
    {
      if (integer_zerop (TREE_OPERAND (lhs, 1)))
	{
	  ssaname = TREE_OPERAND (lhs, 0);
	  idx = get_stridx (ssaname);
	}
    }
  else
    idx = get_addr_stridx (lhs);

  if (idx > 0)
    {
      si = get_strinfo (idx);
      if (si != NULL && si->length != NULL_TREE && integer_zerop (si->length))
	{
	  if (initializer_zerop (gimple_assign_rhs1 (stmt)))
	    {
	      /* When storing '\0', the store can be removed
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
	  else
	    /* Otherwise this statement overwrites the '\0' with
	       something, if the previous stmt was a memcpy,
	       its length may be decreased.  */
	    adjust_last_stmt (si, stmt, false);
	}
      else if (si != NULL && integer_zerop (gimple_assign_rhs1 (stmt)))
	{
	  si = unshare_strinfo (si);
	  si->length = build_int_cst (size_type_node, 0);
	  si->endptr = NULL;
	  si->prev = 0;
	  si->next = 0;
	  si->stmt = NULL;
	  si->first = 0;
	  si->writable = true;
	  if (ssaname && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssaname))
	    si->endptr = ssaname;
	  si->dont_invalidate = true;
	}
      /* If si->length is non-zero constant, we aren't overwriting '\0',
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
      else if (si != NULL && si->length != NULL_TREE
	       && TREE_CODE (si->length) == INTEGER_CST
	       && integer_nonzerop (gimple_assign_rhs1 (stmt)))
	{
	  gsi_next (gsi);
	  return false;
	}
    }
  else if (idx == 0 && initializer_zerop (gimple_assign_rhs1 (stmt)))
    {
      if (ssaname)
	{
	  si = zero_length_string (ssaname, NULL);
	  if (si != NULL)
	    si->dont_invalidate = true;
	}
      else
	{
	  int idx = new_addr_stridx (lhs);
	  if (idx != 0)
	    {
	      si = new_strinfo (build_fold_addr_expr (lhs), idx,
				build_int_cst (size_type_node, 0));
	      set_strinfo (idx, si);
	      si->dont_invalidate = true;
	    }
	}
      if (si != NULL)
	si->writable = true;
    }
  else if (idx == 0
	   && TREE_CODE (gimple_assign_rhs1 (stmt)) == STRING_CST
	   && ssaname == NULL_TREE
	   && TREE_CODE (TREE_TYPE (lhs)) == ARRAY_TYPE)
    {
      size_t l = strlen (TREE_STRING_POINTER (gimple_assign_rhs1 (stmt)));
      HOST_WIDE_INT a = int_size_in_bytes (TREE_TYPE (lhs));
      if (a > 0 && (unsigned HOST_WIDE_INT) a > l)
	{
	  int idx = new_addr_stridx (lhs);
	  if (idx != 0)
	    {
	      si = new_strinfo (build_fold_addr_expr (lhs), idx,
				build_int_cst (size_type_node, l));
	      set_strinfo (idx, si);
	      si->dont_invalidate = true;
	    }
	}
    }

  if (si != NULL && initializer_zerop (gimple_assign_rhs1 (stmt)))
    {
      /* Allow adjust_last_stmt to remove it if the stored '\0'
	 is immediately overwritten.  */
      laststmt.stmt = stmt;
      laststmt.len = build_int_cst (size_type_node, 1);
      laststmt.stridx = si->idx;
    }
  return true;
}

/* Attempt to optimize a single statement at *GSI using string length
   knowledge.  */

static bool
strlen_optimize_stmt (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);

  if (is_gimple_call (stmt))
    {
      tree callee = gimple_call_fndecl (stmt);
      if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	switch (DECL_FUNCTION_CODE (callee))
	  {
	  case BUILT_IN_STRLEN:
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
	  default:
	    break;
	  }
    }
  else if (is_gimple_assign (stmt))
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

  if (gimple_vdef (stmt))
    maybe_invalidate (stmt);
  return true;
}

/* Recursively call maybe_invalidate on stmts that might be executed
   in between dombb and current bb and that contain a vdef.  Stop when
   *count stmts are inspected, or if the whole strinfo vector has
   been invalidated.  */

static void
do_invalidate (basic_block dombb, gimple phi, bitmap visited, int *count)
{
  unsigned int i, n = gimple_phi_num_args (phi);

  for (i = 0; i < n; i++)
    {
      tree vuse = gimple_phi_arg_def (phi, i);
      gimple stmt = SSA_NAME_DEF_STMT (vuse);
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
  strlen_dom_walker (cdi_direction direction) : dom_walker (direction) {}

  virtual void before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);
};

/* Callback for walk_dominator_tree.  Attempt to optimize various
   string ops by remembering string lenths pointed by pointer SSA_NAMEs.  */

void
strlen_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;
  basic_block dombb = get_immediate_dominator (CDI_DOMINATORS, bb);

  if (dombb == NULL)
    stridx_to_strinfo = NULL;
  else
    {
      stridx_to_strinfo = ((vec<strinfo, va_heap, vl_embed> *) dombb->aux);
      if (stridx_to_strinfo)
	{
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple phi = gsi_stmt (gsi);
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
			  strinfo si;

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
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
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

  /* Attempt to optimize individual statements.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
    if (strlen_optimize_stmt (&gsi))
      gsi_next (&gsi);

  bb->aux = stridx_to_strinfo;
  if (vec_safe_length (stridx_to_strinfo) && !strinfo_shared ())
    (*stridx_to_strinfo)[0] = (strinfo) bb;
}

/* Callback for walk_dominator_tree.  Free strinfo vector if it is
   owned by the current bb, clear bb->aux.  */

void
strlen_dom_walker::after_dom_children (basic_block bb)
{
  if (bb->aux)
    {
      stridx_to_strinfo = ((vec<strinfo, va_heap, vl_embed> *) bb->aux);
      if (vec_safe_length (stridx_to_strinfo)
	  && (*stridx_to_strinfo)[0] == (strinfo) bb)
	{
	  unsigned int i;
	  strinfo si;

	  for (i = 1; vec_safe_iterate (stridx_to_strinfo, i, &si); ++i)
	    free_strinfo (si);
	  vec_free (stridx_to_strinfo);
	}
      bb->aux = NULL;
    }
}

/* Main entry point.  */

static unsigned int
tree_ssa_strlen (void)
{
  ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names);
  max_stridx = 1;
  strinfo_pool = create_alloc_pool ("strinfo_struct pool",
				    sizeof (struct strinfo_struct), 64);

  calculate_dominance_info (CDI_DOMINATORS);

  /* String length optimization is implemented as a walk of the dominator
     tree and a forward walk of statements within each block.  */
  strlen_dom_walker (CDI_DOMINATORS).walk (cfun->cfg->x_entry_block_ptr);

  ssa_ver_to_stridx.release ();
  free_alloc_pool (strinfo_pool);
  if (decl_to_stridxlist_htab.is_created ())
    {
      obstack_free (&stridx_obstack, NULL);
      decl_to_stridxlist_htab.dispose ();
    }
  laststmt.stmt = NULL;
  laststmt.len = NULL_TREE;
  laststmt.stridx = 0;

  return 0;
}

static bool
gate_strlen (void)
{
  return flag_optimize_strlen != 0;
}

namespace {

const pass_data pass_data_strlen =
{
  GIMPLE_PASS, /* type */
  "strlen", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_STRLEN, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_strlen : public gimple_opt_pass
{
public:
  pass_strlen (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_strlen, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_strlen (); }
  unsigned int execute () { return tree_ssa_strlen (); }

}; // class pass_strlen

} // anon namespace

gimple_opt_pass *
make_pass_strlen (gcc::context *ctxt)
{
  return new pass_strlen (ctxt);
}
