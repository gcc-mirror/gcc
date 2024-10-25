/* String length optimization
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
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
#include "gimple-ssa-warn-access.h"
#include "gimple-ssa-warn-restrict.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "expr.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "domwalk.h"
#include "tree-ssa-alias.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-strlen.h"
#include "tree-hash-traits.h"
#include "builtins.h"
#include "pointer-query.h"
#include "target.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "intl.h"
#include "attribs.h"
#include "calls.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "vr-values.h"
#include "gimple-range.h"
#include "tree-ssa.h"

/* A vector indexed by SSA_NAME_VERSION.  0 means unknown, positive value
   is an index into strinfo vector, negative value stands for
   string length of a string literal (~strlen).  */
static vec<int> ssa_ver_to_stridx;

/* Number of currently active string indexes plus one.  */
static int max_stridx;

/* Set to true to optimize, false when just checking.  */
static bool strlen_optimize;

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
  /* STMT is used for two things:

     - To record the statement that should be used for delayed length
       computations.  We maintain the invariant that all related strinfos
       have delayed lengths or none do.

     - To record the malloc or calloc call that produced this result
       to optimize away malloc/memset sequences.  STMT is reset after
       a calloc-allocated object has been stored a non-zero value into.  */
  gimple *stmt;
  /* Set to the dynamic allocation statement for the object (alloca,
     calloc, malloc, or VLA).  Unlike STMT, once set for a strinfo
     object, ALLOC doesn't change.  */
  gimple *alloc;
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
typedef hash_map<tree_decl_hash, stridxlist> decl_to_stridxlist_htab_t;
static decl_to_stridxlist_htab_t *decl_to_stridxlist_htab;

/* Hash table mapping strlen (or strnlen with constant bound and return
   smaller than bound) calls to stridx instances describing
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
static bool get_range_strlen_dynamic (tree, gimple *, c_strlen_data *,
				      bitmap, pointer_query *, unsigned *);

/* Sets MINMAX to either the constant value or the range VAL is in
   and returns either the constant value or VAL on success or null
   when the range couldn't be determined.  Uses RVALS or CFUN for
   range info, whichever is nonnull.  */

tree
get_range (tree val, gimple *stmt, wide_int minmax[2],
	   range_query *rvals /* = NULL */)
{
  if (!rvals)
    {
      if (!cfun)
	/* When called from front ends for global initializers CFUN
	   may be null.  */
	return NULL_TREE;

      rvals = get_range_query (cfun);
    }

  value_range vr (TREE_TYPE (val));
  if (!rvals->range_of_expr (vr, val, stmt))
    return NULL_TREE;

  tree vrmin, vrmax;
  value_range_kind rng = get_legacy_range (vr, vrmin, vrmax);
  if (rng == VR_RANGE)
    {
      /* Only handle straight ranges.  */
      minmax[0] = wi::to_wide (vrmin);
      minmax[1] = wi::to_wide (vrmax);
      return val;
    }

  return NULL_TREE;
}

class strlen_pass : public dom_walker
{
public:
  strlen_pass (function *fun, cdi_direction direction)
    : dom_walker (direction),
      ptr_qry (get_range_query (fun)),
      m_cleanup_cfg (false)
  {
  }

  ~strlen_pass ();

  edge before_dom_children (basic_block) final override;
  void after_dom_children (basic_block) final override;

  bool check_and_optimize_stmt (bool *cleanup_eh);
  bool check_and_optimize_call (bool *zero_write);
  bool handle_assign (tree lhs, bool *zero_write);
  bool handle_store (bool *zero_write);
  void handle_pointer_plus ();
  void handle_builtin_strlen ();
  void handle_builtin_strchr ();
  void handle_builtin_strcpy (built_in_function);
  void handle_integral_assign (bool *cleanup_eh);
  void handle_builtin_stxncpy_strncat (bool append_p);
  void handle_builtin_memcpy (built_in_function bcode);
  void handle_builtin_strcat (built_in_function bcode);
  void handle_builtin_strncat (built_in_function);
  bool handle_builtin_memset (bool *zero_write);
  bool handle_builtin_memcmp ();
  bool handle_builtin_string_cmp ();
  void handle_alloc_call (built_in_function);
  void maybe_warn_overflow (gimple *stmt, bool call_lhs, tree len,
			    strinfo *si = NULL, bool plus_one = false,
			    bool rawmem = false);
  void maybe_warn_overflow (gimple *stmt, bool call_lhs,
			    unsigned HOST_WIDE_INT len,
			    strinfo *si = NULL,
			    bool plus_one = false, bool rawmem = false);
  void adjust_last_stmt (strinfo *si, gimple *stmt, bool is_strcat);
  tree strxcmp_eqz_result (gimple *stmt, tree arg1, int idx1,
			   tree arg2, int idx2,
			   unsigned HOST_WIDE_INT bound,
			   unsigned HOST_WIDE_INT len[2],
			   unsigned HOST_WIDE_INT *psize);
  bool count_nonzero_bytes (tree expr_or_type,
			    gimple *stmt,
			    unsigned lenrange[3], bool *nulterm,
			    bool *allnul, bool *allnonnul);
  bool count_nonzero_bytes (tree exp, tree vuse,
			    gimple *stmt,
			    unsigned HOST_WIDE_INT offset,
			    unsigned HOST_WIDE_INT nbytes,
			    unsigned lenrange[3], bool *nulterm,
			    bool *allnul, bool *allnonnul,
			    ssa_name_limit_t &snlim);
  bool count_nonzero_bytes_addr (tree exp, tree vuse,
				 gimple *stmt,
				 unsigned HOST_WIDE_INT offset,
				 unsigned HOST_WIDE_INT nbytes,
				 unsigned lenrange[3], bool *nulterm,
				 bool *allnul, bool *allnonnul,
				 ssa_name_limit_t &snlim);
  bool get_len_or_size (gimple *stmt, tree arg, int idx,
			unsigned HOST_WIDE_INT lenrng[2],
			unsigned HOST_WIDE_INT *size, bool *nulterm);

  /* A pointer_query object to store information about pointers and
     their targets in.  */
  pointer_query ptr_qry;

  gimple_stmt_iterator m_gsi;

  /* Flag that will trigger TODO_cleanup_cfg to be returned in strlen
     execute function.  */
  bool m_cleanup_cfg;
};

/* Return:

   *  +1  if SI is known to start with more than OFF nonzero characters.

   *   0  if SI is known to start with exactly OFF nonzero characters.

   *  -1  if SI either does not start with OFF nonzero characters
	  or the relationship between the number of leading nonzero
	  characters in SI and OFF is unknown.  */

static int
compare_nonzero_chars (strinfo *si, unsigned HOST_WIDE_INT off)
{
  if (si->nonzero_chars
      && TREE_CODE (si->nonzero_chars) == INTEGER_CST)
    return compare_tree_int (si->nonzero_chars, off);
  else
    return -1;
}

/* Same as above but suitable also for strings with non-constant lengths.
   Uses RVALS to determine length range.  */

static int
compare_nonzero_chars (strinfo *si, gimple *stmt,
		       unsigned HOST_WIDE_INT off,
		       range_query *rvals)
{
  if (!si->nonzero_chars)
    return -1;

  if (TREE_CODE (si->nonzero_chars) == INTEGER_CST)
    return compare_tree_int (si->nonzero_chars, off);

  if (!rvals || TREE_CODE (si->nonzero_chars) != SSA_NAME)
    return -1;

  int_range_max vr;
  if (!rvals->range_of_expr (vr, si->nonzero_chars, stmt)
      || vr.varying_p ()
      || vr.undefined_p ())
    return -1;

  /* If the offset is less than the minimum length or if the bounds
     of the length range are equal return the result of the comparison
     same as in the constant case.  Otherwise return a conservative
     result.  */
  signop sign = TYPE_SIGN (vr.type ());
  unsigned prec = TYPE_PRECISION (vr.type ());
  int cmpmin = wi::cmp (vr.lower_bound (), wi::uhwi (off, prec), sign);
  if (cmpmin > 0 || vr.singleton_p ())
    return cmpmin;

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
   *OFFSET_OUT.  When RVALS is nonnull uses it to determine range
   information.  */

static int
get_addr_stridx (tree exp, gimple *stmt,
		 tree ptr, unsigned HOST_WIDE_INT *offset_out,
		 range_query *rvals = NULL)
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
      if (si && compare_nonzero_chars (si, stmt, rel_off, rvals) >= 0)
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

/* Returns string index for EXP.  When EXP is an SSA_NAME that refers
   to a known strinfo with an offset and OFFRNG is non-null, sets
   both elements of the OFFRNG array to the range of the offset and
   returns the index of the known strinfo.  In this case the result
   must not be used in for functions that modify the string.
   When nonnull, uses RVALS to determine range information.  */

static int
get_stridx (tree exp, gimple *stmt,
	    wide_int offrng[2] = NULL, range_query *rvals = NULL)
{
  if (offrng)
    offrng[0] = offrng[1] = wi::zero (TYPE_PRECISION (ptrdiff_type_node));

  if (TREE_CODE (exp) == SSA_NAME)
    {
      if (ssa_ver_to_stridx[SSA_NAME_VERSION (exp)])
	return ssa_ver_to_stridx[SSA_NAME_VERSION (exp)];

      tree e = exp;
      int last_idx = 0;
      HOST_WIDE_INT offset = 0;
      /* Follow a chain of at most 5 assignments.  */
      for (int i = 0; i < 5; i++)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (e);
	  if (!is_gimple_assign (def_stmt))
	    return last_idx;

	  tree_code rhs_code = gimple_assign_rhs_code (def_stmt);
	  tree ptr, off;

	  if (rhs_code == ADDR_EXPR)
	    {
	      /* Handle indices/offsets into VLAs which are implemented
	         as pointers to arrays.  */
	      ptr = gimple_assign_rhs1 (def_stmt);
	      ptr = TREE_OPERAND (ptr, 0);

	      /* Handle also VLAs of types larger than char.  */
	      if (tree eltsize = TYPE_SIZE_UNIT (TREE_TYPE (ptr)))
		{
		  if (TREE_CODE (ptr) == ARRAY_REF)
		    {
		      off = TREE_OPERAND (ptr, 1);
		      ptr = TREE_OPERAND (ptr, 0);
		      if (!integer_onep (eltsize))
			{
			  /* Scale the array index by the size of the element
			     type in the rare case that it's greater than
			     the typical 1 for char, making sure both operands
			     have the same type.  */
			  eltsize = fold_convert (ssizetype, eltsize);
			  off = fold_convert (ssizetype, off);
			  off = fold_build2 (MULT_EXPR, ssizetype, off, eltsize);
			}
		    }
		  else
		    off = integer_zero_node;
		}
	      else
		return 0;

	      if (TREE_CODE (ptr) != MEM_REF)
	        return 0;

	      /* Add the MEM_REF byte offset.  */
	      tree mem_off = TREE_OPERAND (ptr, 1);
	      off = fold_build2 (PLUS_EXPR, TREE_TYPE (off), off, mem_off);
	      ptr = TREE_OPERAND (ptr, 0);
	    }
	  else if (rhs_code == POINTER_PLUS_EXPR)
	    {
	      ptr = gimple_assign_rhs1 (def_stmt);
	      off = gimple_assign_rhs2 (def_stmt);
	    }
	  else
	    return 0;

	  if (TREE_CODE (ptr) != SSA_NAME)
	    return 0;

	  if (!tree_fits_shwi_p (off))
	    {
	      if (int idx = ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)])
		if (offrng)
		  {
		    /* Only when requested by setting OFFRNG to non-null,
		       return the index corresponding to the SSA_NAME.
		       Do this irrespective of the whether the offset
		       is known.  */
		    if (get_range (off, def_stmt, offrng, rvals))
		      {
			/* When the offset range is known, increment it
			   it by the constant offset computed in prior
			   iterations and store it in the OFFRNG array.  */
 			offrng[0] += offset;
			offrng[1] += offset;
		      }
		    else
		      {
			/* When the offset range cannot be determined
			   store [0, SIZE_MAX] and let the caller decide
			   if the offset matters.  */
			offrng[1] = wi::to_wide (TYPE_MAX_VALUE (sizetype));
			offrng[0] = wi::zero (offrng[1].get_precision ());
		      }
		    return idx;
		  }
	      return 0;
	    }

	  HOST_WIDE_INT this_off = tree_to_shwi (off);
	  if (offrng)
	    {
	      offrng[0] += wi::shwi (this_off, offrng->get_precision ());
	      offrng[1] += offrng[0];
	    }

	  if (this_off < 0)
	    return last_idx;

	  offset = (unsigned HOST_WIDE_INT) offset + this_off;
	  if (offset < 0)
	    return last_idx;

	  if (int idx = ssa_ver_to_stridx[SSA_NAME_VERSION (ptr)])
	    {
	      strinfo *si = get_strinfo (idx);
	      if (si)
		{
		  if (compare_nonzero_chars (si, offset) >= 0)
		    return get_stridx_plus_constant (si, offset, exp);

		  if (offrng)
		    last_idx = idx;
		}
	    }
	  e = ptr;
	}

      return last_idx;
    }

  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      int idx = get_addr_stridx (TREE_OPERAND (exp, 0), stmt, exp, NULL);
      if (idx != 0)
	return idx;
    }

  const char *p = c_getstr (exp);
  if (p)
    return ~(int) strlen (p);

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
  if (max_stridx >= param_max_tracked_strlens)
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
  if (max_stridx >= param_max_tracked_strlens)
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
  STRIP_USELESS_TYPE_CONVERSION (ptr);
  si->ptr = ptr;
  si->stmt = NULL;
  si->alloc = NULL;
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
    vec_safe_grow_cleared (stridx_to_strinfo, idx + 1, true);
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

/* Return the string length, or NULL if it can't be computed.
   The length may but need not be constant.  Instead, it might be
   the result of a strlen() call.  */

static tree
get_string_length (strinfo *si)
{
  /* If the length has already been computed return it if it's exact
     (i.e., the string is nul-terminated at NONZERO_CHARS), or return
     null if it isn't.  */
  if (si->nonzero_chars)
    return si->full_string_p ? si->nonzero_chars : NULL;

  /* If the string is the result of one of the built-in calls below
     attempt to compute the length from the call statement.  */
  if (si->stmt)
    {
      gimple *stmt = si->stmt, *lenstmt;
      tree callee, lhs, fn, tem;
      location_t loc;
      gimple_stmt_iterator gsi;

      gcc_assert (is_gimple_call (stmt));
      callee = gimple_call_fndecl (stmt);
      gcc_assert (callee && fndecl_built_in_p (callee, BUILT_IN_NORMAL));
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
	case BUILT_IN_ALLOCA:
	case BUILT_IN_ALLOCA_WITH_ALIGN:
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

/* Dump strlen data to FP for statement STMT.  When non-null, RVALS
   points to the valuation engine used to calculate ranges, and is
   used to dump strlen range for non-constant results.  */

DEBUG_FUNCTION void
dump_strlen_info (FILE *fp, gimple *stmt, range_query *rvals)
{
  if (stmt)
    {
      fprintf (fp, "\nDumping strlen pass data after ");
      print_gimple_expr (fp, stmt, TDF_LINENO);
      fputc ('\n', fp);
    }
  else
    fprintf (fp, "\nDumping strlen pass data\n");

  fprintf (fp, "max_stridx = %i\n", max_stridx);
  fprintf (fp, "ssa_ver_to_stridx has %u elements\n",
	   ssa_ver_to_stridx.length ());
  fprintf (fp, "stridx_to_strinfo");
  if (stridx_to_strinfo)
    {
      fprintf (fp, " has %u elements\n", stridx_to_strinfo->length ());
      for (unsigned i = 0; i != stridx_to_strinfo->length (); ++i)
	{
	  if (strinfo *si = (*stridx_to_strinfo)[i])
	    {
	      if (!si->idx)
		continue;
	      fprintf (fp, "  idx = %i", si->idx);
	      if (si->ptr)
		{
		  fprintf (fp, ", ptr = ");
		  print_generic_expr (fp, si->ptr);
		}

	      if (si->nonzero_chars)
		{
		  fprintf (fp, ", nonzero_chars = ");
		  print_generic_expr (fp, si->nonzero_chars);
		  if (TREE_CODE (si->nonzero_chars) == SSA_NAME)
		    {
		      int_range_max vr;
		      if (rvals)
			rvals->range_of_expr (vr, si->nonzero_chars,
					      si->stmt);
		      else
			get_range_query (cfun)->range_of_expr (vr,
							si->nonzero_chars);
		      vr.dump (fp);
		    }
		}

	      fprintf (fp, ", refcount = %i", si->refcount);
	      if (si->stmt)
		{
		  fprintf (fp, ", stmt = ");
		  print_gimple_expr (fp, si->stmt, 0);
		}
	      if (si->alloc)
		{
		  fprintf (fp, ", alloc = ");
		  print_gimple_expr (fp, si->alloc, 0);
		}
	      if (si->writable)
		fprintf (fp, ", writable");
	      if (si->dont_invalidate)
		fprintf (fp, ", dont_invalidate");
	      if (si->full_string_p)
		fprintf (fp, ", full_string_p");
	      if (strinfo *next = get_next_strinfo (si))
		{
		  fprintf (fp, ", {");
		  do
		    fprintf (fp, "%i%s", next->idx, next->first ? ", " : "");
		  while ((next = get_next_strinfo (next)));
		  fprintf (fp, "}");
		}
	      fputs ("\n", fp);
	    }
	}
    }
  else
    fprintf (fp, " = null\n");

  fprintf (fp, "decl_to_stridxlist_htab");
  if (decl_to_stridxlist_htab)
    {
      fputs ("\n", fp);
      typedef decl_to_stridxlist_htab_t::iterator iter_t;
      for (iter_t it = decl_to_stridxlist_htab->begin ();
	   it != decl_to_stridxlist_htab->end (); ++it)
	{
	  tree decl = (*it).first;
	  stridxlist *list = &(*it).second;
	  fprintf (fp, "  decl = ");
	  print_generic_expr (fp, decl);
	  if (list)
	    {
	      fprintf (fp, ", offsets = {");
	      for (; list; list = list->next)
		fprintf (fp, "%lli%s", (long long) list->offset,
			 list->next ? ", " : "");
	      fputs ("}", fp);
	    }
	  fputs ("\n", fp);
	}
    }
  else
    fprintf (fp, " = null\n");

  if (laststmt.stmt)
    {
      fprintf (fp, "laststmt = ");
      print_gimple_expr (fp, laststmt.stmt, 0);
      fprintf (fp, ", len = ");
      print_generic_expr (fp, laststmt.len);
      fprintf (fp, ", stridx = %i\n", laststmt.stridx);
    }
}

/* Helper of get_range_strlen_dynamic().  See below.  */

static bool
get_range_strlen_phi (tree src, gphi *phi,
		      c_strlen_data *pdata, bitmap visited,
		      pointer_query *ptr_qry, unsigned *pssa_def_max)
{
  if (!bitmap_set_bit (visited, SSA_NAME_VERSION (src)))
    return true;

  if (*pssa_def_max == 0)
    return false;

  --*pssa_def_max;

  /* Iterate over the PHI arguments and determine the minimum and maximum
     length/size of each and incorporate them into the overall result.  */
  for (unsigned i = 0; i != gimple_phi_num_args (phi); ++i)
    {
      tree arg = gimple_phi_arg_def (phi, i);
      if (arg == gimple_phi_result (phi))
	continue;

      c_strlen_data argdata = { };
      if (!get_range_strlen_dynamic (arg, phi, &argdata, visited, ptr_qry,
				     pssa_def_max))
	{
	  pdata->maxlen = build_all_ones_cst (size_type_node);
	  continue;
	}

      /* Set the DECL of an unterminated array this argument refers to
	 if one hasn't been found yet.  */
      if (!pdata->decl && argdata.decl)
	pdata->decl = argdata.decl;

      if (!argdata.minlen
	  || (integer_zerop (argdata.minlen)
	      && (!argdata.maxbound
		  || integer_all_onesp (argdata.maxbound))
	      && integer_all_onesp (argdata.maxlen)))
	{
	  /* Set the upper bound of the length to unbounded.  */
	  pdata->maxlen = build_all_ones_cst (size_type_node);
	  continue;
	}

      /* Adjust the minimum and maximum length determined so far and
	 the upper bound on the array size.  */
      if (TREE_CODE (argdata.minlen) == INTEGER_CST
	  && (!pdata->minlen
	      || tree_int_cst_lt (argdata.minlen, pdata->minlen)))
	pdata->minlen = argdata.minlen;

      if (TREE_CODE (argdata.maxlen) == INTEGER_CST
	  && (!pdata->maxlen
	      || (argdata.maxlen
		  && tree_int_cst_lt (pdata->maxlen, argdata.maxlen))))
	pdata->maxlen = argdata.maxlen;

      if (!pdata->maxbound
	  || TREE_CODE (pdata->maxbound) != INTEGER_CST
	  || (argdata.maxbound
	      && tree_int_cst_lt (pdata->maxbound, argdata.maxbound)
	      && !integer_all_onesp (argdata.maxbound)))
	pdata->maxbound = argdata.maxbound;
    }

  return true;
}

/* Return the maximum possible length of the string PTR that's less
   than MAXLEN given the size of the object of subobject it points
   to at the given STMT.  MAXLEN is the maximum length of the string
   determined so far.  Return null when no such maximum can be
   determined.  */

static tree
get_maxbound (tree ptr, gimple *stmt, offset_int maxlen,
	      pointer_query *ptr_qry)
{
  access_ref aref;
  if (!ptr_qry->get_ref (ptr, stmt, &aref))
    return NULL_TREE;

  offset_int sizrem = aref.size_remaining ();
  if (sizrem <= 0)
    return NULL_TREE;

  if (sizrem < maxlen)
    maxlen = sizrem - 1;

  /* Try to determine the maximum from the subobject at the offset.
     This handles MEM [&some-struct, member-offset] that's often
     the result of folding COMPONENT_REF [some-struct, member].  */
  tree reftype = TREE_TYPE (aref.ref);
  if (!RECORD_OR_UNION_TYPE_P (reftype)
      || aref.offrng[0] != aref.offrng[1]
      || !wi::fits_shwi_p (aref.offrng[0]))
    return wide_int_to_tree (size_type_node, maxlen);

  HOST_WIDE_INT off = aref.offrng[0].to_shwi ();
  tree fld = field_at_offset (reftype, NULL_TREE, off);
  if (!fld || !DECL_SIZE_UNIT (fld))
    return wide_int_to_tree (size_type_node, maxlen);

  offset_int size = wi::to_offset (DECL_SIZE_UNIT (fld));
  if (maxlen < size)
    return wide_int_to_tree (size_type_node, maxlen);

  return wide_int_to_tree (size_type_node, size - 1);
}

/* Attempt to determine the length of the string SRC.  On success, store
   the length in *PDATA and return true.  Otherwise, return false.
   VISITED is a bitmap of visited PHI nodes.  RVALS points to the valuation
   engine used to calculate ranges.  PSSA_DEF_MAX to an SSA_NAME
   assignment limit used to prevent runaway recursion.  */

static bool
get_range_strlen_dynamic (tree src, gimple *stmt,
			  c_strlen_data *pdata, bitmap visited,
			  pointer_query *ptr_qry, unsigned *pssa_def_max)
{
  int idx = get_stridx (src, stmt);
  if (!idx)
    {
      if (TREE_CODE (src) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (src);
	  if (gphi *phi = dyn_cast<gphi *>(def_stmt))
	    return get_range_strlen_phi (src, phi, pdata, visited, ptr_qry,
					 pssa_def_max);
	}

      /* Return success regardless of the result and handle *PDATA
	 in the caller.  */
      get_range_strlen (src, pdata, 1);
      return true;
    }

  if (idx < 0)
    {
      /* SRC is a string of constant length.  */
      pdata->minlen = build_int_cst (size_type_node, ~idx);
      pdata->maxlen = pdata->minlen;
      pdata->maxbound = pdata->maxlen;
      return true;
    }

  if (strinfo *si = get_strinfo (idx))
    {
      pdata->minlen = get_string_length (si);
      if (!pdata->minlen && si->nonzero_chars)
	{
	  if (TREE_CODE (si->nonzero_chars) == INTEGER_CST)
	    pdata->minlen = si->nonzero_chars;
	  else if (TREE_CODE (si->nonzero_chars) == SSA_NAME)
	    {
	      int_range_max vr;
	      ptr_qry->rvals->range_of_expr (vr, si->nonzero_chars, si->stmt);
	      if (vr.undefined_p () || vr.varying_p ())
		pdata->minlen = build_zero_cst (size_type_node);
	      else
		{
		  tree type = vr.type ();
		  pdata->minlen = wide_int_to_tree (type, vr.lower_bound ());
		}
	    }
	  else
	    pdata->minlen = build_zero_cst (size_type_node);

	  tree base = si->ptr;
	  if (TREE_CODE (base) == ADDR_EXPR)
	    base = TREE_OPERAND (base, 0);

	  HOST_WIDE_INT off;
	  poly_int64 poff;
	  base = get_addr_base_and_unit_offset (base, &poff);
	  if (base
	      && DECL_P (base)
	      && TREE_CODE (TREE_TYPE (base)) == ARRAY_TYPE
	      && TYPE_SIZE_UNIT (TREE_TYPE (base))
	      && poff.is_constant (&off))
	    {
	      tree basetype = TREE_TYPE (base);
	      tree size = TYPE_SIZE_UNIT (basetype);
	      if (TREE_CODE (size) == INTEGER_CST)
		{
		  ++off;   /* Increment for the terminating nul.  */
		  tree toffset = build_int_cst (size_type_node, off);
		  pdata->maxlen = fold_build2 (MINUS_EXPR, size_type_node,
					       size, toffset);
		  if (tree_int_cst_lt (pdata->maxlen, pdata->minlen))
		    /* This can happen when triggering UB, when base is an
		       array which is known to be filled with at least size
		       non-zero bytes.  E.g. for
		       char a[2]; memcpy (a, "12", sizeof a);
		       We don't want to create an invalid range [2, 1]
		       where 2 comes from the number of non-zero bytes and
		       1 from longest valid zero-terminated string that can
		       be stored in such an array, so pick just one of
		       those, pdata->minlen.  See PR110603.  */
		    pdata->maxlen = build_all_ones_cst (size_type_node);
		  else
		    pdata->maxbound = pdata->maxlen;
		}
	      else
		pdata->maxlen = build_all_ones_cst (size_type_node);
	    }
	  else
	    pdata->maxlen = build_all_ones_cst (size_type_node);
	}
      else if (pdata->minlen && TREE_CODE (pdata->minlen) == SSA_NAME)
	{
	  int_range_max vr;
	  ptr_qry->rvals->range_of_expr (vr, si->nonzero_chars, stmt);
	  if (vr.varying_p () || vr.undefined_p ())
	    {
	      pdata->minlen = build_zero_cst (size_type_node);
	      pdata->maxlen = build_all_ones_cst (size_type_node);
	    }
	  else
	    {
	      tree type = vr.type ();
	      pdata->minlen = wide_int_to_tree (type, vr.lower_bound ());
	      pdata->maxlen = wide_int_to_tree (type, vr.upper_bound ());
	      offset_int max = offset_int::from (vr.upper_bound (0), SIGNED);
	      if (tree maxbound = get_maxbound (si->ptr, stmt, max, ptr_qry))
		pdata->maxbound = maxbound;
	      else
		pdata->maxbound = pdata->maxlen;
	    }
	}
      else if (pdata->minlen && TREE_CODE (pdata->minlen) == INTEGER_CST)
	{
	  pdata->maxlen = pdata->minlen;
	  pdata->maxbound = pdata->minlen;
	}
      else
	{
	  /* For PDATA->MINLEN that's a non-constant expression such
	     as PLUS_EXPR whose value range is unknown, set the bounds
	     to zero and SIZE_MAX.  */
	  pdata->minlen = build_zero_cst (size_type_node);
	  pdata->maxlen = build_all_ones_cst (size_type_node);
	}

      return true;
    }

  return false;
}

/* Analogous to get_range_strlen but for dynamically created strings,
   i.e., those created by calls to strcpy as opposed to just string
   constants.
   Try to obtain the range of the lengths of the string(s) referenced
   by SRC, or the size of the largest array SRC refers to if the range
   of lengths cannot be determined, and store all in *PDATA.  RVALS
   points to the valuation engine used to calculate ranges.  */

void
get_range_strlen_dynamic (tree src, gimple *stmt, c_strlen_data *pdata,
			  pointer_query &ptr_qry)
{
  auto_bitmap visited;
  tree maxbound = pdata->maxbound;

  unsigned limit = param_ssa_name_def_chain_limit;
  if (!get_range_strlen_dynamic (src, stmt, pdata, visited, &ptr_qry, &limit))
    {
      /* On failure extend the length range to an impossible maximum
	 (a valid MAXLEN must be less than PTRDIFF_MAX - 1).  Other
	 members can stay unchanged regardless.  */
      pdata->minlen = ssize_int (0);
      pdata->maxlen = build_all_ones_cst (size_type_node);
    }
  else if (!pdata->minlen)
    pdata->minlen = ssize_int (0);

  /* If it's unchanged from it initial non-null value, set the conservative
     MAXBOUND to SIZE_MAX.  Otherwise leave it null (if it is null).  */
  if (maxbound && pdata->maxbound == maxbound)
    pdata->maxbound = build_all_ones_cst (size_type_node);
}

/* Invalidate string length information for strings whose length might
   change due to stores in STMT, except those marked DONT_INVALIDATE.
   For string-modifying statements, ZERO_WRITE is set when the statement
   wrote only zeros.
   Returns true if any STRIDX_TO_STRINFO entries were considered
   for invalidation.  */

static bool
maybe_invalidate (gimple *stmt, bool zero_write = false)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "%s called for ", __func__);
      print_gimple_stmt (dump_file, stmt, TDF_LINENO);
    }

  strinfo *si;
  bool nonempty = false;

  for (unsigned i = 1; vec_safe_iterate (stridx_to_strinfo, i, &si); ++i)
    {
      if (si == NULL || !POINTER_TYPE_P (TREE_TYPE (si->ptr)))
	continue;

      nonempty = true;

      /* Unconditionally reset DONT_INVALIDATE.  */
      bool dont_invalidate = si->dont_invalidate;
      si->dont_invalidate = false;

      if (dont_invalidate)
	continue;

      ao_ref r;
      tree size = si->nonzero_chars;
      ao_ref_init_from_ptr_and_size (&r, si->ptr, size);
      /* Include the terminating nul in the size of the string
	 to consider when determining possible clobber.  But do not
	 add it to 'size' since we don't know whether it would
	 actually fit the allocated area.  */
      if (known_size_p (r.size))
	{
	  if (known_le (r.size, HOST_WIDE_INT_MAX - BITS_PER_UNIT))
	    r.max_size += BITS_PER_UNIT;
	  else
	    r.max_size = -1;
	}
      if (stmt_may_clobber_ref_p_1 (stmt, &r))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fputs ("  statement may clobber object ", dump_file);
	      print_generic_expr (dump_file, si->ptr);
	      if (size && tree_fits_uhwi_p (size))
		fprintf (dump_file, " " HOST_WIDE_INT_PRINT_UNSIGNED
			 " bytes in size", tree_to_uhwi (size));
	      fputc ('\n', dump_file);
	    }

	  set_strinfo (i, NULL);
	  free_strinfo (si);
	  continue;
	}

      if (size
	  && !zero_write
	  && si->stmt
	  && is_gimple_call (si->stmt)
	  && (DECL_FUNCTION_CODE (gimple_call_fndecl (si->stmt))
	      == BUILT_IN_CALLOC))
	{
	  /* If the clobber test above considered the length of
	     the string (including the nul), then for (potentially)
	     non-zero writes that might modify storage allocated by
	     calloc consider the whole object and if it might be
	     clobbered by the statement reset the statement.  */
	  ao_ref_init_from_ptr_and_size (&r, si->ptr, NULL_TREE);
	  if (stmt_may_clobber_ref_p_1 (stmt, &r))
	    si->stmt = NULL;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "%s returns %i\n", __func__, nonempty);

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
  nsi->alloc = si->alloc;
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
    ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names, true);

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
    ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names, true);
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
	  /* We shouldn't see delayed lengths here; the caller must
	     have calculated the old length in order to calculate
	     the adjustment.  */
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
	ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names, true);

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
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRNCMP:
    case BUILT_IN_STRCHR:
    case BUILT_IN_STRLEN:
    case BUILT_IN_STRNLEN:
      /* The above functions should be pure.  Punt if they aren't.  */
      if (gimple_vdef (stmt) || gimple_vuse (stmt) == NULL_TREE)
	return false;
      break;

    case BUILT_IN_ALLOCA:
    case BUILT_IN_ALLOCA_WITH_ALIGN:
    case BUILT_IN_CALLOC:
    case BUILT_IN_MALLOC:
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMSET:
    case BUILT_IN_STPCPY:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STPNCPY:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
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

void
strlen_pass::adjust_last_stmt (strinfo *si, gimple *stmt, bool is_strcat)
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
      if (stmt_could_throw_p (cfun, last.stmt))
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

      /* Don't fold away an out of bounds access, as this defeats proper
	 warnings.  */
      tree dst = gimple_call_arg (last.stmt, 0);

      access_ref aref;
      tree size = compute_objsize (dst, stmt, 1, &aref, &ptr_qry);
      if (size && tree_int_cst_lt (size, len))
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

/* For an LHS that is an SSA_NAME that is the result of a strlen()
   call, or when BOUND is non-null, of a strnlen() call, set LHS
   range info to [0, min (MAX, BOUND)] when the range includes more
   than one value and return LHS.  Otherwise, when the range
   [MIN, MAX] is such that MIN == MAX, return the tree representation
   of (MIN). The latter allows callers to fold suitable strnlen() calls
   to constants.  */

tree
set_strlen_range (tree lhs, wide_int min, wide_int max,
		  tree bound /* = NULL_TREE */)
{
  if (TREE_CODE (lhs) != SSA_NAME
      || !INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
    return NULL_TREE;

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
	  int_range_max r;
	  get_range_query (cfun)->range_of_expr (r, bound);
	  if (!r.undefined_p ())
	    {
	      /* For a bound in a known range, adjust the range determined
		 above as necessary.  For a bound in some anti-range or
		 in an unknown range, use the range determined by callers.  */
	      if (wi::ltu_p (r.lower_bound (), min))
		min = r.lower_bound ();
	      if (wi::ltu_p (r.upper_bound (), max))
		max = r.upper_bound ();
	    }
	}
    }

  if (min == max)
    return wide_int_to_tree (size_type_node, min);

  int_range_max vr (TREE_TYPE (lhs), min, max);
  set_range_info (lhs, vr);
  return lhs;
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

  /* The longest string is PTRDIFF_MAX - 1 bytes including the final
     NUL so that the difference between a pointer to just past it and
     one to its beginning is positive.  */
  wide_int max = wi::to_wide (TYPE_MAX_VALUE (ptrdiff_type_node)) - 2;

  if (TREE_CODE (src) == ADDR_EXPR)
    {
      /* The last array member of a struct can be bigger than its size
	 suggests if it's treated as a poor-man's flexible array member.  */
      src = TREE_OPERAND (src, 0);
      if (TREE_CODE (src) != MEM_REF
	  && !array_ref_flexible_size_p (src))
	{
	  tree type = TREE_TYPE (src);
	  tree size = TYPE_SIZE_UNIT (type);
	  if (size
	      && TREE_CODE (size) == INTEGER_CST
	      && !integer_zerop (size))
	    {
	      /* Even though such uses of strlen would be undefined,
		 avoid relying on arrays of arrays in case some genius
		 decides to call strlen on an unterminated array element
		 that's followed by a terminated one.  Likewise, avoid
		 assuming that a struct array member is necessarily
		 nul-terminated (the nul may be in the member that
		 follows).  In those cases, assume that the length
		 of the string stored in such an array is bounded
		 by the size of the enclosing object if one can be
		 determined.  */
	      tree base = get_base_address (src);
	      if (VAR_P (base))
		{
		  if (tree size = DECL_SIZE_UNIT (base))
		    if (size
			&& TREE_CODE (size) == INTEGER_CST
			&& TREE_CODE (TREE_TYPE (base)) != POINTER_TYPE)
		      max = wi::to_wide (size);
		}
	    }

	  /* For strlen() the upper bound above is equal to
	     the longest string that can be stored in the array
	     (i.e., it accounts for the terminating nul.  For
	     strnlen() bump up the maximum by one since the array
	     need not be nul-terminated.  */
	  if (!bound && max != 0)
	    --max;
	}
    }

  wide_int min = wi::zero (max.get_precision ());
  return set_strlen_range (lhs, min, max, bound);
}

/* Diagnose buffer overflow by a STMT writing LEN + PLUS_ONE bytes,
   either into a region allocated for the object SI when non-null,
   or into an object designated by the LHS of STMT otherwise.
   For a call STMT, when CALL_LHS is set use its left hand side
   as the destination, otherwise use argument zero.
   When nonnull uses RVALS to determine range information.
   RAWMEM may be set by memcpy and other raw memory functions
   to allow accesses across subobject boundaries.  */

void
strlen_pass::maybe_warn_overflow (gimple *stmt, bool call_lhs, tree len,
				  strinfo *si, bool plus_one, bool rawmem)
{
  if (!len || warning_suppressed_p (stmt, OPT_Wstringop_overflow_))
    return;

  /* The DECL of the function performing the write if it is done
     by one.  */
  tree writefn = NULL_TREE;
  /* The destination expression involved in the store or call STMT.  */
  tree dest = NULL_TREE;

  if (is_gimple_assign (stmt))
    dest = gimple_assign_lhs (stmt);
  else if (is_gimple_call (stmt))
    {
      if (call_lhs)
	dest = gimple_call_lhs (stmt);
      else
	{
	  gcc_assert (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL));
	  dest = gimple_call_arg (stmt, 0);
	}

      if (!dest)
	return;
      writefn = gimple_call_fndecl (stmt);
    }
  else
    return;

  if (warning_suppressed_p (dest, OPT_Wstringop_overflow_))
    return;

  const int ostype = rawmem ? 0 : 1;

  /* Use maximum precision to avoid overflow in the addition below.
     Make sure all operands have the same precision to keep wide_int
     from ICE'ing.  */

  access_ref aref;
  /* The size of the destination region (which is smaller than
     the destination object for stores at a non-zero offset).  */
  tree destsize = compute_objsize (dest, stmt, ostype, &aref, &ptr_qry);

  if (!destsize)
    {
      aref.sizrng[0] = 0;
      aref.sizrng[1] = wi::to_offset (max_object_size ());
    }

  /* Return early if the DESTSIZE size expression is the same as LEN
     and the offset into the destination is zero.  This might happen
     in the case of a pair of malloc and memset calls to allocate
     an object and clear it as if by calloc.  */
  if (destsize == len && !plus_one
      && aref.offrng[0] == 0 && aref.offrng[0] == aref.offrng[1])
    return;

  wide_int rng[2];
  if (!get_range (len, stmt, rng, ptr_qry.rvals))
    return;

  widest_int lenrng[2] =
    { widest_int::from (rng[0], SIGNED), widest_int::from (rng[1], SIGNED) };

  if (plus_one)
    {
      lenrng[0] += 1;
      lenrng[1] += 1;
    }

  /* The size of the remaining space in the destination computed
     as the size of the latter minus the offset into it.  */
  widest_int spcrng[2];
  {
    offset_int remrng[2];
    remrng[1] = aref.size_remaining (remrng);
    spcrng[0] = remrng[0] == -1 ? 0 : widest_int::from (remrng[0], UNSIGNED);
    spcrng[1] = widest_int::from (remrng[1], UNSIGNED);
  }

  if (wi::leu_p (lenrng[0], spcrng[0])
      && wi::leu_p (lenrng[1], spcrng[1]))
    return;

  location_t loc = gimple_or_expr_nonartificial_location (stmt, dest);
  bool warned = false;
  if (wi::leu_p (lenrng[0], spcrng[1]))
    {
      if (len != destsize
	  && (!si || rawmem || !is_strlen_related_p (si->ptr, len)))
	return;

      warned = (writefn
		? warning_at (loc, OPT_Wstringop_overflow_,
			      "%qD writing one too many bytes into a region "
			      "of a size that depends on %<strlen%>",
			      writefn)
		: warning_at (loc, OPT_Wstringop_overflow_,
			      "writing one too many bytes into a region "
			      "of a size that depends on %<strlen%>"));
    }
  else if (lenrng[0] == lenrng[1])
    {
      if (spcrng[0] == spcrng[1])
	warned = (writefn
		  ? warning_n (loc, OPT_Wstringop_overflow_,
			       lenrng[0].to_uhwi (),
			       "%qD writing %wu byte into a region "
			       "of size %wu",
			       "%qD writing %wu bytes into a region "
			       "of size %wu",
			       writefn, lenrng[0].to_uhwi (),
			       spcrng[0].to_uhwi ())
		  : warning_n (loc, OPT_Wstringop_overflow_,
			       lenrng[0].to_uhwi (),
			       "writing %wu byte into a region "
			       "of size %wu",
			       "writing %wu bytes into a region "
			       "of size %wu",
			       lenrng[0].to_uhwi (),
			       spcrng[0].to_uhwi ()));
      else
	warned = (writefn
		  ? warning_n (loc, OPT_Wstringop_overflow_,
			       lenrng[0].to_uhwi (),
			       "%qD writing %wu byte into a region "
			       "of size between %wu and %wu",
			       "%qD writing %wu bytes into a region "
			       "of size between %wu and %wu",
			       writefn, lenrng[0].to_uhwi (),
			       spcrng[0].to_uhwi (), spcrng[1].to_uhwi ())
		  : warning_n (loc, OPT_Wstringop_overflow_,
			       lenrng[0].to_uhwi (),
			       "writing %wu byte into a region "
			       "of size between %wu and %wu",
			       "writing %wu bytes into a region "
			       "of size between %wu and %wu",
			       lenrng[0].to_uhwi (),
			       spcrng[0].to_uhwi (), spcrng[1].to_uhwi ()));
    }
  else if (spcrng[0] == spcrng[1])
    warned = (writefn
	      ? warning_at (loc, OPT_Wstringop_overflow_,
			    "%qD writing between %wu and %wu bytes "
			    "into a region of size %wu",
			    writefn, lenrng[0].to_uhwi (),
			    lenrng[1].to_uhwi (),
			    spcrng[0].to_uhwi ())
	      : warning_at (loc, OPT_Wstringop_overflow_,
			    "writing between %wu and %wu bytes "
			    "into a region of size %wu",
			    lenrng[0].to_uhwi (),
			    lenrng[1].to_uhwi (),
			    spcrng[0].to_uhwi ()));
  else
    warned = (writefn
	      ? warning_at (loc, OPT_Wstringop_overflow_,
			    "%qD writing between %wu and %wu bytes "
			    "into a region of size between %wu and %wu",
			    writefn, lenrng[0].to_uhwi (),
			    lenrng[1].to_uhwi (),
			    spcrng[0].to_uhwi (), spcrng[1].to_uhwi ())
	      : warning_at (loc, OPT_Wstringop_overflow_,
			    "writing between %wu and %wu bytes "
			    "into a region of size between %wu and %wu",
			    lenrng[0].to_uhwi (),
			    lenrng[1].to_uhwi (),
			    spcrng[0].to_uhwi (), spcrng[1].to_uhwi ()));

  if (!warned)
    return;

  suppress_warning (stmt, OPT_Wstringop_overflow_);

  aref.inform_access (access_write_only);
}

/* Convenience wrapper for the above.  */

void
strlen_pass::maybe_warn_overflow (gimple *stmt, bool call_lhs,
				  unsigned HOST_WIDE_INT len,
				  strinfo *si, bool plus_one, bool rawmem)
{
  tree tlen = build_int_cst (size_type_node, len);
  maybe_warn_overflow (stmt, call_lhs, tlen, si, plus_one, rawmem);
}

/* Handle a strlen call.  If strlen of the argument is known, replace
   the strlen call with the known value, otherwise remember that strlen
   of the argument is stored in the lhs SSA_NAME.  */

void
strlen_pass::handle_builtin_strlen ()
{
  gimple *stmt = gsi_stmt (m_gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL_TREE)
    return;

  location_t loc = gimple_location (stmt);
  tree callee = gimple_call_fndecl (stmt);
  tree src = gimple_call_arg (stmt, 0);
  tree bound = (DECL_FUNCTION_CODE (callee) == BUILT_IN_STRNLEN
		? gimple_call_arg (stmt, 1) : NULL_TREE);
  int idx = get_stridx (src, stmt);
  if (idx || (bound && integer_zerop (bound)))
    {
      strinfo *si = NULL;
      tree rhs;

      if (idx < 0)
	rhs = build_int_cst (TREE_TYPE (lhs), ~idx);
      else if (idx == 0)
	rhs = bound;
      else
	{
	  rhs = NULL_TREE;
	  si = get_strinfo (idx);
	  if (si != NULL)
	    {
	      rhs = get_string_length (si);
	      /* For strnlen, if bound is constant, even if si is not known
		 to be zero terminated, if we know at least bound bytes are
		 not zero, the return value will be bound.  */
	      if (rhs == NULL_TREE
		  && bound != NULL_TREE
		  && TREE_CODE (bound) == INTEGER_CST
		  && si->nonzero_chars != NULL_TREE
		  && TREE_CODE (si->nonzero_chars) == INTEGER_CST
		  && tree_int_cst_le (bound, si->nonzero_chars))
		rhs = bound;
	    }
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

	  gimplify_and_update_call_from_tree (&m_gsi, rhs);
	  stmt = gsi_stmt (m_gsi);
	  update_stmt (stmt);
	  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	    {
	      fprintf (dump_file, "into: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }

	  if (si != NULL
	      /* Don't update anything for strnlen.  */
	      && bound == NULL_TREE
	      && TREE_CODE (si->nonzero_chars) != SSA_NAME
	      && TREE_CODE (si->nonzero_chars) != INTEGER_CST
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    {
	      si = unshare_strinfo (si);
	      si->nonzero_chars = lhs;
	      gcc_assert (si->full_string_p);
	    }

	  if (strlen_to_stridx
	      && (bound == NULL_TREE
		  /* For strnlen record this only if the call is proven
		     to return the same value as strlen would.  */
		  || (TREE_CODE (bound) == INTEGER_CST
		      && TREE_CODE (rhs) == INTEGER_CST
		      && tree_int_cst_lt (rhs, bound))))
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
	      if (old && TREE_CODE (old) == INTEGER_CST)
		{
		  old = fold_convert_loc (loc, TREE_TYPE (lhs), old);
		  tree adj = fold_build2_loc (loc, MINUS_EXPR,
					      TREE_TYPE (lhs), lhs, old);
		  adjust_related_strinfos (loc, si, adj);
		  /* Use the constant minimum length as the lower bound
		     of the non-constant length.  */
		  wide_int min = wi::to_wide (old);
		  wide_int max
		    = wi::to_wide (TYPE_MAX_VALUE (ptrdiff_type_node)) - 2;
		  if (wi::gtu_p (min, max))
		    max = wi::to_wide (TYPE_MAX_VALUE (TREE_TYPE (lhs)));
		  set_strlen_range (lhs, min, max);
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
      if (!bound)
	{
	  /* Only store the new length information for calls to strlen(),
	     not for those to strnlen().  */
	  strinfo *si = new_strinfo (src, idx, lhs, true);
	  set_strinfo (idx, si);
	  find_equal_ptrs (src, idx);
	}

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
	    gimplify_and_update_call_from_tree (&m_gsi, ret);
	    stmt = gsi_stmt (m_gsi);
	    update_stmt (stmt);
	    if (dump_file && (dump_flags & TDF_DETAILS) != 0)
	      {
		fprintf (dump_file, "into: ");
		print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      }
	  }

      if (strlen_to_stridx && !bound)
	strlen_to_stridx->put (lhs, stridx_strlenloc (idx, loc));
    }
}

/* Handle a strchr call.  If strlen of the first argument is known, replace
   the strchr (x, 0) call with the endptr or x + strlen, otherwise remember
   that lhs of the call is endptr and strlen of the argument is endptr - x.  */

void
strlen_pass::handle_builtin_strchr ()
{
  gimple *stmt = gsi_stmt (m_gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL_TREE)
    return;

  if (!integer_zerop (gimple_call_arg (stmt, 1)))
    return;

  tree src = gimple_call_arg (stmt, 0);

  /* Avoid folding if the first argument is not a nul-terminated array.
     Defer warning until later.  */
  if (!check_nul_terminated_array (NULL_TREE, src))
    return;

  int idx = get_stridx (src, stmt);
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
	  gimplify_and_update_call_from_tree (&m_gsi, rhs);
	  stmt = gsi_stmt (m_gsi);
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
   memcpy.  Uses RVALS to determine range information.  */

void
strlen_pass::handle_builtin_strcpy (built_in_function bcode)
{
  int idx, didx;
  tree src, dst, srclen, len, lhs, type, fn, oldlen;
  bool success;
  gimple *stmt = gsi_stmt (m_gsi);
  strinfo *si, *dsi, *olddsi, *zsi;
  location_t loc;

  src = gimple_call_arg (stmt, 1);
  dst = gimple_call_arg (stmt, 0);
  lhs = gimple_call_lhs (stmt);
  idx = get_stridx (src, stmt);
  si = NULL;
  if (idx > 0)
    si = get_strinfo (idx);

  didx = get_stridx (dst, stmt);
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

  maybe_warn_overflow (stmt, false, srclen, olddsi, true);

  if (olddsi != NULL)
    adjust_last_stmt (olddsi, stmt, false);

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
	 computation.  If string length of dst will be needed, it
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
	  check_bounds_or_overlap (stmt, olddsi->ptr, src, oldlen, NULL_TREE);
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

  /* Disable warning for the transformed statement?  */
  opt_code no_warning_opt = no_warning;

  if (const strinfo *chksi = si ? olddsi ? olddsi : dsi : NULL)
    {
      no_warning_opt = check_bounds_or_overlap (stmt, chksi->ptr, si->ptr,
						NULL_TREE, len);
      if (no_warning_opt)
	suppress_warning (stmt, no_warning_opt);
    }

  if (fn == NULL_TREE)
    return;

  len = force_gimple_operand_gsi (&m_gsi, len, true, NULL_TREE, true,
				  GSI_SAME_STMT);
  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
    {
      fprintf (dump_file, "Optimizing: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  if (gimple_call_num_args (stmt) == 2)
    success = update_gimple_call (&m_gsi, fn, 3, dst, src, len);
  else
    success = update_gimple_call (&m_gsi, fn, 4, dst, src, len,
				  gimple_call_arg (stmt, 2));
  if (success)
    {
      stmt = gsi_stmt (m_gsi);
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

  if (no_warning_opt)
    suppress_warning (stmt, no_warning_opt);
}

/* Check the size argument to the built-in forms of stpncpy and strncpy
   for out-of-bounds offsets or overlapping access, and to see if the
   size argument is derived from a call to strlen() on the source argument,
   and if so, issue an appropriate warning.  */

void
strlen_pass::handle_builtin_strncat (built_in_function)
{
  /* Same as stxncpy().  */
  handle_builtin_stxncpy_strncat (true);
}

/* Return true if LEN depends on a call to strlen(SRC) in an interesting
   way.  LEN can either be an integer expression, or a pointer (to char).
   When it is the latter (such as in recursive calls to self) it is
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

  if (TREE_CODE (src) == SSA_NAME)
    {
      gimple *srcdef = SSA_NAME_DEF_STMT (src);
      if (is_gimple_assign (srcdef))
	{
	  /* Handle bitwise AND used in conversions from wider size_t
	     to narrower unsigned types.  */
	  tree_code code = gimple_assign_rhs_code (srcdef);
	  if (code == BIT_AND_EXPR
	      || code == NOP_EXPR)
	    return is_strlen_related_p (gimple_assign_rhs1 (srcdef), len);

	  return false;
	}

      if (gimple_call_builtin_p (srcdef, BUILT_IN_NORMAL))
	{
	  /* If SRC is the result of a call to an allocation function
	     or strlen, use the function's argument instead.  */
	  tree func = gimple_call_fndecl (srcdef);
	  built_in_function code = DECL_FUNCTION_CODE (func);
	  if (code == BUILT_IN_ALLOCA
	      || code == BUILT_IN_ALLOCA_WITH_ALIGN
	      || code == BUILT_IN_MALLOC
	      || code == BUILT_IN_STRLEN)
	    return is_strlen_related_p (gimple_call_arg (srcdef, 0), len);

	  /* FIXME: Handle other functions with attribute alloc_size.  */
	  return false;
	}
    }

  gimple *lendef = SSA_NAME_DEF_STMT (len);
  if (!lendef)
    return false;

  if (is_gimple_call (lendef))
    {
      tree func = gimple_call_fndecl (lendef);
      if (!valid_builtin_call (lendef)
	  || DECL_FUNCTION_CODE (func) != BUILT_IN_STRLEN)
	return false;

      tree arg = gimple_call_arg (lendef, 0);
      return is_strlen_related_p (src, arg);
    }

  if (!is_gimple_assign (lendef))
    return false;

  tree_code code = gimple_assign_rhs_code (lendef);
  tree rhs1 = gimple_assign_rhs1 (lendef);
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

  if (tree rhs2 = gimple_assign_rhs2 (lendef))
    {
      /* Integer subtraction is considered strlen-related when both
	 arguments are integers and second one is strlen-related.  */
      rhstype = TREE_TYPE (rhs2);
      if (INTEGRAL_TYPE_P (rhstype) && code == MINUS_EXPR)
	return is_strlen_related_p (src, rhs2);
    }

  return false;
}

/* Called by handle_builtin_stxncpy_strncat and by
   gimple_fold_builtin_strncpy in gimple-fold.cc.
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
maybe_diag_stxncpy_trunc (gimple_stmt_iterator gsi, tree src, tree cnt,
			  pointer_query *ptr_qry /* = NULL */)
{
  gimple *stmt = gsi_stmt (gsi);
  if (warning_suppressed_p (stmt, OPT_Wstringop_truncation))
    return false;

  wide_int cntrange[2];
  int_range_max r;
  if (!get_range_query (cfun)->range_of_expr (r, cnt)
      || r.varying_p ()
      || r.undefined_p ())
    return false;

  tree min, max;
  value_range_kind kind = get_legacy_range (r, min, max);
  cntrange[0] = wi::to_wide (min);
  cntrange[1] = wi::to_wide (max);
  if (kind == VR_ANTI_RANGE)
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

  /* Negative value is the constant string length.  If it's less than
     the lower bound there is no truncation.  Avoid calling get_stridx()
     when ssa_ver_to_stridx is empty.  That implies the caller isn't
     running under the control of this pass and ssa_ver_to_stridx hasn't
     been created yet.  */
  int sidx = ssa_ver_to_stridx.length () ? get_stridx (src, stmt) : 0;
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

  /* Likewise, if the destination refers to an array/pointer declared
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
      c_strlen_data lendata = { };
      /* Set MAXBOUND to an arbitrary non-null non-integer node as a request
	 to have it set to the length of the longest string in a PHI.  */
      lendata.maxbound = src;
      get_range_strlen (src, &lendata, /* eltsize = */1);
      if (TREE_CODE (lendata.minlen) == INTEGER_CST
	  && TREE_CODE (lendata.maxbound) == INTEGER_CST)
	{
	  /* When LENDATA.MAXLEN is unknown, reset LENDATA.MINLEN
	     which stores the length of the shortest known string.  */
	  if (integer_all_onesp (lendata.maxlen))
	    lenrange[0] = wi::shwi (0, prec);
	  else
	    lenrange[0] = wi::to_wide (lendata.minlen, prec);
	  lenrange[1] = wi::to_wide (lendata.maxbound, prec);
	}
      else
	{
	  lenrange[0] = wi::shwi (0, prec);
	  lenrange[1] = wi::shwi (-1, prec);
	}
    }

  location_t callloc = gimple_or_expr_nonartificial_location (stmt, dst);
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

      /* Set to true for strncat whose bound is derived from the length
	 of the destination (the expected usage pattern).  */
      bool cat_dstlen_bounded = false;
      if (DECL_FUNCTION_CODE (func) == BUILT_IN_STRNCAT)
	cat_dstlen_bounded = is_strlen_related_p (dst, cnt);

      if (lenrange[0] == cntrange[1] && cntrange[0] == cntrange[1])
	return warning_n (callloc, OPT_Wstringop_truncation,
			  cntrange[0].to_uhwi (),
			  "%qD output truncated before terminating "
			  "nul copying %E byte from a string of the "
			  "same length",
			  "%qD output truncated before terminating nul "
			  "copying %E bytes from a string of the same "
			  "length",
			  func, cnt);
      else if (!cat_dstlen_bounded)
	{
	  if (wi::geu_p (lenrange[0], cntrange[1]))
	    {
	      /* The shortest string is longer than the upper bound of
		 the count so the truncation is certain.  */
	      if (cntrange[0] == cntrange[1])
		return warning_n (callloc, OPT_Wstringop_truncation,
				  cntrange[0].to_uhwi (),
				  "%qD output truncated copying %E byte "
				  "from a string of length %wu",
				  "%qD output truncated copying %E bytes "
				  "from a string of length %wu",
				  func, cnt, lenrange[0].to_uhwi ());

	      return warning_at (callloc, OPT_Wstringop_truncation,
				 "%qD output truncated copying between %wu "
				 "and %wu bytes from a string of length %wu",
				 func, cntrange[0].to_uhwi (),
				 cntrange[1].to_uhwi (), lenrange[0].to_uhwi ());
	    }
	  else if (wi::geu_p (lenrange[1], cntrange[1]))
	    {
	      /* The longest string is longer than the upper bound of
		 the count so the truncation is possible.  */
	      if (cntrange[0] == cntrange[1])
		return warning_n (callloc, OPT_Wstringop_truncation,
				  cntrange[0].to_uhwi (),
				  "%qD output may be truncated copying %E "
				  "byte from a string of length %wu",
				  "%qD output may be truncated copying %E "
				  "bytes from a string of length %wu",
				  func, cnt, lenrange[1].to_uhwi ());

	      return warning_at (callloc, OPT_Wstringop_truncation,
				 "%qD output may be truncated copying between "
				 "%wu and %wu bytes from a string of length %wu",
				 func, cntrange[0].to_uhwi (),
				 cntrange[1].to_uhwi (), lenrange[1].to_uhwi ());
	    }
	}

      if (!cat_dstlen_bounded
	  && cntrange[0] != cntrange[1]
	  && wi::leu_p (cntrange[0], lenrange[0])
	  && wi::leu_p (cntrange[1], lenrange[0] + 1))
	{
	  /* If the source (including the terminating nul) is longer than
	     the lower bound of the specified count but shorter than the
	     upper bound the copy may (but need not) be truncated.  */
	  return warning_at (callloc, OPT_Wstringop_truncation,
			     "%qD output may be truncated copying between "
			     "%wu and %wu bytes from a string of length %wu",
			     func, cntrange[0].to_uhwi (),
			     cntrange[1].to_uhwi (), lenrange[0].to_uhwi ());
	}
    }

  access_ref aref;
  if (tree dstsize = compute_objsize (dst, stmt, 1, &aref, ptr_qry))
    {
      /* The source length is unknown.  Try to determine the destination
	 size and see if it matches the specified bound.  If not, bail.
	 Otherwise go on to see if it should be diagnosed for possible
	 truncation.  */
      if (!dstsize)
	return false;

      if (wi::to_wide (dstsize) != cntrange[1])
	return false;

      /* Avoid warning for strncpy(a, b, N) calls where the following
	 equalities hold:
	   N == sizeof a && N == sizeof b */
      if (tree srcsize = compute_objsize (src, stmt, 1, &aref, ptr_qry))
	if (wi::to_wide (srcsize) == cntrange[1])
	  return false;

      if (cntrange[0] == cntrange[1])
	return warning_at (callloc, OPT_Wstringop_truncation,
			   "%qD specified bound %E equals destination size",
			   func, cnt);
    }

  return false;
}

/* Check the arguments to the built-in forms of stpncpy, strncpy, and
   strncat, for out-of-bounds offsets or overlapping access, and to see
   if the size is derived from calling strlen() on the source argument,
   and if so, issue the appropriate warning.
   APPEND_P is true for strncat.  */

void
strlen_pass::handle_builtin_stxncpy_strncat (bool append_p)
{
  if (!strlen_to_stridx)
    return;

  gimple *stmt = gsi_stmt (m_gsi);

  tree dst = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);
  /* An upper bound of the size of the destination.  */
  tree dstsize = NULL_TREE;
  /* The length of the destination and source strings (plus 1 for those
     whose FULL_STRING_P is set, i.e., whose length is exact rather than
     a lower bound).  */
  tree dstlenp1 = NULL_TREE, srclenp1 = NULL_TREE;;

  int didx = get_stridx (dst, stmt);
  if (strinfo *sidst = didx > 0 ? get_strinfo (didx) : NULL)
    {
      /* Compute the size of the destination string including the nul
	 if it is known to be nul-terminated.  */
      if (sidst->nonzero_chars)
	{
	  if (sidst->full_string_p)
	    {
	      /* String is known to be nul-terminated.  */
	      tree type = TREE_TYPE (sidst->nonzero_chars);
	      dstlenp1 = fold_build2 (PLUS_EXPR, type, sidst->nonzero_chars,
				     build_int_cst (type, 1));
	    }
	  else
	    dstlenp1 = sidst->nonzero_chars;
	}
      else if (TREE_CODE (sidst->ptr) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (sidst->ptr);
	  dstsize = gimple_call_alloc_size (def_stmt);
	}

      dst = sidst->ptr;
    }

  int sidx = get_stridx (src, stmt);
  strinfo *sisrc = sidx > 0 ? get_strinfo (sidx) : NULL;
  if (sisrc)
    {
      /* strncat() and strncpy() can modify the source string by writing
	 over the terminating nul so SISRC->DONT_INVALIDATE must be left
	 clear.  */

      /* Compute the size of the source string including the terminating
	 nul if its known to be nul-terminated.  */
      if (sisrc->nonzero_chars)
	{
	  if (sisrc->full_string_p)
	    {
	      tree type = TREE_TYPE (sisrc->nonzero_chars);
	      srclenp1 = fold_build2 (PLUS_EXPR, type, sisrc->nonzero_chars,
				     build_int_cst (type, 1));
	    }
	  else
	    srclenp1 = sisrc->nonzero_chars;
	}

	src = sisrc->ptr;
    }
  else
    srclenp1 = NULL_TREE;

  opt_code opt = check_bounds_or_overlap (stmt, dst, src, dstlenp1, srclenp1);
  if (opt != no_warning)
    {
      suppress_warning (stmt, opt);
      return;
    }

  /* If the length argument was computed from strlen(S) for some string
     S retrieve the strinfo index for the string (PSS->FIRST) along with
     the location of the strlen() call (PSS->SECOND).  */
  stridx_strlenloc *pss = strlen_to_stridx->get (len);
  if (!pss || pss->first <= 0)
    {
      if (maybe_diag_stxncpy_trunc (m_gsi, src, len))
	suppress_warning (stmt, OPT_Wstringop_truncation);

      return;
    }

  /* Retrieve the strinfo data for the string S that LEN was computed
     from as some function F of strlen (S) (i.e., LEN need not be equal
     to strlen(S)).  */
  strinfo *silen = get_strinfo (pss->first);

  location_t callloc = gimple_or_expr_nonartificial_location (stmt, dst);

  tree func = gimple_call_fndecl (stmt);

  bool warned = false;

  /* When -Wstringop-truncation is set, try to determine truncation
     before diagnosing possible overflow.  Truncation is implied by
     the LEN argument being equal to strlen(SRC), regardless of
     whether its value is known.  Otherwise, when appending, or
     when copying into a destination of known size, issue the more
     generic -Wstringop-overflow which triggers for LEN arguments
     that in any meaningful way depend on strlen(SRC).  */
  if (!append_p
      && sisrc == silen
      && is_strlen_related_p (src, len)
      && warning_at (callloc, OPT_Wstringop_truncation,
		     "%qD output truncated before terminating nul "
		     "copying as many bytes from a string as its length",
		     func))
    warned = true;
  else if ((append_p || !dstsize || len == dstlenp1)
	   && silen && is_strlen_related_p (src, silen->ptr))
    {
      /* Issue -Wstringop-overflow when appending or when writing into
	 a destination of a known size.  Otherwise, when copying into
	 a destination of an unknown size, it's truncation.  */
      opt_code opt = (append_p || dstsize
		      ? OPT_Wstringop_overflow_ : OPT_Wstringop_truncation);
      warned = warning_at (callloc, opt,
			   "%qD specified bound depends on the length "
			   "of the source argument",
			   func);
    }
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
   call.  Uses RVALS to determine range information.  */

void
strlen_pass::handle_builtin_memcpy (built_in_function bcode)
{
  tree lhs, oldlen, newlen;
  gimple *stmt = gsi_stmt (m_gsi);
  strinfo *si, *dsi;

  tree len = gimple_call_arg (stmt, 2);
  tree src = gimple_call_arg (stmt, 1);
  tree dst = gimple_call_arg (stmt, 0);

  int didx = get_stridx (dst, stmt);
  strinfo *olddsi = NULL;
  if (didx > 0)
    olddsi = get_strinfo (didx);
  else if (didx < 0)
    return;

  if (olddsi != NULL
      && !integer_zerop (len))
    {
      maybe_warn_overflow (stmt, false, len, olddsi, false, true);
      if (tree_fits_uhwi_p (len))
	adjust_last_stmt (olddsi, stmt, false);
    }

  int idx = get_stridx (src, stmt);
  if (idx == 0)
    return;

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

  if (!full_string_p
      && olddsi
      && olddsi->nonzero_chars
      && TREE_CODE (olddsi->nonzero_chars) == INTEGER_CST
      && tree_int_cst_le (newlen, olddsi->nonzero_chars))
    {
      /* The SRC substring being written strictly overlaps
	 a subsequence of the existing string OLDDSI.  */
      newlen = olddsi->nonzero_chars;
      full_string_p = olddsi->full_string_p;
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

void
strlen_pass::handle_builtin_strcat (built_in_function bcode)
{
  int idx, didx;
  tree srclen, args, type, fn, objsz, endptr;
  bool success;
  gimple *stmt = gsi_stmt (m_gsi);
  strinfo *si, *dsi;
  location_t loc = gimple_location (stmt);

  tree src = gimple_call_arg (stmt, 1);
  tree dst = gimple_call_arg (stmt, 0);

  /* Bail if the source is the same as destination.  It will be diagnosed
     elsewhere.  */
  if (operand_equal_p (src, dst, 0))
    return;

  tree lhs = gimple_call_lhs (stmt);

  didx = get_stridx (dst, stmt);
  if (didx < 0)
    return;

  dsi = NULL;
  if (didx > 0)
    dsi = get_strinfo (didx);

  srclen = NULL_TREE;
  si = NULL;
  idx = get_stridx (src, stmt);
  if (idx < 0)
    srclen = build_int_cst (size_type_node, ~idx);
  else if (idx > 0)
    {
      si = get_strinfo (idx);
      if (si != NULL)
	srclen = get_string_length (si);
    }

  /* Disable warning for the transformed statement?  */
  opt_code no_warning_opt = no_warning;

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
	no_warning_opt = check_bounds_or_overlap (stmt, dst, sptr, NULL_TREE,
						  slen);
	if (no_warning_opt)
	  suppress_warning (stmt, no_warning_opt);
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
      tree one = build_int_cst (type, 1);
      srcsize = fold_build2 (PLUS_EXPR, type, srcsize, one);
      tree dstsize = fold_build2 (PLUS_EXPR, type, dstlen, one);
      tree sptr = si && si->ptr ? si->ptr : src;

      no_warning_opt = check_bounds_or_overlap (stmt, dst, sptr, dstsize,
						srcsize);
      if (no_warning_opt)
	suppress_warning (stmt, no_warning_opt);
    }

  tree len = NULL_TREE;
  if (srclen != NULL_TREE)
    {
      args = TYPE_ARG_TYPES (TREE_TYPE (fn));
      type = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));

      len = fold_convert_loc (loc, type, unshare_expr (srclen));
      len = fold_build2_loc (loc, PLUS_EXPR, type, len,
			     build_int_cst (type, 1));
      len = force_gimple_operand_gsi (&m_gsi, len, true, NULL_TREE, true,
				      GSI_SAME_STMT);
    }
  if (endptr)
    dst = fold_convert_loc (loc, TREE_TYPE (dst), unshare_expr (endptr));
  else
    dst = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (dst), dst,
			   fold_convert_loc (loc, sizetype,
					     unshare_expr (dstlen)));
  dst = force_gimple_operand_gsi (&m_gsi, dst, true, NULL_TREE, true,
				  GSI_SAME_STMT);
  if (objsz)
    {
      objsz = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (objsz), objsz,
			       fold_convert_loc (loc, TREE_TYPE (objsz),
						 unshare_expr (dstlen)));
      objsz = force_gimple_operand_gsi (&m_gsi, objsz, true, NULL_TREE, true,
					GSI_SAME_STMT);
    }
  if (dump_file && (dump_flags & TDF_DETAILS) != 0)
    {
      fprintf (dump_file, "Optimizing: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  if (srclen != NULL_TREE)
    success = update_gimple_call (&m_gsi, fn, 3 + (objsz != NULL_TREE),
				  dst, src, len, objsz);
  else
    success = update_gimple_call (&m_gsi, fn, 2 + (objsz != NULL_TREE),
				  dst, src, objsz);
  if (success)
    {
      stmt = gsi_stmt (m_gsi);
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

  if (no_warning_opt)
    suppress_warning (stmt, no_warning_opt);
}

/* Handle a call to an allocation function like alloca, malloc or calloc,
   or an ordinary allocation function declared with attribute alloc_size.  */

void
strlen_pass::handle_alloc_call (built_in_function bcode)
{
  gimple *stmt = gsi_stmt (m_gsi);
  tree lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;

  gcc_assert (get_stridx (lhs, stmt) == 0);
  int idx = new_stridx (lhs);
  tree length = NULL_TREE;
  if (bcode == BUILT_IN_CALLOC)
    length = build_int_cst (size_type_node, 0);
  strinfo *si = new_strinfo (lhs, idx, length, length != NULL_TREE);
  if (bcode == BUILT_IN_CALLOC)
    {
      /* Only set STMT for calloc and malloc.  */
      si->stmt = stmt;
      /* Only set ENDPTR for calloc.  */
      si->endptr = lhs;
    }
  else if (bcode == BUILT_IN_MALLOC)
    si->stmt = stmt;

  /* Set ALLOC is set for all allocation functions.  */
  si->alloc = stmt;
  set_strinfo (idx, si);
  si->writable = true;
  si->dont_invalidate = true;
}

/* Handle a call to memset.
   After a call to calloc, memset(,0,) is unnecessary.
   memset(malloc(n),0,n) is calloc(n,1).
   return true when the call is transformed, false otherwise.
   When nonnull uses RVALS to determine range information.  */

bool
strlen_pass::handle_builtin_memset (bool *zero_write)
{
  gimple *memset_stmt = gsi_stmt (m_gsi);
  tree ptr = gimple_call_arg (memset_stmt, 0);
  tree memset_val = gimple_call_arg (memset_stmt, 1);
  tree memset_size = gimple_call_arg (memset_stmt, 2);

  /* Set to the non-constant offset added to PTR.  */
  wide_int offrng[2];
  int idx1 = get_stridx (ptr, memset_stmt, offrng, ptr_qry.rvals);
  if (idx1 == 0
      && TREE_CODE (memset_val) == INTEGER_CST
      && ((TREE_CODE (memset_size) == INTEGER_CST
	   && !integer_zerop (memset_size))
	  || TREE_CODE (memset_size) == SSA_NAME))
    {
      unsigned HOST_WIDE_INT mask = (HOST_WIDE_INT_1U << CHAR_TYPE_SIZE) - 1;
      bool full_string_p = (wi::to_wide (memset_val) & mask) == 0;

      /* We only handle symbolic lengths when writing non-zero values.  */
      if (full_string_p && TREE_CODE (memset_size) != INTEGER_CST)
	return false;

      idx1 = new_stridx (ptr);
      if (idx1 == 0)
	return false;
      tree newlen;
      if (full_string_p)
	newlen = build_int_cst (size_type_node, 0);
      else if (TREE_CODE (memset_size) == INTEGER_CST)
	newlen = fold_convert (size_type_node, memset_size);
      else
	newlen = memset_size;

      strinfo *dsi = new_strinfo (ptr, idx1, newlen, full_string_p);
      set_strinfo (idx1, dsi);
      find_equal_ptrs (ptr, idx1);
      dsi->dont_invalidate = true;
      dsi->writable = true;
      return false;
    }

  if (idx1 <= 0)
    return false;
  strinfo *si1 = get_strinfo (idx1);
  if (!si1)
    return false;
  gimple *alloc_stmt = si1->alloc;
  if (!alloc_stmt || !is_gimple_call (alloc_stmt))
    return false;
  tree callee1 = gimple_call_fndecl (alloc_stmt);
  if (!valid_builtin_call (alloc_stmt))
    return false;
  tree alloc_size = gimple_call_arg (alloc_stmt, 0);

  /* Check for overflow.  */
  maybe_warn_overflow (memset_stmt, false, memset_size, NULL, false, true);

  /* Bail when there is no statement associated with the destination
     (the statement may be null even when SI1->ALLOC is not).  */
  if (!si1->stmt)
    return false;

  /* Avoid optimizing if store is at a variable offset from the beginning
     of the allocated object.  */
  if (offrng[0] != 0 || offrng[0] != offrng[1])
    return false;

  /* Bail when the call writes a non-zero value.  */
  if (!integer_zerop (memset_val))
    return false;

  /* Let the caller know the memset call cleared the destination.  */
  *zero_write = true;

  enum built_in_function code1 = DECL_FUNCTION_CODE (callee1);
  if (code1 == BUILT_IN_CALLOC)
    /* Not touching alloc_stmt */ ;
  else if (code1 == BUILT_IN_MALLOC
	   && operand_equal_p (memset_size, alloc_size, 0))
    {
      /* Replace the malloc + memset calls with calloc.  */
      gimple_stmt_iterator gsi1 = gsi_for_stmt (si1->stmt);
      update_gimple_call (&gsi1, builtin_decl_implicit (BUILT_IN_CALLOC), 2,
			  alloc_size, build_one_cst (size_type_node));
      si1->nonzero_chars = build_int_cst (size_type_node, 0);
      si1->full_string_p = true;
      si1->stmt = gsi_stmt (gsi1);
    }
  else
    return false;
  tree lhs = gimple_call_lhs (memset_stmt);
  unlink_stmt_vdef (memset_stmt);
  if (lhs)
    {
      gimple *assign = gimple_build_assign (lhs, ptr);
      gsi_replace (&m_gsi, assign, false);
    }
  else
    {
      gsi_remove (&m_gsi, true);
      release_defs (memset_stmt);
    }

  return true;
}

/* Return first such statement if RES is used in statements testing its
   equality to zero, and null otherwise.  If EXCLUSIVE is true, return
   nonnull if and only RES is used in such expressions exclusively and
   in none other.  */

gimple *
use_in_zero_equality (tree res, bool exclusive)
{
  gimple *first_use = NULL;

  use_operand_p use_p;
  imm_use_iterator iter;

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
		{
		  if (exclusive)
		    return NULL;
		  continue;
		}
	    }
	  else if (code == EQ_EXPR || code == NE_EXPR)
	    {
	      if (!integer_zerop (gimple_assign_rhs2 (use_stmt)))
		{
		  if (exclusive)
		    return NULL;
		  continue;
		}
            }
	  else if (exclusive)
	    return NULL;
	  else
	    continue;
	}
      else if (gimple_code (use_stmt) == GIMPLE_COND)
	{
	  tree_code code = gimple_cond_code (use_stmt);
	  if ((code != EQ_EXPR && code != NE_EXPR)
	      || !integer_zerop (gimple_cond_rhs (use_stmt)))
	    {
	      if (exclusive)
		return NULL;
	      continue;
	    }
	}
      else if (exclusive)
	return NULL;
      else
	continue;

      if (!first_use)
	first_use = use_stmt;
    }

  return first_use;
}

/* Handle a call to memcmp.  We try to handle small comparisons by
   converting them to load and compare, and replacing the call to memcmp
   with a __builtin_memcmp_eq call where possible.
   return true when call is transformed, return false otherwise.  */

bool
strlen_pass::handle_builtin_memcmp ()
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (m_gsi));
  tree res = gimple_call_lhs (stmt);

  if (!res || !use_in_zero_equality (res))
    return false;

  tree arg1 = gimple_call_arg (stmt, 0);
  tree arg2 = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);
  unsigned HOST_WIDE_INT leni;

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
	  location_t loc = gimple_location (stmt);
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
	  gimplify_and_update_call_from_tree (&m_gsi, res);
	  return true;
	}
    }

  gimple_call_set_fndecl (stmt, builtin_decl_explicit (BUILT_IN_MEMCMP_EQ));
  return true;
}

/* Given strinfo IDX for ARG, sets LENRNG[] to the range of lengths
   of the string(s) referenced by ARG if it can be determined.
   If the length cannot be determined, sets *SIZE to the size of
   the array the string is stored in, if any.  If no such array is
   known, sets *SIZE to -1.  When the strings are nul-terminated sets
   *NULTERM to true, otherwise to false.  When nonnull uses RVALS to
   determine range information. Returns true on success.  */

bool
strlen_pass::get_len_or_size (gimple *stmt, tree arg, int idx,
			      unsigned HOST_WIDE_INT lenrng[2],
			      unsigned HOST_WIDE_INT *size, bool *nulterm)
{
  /* Invalidate.  */
  *size = HOST_WIDE_INT_M1U;

  if (idx < 0)
    {
      /* IDX is the inverted constant string length.  */
      lenrng[0] = ~idx;
      lenrng[1] = lenrng[0];
      *nulterm = true;
      return true;
    }

  /* Set so that both LEN and ~LEN are invalid lengths, i.e., maximum
     possible length + 1.  */
  lenrng[0] = lenrng[1] = HOST_WIDE_INT_MAX;

  if (strinfo *si = idx ? get_strinfo (idx) : NULL)
    {
      /* FIXME: Handle all this in_range_strlen_dynamic.  */
      if (!si->nonzero_chars)
	;
      else if (tree_fits_uhwi_p (si->nonzero_chars))
	{
	  lenrng[0] = tree_to_uhwi (si->nonzero_chars);
	  *nulterm = si->full_string_p;
	  /* Set the upper bound only if the string is known to be
	     nul-terminated, otherwise leave it at maximum + 1.  */
	  if (*nulterm)
	    lenrng[1] = lenrng[0];
	}
      else if (TREE_CODE (si->nonzero_chars) == SSA_NAME)
	{
	  int_range_max r;
	  if (get_range_query (cfun)->range_of_expr (r, si->nonzero_chars)
	      && !r.undefined_p ()
	      && !r.varying_p ())
	    {
	      lenrng[0] = r.lower_bound ().to_uhwi ();
	      lenrng[1] = r.upper_bound ().to_uhwi ();
	      *nulterm = si->full_string_p;
	    }
	}
    }

  if (lenrng[0] != HOST_WIDE_INT_MAX)
    return true;

  /* Compute the minimum and maximum real or possible lengths.  */
  c_strlen_data lendata = { };
  /* Set MAXBOUND to an arbitrary non-null non-integer node as a request
     to have it set to the length of the longest string in a PHI.  */
  lendata.maxbound = arg;
  get_range_strlen_dynamic (arg, stmt, &lendata, ptr_qry);

  unsigned HOST_WIDE_INT maxbound = HOST_WIDE_INT_M1U;
  if (tree_fits_uhwi_p (lendata.maxbound)
      && !integer_all_onesp (lendata.maxbound))
    maxbound = tree_to_uhwi (lendata.maxbound);

  if (tree_fits_uhwi_p (lendata.minlen) && tree_fits_uhwi_p (lendata.maxlen))
    {
      unsigned HOST_WIDE_INT minlen = tree_to_uhwi (lendata.minlen);
      unsigned HOST_WIDE_INT maxlen = tree_to_uhwi (lendata.maxlen);

      /* The longest string in this data model.  */
      const unsigned HOST_WIDE_INT lenmax
	= tree_to_uhwi (max_object_size ()) - 2;

      if (maxbound == HOST_WIDE_INT_M1U)
	{
	  lenrng[0] = minlen;
	  lenrng[1] = maxlen;
	  *nulterm = minlen == maxlen;
	}
      else if (maxlen < lenmax)
	{
	  *size = maxbound + 1;
	  *nulterm = false;
	}
      else
	return false;

      return true;
    }

  if (maxbound != HOST_WIDE_INT_M1U
      && lendata.maxlen
      && !integer_all_onesp (lendata.maxlen))
    {
      /* Set *SIZE to LENDATA.MAXBOUND which is a conservative estimate
	 of the longest string based on the sizes of the arrays referenced
	 by ARG.  */
      *size = maxbound + 1;
      *nulterm = false;
      return true;
    }

  return false;
}

/* If IDX1 and IDX2 refer to strings A and B of unequal lengths, return
   the result of 0 == strncmp (A, B, BOUND) (which is the same as strcmp
   for a sufficiently large BOUND).  If the result is based on the length
   of one string being greater than the longest string that would fit in
   the array pointer to by the argument, set *PLEN and *PSIZE to
   the corresponding length (or its complement when the string is known
   to be at least as long and need not be nul-terminated) and size.
   Otherwise return null.  */

tree
strlen_pass::strxcmp_eqz_result (gimple *stmt, tree arg1, int idx1,
				 tree arg2, int idx2,
				 unsigned HOST_WIDE_INT bound,
				 unsigned HOST_WIDE_INT len[2],
				 unsigned HOST_WIDE_INT *psize)
{
  /* Determine the range the length of each string is in and whether it's
     known to be nul-terminated, or the size of the array it's stored in.  */
  bool nul1, nul2;
  unsigned HOST_WIDE_INT siz1, siz2;
  unsigned HOST_WIDE_INT len1rng[2], len2rng[2];
  if (!get_len_or_size (stmt, arg1, idx1, len1rng, &siz1, &nul1)
      || !get_len_or_size (stmt, arg2, idx2, len2rng, &siz2, &nul2))
    return NULL_TREE;

  /* BOUND is set to HWI_M1U for strcmp and less to strncmp, and LENiRNG
     to HWI_MAX when invalid.  Adjust the length of each string to consider
     to be no more than BOUND.  */
  if (len1rng[0] < HOST_WIDE_INT_MAX && len1rng[0] > bound)
    len1rng[0] = bound;
  if (len1rng[1] < HOST_WIDE_INT_MAX && len1rng[1] > bound)
    len1rng[1] = bound;
  if (len2rng[0] < HOST_WIDE_INT_MAX && len2rng[0] > bound)
    len2rng[0] = bound;
  if (len2rng[1] < HOST_WIDE_INT_MAX && len2rng[1] > bound)
    len2rng[1] = bound;

  /* Two empty strings are equal.  */
  if (len1rng[1] == 0 && len2rng[1] == 0)
    return integer_one_node;

  /* The strings are definitely unequal when the lower bound of the length
     of one of them is greater than the length of the longest string that
     would fit into the other array.  */
  if (len1rng[0] == HOST_WIDE_INT_MAX
      && len2rng[0] != HOST_WIDE_INT_MAX
      && ((len2rng[0] < bound && len2rng[0] >= siz1)
	  || len2rng[0] > siz1))
    {
      *psize = siz1;
      len[0] = len1rng[0];
      /* Set LEN[0] to the lower bound of ARG1's length when it's
	 nul-terminated or to the complement of its minimum length
	 otherwise,  */
      len[1] = nul2 ? len2rng[0] : ~len2rng[0];
      return integer_zero_node;
    }

  if (len2rng[0] == HOST_WIDE_INT_MAX
      && len1rng[0] != HOST_WIDE_INT_MAX
      && ((len1rng[0] < bound && len1rng[0] >= siz2)
	  || len1rng[0] > siz2))
    {
      *psize = siz2;
      len[0] = nul1 ? len1rng[0] : ~len1rng[0];
      len[1] = len2rng[0];
      return integer_zero_node;
    }

  /* The strings are also definitely unequal when their lengths are unequal
     and at least one is nul-terminated.  */
  if (len1rng[0] != HOST_WIDE_INT_MAX
      && len2rng[0] != HOST_WIDE_INT_MAX
      && ((len1rng[1] < len2rng[0] && nul1)
	  || (len2rng[1] < len1rng[0] && nul2)))
    {
      if (bound <= len1rng[0] || bound <= len2rng[0])
	*psize = bound;
      else
	*psize = HOST_WIDE_INT_M1U;

      len[0] = len1rng[0];
      len[1] = len2rng[0];
      return integer_zero_node;
    }

  /* The string lengths may be equal or unequal.  Even when equal and
     both strings nul-terminated, without the string contents there's
     no way to determine whether they are equal.  */
  return NULL_TREE;
}

/* Diagnose pointless calls to strcmp or strncmp STMT with string
   arguments of lengths LEN or size SIZ and (for strncmp) BOUND,
   whose result is used in equality expressions that evaluate to
   a constant due to one argument being longer than the size of
   the other.  */

static void
maybe_warn_pointless_strcmp (gimple *stmt, HOST_WIDE_INT bound,
			     unsigned HOST_WIDE_INT len[2],
			     unsigned HOST_WIDE_INT siz)
{
  tree lhs = gimple_call_lhs (stmt);
  gimple *use = use_in_zero_equality (lhs, /* exclusive = */ false);
  if (!use)
    return;

  bool at_least = false;

  /* Excessive LEN[i] indicates a lower bound.  */
  if (len[0] > HOST_WIDE_INT_MAX)
    {
      at_least = true;
      len[0] = ~len[0];
    }

  if (len[1] > HOST_WIDE_INT_MAX)
    {
      at_least = true;
      len[1] = ~len[1];
    }

  unsigned HOST_WIDE_INT minlen = MIN (len[0], len[1]);

  /* FIXME: Include a note pointing to the declaration of the smaller
     array.  */
  location_t stmt_loc = gimple_or_expr_nonartificial_location (stmt, lhs);

  tree callee = gimple_call_fndecl (stmt);
  bool warned = false;
  if (siz <= minlen && bound == -1)
    warned = warning_at (stmt_loc, OPT_Wstring_compare,
			 (at_least
			  ? G_("%qD of a string of length %wu or more and "
			       "an array of size %wu evaluates to nonzero")
			  : G_("%qD of a string of length %wu and an array "
			       "of size %wu evaluates to nonzero")),
			 callee, minlen, siz);
  else if (!at_least && siz <= HOST_WIDE_INT_MAX)
    {
      if (len[0] != HOST_WIDE_INT_MAX && len[1] != HOST_WIDE_INT_MAX)
	warned = warning_at (stmt_loc, OPT_Wstring_compare,
			     "%qD of strings of length %wu and %wu "
			     "and bound of %wu evaluates to nonzero",
			     callee, len[0], len[1], bound);
      else
	warned = warning_at (stmt_loc, OPT_Wstring_compare,
			     "%qD of a string of length %wu, an array "
			     "of size %wu and bound of %wu evaluates to "
			     "nonzero",
			     callee, minlen, siz, bound);
    }

  if (!warned)
    return;

  location_t use_loc = gimple_location (use);
  if (LOCATION_LINE (stmt_loc) != LOCATION_LINE (use_loc))
    inform (use_loc, "in this expression");
}


/* Optimize a call to strcmp or strncmp either by folding it to a constant
   when possible or by transforming the latter to the former.  Warn about
   calls where the length of one argument is greater than the size of
   the array to which the other argument points if the latter's length
   is not known.  Return true when the call has been transformed into
   another and false otherwise.  */

bool
strlen_pass::handle_builtin_string_cmp ()
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (m_gsi));
  tree lhs = gimple_call_lhs (stmt);

  if (!lhs)
    return false;

  tree arg1 = gimple_call_arg (stmt, 0);
  tree arg2 = gimple_call_arg (stmt, 1);
  int idx1 = get_stridx (arg1, stmt);
  int idx2 = get_stridx (arg2, stmt);

  /* For strncmp set to the value of the third argument if known.  */
  HOST_WIDE_INT bound = -1;
  tree len = NULL_TREE;
  /* Extract the strncmp bound.  */
  if (gimple_call_num_args (stmt) == 3)
    {
      len = gimple_call_arg (stmt, 2);
      if (tree_fits_shwi_p (len))
        bound = tree_to_shwi (len);

      /* If the bound argument is NOT known, do nothing.  */
      if (bound < 0)
	return false;
    }

  /* Avoid folding if either argument is not a nul-terminated array.
     Defer warning until later.  */
  if (!check_nul_terminated_array (NULL_TREE, arg1, len)
      || !check_nul_terminated_array (NULL_TREE, arg2, len))
    return false;

  {
    /* Set to the length of one argument (or its complement if it's
       the lower bound of a range) and the size of the array storing
       the other if the result is based on the former being equal to
       or greater than the latter.  */
    unsigned HOST_WIDE_INT len[2] = { HOST_WIDE_INT_MAX, HOST_WIDE_INT_MAX };
    unsigned HOST_WIDE_INT siz = HOST_WIDE_INT_M1U;

    /* Try to determine if the two strings are either definitely equal
       or definitely unequal and if so, either fold the result to zero
       (when equal) or set the range of the result to ~[0, 0] otherwise.  */
    if (tree eqz = strxcmp_eqz_result (stmt, arg1, idx1, arg2, idx2, bound,
				       len, &siz))
      {
	if (integer_zerop (eqz))
	  {
	    maybe_warn_pointless_strcmp (stmt, bound, len, siz);

	    /* When the lengths of the first two string arguments are
	       known to be unequal set the range of the result to non-zero.
	       This allows the call to be eliminated if its result is only
	       used in tests for equality to zero.  */
	    int_range_max nz;
	    nz.set_nonzero (TREE_TYPE (lhs));
	    set_range_info (lhs, nz);
	    return false;
	  }
	/* When the two strings are definitely equal (such as when they
	   are both empty) fold the call to the constant result.  */
	replace_call_with_value (&m_gsi, integer_zero_node);
	return true;
      }
  }

  /* Return if nothing is known about the strings pointed to by ARG1
     and ARG2.  */
  if (idx1 == 0 && idx2 == 0)
    return false;

  /* Determine either the length or the size of each of the strings,
     whichever is available.  */
  HOST_WIDE_INT cstlen1 = -1, cstlen2 = -1;
  HOST_WIDE_INT arysiz1 = -1, arysiz2 = -1;

  {
    unsigned HOST_WIDE_INT len1rng[2], len2rng[2];
    unsigned HOST_WIDE_INT arsz1, arsz2;
    bool nulterm[2];

    if (!get_len_or_size (stmt, arg1, idx1, len1rng, &arsz1, nulterm)
	|| !get_len_or_size (stmt, arg2, idx2, len2rng, &arsz2, nulterm + 1))
      return false;

    if (len1rng[0] == len1rng[1] && len1rng[0] < HOST_WIDE_INT_MAX)
      cstlen1 = len1rng[0];
    else if (arsz1 < HOST_WIDE_INT_M1U)
      arysiz1 = arsz1;

    if (len2rng[0] == len2rng[1] && len2rng[0] < HOST_WIDE_INT_MAX)
      cstlen2 = len2rng[0];
    else if (arsz2 < HOST_WIDE_INT_M1U)
      arysiz2 = arsz2;
  }

  /* Bail if neither the string length nor the size of the array
     it is stored in can be determined.  */
  if ((cstlen1 < 0 && arysiz1 < 0)
      || (cstlen2 < 0 && arysiz2 < 0)
      || (cstlen1 < 0 && cstlen2 < 0))
    return false;

  if (cstlen1 >= 0)
    ++cstlen1;
  if (cstlen2 >= 0)
    ++cstlen2;

  /* The exact number of characters to compare.  */
  HOST_WIDE_INT cmpsiz;
  if (cstlen1 >= 0 && cstlen2 >= 0)
    cmpsiz = MIN (cstlen1, cstlen2);
  else if (cstlen1 >= 0)
    cmpsiz = cstlen1;
  else
    cmpsiz = cstlen2;
  if (bound >= 0)
    cmpsiz = MIN (cmpsiz, bound);
  /* The size of the array in which the unknown string is stored.  */
  HOST_WIDE_INT varsiz = arysiz1 < 0 ? arysiz2 : arysiz1;

  if ((varsiz < 0 || cmpsiz < varsiz) && use_in_zero_equality (lhs))
    {
      /* If the known length is less than the size of the other array
	 and the strcmp result is only used to test equality to zero,
	 transform the call to the equivalent _eq call.  */
      if (tree fn = builtin_decl_implicit (bound < 0 ? BUILT_IN_STRCMP_EQ
					   : BUILT_IN_STRNCMP_EQ))
	{
	  tree n = build_int_cst (size_type_node, cmpsiz);
	  update_gimple_call (&m_gsi, fn, 3, arg1, arg2, n);
	  return true;
	}
    }

  return false;
}

/* Handle a POINTER_PLUS_EXPR statement.
   For p = "abcd" + 2; compute associated length, or if
   p = q + off is pointing to a '\0' character of a string, call
   zero_length_string on it.  */

void
strlen_pass::handle_pointer_plus ()
{
  gimple *stmt = gsi_stmt (m_gsi);
  tree lhs = gimple_assign_lhs (stmt), off;
  int idx = get_stridx (gimple_assign_rhs1 (stmt), stmt);
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
      gimple_assign_set_rhs_with_ops (&m_gsi, rhs_code, si->endptr);
      gcc_assert (gsi_stmt (m_gsi) == stmt);
      update_stmt (stmt);
    }
}

/* Set LENRANGE to the number of nonzero bytes for a store of TYPE and
   clear all flags.  Return true on success and false on failure.  */

static bool
nonzero_bytes_for_type (tree type, unsigned lenrange[3],
			bool *nulterm, bool *allnul, bool *allnonnul)
{
  /* Use the size of the type of the expression as the size of the store,
     and set the upper bound of the length range to that of the size.
     Nothing is known about the contents so clear all flags.  */
  tree typesize = TYPE_SIZE_UNIT (type);
  if (!type)
    return false;

  if (!tree_fits_uhwi_p (typesize))
    return false;

  unsigned HOST_WIDE_INT sz = tree_to_uhwi (typesize);
  if (sz > UINT_MAX)
    return false;

  lenrange[2] = sz;
  lenrange[1] = lenrange[2] ? lenrange[2] - 1 : 0;
  lenrange[0] = 0;
  *nulterm = false;
  *allnul = false;
  *allnonnul = false;
  return true;
}

/* Recursively determine the minimum and maximum number of leading nonzero
   bytes in the representation of EXP at memory state VUSE and set
   LENRANGE[0] and LENRANGE[1] to each.
   Sets LENRANGE[2] to the total size of the access (which may be less
   than LENRANGE[1] when what's being referenced by EXP is a pointer
   rather than an array).
   Sets *NULTERM if the representation contains a zero byte, sets *ALLNUL
   if all the bytes are zero, and *ALLNONNUL is all are nonzero.
   OFFSET and NBYTES are the offset into the representation and
   the size of the access to it determined from an ADDR_EXPR (i.e.,
   a pointer) or MEM_REF or zero for other expressions.
   Uses RVALS to determine range information.
   Avoids recursing deeper than the limits in SNLIM allow.
   Returns true on success and false otherwise.  */

bool
strlen_pass::count_nonzero_bytes (tree exp, tree vuse, gimple *stmt,
				  unsigned HOST_WIDE_INT offset,
				  unsigned HOST_WIDE_INT nbytes,
				  unsigned lenrange[3], bool *nulterm,
				  bool *allnul, bool *allnonnul,
				  ssa_name_limit_t &snlim)
{
  if (TREE_CODE (exp) == SSA_NAME)
    {
      /* Handle non-zero single-character stores specially.  */
      tree type = TREE_TYPE (exp);
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_MODE (type) == TYPE_MODE (char_type_node)
	  && TYPE_PRECISION (type) == TYPE_PRECISION (char_type_node)
	  && tree_expr_nonzero_p (exp))
	{
	  /* If the character EXP is known to be non-zero (even if its
	     exact value is not known) recurse once to set the range
	     for an arbitrary constant.  */
	  exp = build_int_cst (type, 1);
	  return count_nonzero_bytes (exp, vuse, stmt,
				      offset, 1, lenrange,
				      nulterm, allnul, allnonnul, snlim);
	}

      gimple *g = SSA_NAME_DEF_STMT (exp);
      if (gimple_assign_single_p (g))
	{
	  exp = gimple_assign_rhs1 (g);
	  if (!DECL_P (exp)
	      && TREE_CODE (exp) != CONSTRUCTOR
	      && TREE_CODE (exp) != MEM_REF)
	    return false;
	  /* Handle DECLs, CONSTRUCTOR and MEM_REF below.  */
	  stmt = g;
	}
      else if (gimple_code (g) == GIMPLE_PHI)
	{
	  /* Avoid processing an SSA_NAME that has already been visited
	     or if an SSA_NAME limit has been reached.  Indicate success
	     if the former and failure if the latter.  */
	  if (int res = snlim.next_phi (exp))
	    return res > 0;

	  /* Determine the minimum and maximum from the PHI arguments.  */
	  unsigned int n = gimple_phi_num_args (g);
	  for (unsigned i = 0; i != n; i++)
	    {
	      tree def = gimple_phi_arg_def (g, i);
	      if (!count_nonzero_bytes (def, vuse, g,
					offset, nbytes, lenrange, nulterm,
					allnul, allnonnul, snlim))
		return false;
	    }

	  return true;
	}
    }

  if (TREE_CODE (exp) == CONSTRUCTOR)
    {
      if (nbytes)
	/* If NBYTES has already been determined by an outer MEM_REF
	   fail rather than overwriting it (this shouldn't happen).  */
	return false;

      tree type = TREE_TYPE (exp);
      tree size = TYPE_SIZE_UNIT (type);
      if (!size || !tree_fits_uhwi_p (size))
	return false;

      unsigned HOST_WIDE_INT byte_size = tree_to_uhwi (size);
      if (byte_size < offset)
	return false;

      nbytes = byte_size - offset;
    }

  if (TREE_CODE (exp) == MEM_REF)
    {
      if (nbytes)
	return false;

      tree arg = TREE_OPERAND (exp, 0);
      tree off = TREE_OPERAND (exp, 1);

      if (TREE_CODE (off) != INTEGER_CST || !tree_fits_uhwi_p (off))
	return false;

      unsigned HOST_WIDE_INT wioff = tree_to_uhwi (off);
      if (INT_MAX < wioff)
	return false;

      offset += wioff;
      if (INT_MAX < offset)
	return false;

      /* The size of the MEM_REF access determines the number of bytes.  */
      tree type = TREE_TYPE (exp);
      tree typesize = TYPE_SIZE_UNIT (type);
      if (!typesize || !tree_fits_uhwi_p (typesize))
	return false;
      nbytes = tree_to_uhwi (typesize);
      if (!nbytes)
	return false;

      /* Handle MEM_REF = SSA_NAME types of assignments.  */
      return count_nonzero_bytes_addr (arg, vuse, stmt,
				       offset, nbytes, lenrange, nulterm,
				       allnul, allnonnul, snlim);
    }

  if (VAR_P (exp) || TREE_CODE (exp) == CONST_DECL)
    {
      /* If EXP can be folded into a constant use the result.  Otherwise
	 proceed to use EXP to determine a range of the result.  */
      if (tree fold_exp = ctor_for_folding (exp))
	if (fold_exp != error_mark_node)
	  exp = fold_exp;
    }

  const char *prep = NULL;
  if (TREE_CODE (exp) == STRING_CST)
    {
      unsigned nchars = TREE_STRING_LENGTH (exp);
      if (nchars < offset)
	return false;

      if (!nbytes)
	/* If NBYTES hasn't been determined earlier, either from ADDR_EXPR
	   (i.e., it's the size of a pointer), or from MEM_REF (as the size
	   of the access), set it here to the size of the string, including
	   all internal and trailing nuls if the string has any.  */
	nbytes = nchars - offset;
      else if (nchars - offset < nbytes)
	return false;

      prep = TREE_STRING_POINTER (exp) + offset;
    }

  unsigned char buf[256];
  if (!prep)
    {
      if (CHAR_BIT != 8 || BITS_PER_UNIT != 8)
	return false;
      /* If the pointer to representation hasn't been set above
	 for STRING_CST point it at the buffer.  */
      prep = reinterpret_cast <char *>(buf);
      /* Try to extract the representation of the constant object
	 or expression starting from the offset.  */
      unsigned repsize = native_encode_expr (exp, buf, sizeof buf, offset);
      if (repsize < nbytes)
	{
	  /* This should only happen when REPSIZE is zero because EXP
	     doesn't denote an object with a known initializer, except
	     perhaps when the reference reads past its end.  */
	  lenrange[0] = 0;
	  prep = NULL;
	}
      else if (!nbytes)
	nbytes = repsize;
      else if (nbytes < repsize)
	return false;
    }

  if (!nbytes)
    return nonzero_bytes_for_type (TREE_TYPE (exp), lenrange,
				   nulterm, allnul, allnonnul);

  /* Compute the number of leading nonzero bytes in the representation
     and update the minimum and maximum.  */
  unsigned HOST_WIDE_INT n = prep ? strnlen (prep, nbytes) : nbytes;

  if (n < lenrange[0])
    lenrange[0] = n;
  if (lenrange[1] < n)
    lenrange[1] = n;

  /* Set the size of the representation.  */
  if (lenrange[2] < nbytes)
    lenrange[2] = nbytes;

  /* Clear NULTERM if none of the bytes is zero.  */
  if (n == nbytes)
    *nulterm = false;

  if (n)
    {
      /* When the initial number of non-zero bytes N is non-zero, reset
	 *ALLNUL; if N is less than that the size of the representation
	 also clear *ALLNONNUL.  */
      *allnul = false;
      if (n < nbytes)
	*allnonnul = false;
    }
  else if (*allnul || *allnonnul)
    {
      *allnonnul = false;

      if (*allnul)
	{
	  /* When either ALLNUL is set and N is zero, also determine
	     whether all subsequent bytes after the first one (which
	     is nul) are zero or nonzero and clear ALLNUL if not.  */
	  for (const char *p = prep; p != prep + nbytes; ++p)
	    if (*p)
	      {
		*allnul = false;
		break;
	      }
	}
    }

  return true;
}

/* Like count_nonzero_bytes, but instead of counting bytes in EXP, count
   bytes that are pointed to by EXP, which should be a pointer.  */

bool
strlen_pass::count_nonzero_bytes_addr (tree exp, tree vuse, gimple *stmt,
				       unsigned HOST_WIDE_INT offset,
				       unsigned HOST_WIDE_INT nbytes,
				       unsigned lenrange[3], bool *nulterm,
				       bool *allnul, bool *allnonnul,
				       ssa_name_limit_t &snlim)
{
  int idx = get_stridx (exp, stmt);
  if (idx > 0)
    {
      /* get_strinfo reflects string lengths before the current statement,
	 where the current statement is the outermost count_nonzero_bytes
	 stmt.  If there are any stores in between stmt and that
	 current statement, the string length information might describe
	 something significantly different.  */
      if (gimple_vuse (stmt) != vuse)
	return false;

      strinfo *si = get_strinfo (idx);
      if (!si)
	return false;

      /* Handle both constant lengths as well non-constant lengths
	 in some range.  */
      unsigned HOST_WIDE_INT minlen, maxlen;
      if (tree_fits_shwi_p (si->nonzero_chars))
	minlen = maxlen = tree_to_shwi (si->nonzero_chars);
      else if (si->nonzero_chars
	       && TREE_CODE (si->nonzero_chars) == SSA_NAME)
	{
	  int_range_max vr;
	  if (!ptr_qry.rvals->range_of_expr (vr, si->nonzero_chars, stmt)
	      || vr.undefined_p ()
	      || vr.varying_p ())
	    return false;

	  minlen = vr.lower_bound ().to_uhwi ();
	  maxlen = vr.upper_bound ().to_uhwi ();
	}
      else
	return false;

      if (maxlen < offset)
	return false;

      minlen = minlen < offset ? 0 : minlen - offset;
      maxlen -= offset;
      if (maxlen + 1 < nbytes)
	return false;

      if (nbytes <= minlen || !si->full_string_p)
	*nulterm = false;

      if (nbytes < minlen)
	{
	  minlen = nbytes;
	  if (nbytes < maxlen)
	    maxlen = nbytes;
	}

      if (!si->full_string_p)
	maxlen = nbytes;

      if (minlen < lenrange[0])
	lenrange[0] = minlen;
      if (lenrange[1] < maxlen)
	lenrange[1] = maxlen;

      if (lenrange[2] < nbytes)
	lenrange[2] = nbytes;

      /* Since only the length of the string are known and not its contents,
	 clear ALLNUL and ALLNONNUL purely on the basis of the length.  */
      *allnul = false;
      if (minlen < nbytes)
	*allnonnul = false;

      return true;
    }

  if (TREE_CODE (exp) == ADDR_EXPR)
    return count_nonzero_bytes (TREE_OPERAND (exp, 0), vuse, stmt,
				offset, nbytes,
				lenrange, nulterm, allnul, allnonnul, snlim);

  if (TREE_CODE (exp) == SSA_NAME)
    {
      gimple *g = SSA_NAME_DEF_STMT (exp);
      if (gimple_code (g) == GIMPLE_PHI)
	{
	  /* Avoid processing an SSA_NAME that has already been visited
	     or if an SSA_NAME limit has been reached.  Indicate success
	     if the former and failure if the latter.  */
	  if (int res = snlim.next_phi (exp))
	    return res > 0;

	  /* Determine the minimum and maximum from the PHI arguments.  */
	  unsigned int n = gimple_phi_num_args (g);
	  for (unsigned i = 0; i != n; i++)
	    {
	      tree def = gimple_phi_arg_def (g, i);
	      if (!count_nonzero_bytes_addr (def, vuse, g,
					     offset, nbytes, lenrange,
					     nulterm, allnul, allnonnul,
					     snlim))
		return false;
	    }

	  return true;
	}
    }

  /* Otherwise we don't know anything.  */
  lenrange[0] = 0;
  if (lenrange[1] < nbytes)
    lenrange[1] = nbytes;
  if (lenrange[2] < nbytes)
    lenrange[2] = nbytes;
  *nulterm = false;
  *allnul = false;
  *allnonnul = false;
  return true;
}

/* Same as above except with an implicit SSA_NAME limit.  When EXPR_OR_TYPE
   is a type rather than an expression use its size to compute the range.
   RVALS is used to determine ranges of dynamically computed string lengths
   (the results of strlen).  */

bool
strlen_pass::count_nonzero_bytes (tree expr_or_type, gimple *stmt,
				  unsigned lenrange[3], bool *nulterm,
				  bool *allnul, bool *allnonnul)
{
  if (TYPE_P (expr_or_type))
    return nonzero_bytes_for_type (expr_or_type, lenrange,
				   nulterm, allnul, allnonnul);

  /* Set to optimistic values so the caller doesn't have to worry about
     initializing these and to what.  On success, the function will clear
     these if it determines their values are different but being recursive
     it never sets either to true.  On failure, their values are
     unspecified.  */
  *nulterm = true;
  *allnul = true;
  *allnonnul = true;

  ssa_name_limit_t snlim;
  tree expr = expr_or_type;
  return count_nonzero_bytes (expr, gimple_vuse (stmt), stmt,
			      0, 0, lenrange, nulterm, allnul, allnonnul,
			      snlim);
}

/* Handle a single or multibyte store other than by a built-in function,
   either via a single character assignment or by multi-byte assignment
   either via MEM_REF or via a type other than char (such as in
   '*(int*)a = 12345').  Return true to let the caller advance *GSI to
   the next statement in the basic block and false otherwise.  */

bool
strlen_pass::handle_store (bool *zero_write)
{
  gimple *stmt = gsi_stmt (m_gsi);
  /* The LHS and RHS of the store.  The RHS is null if STMT is a function
     call.  STORETYPE is the type of the store (determined from either
     the RHS of the assignment statement or the LHS of a function call.  */
  tree lhs, rhs, storetype;
  if (is_gimple_assign (stmt))
    {
      lhs = gimple_assign_lhs (stmt);
      rhs = gimple_assign_rhs1 (stmt);
      storetype = TREE_TYPE (rhs);
    }
  else if (is_gimple_call (stmt))
    {
      lhs = gimple_call_lhs (stmt);
      rhs = NULL_TREE;
      storetype = TREE_TYPE (lhs);
    }
  else
    return true;

  tree ssaname = NULL_TREE;
  strinfo *si = NULL;
  int idx = -1;

  range_query *const rvals = ptr_qry.rvals;

  /* The offset of the first byte in LHS modified by the store.  */
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
	  idx = get_stridx (TREE_OPERAND (lhs, 0), stmt);
	  if (idx > 0)
	    si = get_strinfo (idx);
	  if (offset == 0)
	    ssaname = TREE_OPERAND (lhs, 0);
	  else if (si == NULL
		   || compare_nonzero_chars (si, stmt, offset, rvals) < 0)
	    {
	      *zero_write = rhs ? initializer_zerop (rhs) : false;

	      bool dummy;
	      unsigned lenrange[] = { UINT_MAX, 0, 0 };
	      if (count_nonzero_bytes (rhs ? rhs : storetype, stmt, lenrange,
				       &dummy, &dummy, &dummy))
		maybe_warn_overflow (stmt, true, lenrange[2]);

	      return true;
	    }
	}
    }
  else
    {
      idx = get_addr_stridx (lhs, stmt, NULL_TREE, &offset, rvals);
      if (idx > 0)
	si = get_strinfo (idx);
    }

  /* Minimum and maximum leading non-zero bytes and the size of the store.  */
  unsigned lenrange[] = { UINT_MAX, 0, 0 };

  /* Set to the minimum length of the string being assigned if known.  */
  unsigned HOST_WIDE_INT rhs_minlen;

  /* STORING_NONZERO_P is true iff not all stored characters are zero.
     STORING_ALL_NONZERO_P is true if all stored characters are zero.
     STORING_ALL_ZEROS_P is true iff all stored characters are zero.
     Both are false when it's impossible to determine which is true.  */
  bool storing_nonzero_p;
  bool storing_all_nonzero_p;
  bool storing_all_zeros_p;
  /* FULL_STRING_P is set when the stored sequence of characters form
     a nul-terminated string.  */
  bool full_string_p;

  const bool ranges_valid
    = count_nonzero_bytes (rhs ? rhs : storetype, stmt,
			   lenrange, &full_string_p,
			   &storing_all_zeros_p, &storing_all_nonzero_p);

  if (ranges_valid)
    {
      rhs_minlen = lenrange[0];
      storing_nonzero_p = lenrange[1] > 0;
      *zero_write = storing_all_zeros_p;

      maybe_warn_overflow (stmt, true, lenrange[2]);
    }
  else
    {
      rhs_minlen = HOST_WIDE_INT_M1U;
      full_string_p = false;
      storing_nonzero_p = false;
      storing_all_zeros_p = false;
      storing_all_nonzero_p = false;
    }

  if (si != NULL)
    {
      /* The count_nonzero_bytes call above might have unshared si.
	 Fetch it again from the vector.  */
      si = get_strinfo (idx);
      /* The corresponding element is set to 1 if the first and last
	 element, respectively, of the sequence of characters being
	 written over the string described by SI ends before
	 the terminating nul (if it has one), to zero if the nul is
	 being overwritten but not beyond, or negative otherwise.  */
      int store_before_nul[2];
      if (ranges_valid)
	{
	  /* The offset of the last stored byte.  */
	  unsigned HOST_WIDE_INT endoff = offset + lenrange[2] - 1;
	  store_before_nul[0]
	    = compare_nonzero_chars (si, stmt, offset, rvals);
	  if (endoff == offset)
	    store_before_nul[1] = store_before_nul[0];
	  else
	    store_before_nul[1]
	      = compare_nonzero_chars (si, stmt, endoff, rvals);
	}
      else
	{
	  store_before_nul[0]
	    = compare_nonzero_chars (si, stmt, offset, rvals);
	  store_before_nul[1] = store_before_nul[0];
	  gcc_assert (offset == 0 || store_before_nul[0] >= 0);
	}

      if (storing_all_zeros_p
	  && store_before_nul[0] == 0
	  && store_before_nul[1] == 0
	  && si->full_string_p)
	{
	  /* When overwriting a '\0' with a '\0', the store can be removed
	     if we know it has been stored in the current function.  */
	  if (!stmt_could_throw_p (cfun, stmt) && si->writable)
	    {
	      unlink_stmt_vdef (stmt);
	      release_defs (stmt);
	      gsi_remove (&m_gsi, true);
	      return false;
	    }
	  else
	    {
	      si->writable = true;
	      gsi_next (&m_gsi);
	      return false;
	    }
	}

      if (store_before_nul[1] > 0
	  && storing_nonzero_p
	  && lenrange[0] == lenrange[1]
	  && lenrange[0] == lenrange[2]
	  && TREE_CODE (storetype) == INTEGER_TYPE)
	{
	  /* Handle a store of one or more non-nul characters that ends
	     before the terminating nul of the destination and so does
	     not affect its length
	     If si->nonzero_chars > OFFSET, we aren't overwriting '\0',
	     and if we aren't storing '\0', we know that the length of
	     the string and any other zero terminated string in memory
	     remains the same.  In that case we move to the next gimple
	     statement and return to signal the caller that it shouldn't
	     invalidate anything.

	     This is beneficial for cases like:

	     char p[20];
	     void foo (char *q)
	     {
	       strcpy (p, "foobar");
	       size_t len = strlen (p);     // can be folded to 6
	       size_t len2 = strlen (q);    // has to be computed
	       p[0] = 'X';
	       size_t len3 = strlen (p);    // can be folded to 6
	       size_t len4 = strlen (q);    // can be folded to len2
	       bar (len, len2, len3, len4);
	       } */
	  gsi_next (&m_gsi);
	  return false;
	}

      if (storing_nonzero_p
	  || storing_all_zeros_p
	  || (full_string_p && lenrange[1] == 0)
	  || (offset != 0 && store_before_nul[1] > 0))
	{
	  /* When STORING_NONZERO_P, we know that the string will start
	     with at least OFFSET + 1 nonzero characters.  If storing
	     a single character, set si->NONZERO_CHARS to the result.
	     If storing multiple characters, try to determine the number
	     of leading non-zero characters and set si->NONZERO_CHARS to
	     the result instead.

	     When STORING_ALL_ZEROS_P, or the first byte written is zero,
	     i.e. FULL_STRING_P && LENRANGE[1] == 0, we know that the
	     string is now OFFSET characters long.

	     Otherwise, we're storing an unknown value at offset OFFSET,
	     so need to clip the nonzero_chars to OFFSET.
	     Use the minimum length of the string (or individual character)
	     being stored if it's known.  Otherwise, STORING_NONZERO_P
	     guarantees it's at least 1.  */
	  HOST_WIDE_INT len
	    = storing_nonzero_p && ranges_valid ? lenrange[0] : 1;
	  location_t loc = gimple_location (stmt);
	  tree oldlen = si->nonzero_chars;
	  if (store_before_nul[1] == 0 && si->full_string_p)
	    /* We're overwriting the nul terminator with a nonzero or
	       unknown character.  If the previous stmt was a memcpy,
	       its length may be decreased.  */
	    adjust_last_stmt (si, stmt, false);
	  si = unshare_strinfo (si);
	  if (storing_nonzero_p)
	    {
	      gcc_assert (len >= 0);
	      si->nonzero_chars = build_int_cst (size_type_node, offset + len);
	    }
	  else
	    si->nonzero_chars = build_int_cst (size_type_node, offset);

	  /* Set FULL_STRING_P only if the length of the strings being
	     written is the same, and clear it if the strings have
	     different lengths.  In the latter case the length stored
	     in si->NONZERO_CHARS becomes the lower bound.
	     FIXME: Handle the upper bound of the length if possible.  */
	  si->full_string_p = full_string_p && lenrange[0] == lenrange[1];

	  if (storing_all_zeros_p
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
  else if (idx == 0 && (storing_all_zeros_p || storing_nonzero_p))
    {
      if (ssaname)
	idx = new_stridx (ssaname);
      else
	idx = new_addr_stridx (lhs);
      if (idx != 0)
	{
	  tree ptr = (ssaname ? ssaname : build_fold_addr_expr (lhs));

	  HOST_WIDE_INT slen;
	  if (storing_all_zeros_p)
	    slen = 0;
	  else if (storing_nonzero_p && ranges_valid)
	    {
	      /* FIXME: Handle the upper bound of the length when
		 LENRANGE[0] != LENRANGE[1].  */
	      slen = lenrange[0];
	      if (lenrange[0] != lenrange[1])
		/* Set the minimum length but ignore the maximum
		   for now.  */
		full_string_p = false;
	    }
	  else
	    slen = -1;

	  tree len = (slen <= 0
		      ? size_zero_node
		      : build_int_cst (size_type_node, slen));
	  si = new_strinfo (ptr, idx, len, slen >= 0 && full_string_p);
	  set_strinfo (idx, si);
	  if (storing_all_zeros_p
	      && ssaname
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssaname))
	    si->endptr = ssaname;
	  si->dont_invalidate = true;
	  si->writable = true;
	}
    }
  else if (idx == 0
	   && rhs_minlen < HOST_WIDE_INT_M1U
	   && ssaname == NULL_TREE
	   && TREE_CODE (TREE_TYPE (lhs)) == ARRAY_TYPE)
    {
      HOST_WIDE_INT a = int_size_in_bytes (TREE_TYPE (lhs));
      if (a > 0 && (unsigned HOST_WIDE_INT) a > rhs_minlen)
	{
	  int idx = new_addr_stridx (lhs);
	  if (idx != 0)
	    {
	      si = new_strinfo (build_fold_addr_expr (lhs), idx,
				build_int_cst (size_type_node, rhs_minlen),
				full_string_p);
	      set_strinfo (idx, si);
	      si->dont_invalidate = true;
	    }
	}
    }

  if (si != NULL && offset == 0 && storing_all_zeros_p && lenrange[2] == 1)
    {
      /* For single-byte stores only, allow adjust_last_stmt to remove
	 the statement if the stored '\0' is immediately overwritten.  */
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
	  int idx = get_stridx (arg1, call_stmt);

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

/* Return true if TYPE corresponds to a narrow character type.  */

static bool
is_char_type (tree type)
{
  return (TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_MODE (type) == TYPE_MODE (char_type_node)
	  && TYPE_PRECISION (type) == TYPE_PRECISION (char_type_node));
}

/* Check the built-in call at GSI for validity and optimize it.
   Uses RVALS to determine range information.
   Return true to let the caller advance *GSI to the next statement
   in the basic block and false otherwise.  */

bool
strlen_pass::check_and_optimize_call (bool *zero_write)
{
  gimple *stmt = gsi_stmt (m_gsi);

  if (!gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      tree fntype = gimple_call_fntype (stmt);
      if (!fntype)
	return true;

      if (lookup_attribute ("alloc_size", TYPE_ATTRIBUTES (fntype)))
	{
	  handle_alloc_call (BUILT_IN_NONE);
	  return true;
	}

      if (tree lhs = gimple_call_lhs (stmt))
	handle_assign (lhs, zero_write);

      /* Proceed to handle user-defined formatting functions.  */
    }

  /* When not optimizing we must be checking printf calls which
     we do even for user-defined functions when they are declared
     with attribute format.  */
  if (!flag_optimize_strlen
      || !strlen_optimize
      || !valid_builtin_call (stmt))
    return !handle_printf_call (&m_gsi, ptr_qry);

  tree callee = gimple_call_fndecl (stmt);
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
    case BUILT_IN_STRNLEN:
      handle_builtin_strlen ();
      break;
    case BUILT_IN_STRCHR:
      handle_builtin_strchr ();
      break;
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY:
    case BUILT_IN_STPCPY_CHK:
      handle_builtin_strcpy (DECL_FUNCTION_CODE (callee));
      break;

    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCAT_CHK:
      handle_builtin_strncat (DECL_FUNCTION_CODE (callee));
      break;

    case BUILT_IN_STPNCPY:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
      handle_builtin_stxncpy_strncat (false);
      break;

    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
      handle_builtin_memcpy (DECL_FUNCTION_CODE (callee));
      break;
    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCAT_CHK:
      handle_builtin_strcat (DECL_FUNCTION_CODE (callee));
      break;
    case BUILT_IN_ALLOCA:
    case BUILT_IN_ALLOCA_WITH_ALIGN:
    case BUILT_IN_MALLOC:
    case BUILT_IN_CALLOC:
      handle_alloc_call (DECL_FUNCTION_CODE (callee));
      break;
    case BUILT_IN_MEMSET:
      if (handle_builtin_memset (zero_write))
	return false;
      break;
    case BUILT_IN_MEMCMP:
      if (handle_builtin_memcmp ())
	return false;
      break;
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRNCMP:
      if (handle_builtin_string_cmp ())
	return false;
      break;
    default:
      if (handle_printf_call (&m_gsi, ptr_qry))
	return false;
      break;
    }

  return true;
}

/* Handle an assignment statement at *GSI to a LHS of integral type.
   If GSI's basic block needs clean-up of EH, set *CLEANUP_EH to true.  */

void
strlen_pass::handle_integral_assign (bool *cleanup_eh)
{
  gimple *stmt = gsi_stmt (m_gsi);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);

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
	   && TREE_CODE (lhs_type) == INTEGER_TYPE
	   && TYPE_MODE (lhs_type) == TYPE_MODE (char_type_node)
	   && (TYPE_PRECISION (lhs_type)
	       == TYPE_PRECISION (char_type_node))
	   && !gimple_has_volatile_ops (stmt))
    {
      tree off = integer_zero_node;
      unsigned HOST_WIDE_INT coff = 0;
      int idx = 0;
      tree rhs1 = gimple_assign_rhs1 (stmt);
      if (code == MEM_REF)
	{
	  idx = get_stridx (TREE_OPERAND (rhs1, 0), stmt);
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
	idx = get_addr_stridx (rhs1, stmt, NULL_TREE, &coff);
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
		  tree zero = build_int_cst (lhs_type, 0);
		  gimple_set_vuse (stmt, NULL_TREE);
		  gimple_assign_set_rhs_from_tree (&m_gsi, zero);
		  *cleanup_eh
		    |= maybe_clean_or_replace_eh_stmt (stmt,
						       gsi_stmt (m_gsi));
		  stmt = gsi_stmt (m_gsi);
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
		  int_range_max r;
		  if (!get_range_query (cfun)->range_of_expr (r, lhs)
		      || r.varying_p ())
		    {
		      r.set_nonzero (lhs_type);
		      set_range_info (lhs, r);
		    }
		}
	    }
	}
    }
  else if (code == MEM_REF && TREE_CODE (lhs) == SSA_NAME)
    {
      if (int idx = new_stridx (lhs))
	{
	  /* Record multi-byte assignments from MEM_REFs.  */
	  bool storing_all_nonzero_p;
	  bool storing_all_zeros_p;
	  bool full_string_p;
	  unsigned lenrange[] = { UINT_MAX, 0, 0 };
	  tree rhs = gimple_assign_rhs1 (stmt);
	  const bool ranges_valid
	    = count_nonzero_bytes (rhs, stmt,
				   lenrange, &full_string_p,
				   &storing_all_zeros_p,
				   &storing_all_nonzero_p);
	  if (ranges_valid)
	    {
	      tree length = build_int_cst (sizetype, lenrange[0]);
	      strinfo *si = new_strinfo (lhs, idx, length, full_string_p);
	      set_strinfo (idx, si);
	      si->writable = true;
	      si->dont_invalidate = true;
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

/* Handle assignment statement at *GSI to LHS.  Set *ZERO_WRITE if
   the assignment stores all zero bytes.  */

bool
strlen_pass::handle_assign (tree lhs, bool *zero_write)
{
  tree type = TREE_TYPE (lhs);
  if (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  bool is_char_store = is_char_type (type);
  if (!is_char_store && TREE_CODE (lhs) == MEM_REF)
    {
      /* To consider stores into char objects via integer types other
	 than char but not those to non-character objects, determine
	 the type of the destination rather than just the type of
	 the access.  */
      for (int i = 0; i != 2; ++i)
	{
	  tree ref = TREE_OPERAND (lhs, i);
	  type = TREE_TYPE (ref);
	  if (TREE_CODE (type) == POINTER_TYPE)
	    type = TREE_TYPE (type);
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    type = TREE_TYPE (type);
	  if (is_char_type (type))
	    {
	      is_char_store = true;
	      break;
	    }
	}
    }

  /* Handle a single or multibyte assignment.  */
  if (is_char_store && !handle_store (zero_write))
    return false;

  return true;
}


/* Attempt to check for validity of the performed access a single statement
   at *GSI using string length knowledge, and to optimize it.
   If the given basic block needs clean-up of EH, CLEANUP_EH is set to
   true.  Return true to let the caller advance *GSI to the next statement
   in the basic block and false otherwise.  */

bool
strlen_pass::check_and_optimize_stmt (bool *cleanup_eh)
{
  gimple *stmt = gsi_stmt (m_gsi);

  /* For statements that modify a string, set to true if the write
     is only zeros.  */
  bool zero_write = false;

  if (is_gimple_call (stmt))
    {
      if (!check_and_optimize_call (&zero_write))
	return false;
    }
  else if (!flag_optimize_strlen || !strlen_optimize)
    return true;
  else if (is_gimple_assign (stmt) && !gimple_clobber_p (stmt))
    {
      /* Handle non-clobbering assignment.  */
      tree lhs = gimple_assign_lhs (stmt);
      tree lhs_type = TREE_TYPE (lhs);

      if (TREE_CODE (lhs) == SSA_NAME && POINTER_TYPE_P (lhs_type))
	{
	  if (gimple_assign_single_p (stmt)
	      || (gimple_assign_cast_p (stmt)
		  && POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (stmt)))))
	    {
	      int idx = get_stridx (gimple_assign_rhs1 (stmt), stmt);
	      ssa_ver_to_stridx[SSA_NAME_VERSION (lhs)] = idx;
	    }
	  else if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
	    handle_pointer_plus ();
	}
      else if (TREE_CODE (lhs) == SSA_NAME && INTEGRAL_TYPE_P (lhs_type))
	/* Handle assignment to a character.  */
	handle_integral_assign (cleanup_eh);
      else if (TREE_CODE (lhs) != SSA_NAME && !TREE_SIDE_EFFECTS (lhs))
	if (!handle_assign (lhs, &zero_write))
	  return false;
    }
  else if (gcond *cond = dyn_cast<gcond *> (stmt))
    {
      enum tree_code code = gimple_cond_code (cond);
      if (code == EQ_EXPR || code == NE_EXPR)
	fold_strstr_to_strncmp (gimple_cond_lhs (stmt),
				gimple_cond_rhs (stmt), stmt);
    }

  if (gimple_vdef (stmt))
    maybe_invalidate (stmt, zero_write);
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

/* Release pointer_query cache.  */

strlen_pass::~strlen_pass ()
{
  ptr_qry.flush_cache ();
}

/* Callback for walk_dominator_tree.  Attempt to optimize various
   string ops by remembering string lengths pointed by pointer SSA_NAMEs.  */

edge
strlen_pass::before_dom_children (basic_block bb)
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
	  int idx = get_stridx (gimple_phi_arg_def (phi, 0), phi);
	  if (idx != 0)
	    {
	      unsigned int i, n = gimple_phi_num_args (phi);
	      for (i = 1; i < n; i++)
		if (idx != get_stridx (gimple_phi_arg_def (phi, i), phi))
		  break;
	      if (i == n)
		ssa_ver_to_stridx[SSA_NAME_VERSION (result)] = idx;
	    }
	}
    }

  bool cleanup_eh = false;

  /* Attempt to optimize individual statements.  */
  for (m_gsi = gsi_start_bb (bb); !gsi_end_p (m_gsi); )
    {
      /* Reset search depth performance counter.  */
      ptr_qry.depth = 0;

      if (check_and_optimize_stmt (&cleanup_eh))
	gsi_next (&m_gsi);
    }

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
strlen_pass::after_dom_children (basic_block bb)
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

namespace {

static unsigned int
printf_strlen_execute (function *fun, bool warn_only)
{
  strlen_optimize = !warn_only;

  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (LOOPS_NORMAL);
  scev_initialize ();

  gcc_assert (!strlen_to_stridx);
  if (warn_stringop_overflow || warn_stringop_truncation)
    strlen_to_stridx = new hash_map<tree, stridx_strlenloc> ();

  /* This has to happen after initializing the loop optimizer
     and initializing SCEV as they create new SSA_NAMEs.  */
  ssa_ver_to_stridx.safe_grow_cleared (num_ssa_names, true);
  max_stridx = 1;

  enable_ranger (fun);
  /* String length optimization is implemented as a walk of the dominator
     tree and a forward walk of statements within each block.  */
  strlen_pass walker (fun, CDI_DOMINATORS);
  walker.walk (ENTRY_BLOCK_PTR_FOR_FN (fun));

  if (dump_file && (dump_flags & TDF_DETAILS))
    walker.ptr_qry.dump (dump_file, true);

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

  disable_ranger (fun);
  scev_finalize ();
  loop_optimizer_finalize ();

  return walker.m_cleanup_cfg ? TODO_cleanup_cfg : 0;
}

/* This file defines two passes: one for warnings that runs only when
   optimization is disabled, and another that implements optimizations
   and also issues warnings.  */

const pass_data pass_data_warn_printf =
{
  GIMPLE_PASS, /* type */
  "warn-printf", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  /* Normally an optimization pass would require PROP_ssa but because
     this pass runs early, with no optimization, to do sprintf format
     checking, it only requires PROP_cfg.  */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_printf : public gimple_opt_pass
{
public:
  pass_warn_printf (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_warn_printf, ctxt)
  {}

  bool gate (function *) final override;
  unsigned int execute (function *fun) final override
  {
    return printf_strlen_execute (fun, true);
  }
};


/* Return true to run the warning pass only when not optimizing and
   iff either -Wformat-overflow or -Wformat-truncation is specified.  */

bool
pass_warn_printf::gate (function *)
{
  return !optimize && (warn_format_overflow > 0 || warn_format_trunc > 0);
}

const pass_data pass_data_strlen =
{
  GIMPLE_PASS, /* type */
  "strlen", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_STRLEN, /* tv_id */
  PROP_cfg | PROP_ssa, /* properties_required */
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

  opt_pass * clone () final override { return new pass_strlen (m_ctxt); }

  bool gate (function *) final override;
  unsigned int execute (function *fun) final override
  {
    return printf_strlen_execute (fun, false);
  }
};

/* Return true to run the pass only when the sprintf and/or strlen
   optimizations are enabled and -Wformat-overflow or -Wformat-truncation
   are specified.  */

bool
pass_strlen::gate (function *)
{
  return ((warn_format_overflow > 0
	   || warn_format_trunc > 0
	   || warn_restrict > 0
	   || flag_optimize_strlen > 0
	   || flag_printf_return_value)
	  && optimize > 0);
}

} // anon namespace

gimple_opt_pass *
make_pass_warn_printf (gcc::context *ctxt)
{
  return new pass_warn_printf (ctxt);
}

gimple_opt_pass *
make_pass_strlen (gcc::context *ctxt)
{
  return new pass_strlen (ctxt);
}
