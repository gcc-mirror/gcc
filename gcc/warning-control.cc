/* Functions to enable and disable individual warnings on an expression
   and statement basis.

   Copyright (C) 2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "bitmap.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "hash-map.h"
#include "diagnostic-spec.h"

/* Return the no-warning bit for EXPR.  */

static inline bool
get_no_warning_bit (const_tree expr)
{
  return expr->base.nowarning_flag;
}

/* Return the no-warning bit for statement STMT.  */

static inline bool
get_no_warning_bit (const gimple *stmt)
{
  return stmt->no_warning;
}

/* Set the no-warning bit for EXPR to VALUE.  */

static inline void
set_no_warning_bit (tree expr, bool value)
{
  expr->base.nowarning_flag = value;
}

/* Set the no-warning bit for statement STMT to VALUE.  */

static inline void
set_no_warning_bit (gimple *stmt, bool value)
{
  stmt->no_warning = value;
}

/* Return EXPR location or zero.  */

static inline key_type_t
convert_to_key (const_tree expr)
{
  if (DECL_P (expr))
    return DECL_SOURCE_LOCATION (expr);
  if (EXPR_P (expr))
    return EXPR_LOCATION (expr);
  return 0;
}

/* Return STMT location (may be zero).  */

static inline key_type_t
convert_to_key (const gimple *stmt)
{
  return gimple_location (stmt);
}

/* Return the no-warning bitmap for decl/expression EXPR.  */

static nowarn_spec_t *
get_nowarn_spec (const_tree expr)
{
  const key_type_t key = convert_to_key (expr);

  if (!get_no_warning_bit (expr) || !key)
    return NULL;

  return nowarn_map ? nowarn_map->get (key) : NULL;
}

/* Return the no-warning bitmap for stateemt STMT.  */

static nowarn_spec_t *
get_nowarn_spec (const gimple *stmt)
{
  const key_type_t key = convert_to_key (stmt);

  if (!get_no_warning_bit (stmt))
    return NULL;

  return nowarn_map ? nowarn_map->get (key) : NULL;
}

/* Return true if warning OPT is suppressed for decl/expression EXPR.
   By default tests the disposition for any warning.  */

bool
warning_suppressed_p (const_tree expr, opt_code opt /* = all_warnings */)
{
  const nowarn_spec_t *spec = get_nowarn_spec (expr);

  if (!spec)
    return get_no_warning_bit (expr);

  const nowarn_spec_t optspec (opt);
  bool dis = *spec & optspec;
  gcc_assert (get_no_warning_bit (expr) || !dis);
  return dis;
}

/* Return true if warning OPT is suppressed for statement STMT.
   By default tests the disposition for any warning.  */

bool
warning_suppressed_p (const gimple *stmt, opt_code opt /* = all_warnings */)
{
  const nowarn_spec_t *spec = get_nowarn_spec (stmt);

  if (!spec)
    /* Fall back on the single no-warning bit.  */
    return get_no_warning_bit (stmt);

  const nowarn_spec_t optspec (opt);
  bool dis = *spec & optspec;
  gcc_assert (get_no_warning_bit (stmt) || !dis);
  return dis;
}

/* Enable, or by default disable, a warning for the expression.
   The wildcard OPT of -1 controls all warnings.  */

void
suppress_warning (tree expr, opt_code opt /* = all_warnings */,
		  bool supp /* = true */)
{
  if (opt == no_warning)
    return;

  const key_type_t key = convert_to_key (expr);

  supp = suppress_warning_at (key, opt, supp) || supp;
  set_no_warning_bit (expr, supp);
}

/* Enable, or by default disable, a warning for the statement STMT.
   The wildcard OPT of -1 controls all warnings.  */

void
suppress_warning (gimple *stmt, opt_code opt /* = all_warnings */,
		  bool supp /* = true */)
{
  if (opt == no_warning)
    return;

  const key_type_t key = convert_to_key (stmt);

  supp = suppress_warning_at (key, opt, supp) || supp;
  set_no_warning_bit (stmt, supp);
}

/* Copy the warning disposition mapping between an expression and/or
   a statement.  */

template <class ToType, class FromType>
void copy_warning (ToType to, FromType from)
{
  const key_type_t to_key = convert_to_key (to);

  if (nowarn_spec_t *from_map = get_nowarn_spec (from))
    {
      /* If there's an entry in the map the no-warning bit must be set.  */
      gcc_assert (get_no_warning_bit (from));

      if (!nowarn_map)
	nowarn_map = xint_hash_map_t::create_ggc (32);

      nowarn_map->put (to_key, *from_map);
      set_no_warning_bit (to, true);
    }
  else
    {
      if (nowarn_map)
	nowarn_map->remove (to_key);

      /* The no-warning bit might be set even if there's no entry
	 in the map.  */
      set_no_warning_bit (to, get_no_warning_bit (from));
    }
}

/* Copy the warning disposition mapping from one expression to another.  */

void
copy_warning (tree to, const_tree from)
{
  copy_warning<tree, const_tree>(to, from);
}

/* Copy the warning disposition mapping from a statement to an expression.  */

void
copy_warning (tree to, const gimple *from)
{
  copy_warning<tree, const gimple *>(to, from);
}

/* Copy the warning disposition mapping from an expression to a statement.  */

void
copy_warning (gimple *to, const_tree from)
{
  copy_warning<gimple *, const_tree>(to, from);
}

/* Copy the warning disposition mapping from one statement to another.  */

void
copy_warning (gimple *to, const gimple *from)
{
  copy_warning<gimple *, const gimple *>(to, from);
}
