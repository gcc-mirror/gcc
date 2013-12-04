/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

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

#ifndef GCC_UBSAN_H
#define GCC_UBSAN_H

/* The various kinds of NULL pointer checks.  */
enum ubsan_null_ckind {
  UBSAN_LOAD_OF,
  UBSAN_STORE_OF,
  UBSAN_REF_BINDING,
  UBSAN_MEMBER_ACCESS,
  UBSAN_MEMBER_CALL
};

/* An extra data used by ubsan pointer checking.  */
struct ubsan_mismatch_data {
  tree align;
  tree ckind;
};

extern void ubsan_expand_null_ifn (gimple_stmt_iterator);
extern tree ubsan_instrument_unreachable (location_t);
extern tree ubsan_create_data (const char *, location_t,
			       const struct ubsan_mismatch_data *, ...);
extern tree ubsan_type_descriptor (tree, bool);
extern tree ubsan_encode_value (tree);
extern bool is_ubsan_builtin_p (tree);
extern tree ubsan_build_overflow_builtin (tree_code, location_t, tree, tree, tree);

#endif  /* GCC_UBSAN_H  */

