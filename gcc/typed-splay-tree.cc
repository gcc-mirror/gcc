/* Selftests for typed-splay-tree.h.
   Copyright (C) 2016-2023 Free Software Foundation, Inc.

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
#include "typed-splay-tree.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

/* Callback for use by test_str_to_int.  */

static int
append_cb (const char *, int value, void *user_data)
{
  auto_vec <int> *vec = (auto_vec <int> *)user_data;
  vec->safe_push (value);
  return 0;
}

/* Test of typed_splay_tree <const char *, int>.  */

static void
test_str_to_int ()
{
  typed_splay_tree <const char *, int> t (strcmp, NULL, NULL);

  t.insert ("a", 1);
  t.insert ("b", 2);
  t.insert ("c", 3);
  t.insert ("d", 4);

  t.remove ("d");

  ASSERT_EQ (1, t.lookup ("a"));
  ASSERT_EQ (2, t.lookup ("b"));
  ASSERT_EQ (3, t.lookup ("c"));

  ASSERT_EQ (2, t.predecessor ("c"));
  ASSERT_EQ (3, t.successor ("b"));
  ASSERT_EQ (1, t.min ());
  ASSERT_EQ (3, t.max ());

  /* Test foreach by appending to a vec, and verifying the vec.  */
  auto_vec <int> v;
  t.foreach (append_cb, &v);
  ASSERT_EQ (3, v.length ());
  ASSERT_EQ (1, v[0]);
  ASSERT_EQ (2, v[1]);
  ASSERT_EQ (3, v[2]);
}

/* Run all of the selftests within this file.  */

void
typed_splay_tree_cc_tests ()
{
  test_str_to_int ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
