/* Unit tests for unique-ptr.h.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
#define INCLUDE_UNIQUE_PTR
#include "system.h"
#include "coretypes.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

namespace {

/* A class for counting ctor and dtor invocations.  */

class stats
{
public:
  stats () : ctor_count (0), dtor_count (0) {}

  int ctor_count;
  int dtor_count;
};

/* A class that uses "stats" to track its ctor and dtor invocations.  */

class foo
{
public:
  foo (stats &s) : m_s (s) { ++m_s.ctor_count; }
  ~foo () { ++m_s.dtor_count; }

  int example_method () const { return 42; }

private:
  foo (const foo&);
  foo & operator= (const foo &);

private:
  stats &m_s;
};

/* A struct for testing unique_ptr<T[]>.  */

class has_default_ctor
{
public:
  has_default_ctor () : m_field (42) {}
  int m_field;
};

/* A dummy struct for testing unique_xmalloc_ptr.  */

struct dummy
{
  int field;
};

} // anonymous namespace

/* Verify that the default ctor inits ptrs to NULL.  */

static void
test_null_ptr ()
{
  gnu::unique_ptr<void *> p;
  ASSERT_EQ (NULL, p);

  gnu::unique_xmalloc_ptr<void *> q;
  ASSERT_EQ (NULL, q);
}

/* Verify that deletion happens when a unique_ptr goes out of scope.  */

static void
test_implicit_deletion ()
{
  stats s;
  ASSERT_EQ (0, s.ctor_count);
  ASSERT_EQ (0, s.dtor_count);

  {
    gnu::unique_ptr<foo> f (new foo (s));
    ASSERT_NE (NULL, f);
    ASSERT_EQ (1, s.ctor_count);
    ASSERT_EQ (0, s.dtor_count);
  }

  /* Verify that the foo was implicitly deleted.  */
  ASSERT_EQ (1, s.ctor_count);
  ASSERT_EQ (1, s.dtor_count);
}

/* Verify that we can assign to a NULL unique_ptr.  */

static void
test_overwrite_of_null ()
{
  stats s;
  ASSERT_EQ (0, s.ctor_count);
  ASSERT_EQ (0, s.dtor_count);

  {
    gnu::unique_ptr<foo> f;
    ASSERT_EQ (NULL, f);
    ASSERT_EQ (0, s.ctor_count);
    ASSERT_EQ (0, s.dtor_count);

    /* Overwrite with a non-NULL value.  */
    f = gnu::unique_ptr<foo> (new foo (s));
    ASSERT_EQ (1, s.ctor_count);
    ASSERT_EQ (0, s.dtor_count);
  }

  /* Verify that the foo is implicitly deleted.  */
  ASSERT_EQ (1, s.ctor_count);
  ASSERT_EQ (1, s.dtor_count);
}

/* Verify that we can assign to a non-NULL unique_ptr.  */

static void
test_overwrite_of_non_null ()
{
  stats s;
  ASSERT_EQ (0, s.ctor_count);
  ASSERT_EQ (0, s.dtor_count);

  {
    gnu::unique_ptr<foo> f (new foo (s));
    ASSERT_NE (NULL, f);
    ASSERT_EQ (1, s.ctor_count);
    ASSERT_EQ (0, s.dtor_count);

    /* Overwrite with a different value.  */
    f = gnu::unique_ptr<foo> (new foo (s));
    ASSERT_EQ (2, s.ctor_count);
    ASSERT_EQ (1, s.dtor_count);
  }

  /* Verify that the 2nd foo was implicitly deleted.  */
  ASSERT_EQ (2, s.ctor_count);
  ASSERT_EQ (2, s.dtor_count);
}

/* Verify that unique_ptr's overloaded ops work.  */

static void
test_overloaded_ops ()
{
  stats s;
  gnu::unique_ptr<foo> f (new foo (s));
  ASSERT_EQ (42, f->example_method ());
  ASSERT_EQ (42, (*f).example_method ());
  ASSERT_EQ (f, f);
  ASSERT_NE (NULL, f.get ());

  gnu::unique_ptr<foo> g (new foo (s));
  ASSERT_NE (f, g);
}

/* Verify that the gnu::unique_ptr specialization for T[] works.  */

static void
test_array_new ()
{
  const int num = 10;
  gnu::unique_ptr<has_default_ctor[]> p (new has_default_ctor[num]);
  ASSERT_NE (NULL, p.get ());
  /* Verify that operator[] works, and that the default ctor was called
     on each element.  */
  for (int i = 0; i < num; i++)
    ASSERT_EQ (42, p[i].m_field);
}

/* Verify that gnu::unique_xmalloc_ptr works.  */

static void
test_xmalloc ()
{
  gnu::unique_xmalloc_ptr<dummy> p (XNEW (dummy));
  ASSERT_NE (NULL, p.get ());
}

/* Verify the gnu::unique_xmalloc_ptr specialization for T[].  */

static void
test_xmalloc_array ()
{
  const int num = 10;
  gnu::unique_xmalloc_ptr<dummy[]> p (XNEWVEC (dummy, num));
  ASSERT_NE (NULL, p.get ());

  /* Verify that operator[] works.  */
  for (int i = 0; i < num; i++)
    p[i].field = 42;
  for (int i = 0; i < num; i++)
    ASSERT_EQ (42, p[i].field);
}

/* Run all of the selftests within this file.  */

void
unique_ptr_tests_cc_tests ()
{
  test_null_ptr ();
  test_implicit_deletion ();
  test_overwrite_of_null ();
  test_overwrite_of_non_null ();
  test_overloaded_ops ();
  test_array_new ();
  test_xmalloc ();
  test_xmalloc_array ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
