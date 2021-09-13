/* "True" vs "False" vs "Unknown".
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tristate.h"
#include "selftest.h"

const char *
tristate::as_string () const
{
  switch (m_value)
    {
    default:
      gcc_unreachable ();
    case TS_UNKNOWN:
      return "UNKNOWN";
    case TS_TRUE:
      return "TRUE";
    case TS_FALSE:
      return "FALSE";
    }
}

tristate
tristate::not_ () const
{
  switch (m_value)
    {
    default:
      gcc_unreachable ();
    case TS_UNKNOWN:
      return tristate (TS_UNKNOWN);
    case TS_TRUE:
      return tristate (TS_FALSE);
    case TS_FALSE:
      return tristate (TS_TRUE);
    }
}

tristate
tristate::or_ (tristate other) const
{
  switch (m_value)
    {
    default:
      gcc_unreachable ();
    case TS_UNKNOWN:
      if (other.is_true ())
	return tristate (TS_TRUE);
      else
	return tristate (TS_UNKNOWN);
    case TS_FALSE:
      return other;
    case TS_TRUE:
      return tristate (TS_TRUE);
    }
}

tristate
tristate::and_ (tristate other) const
{
  switch (m_value)
    {
    default:
      gcc_unreachable ();
    case TS_UNKNOWN:
      if (other.is_false ())
	return tristate (TS_FALSE);
      else
	return tristate (TS_UNKNOWN);
    case TS_TRUE:
      return other;
    case TS_FALSE:
      return tristate (TS_FALSE);
    }
}

#if CHECKING_P

namespace selftest {

#define ASSERT_TRISTATE_TRUE(TRISTATE) \
  SELFTEST_BEGIN_STMT					\
  ASSERT_EQ (TRISTATE, tristate (tristate::TS_TRUE));	\
  SELFTEST_END_STMT

#define ASSERT_TRISTATE_FALSE(TRISTATE) \
  SELFTEST_BEGIN_STMT					\
  ASSERT_EQ (TRISTATE, tristate (tristate::TS_FALSE));	\
  SELFTEST_END_STMT

#define ASSERT_TRISTATE_UNKNOWN(TRISTATE) \
  SELFTEST_BEGIN_STMT						\
  ASSERT_EQ (TRISTATE, tristate (tristate::TS_UNKNOWN));	\
  SELFTEST_END_STMT

/* Test tristate's ctors, along with is_*, as_string, operator==, and
   operator!=.  */

static void
test_ctors ()
{
  tristate u (tristate::TS_UNKNOWN);
  ASSERT_FALSE (u.is_known ());
  ASSERT_FALSE (u.is_true ());
  ASSERT_FALSE (u.is_false ());
  ASSERT_STREQ (u.as_string (), "UNKNOWN");

  tristate t (tristate::TS_TRUE);
  ASSERT_TRUE (t.is_known ());
  ASSERT_TRUE (t.is_true ());
  ASSERT_FALSE (t.is_false ());
  ASSERT_STREQ (t.as_string (), "TRUE");

  tristate f (tristate::TS_FALSE);
  ASSERT_TRUE (f.is_known ());
  ASSERT_FALSE (f.is_true ());
  ASSERT_TRUE (f.is_false ());
  ASSERT_STREQ (f.as_string (), "FALSE");

  ASSERT_EQ (u, u);
  ASSERT_EQ (t, t);
  ASSERT_EQ (f, f);
  ASSERT_NE (u, t);
  ASSERT_NE (u, f);
  ASSERT_NE (t, f);

  tristate t2 (true);
  ASSERT_TRUE (t2.is_true ());
  ASSERT_EQ (t, t2);

  tristate f2 (false);
  ASSERT_TRUE (f2.is_false ());
  ASSERT_EQ (f, f2);

  tristate u2 (tristate::unknown ());
  ASSERT_TRUE (!u2.is_known ());
  ASSERT_EQ (u, u2);
}

/* Test && on tristate instances.  */

static void
test_and ()
{
  ASSERT_TRISTATE_UNKNOWN (tristate::unknown () && tristate::unknown ());

  ASSERT_TRISTATE_FALSE (tristate (false) && tristate (false));
  ASSERT_TRISTATE_FALSE (tristate (false) && tristate (true));
  ASSERT_TRISTATE_FALSE (tristate (true) && tristate (false));
  ASSERT_TRISTATE_TRUE (tristate (true) && tristate (true));

  ASSERT_TRISTATE_UNKNOWN (tristate::unknown () && tristate (true));
  ASSERT_TRISTATE_UNKNOWN (tristate (true) && tristate::unknown ());

  ASSERT_TRISTATE_FALSE (tristate::unknown () && tristate (false));
  ASSERT_TRISTATE_FALSE (tristate (false) && tristate::unknown ());
}

/* Test || on tristate instances.  */

static void
test_or ()
{
  ASSERT_TRISTATE_UNKNOWN (tristate::unknown () || tristate::unknown ());

  ASSERT_TRISTATE_FALSE (tristate (false) || tristate (false));
  ASSERT_TRISTATE_TRUE (tristate (false) || tristate (true));
  ASSERT_TRISTATE_TRUE (tristate (true) || tristate (false));
  ASSERT_TRISTATE_TRUE (tristate (true) || tristate (true));

  ASSERT_TRISTATE_TRUE (tristate::unknown () || tristate (true));
  ASSERT_TRISTATE_TRUE (tristate (true) || tristate::unknown ());

  ASSERT_TRISTATE_UNKNOWN (tristate::unknown () || tristate (false));
  ASSERT_TRISTATE_UNKNOWN (tristate (false) || tristate::unknown ());
}

/* Test ! on tristate instances.  */

static void
test_not ()
{
  ASSERT_TRISTATE_UNKNOWN (!tristate::unknown ());
  ASSERT_TRISTATE_FALSE (!tristate (true));
  ASSERT_TRISTATE_TRUE (!tristate (false));
}

/* Run all of the selftests within this file.  */

void
tristate_cc_tests ()
{
  test_ctors ();
  test_and ();
  test_or ();
  test_not ();
}

} // namespace selftest

#endif /* CHECKING_P */
