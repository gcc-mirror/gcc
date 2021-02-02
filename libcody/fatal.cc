// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2019-2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

// Cody
#include "internal.hh"
// C
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace Cody {

#if NMS_CHECKING
void (AssertFailed) (Location loc) noexcept
{
  (HCF) ("assertion failed", loc);
}
void (Unreachable) (Location loc) noexcept
{
  (HCF) ("unreachable reached", loc);
}
#endif

void (HCF) (char const *msg
#if NMS_CHECKING
	  , Location const loc
#endif
	  ) noexcept
{ // HCF - you goofed!

#if !NMS_CHECKING
  constexpr Location loc (nullptr, 0);
#endif

  fprintf (stderr, "CODYlib: %s", msg ? msg : "internal error");
  if (char const *file = loc.File ())
    {
      char const *src = SRCDIR;

      if (src[0])
	{
	  size_t l = strlen (src);

	  if (!strncmp (src, file, l) && file[l] == '/')
	    file += l + 1;
	}
      fprintf (stderr, " at %s:%u", file, loc.Line ());
    }
  fprintf (stderr, "\n");
  raise (SIGABRT);
  exit (2);
}

}
