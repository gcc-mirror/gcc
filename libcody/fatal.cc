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
  // A useful place for a breakpoint to land.
  //__asm__ volatile ("nop");  // HCF - you goofed!

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

void BuildNote (FILE *stream) noexcept
{
  fprintf (stream, "Version %s.\n", PACKAGE_NAME " " PACKAGE_VERSION);
  fprintf (stream, "Report bugs to %s.\n", BUGURL[0] ? BUGURL : "you");
  if (PACKAGE_URL[0])
    fprintf (stream, "See %s for more information.\n", PACKAGE_URL);
  if (REVISION[0])
    fprintf (stream, "Source %s.\n", REVISION);

  fprintf (stream, "Build is %s & %s.\n",
#if !NMS_CHECKING
	   "un"
#endif
	   "checked",
#if !__OPTIMIZE__
	   "un"
#endif
	   "optimized");
}

}
