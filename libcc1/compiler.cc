/* Compiler handling for plugin
   Copyright (C) 2014-2021 Free Software Foundation, Inc.

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

#include <cc1plugin-config.h>
#include <string>
#include <sstream>
#include "libiberty.h"
#include "compiler.hh"
#include "xregex.h"
#include "findcomp.hh"
#include "intl.h"

// Construct an appropriate regexp to match the compiler name.
static std::string
make_regexp (const std::string &triplet_regexp, const char *compiler)
{
  std::stringstream buf;

  buf << "^" << triplet_regexp << "-";

  // Quote the compiler name in case it has something funny in it.
  for (const char *p = compiler; *p; ++p)
    {
      switch (*p)
	{
	case '.':
	case '^':
	case '$':
	case '*':
	case '+':
	case '?':
	case '(':
	case ')':
	case '[':
	case '{':
	case '\\':
	case '|':
	  buf << '\\';
	  break;
	}
      buf << *p;
    }
  buf << "$";

  return buf.str ();
}

char *
cc1_plugin::compiler::find (const char *, std::string &) const
{
  return xstrdup (_("Compiler has not been specified"));
}

char *
cc1_plugin::compiler_triplet_regexp::find (const char *base,
					   std::string &compiler) const
{
  std::string rx = make_regexp (triplet_regexp_, base);
  if (verbose)
    fprintf (stderr, _("searching for compiler matching regex %s\n"),
	     rx.c_str());
  regex_t triplet;
  int code = regcomp (&triplet, rx.c_str (), REG_EXTENDED | REG_NOSUB);
  if (code != 0)
    {
      size_t len = regerror (code, &triplet, NULL, 0);
      char err[len];

      regerror (code, &triplet, err, len);

      return concat ("Could not compile regexp \"",
		     rx.c_str (),
		     "\": ",
		     err,
		     (char *) NULL);
    }

  if (!find_compiler (triplet, &compiler))
    {
      regfree (&triplet);
      return concat ("Could not find a compiler matching \"",
		     rx.c_str (),
		     "\"",
		     (char *) NULL);
    }
  regfree (&triplet);
  if (verbose)
    fprintf (stderr, _("found compiler %s\n"), compiler.c_str());
  return NULL;
}

char *
cc1_plugin::compiler_driver_filename::find (const char *,
					    std::string &compiler) const
{
  // Simulate fnotice by fprintf.
  if (verbose)
    fprintf (stderr, _("using explicit compiler filename %s\n"),
	     driver_filename_.c_str());
  compiler = driver_filename_;
  return NULL;
}
