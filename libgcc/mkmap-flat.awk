# Generate a flat list of symbols to export.
#	Copyright (C) 2007-2020 Free Software Foundation, Inc.
#	Contributed by Richard Henderson <rth@cygnus.com>
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Options:
#   "-v leading_underscore=1" : Symbols in map need leading underscore.
#   "-v pe_dll=1"             : Create .DEF file for Windows PECOFF
#                               DLL link instead of map file.

BEGIN {
  state = "nm";
  excluding = 0;
  if (leading_underscore)
    prefix = "_";
  else
    prefix = "";
}

# Remove comment and blank lines.
/^ *#/ || /^ *$/ {
  next;
}

# We begin with nm input.  Collect the set of symbols that are present
# so that we can elide undefined symbols.

state == "nm" && /^%%/ {
  state = "ver";
  next;
}

state == "nm" && ($1 == "U" || $2 == "U") {
  next;
}

state == "nm" && NF == 3 {
  def[$3] = 1;
  next;
}

state == "nm" {
  next;
}

# Now we process a simplified variant of the Solaris symbol version
# script.  We have one symbol per line, no semicolons, simple markers
# for beginning and ending each section, and %inherit markers for
# describing version inheritance.  A symbol may appear in more than
# one symbol version, and the last seen takes effect.
# The magic version name '%exclude' causes all the symbols given that
# version to be dropped from the output (unless a later version overrides).

NF == 3 && $1 == "%inherit" {
  next;
}

NF == 2 && $2 == "{" {
  if ($1 == "%exclude")
    excluding = 1;
  next;
}

$1 == "}" {
  excluding = 0;
  next;
}

{
  sym = prefix $1;
  if (excluding)
    delete export[sym];
  else
    export[sym] = 1;
  next;
}

END {

  if (pe_dll) {
    print "LIBRARY " pe_dll;
    print "EXPORTS";
  }

  for (sym in export)
    if (def[sym] || (pe_dll && def["_" sym]))
      print sym;
}
