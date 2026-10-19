#!/bin/sh
# Copyright (C) 2026 Free Software Foundation, Inc.
# This file is part of GCC, distributed under the GNU General Public License,
# version 3 or (at your option) any later version.  See COPYING3.

# Escape a literal installation directory for GCC specs, then for a sed
# replacement.  In particular, whitespace must remain in a single argument
# and '%' must not introduce a spec expansion.
test "$#" = 1 || exit 1
runtime_dir=$(printf '%s\n' "$1" | sed 's/[%]/%%/g; s/[\\ \t]/\\&/g; s/[\\&|]/\\&/g') || exit 1
sed "s|@LIBGCOBOL_RUNTIME_DIR@|$runtime_dir|g"
