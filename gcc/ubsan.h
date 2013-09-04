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

extern tree ubsan_instrument_unreachable (location_t);
extern tree ubsan_create_data (const char *, location_t, ...);
extern tree ubsan_type_descriptor (tree);
extern tree ubsan_encode_value (tree);
extern bool is_ubsan_builtin_p (tree);

#endif  /* GCC_UBSAN_H  */

