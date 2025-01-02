/* C++11 wrapper around libiberty's argv.c
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_UNIQUE_ARGV_H
#define GCC_UNIQUE_ARGV_H

/* C++11 wrapper around libiberty's argv.c, with
   ownership of the underlying array and strings.  */

struct unique_argv
{
  /* Take ownership of argv.  */
  unique_argv (char **argv)
  : m_argv (argv)
  {
  }

  ~unique_argv ()
  {
    freeargv (m_argv);
  }

  unique_argv (const unique_argv &other) = delete;
  unique_argv &operator= (const unique_argv &other) = delete;

  unique_argv (unique_argv &&other)
  : m_argv (other.m_argv)
  {
    other.m_argv = nullptr;
  }

  unique_argv &operator= (unique_argv &&other)
  {
    freeargv (m_argv);
    m_argv = other.m_argv;
    other.m_argv = nullptr;
    return *this;
  }

  char **release ()
  {
    char **result = m_argv;
    m_argv = nullptr;
    return result;
  }

  char **m_argv;
};

#endif /* ! GCC_UNIQUE_ARGV_H */
