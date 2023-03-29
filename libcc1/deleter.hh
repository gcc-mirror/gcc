/* Deleter objects
   Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_DELETER_HH
#define CC1_PLUGIN_DELETER_HH

#include <memory>

namespace cc1_plugin
{
  // Any pointer type requires a deleter object that knows how to
  // clean up.  These are used in multiple places.
  template<typename T> struct deleter;

  template<>
  struct deleter<char>
  {
    void operator() (char *s)
    {
      delete[] s;
    }
  };

  template<>
  struct deleter<gcc_type_array>
  {
    void operator() (gcc_type_array *p)
    {
      delete[] p->elements;
      delete p;
    }
  };

  template<typename T> using unique_ptr = std::unique_ptr<T, deleter<T>>;
}

#endif // CC1_PLUGIN_DELETER_HH
