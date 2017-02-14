/* Callback management
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_CALLBACKS_HH
#define CC1_PLUGIN_CALLBACKS_HH

#include "status.hh"
#include "hashtab.h"

namespace cc1_plugin
{
  class connection;

  // The type of a callback method.
  typedef status callback_ftype (connection *);

  // This class manages callback functions.  A callback has a name and
  // an underlying function.  When a query packet arrives, the name is
  // inspected and the corresponding function is called.  A callback
  // function has to know how to decode its own arguments, but
  // wrappers are provided elsewhere to automate this.
  class callbacks
  {
  public:

    callbacks ();
    ~callbacks ();

    // Add a callback named NAME.  FUNC is the function to call when
    // this method is invoked.
    void add_callback (const char *name, callback_ftype *func);

    // Look up a callback by name.  Returns NULL if the method is not
    // found.
    callback_ftype *find_callback (const char *name);

  private:

    // Declared but not defined to avoid use.
    callbacks (const callbacks &);
    callbacks &operator= (const callbacks &);

    // The mapping.
    htab_t m_registry;
  };
};

#endif // CC1_PLUGIN_CALLBACKS_HH
