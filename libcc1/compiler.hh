/* Compiler handling for plugin
   Copyright (C) 2014-2020 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_COMPILER_HH
#define CC1_PLUGIN_COMPILER_HH

namespace cc1_plugin
{

  // Base class for compiler.
  class compiler
  {
  public:
    explicit compiler (bool v)
      : verbose (v)
    {
    }

    virtual ~compiler () = default;

    // Find the compiler.  BASE is the base name of the compiler, see
    // compiler-name.hh.  This sets COMPILER to the resulting path.
    // Returns nullptr on success, or a malloc'd error string on
    // failure.
    virtual char *find (const char *base, std::string &compiler) const;

    void set_verbose (bool v)
    {
      verbose = v;
    }

  protected:
    bool verbose;
  };

  /* Compiler to set by set_triplet_regexp.  */
  class compiler_triplet_regexp : public compiler
  {
  private:
    std::string triplet_regexp_;
  public:

    char *find (const char *base, std::string &compiler) const override;

    compiler_triplet_regexp (bool v, const char *triplet_regexp)
      : compiler (v), triplet_regexp_ (triplet_regexp)
    {
    }
  };

  /* Compiler to set by set_driver_filename.  */
  class compiler_driver_filename : public compiler
  {
  private:
    std::string driver_filename_;
  public:
    char *find (const char *base, std::string &compiler) const override;

    compiler_driver_filename (bool v, const char *driver_filename)
      : compiler (v), driver_filename_ (driver_filename)
    {
    }
  };

}

#endif // CC1_PLUGIN_COMPILER_HH
