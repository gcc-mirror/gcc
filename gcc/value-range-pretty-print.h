/* Pretty print support for value ranges.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_VALUE_RANGE_PRETTY_H
#define GCC_VALUE_RANGE_PRETTY_H

class vrange_printer : public vrange_visitor
{
public:
  vrange_printer (pretty_printer *pp_) : pp (pp_) { }
  void visit (const unsupported_range &) const override;
  void visit (const irange &) const override;
  void visit (const frange &) const override;
private:
  void print_irange_bound (const wide_int &w, tree type) const;
  void print_irange_bitmasks (const irange &) const;
  void print_frange_nan (const frange &) const;
  void print_real_value (tree type, const REAL_VALUE_TYPE &r) const;

  pretty_printer *pp;
};

#endif // GCC_VALUE_RANGE_PRETTY_H
