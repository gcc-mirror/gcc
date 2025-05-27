/* Classes for creating XML trees by appending.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_XML_PRINTER_H
#define GCC_XML_PRINTER_H

namespace xml {

class node;
  class element;

/* A class for creating XML trees by appending to an insertion
   point, with a stack of open tags.  */

class printer
{
public:
  printer (element &insertion_point);

  void push_tag (std::string name,
		 bool preserve_whitespace = false);
  void push_tag_with_class (std::string name,
			    std::string class_,
			    bool preserve_whitespace = false);
  void pop_tag ();

  void set_attr (const char *name, std::string value);

  void add_text (std::string text);

  void add_raw (std::string text);

  void push_element (std::unique_ptr<element> new_element);

  void append (std::unique_ptr<node> new_node);

  element *get_insertion_point () const;

private:
  // borrowed ptrs:
  std::vector<element *> m_open_tags;
};

// RAII for push/pop element on xml::printer

class auto_print_element
{
public:
  auto_print_element (printer &printer,
		      std::string name,
		      bool preserve_whitespace = false)
  : m_printer (printer)
  {
    m_printer.push_tag (name, preserve_whitespace);
  }
  ~auto_print_element ()
  {
    m_printer.pop_tag ();
  }

private:
  printer &m_printer;
};

} // namespace xml

#endif /* GCC_XML_PRINTER_H.  */
