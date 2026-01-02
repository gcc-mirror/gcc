/* Classes for creating XML trees by appending.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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
  printer (element &insertion_point, bool check_popped_tags = true);

  void push_tag (std::string name,
		 bool preserve_whitespace = false);
  void push_tag_with_class (std::string name,
			    std::string class_,
			    bool preserve_whitespace = false);
  void pop_tag (const char *expected_name);

  void set_attr (const char *name, std::string value);

  void add_text (std::string text);
  void add_text_from_pp (pretty_printer &pp);

  void add_raw (std::string text);

  void push_element (std::unique_ptr<element> new_element);

  void append (std::unique_ptr<node> new_node);

  element *get_insertion_point () const;

  size_t get_num_open_tags () const { return m_open_tags.size (); }

  void DEBUG_FUNCTION dump () const;

private:
  // borrowed ptrs:
  std::vector<element *> m_open_tags;
  bool m_check_popped_tags;
};

/* RAII class for ensuring that the tags nested correctly.
   Verify that within an instance's lifetime that any pushes
   to the printer's insertion point have been popped by the end.  */

class auto_check_tag_nesting
{
public:
  auto_check_tag_nesting (const printer &xp)
  : m_xp (xp),
    m_initial_insertion_element (xp.get_insertion_point ())
  {
  }
  ~auto_check_tag_nesting ()
  {
    /* Verify that we didn't pop too many tags within the printer,
       or leave any tags open.  */
    gcc_assert (m_initial_insertion_element == m_xp.get_insertion_point ());
  }

private:
  const printer &m_xp;
  const element *m_initial_insertion_element;
};

// RAII for push/pop element on xml::printer

class auto_print_element
{
public:
  auto_print_element (printer &printer,
		      std::string name,
		      bool preserve_whitespace = false)
  : m_printer (printer),
    m_name (std::move (name))
  {
    m_printer.push_tag (m_name, preserve_whitespace);
  }
  ~auto_print_element ()
  {
    m_printer.pop_tag (m_name.c_str ());
  }

private:
  printer &m_printer;
  std::string m_name;
};

} // namespace xml

#endif /* GCC_XML_PRINTER_H.  */
