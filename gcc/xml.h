/* Classes for representing XML trees.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_XML_H
#define GCC_XML_H

namespace xml {

// Forward decls; indentation reflects inheritance
struct node;
  struct text;
  struct node_with_children;
    struct document;
    struct element;

struct node
{
  virtual ~node () {}
  virtual void write_as_xml (pretty_printer *pp,
			     int depth, bool indent) const = 0;
  virtual text *dyn_cast_text ()
  {
    return 0;
  }
  void dump (FILE *out) const;
  void DEBUG_FUNCTION dump () const { dump (stderr); }
};

struct text : public node
{
  text (std::string str)
  : m_str (std::move (str))
  {}

  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;

  text *dyn_cast_text () final override
  {
    return this;
  }

  std::string m_str;
};

struct node_with_children : public node
{
  void add_child (std::unique_ptr<node> node);
  void add_text (std::string str);

  std::vector<std::unique_ptr<node>> m_children;
};

struct document : public node_with_children
{
  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;
};

struct element : public node_with_children
{
  element (std::string kind, bool preserve_whitespace)
    : m_kind (std::move (kind)),
    m_preserve_whitespace (preserve_whitespace)
  {}

  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;

  void set_attr (const char *name, std::string value);

  std::string m_kind;
  bool m_preserve_whitespace;
  std::map<std::string, std::string> m_attributes;
  std::vector<std::string> m_key_insertion_order;
};

/* A fragment of raw XML source, to be spliced in directly.
   Use sparingly.  */

struct raw : public node
{
  raw (std::string xml_src)
  : m_xml_src (xml_src)
  {
  }

  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;

  std::string m_xml_src;
};

} // namespace xml

#endif /* GCC_XML_H.  */
