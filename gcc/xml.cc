/* XML support for diagnostics.
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

#include "config.h"
#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "xml.h"
#include "xml-printer.h"
#include "pretty-print.h"
#include "selftest.h"
#include "selftest-xml.h"

namespace xml {

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif


/* Implementation.  */

static void
write_escaped_text (pretty_printer *pp, const char *text)
{
  gcc_assert (text);

  for (const char *p = text; *p; ++p)
    {
      char ch = *p;
      switch (ch)
	{
	default:
	  pp_character (pp, ch);
	  break;
	case '\'':
	  pp_string (pp, "&apos;");
	  break;
	case '"':
	  pp_string (pp, "&quot;");
	  break;
	case '&':
	  pp_string (pp, "&amp;");
	  break;
	case '<':
	  pp_string (pp, "&lt;");
	  break;
	case '>':
	  pp_string (pp, "&gt;");
	  break;
	}
    }
}

/* struct node.  */

void
node::dump (FILE *out) const
{
  pretty_printer pp;
  pp.set_output_stream (out);
  write_as_xml (&pp, 0, true);
  pp_flush (&pp);
}

/* struct text : public node.  */

void
text::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  if (indent)
    {
      for (int i = 0; i < depth; ++i)
	pp_string (pp, "  ");
    }
  write_escaped_text (pp, m_str.c_str ());
  if (indent)
    pp_newline (pp);
}

/* struct node_with_children : public node.  */

void
node_with_children::add_child (std::unique_ptr<node> node)
{
  gcc_assert (node.get ());
  m_children.push_back (std::move (node));
}

void
node_with_children::add_text (std::string str)
{
  // Consolidate runs of text
  if (!m_children.empty ())
    if (text *t = m_children.back ()->dyn_cast_text ())
      {
	t->m_str += std::move (str);
	return;
      }
  add_child (std::make_unique <text> (std::move (str)));
}

void
node_with_children::add_text_from_pp (pretty_printer &pp)
{
  add_text (pp_formatted_text (&pp));
}

void
node_with_children::add_comment (std::string str)
{
  add_child (std::make_unique <comment> (std::move (str)));
}

element *
node_with_children::find_child_element (std::string kind) const
{
  for (auto &iter : m_children)
    if (element *e = iter->dyn_cast_element ())
      if (e->m_kind == kind)
	return e;
  return nullptr;
}

/* struct document : public node_with_children.  */

void
document::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  pp_string (pp, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  if (m_doctypedecl)
    m_doctypedecl->write_as_xml (pp, depth, indent);
  for (auto &iter : m_children)
    iter->write_as_xml (pp, depth, indent);
}

/* struct element : public node_with_children.  */

void
element::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  if (indent)
    {
      for (int i = 0; i < depth; ++i)
	pp_string (pp, "  ");
    }

  pp_printf (pp, "<%s", m_kind.c_str ());
  for (auto &key : m_key_insertion_order)
    {
      auto iter = m_attributes.find (key);
      if (iter != m_attributes.end ())
	{
	  pp_printf (pp, " %s=\"", key.c_str ());
	  write_escaped_text (pp, iter->second.c_str ());
	  pp_string (pp, "\"");
	}
    }
  if (m_children.empty ())
    pp_string (pp, "/>");
  else
    {
      const bool indent_children = m_preserve_whitespace ? false : indent;
      pp_string (pp, ">");
      if (indent_children)
	pp_newline (pp);
      for (auto &child : m_children)
	child->write_as_xml (pp, depth + 1, indent_children);
      if (indent_children)
	{
	  for (int i = 0; i < depth; ++i)
	    pp_string (pp, "  ");
	}
      pp_printf (pp, "</%s>", m_kind.c_str ());
    }

  if (indent)
    pp_newline (pp);
}

void
element::set_attr (const char *name, std::string value)
{
  auto iter = m_attributes.find (name);
  if (iter == m_attributes.end ())
    m_key_insertion_order.push_back (name);
  m_attributes[name] = std::move (value);
}

const char *
element::get_attr (const char *name) const
{
  auto iter = m_attributes.find (name);
  if (iter == m_attributes.end ())
    return nullptr;
  return iter->second.c_str ();
}

// struct comment : public node

void
comment::write_as_xml (pretty_printer *pp,
		       int depth, bool indent) const
{
  if (indent)
    {
      for (int i = 0; i < depth; ++i)
	pp_string (pp, "  ");
    }
  pp_string (pp, "<!-- ");
  write_escaped_text (pp, m_text.c_str ());
  pp_string (pp, " -->");
  if (indent)
    pp_newline (pp);
}

// struct raw : public node

void
raw::write_as_xml (pretty_printer *pp,
		   int /*depth*/, bool /*indent*/) const
{
  pp_string (pp, m_xml_src.c_str ());
}

// class printer

printer::printer (element &insertion_point,
		  bool check_popped_tags)
: m_check_popped_tags (check_popped_tags)
{
  m_open_tags.push_back (&insertion_point);
}

void
printer::push_tag (std::string name,
		   bool preserve_whitespace)
{
  push_element
    (std::make_unique<element> (std::move (name),
				preserve_whitespace));
}

void
printer::push_tag_with_class (std::string name, std::string class_,
			      bool preserve_whitespace)
{
  auto new_element
    = std::make_unique<element> (std::move (name),
				 preserve_whitespace);
  new_element->set_attr ("class", class_);
  push_element (std::move (new_element));
}

/*  Pop the current topmost tag.
    If m_check_popped_tags, assert that the tag we're popping is
    EXPECTED_NAME.  */

void
printer::pop_tag (const char *expected_name ATTRIBUTE_UNUSED)
{
  gcc_assert (!m_open_tags.empty ());
  if (m_check_popped_tags)
    gcc_assert (expected_name == get_insertion_point ()->m_kind);
  m_open_tags.pop_back ();
}

void
printer::set_attr (const char *name, std::string value)
{
  m_open_tags.back ()->set_attr (name, value);
}

void
printer::add_text (std::string text)
{
  element *parent = m_open_tags.back ();
  parent->add_text (std::move (text));
}

void
printer::add_text_from_pp (pretty_printer &pp)
{
  element *parent = m_open_tags.back ();
  parent->add_text_from_pp (pp);
}

void
printer::add_raw (std::string text)
{
  element *parent = m_open_tags.back ();
  parent->add_child (std::make_unique<xml::raw> (std::move (text)));
}

void
printer::push_element (std::unique_ptr<element> new_element)
{
  gcc_assert (new_element.get ());
  element *parent = m_open_tags.back ();
  m_open_tags.push_back (new_element.get ());
  parent->add_child (std::move (new_element));
}

void
printer::append (std::unique_ptr<node> new_node)
{
  gcc_assert (new_node.get ());
  element *parent = m_open_tags.back ();
  parent->add_child (std::move (new_node));
}

element *
printer::get_insertion_point () const
{
  return m_open_tags.back ();
}

void
printer::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  pp_printf (&pp, "open tags: %i:", (int)m_open_tags.size ());
  for (auto iter : m_open_tags)
    pp_printf (&pp, " <%s>", iter->m_kind.c_str ());
  pp_newline (&pp);
  pp_printf (&pp, "xml:");
  pp_newline (&pp);
  m_open_tags[0]->write_as_xml (&pp, 1, true);
  pp_flush (&pp);
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

} // namespace xml

#if CHECKING_P

namespace selftest {

void
assert_xml_print_eq (const location &loc,
		     const xml::node &node,
		     const char *expected_value)
{
  pretty_printer pp;
  node.write_as_xml (&pp, 0, true);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected_value);
}

static void
test_no_dtd ()
{
  xml::document doc;
  ASSERT_XML_PRINT_EQ
    (doc,
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
}

static void
test_printer ()
{
  xml::element top ("top", false);
  xml::printer xp (top);
  xp.push_tag ("foo");
  xp.add_text ("hello");
  xp.push_tag ("bar");
  xp.set_attr ("size", "3");
  xp.set_attr ("color", "red");
  xp.add_text ("world");
  xp.push_tag ("baz");
  xp.pop_tag ("baz");
  xp.pop_tag ("bar");
  xp.pop_tag ("foo");

  ASSERT_XML_PRINT_EQ
    (top,
     "<top>\n"
     "  <foo>\n"
     "    hello\n"
     "    <bar size=\"3\" color=\"red\">\n"
     "      world\n"
     "      <baz/>\n"
     "    </bar>\n"
     "  </foo>\n"
     "</top>\n");

  xml::element *foo = top.find_child_element ("foo");
  ASSERT_TRUE (foo);
  ASSERT_EQ (top.find_child_element ("not-foo"), nullptr);
  xml::element *bar = foo->find_child_element ("bar");
  ASSERT_TRUE (bar);
  ASSERT_STREQ (bar->get_attr ("size"), "3");
  ASSERT_STREQ (bar->get_attr ("color"), "red");
  ASSERT_EQ (bar->get_attr ("airspeed-velocity"), nullptr);
}

// Verify that element attributes preserve insertion order.

static void
test_attribute_ordering ()
{
  xml::element top ("top", false);
  xml::printer xp (top);
  xp.push_tag ("chronological");
  xp.set_attr ("maldon", "991");
  xp.set_attr ("hastings", "1066");
  xp.set_attr ("edgehill", "1642");
  xp.set_attr ("naseby", "1645");
  xp.pop_tag ("chronological");
  xp.push_tag ("alphabetical");
  xp.set_attr ("edgehill", "1642");
  xp.set_attr ("hastings", "1066");
  xp.set_attr ("maldon", "991");
  xp.set_attr ("naseby", "1645");
  xp.pop_tag ("alphabetical");

  ASSERT_XML_PRINT_EQ
    (top,
     "<top>\n"
     "  <chronological maldon=\"991\" hastings=\"1066\" edgehill=\"1642\" naseby=\"1645\"/>\n"
     "  <alphabetical edgehill=\"1642\" hastings=\"1066\" maldon=\"991\" naseby=\"1645\"/>\n"
     "</top>\n");
}

static void
test_comment ()
{
  xml::document doc;
  doc.add_comment ("hello");
  doc.add_comment ("world");
  ASSERT_XML_PRINT_EQ
    (doc,
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<!-- hello -->\n"
     "<!-- world -->\n");

}
/* Run all of the selftests within this file.  */

void
xml_cc_tests ()
{
  test_no_dtd ();
  test_printer ();
  test_attribute_ordering ();
  test_comment ();
}

} // namespace selftest

#endif /* CHECKING_P */
