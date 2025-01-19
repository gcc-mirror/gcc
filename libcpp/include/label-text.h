/* A very simple string class.
   Copyright (C) 2015-2025 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#ifndef LIBCPP_LABEL_TEXT_H
#define LIBCPP_LABEL_TEXT_H

/* A struct for the result of range_label::get_text: a NUL-terminated buffer
   of localized text, and a flag to determine if the caller should "free" the
   buffer.  */

class label_text
{
public:
  label_text ()
  : m_buffer (NULL), m_owned (false)
  {}

  ~label_text ()
  {
    if (m_owned)
      free (m_buffer);
  }

  /* Move ctor.  */
  label_text (label_text &&other)
  : m_buffer (other.m_buffer), m_owned (other.m_owned)
  {
    other.release ();
  }

  /* Move assignment.  */
  label_text & operator= (label_text &&other)
  {
    if (m_owned)
      free (m_buffer);
    m_buffer = other.m_buffer;
    m_owned = other.m_owned;
    other.release ();
    return *this;
  }

  /* Delete the copy ctor and copy-assignment operator.  */
  label_text (const label_text &) = delete;
  label_text & operator= (const label_text &) = delete;

  /* Create a label_text instance that borrows BUFFER from a
     longer-lived owner.  */
  static label_text borrow (const char *buffer)
  {
    return label_text (const_cast <char *> (buffer), false);
  }

  /* Create a label_text instance that takes ownership of BUFFER.  */
  static label_text take (char *buffer)
  {
    return label_text (buffer, true);
  }

  void release ()
  {
    m_buffer = NULL;
    m_owned = false;
  }

  const char *get () const
  {
    return m_buffer;
  }

  bool is_owner () const
  {
    return m_owned;
  }

private:
  char *m_buffer;
  bool m_owned;

  label_text (char *buffer, bool owned)
  : m_buffer (buffer), m_owned (owned)
  {}
};

#endif /* !LIBCPP_LABEL_TEXT_H  */
