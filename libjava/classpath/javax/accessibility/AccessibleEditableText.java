/* AccessibleEditableText.java -- aids in accessibly for editable text
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package javax.accessibility;

import javax.swing.text.AttributeSet;

/**
 * Objects which present editable textual information on the display should
 * implement this interface.  Accessibility software can use the
 * implementations of this interface to change the content, attributes,
 * and spacial location of the text.
 *
 * <p>The <code>AccessibleContext.getAccessibleEditableText()</code> method
 * should return <code>null</code> if an object does not implement this
 * interface.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleContext#getAccessibleText()
 * @see AccessibleContext#getAccessibleEditableText()
 * @since 1.2
 * @status updated to 1.4, except for javax.swing support
 */
public interface AccessibleEditableText extends AccessibleText
{
  /**
   * Set the text contents to the given string.
   *
   * @param s the new text
   */
  // XXX What happens if s is null?
  void setTextContents(String s);

  /**
   * Inserts the given string at the specified location.
   *
   * @param index the index for insertion
   * @param s the new text
   */
  // XXX What happens if index is out of bounds, or s is null?
  void insertTextAtIndex(int index, String s);

  /**
   * Return the text between two points.
   *
   * @param start the start position, inclusive
   * @param end the end position, exclusive
   */
  // XXX What happens if indices are out of bounds?
  String getTextRange(int start, int end);

  /**
   * Delete the text between two points.
   *
   * @param start the start position, inclusive
   * @param end the end position, exclusive
   */
  // XXX What happens if indices are out of bounds?
  void delete(int start, int end);

  /**
   * Cut the text between two points to the system clipboard.
   *
   * @param start the start position, inclusive
   * @param end the end position, exclusive
   */
  // XXX What happens if indices are out of bounds?
  void cut(int start, int end);

  /**
   * Paste the text from the system clipboard at the given index.
   *
   * @param start the start position
   */
  // XXX What happens if start is out of bounds?
  void paste(int start);

  /**
   * Replace the text between two points with the given string.
   *
   * @param start the start position, inclusive
   * @param end the end position, exclusive
   * @param s the string to paste
   */
  // XXX What happens if indices are out of bounds, or s is null?
  void replaceText(int start, int end, String s);

  /**
   * Select the text between two points.
   *
   * @param start the start position, inclusive
   * @param stop the end position, exclusive
   */
  // XXX What happens if indices are out of bounds?
  void selectText(int start, int stop);

  /**
   * Set the attributes of text between two points.
   *
   * @param start the start position, inclusive
   * @param end the end position, exclusive
   * @param s the new attribute set for the range
   */
  // XXX What happens if indices are out of bounds, or s is null?
  void setAttributes(int start, int end, AttributeSet s);
} // interface AccessibleEditableText
