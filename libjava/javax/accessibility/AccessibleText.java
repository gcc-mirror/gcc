/* AccessibleText.java -- aids in accessibly manipulating text
   Copyright (C) 2000, 2002, 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.text.AttributeSet;

/**
 * Objects which present textual information on the display should implement
 * this interface.  Accessibility software can use the implementations of
 * this interface to change the attributes and spacial location of the text.
 *
 * <p>The <code>AccessibleContext.getAccessibleText()</code> method
 * should return <code>null</code> if an object does not implement this
 * interface.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleContext#getAccessibleText()
 * @since 1.2
 * @status updated to 1.4
 */
public interface AccessibleText
{
  /**
   * Constant designating that the next selection should be a character.
   *
   * @see #getAtIndex(int, int)
   * @see #getAfterIndex(int, int)
   * @see #getBeforeIndex(int, int)
   */
  int CHARACTER = 1;

  /**
   * Constant designating that the next selection should be a word.
   *
   * @see #getAtIndex(int, int)
   * @see #getAfterIndex(int, int)
   * @see #getBeforeIndex(int, int)
   */
  int WORD = 2;

  /**
   * Constant designating that the next selection should be a sentence.
   *
   * @see #getAtIndex(int, int)
   * @see #getAfterIndex(int, int)
   * @see #getBeforeIndex(int, int)
   */
  int SENTENCE = 3;

  /**
   * Given a point in the coordinate system of this object, return the
   * 0-based index of the character at that point, or -1 if there is none.
   *
   * @param p the point to look at
   * @return the character index, or -1
   */
  int getIndexAtPoint(Point point);

  /**
   * Determines the bounding box of the indexed character. Returns an empty
   * rectangle if the index is out of bounds.
   *
   * @param index the 0-based character index
   * @return the bounding box, may be empty
   */
  Rectangle getCharacterBounds(int index);

  /**
   * Return the number of characters.
   *
   * @return the character count
   */
  int getCharCount();

  /**
   * Return the offset of the character. The offset matches the index of the
   * character to the right, since the carat lies between characters.
   *
   * @return the 0-based caret position
   */
  int getCaretPosition();

  /**
   * Returns the section of text at the index, or null if the index or part
   * is invalid.
   *
   * @param part {@link CHARACTER}, {@link WORD}, or {@link SENTENCE}
   * @param index the 0-based character index
   * @return the selection of text at that index, or null
   */
  String getAtIndex(int part, int index);

  /**
   * Returns the section of text after the index, or null if the index or part
   * is invalid.
   *
   * @param part {@link CHARACTER}, {@link WORD}, or {@link SENTENCE}
   * @param index the 0-based character index
   * @return the selection of text after that index, or null
   */
  String getAfterIndex(int part, int index);

  /**
   * Returns the section of text before the index, or null if the index or part
   * is invalid.
   *
   * @param part {@link CHARACTER}, {@link WORD}, or {@link SENTENCE}
   * @param index the 0-based character index
   * @return the selection of text before that index, or null
   */
  String getBeforeIndex(int part, int index);

  /**
   * Returns the attributes of a character at an index, or null if the index
   * is out of bounds.
   *
   * @param index the 0-based character index
   * @return the character's attributes
   */
  AttributeSet getCharacterAttribute(int index);

  /**
   * Returns the start index of the selection. If there is no selection, this
   * is the same as the caret location.
   *
   * @return the 0-based character index of the selection start
   */
  int getSelectionStart();

  /**
   * Returns the end index of the selection. If there is no selection, this
   * is the same as the caret location.
   *
   * @return the 0-based character index of the selection end
   */
  int getSelectionEnd();

  /**
   * Returns the selected text. This may be null or "" if no text is selected.
   *
   * @return the selected text
   */
  String getSelectedText();
} // interface AccessibleText
