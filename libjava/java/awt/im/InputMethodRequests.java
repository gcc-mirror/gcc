/* InputMethodRequests.java -- handles text insertion via input methods
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

package java.awt.im;

import java.awt.Rectangle;
import java.awt.font.TextHitInfo;
import java.text.AttributedCharacterIterator;
import java.text.AttributedCharacterIterator.Attribute;

/**
 * This interface handles requests made by input methods on text editing
 * components. A component must specify a handler for input methods that
 * implements this interface, and which supports one of two user interfaces:
 * <ul><li><em>on-the-spot</em>: composed text is shown in place</li>
 * <li><em>below-the-spot</em>: composed text is in a separate window,
 * usually below the main text window, until it is committed into place at
 * the insertion point, overwriting any selected text</li></ul>
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Component#getInputMethodRequests()
 * @see InputMethodListener
 * @since 1.2
 * @status updated to 1.4
 */
public interface InputMethodRequests
{
  /**
   * Gets the location of a given offset of the text. This can be used to
   * position a composition window near the location of where the composed
   * text will be inserted.
   *
   * <p>If the component has composed text (from the most recent
   * InputMethodEvent), then offset 0 indicates the location of the first
   * character of this composed text. Otherwise, the offset is ignored, and
   * the location should be the beginning of the final line of selected
   * text (in horizontal left-to-right text, like English, this would be the
   * lower left corner of the selction; in vertical top-to-bottom text, like
   * Chinese, this would be the top right corner of the selection).
   *
   * <p>The location returned is a 0-thickness caret (either horizontal or
   * vertical, depending on text flow), mapped to absolute screen coordinates.
   *
   * @param offset offset within composed text, or null
   * @return the screen location of the caret at the offset
   */
  Rectangle getTextLocation(TextHitInfo offset);

  /**
   * Get the text offset for the given screen coordinate. The offset is
   * relative to the composed text, and the return is null if it is outside
   * the range of composed text. For example, this can be used to find
   * where a mouse click should pop up a text composition window.
   *
   * @param x the x screen coordinate
   * @param y the y screen coordinate
   * @return a text hit info describing the composed text offset
   */
  TextHitInfo getLocationOffset(int x, int y);

  /**
   * Gets the offset where the committed text exists in the text editing
   * component. This can be used to examine the text surrounding the insert
   * position.
   *
   * @return the offset of the insert position
   */
  int getInsertPositionOffset();

  /**
   * Gets an interator which provides access to the text and its attributes,
   * except for the uncommitted text. The input method may provide a list of
   * attributes it is interested in; and the iterator need not provide
   * information on the remaining attributes. If the attribute list is null,
   * the iterator must list all attributes.
   *
   * @param beginIndex the index of the first character in the iteration
   * @param endIndex the index of the last character in the iteration
   * @param attributes a list of attributes interested in, or null
   * @return an iterator over the region of text with its attributes
   */
  AttributedCharacterIterator getCommittedText(int beginIndex, int endIndex,
                                               Attribute[] attributes);

  /**
   * Gets the length of committed text.
   *
   * @return the number of committed characters
   */
  int getCommittedTextLength();

  /**
   * Gets the latest committed text, and removes it from the component's text
   * body. This allows an input method to provide an "Undo" command. In
   * general, this should only be supported immediately after a commit, and
   * not when other actions intervene; if not supported, simply return null.
   * The input method may provide a list of attributes it is interested in;
   * and the iterator need not provide information on the remaining attributes.
   * If the attribute list is null, the iterator must list all attributes.
   *
   * @param attributes a list of attributes interested in, or null
   * @return the latest committed text, or null
   */
  AttributedCharacterIterator cancelLatestCommittedText
    (Attribute[] attributes);

  /**
   * Gets the currently selected text. One use of this is to implement a
   * "Reconvert" feature in an input method, which modifies the selection
   * based on the text in the composition window. The input method may
   * provide a list of attributes it is interested in; and the iterator need
   * not provide information on the remaining attributes. If the attribute
   * list is null, the iterator must list all attributes.
   *
   * @param attributes a list of attributes interested in, or null
   * @return the current selection
   */
  AttributedCharacterIterator getSelectedText(Attribute[] attributes);
} // interface InputMethodRequests
