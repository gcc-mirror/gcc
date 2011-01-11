/* AccessibleExtendedText.java
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import java.awt.Rectangle;

/**
 * This interface provides extended text functionality, similar
 * to AccessibleText.
 * @see AccessibleText
 * @since 1.5
 */
public interface AccessibleExtendedText
{
  /**
   * This constant indicates that the retrieved text should be a
   * complete line.
   */
  int LINE = 4;

  /**
   * This constant indicates that the retrieved text should consist
   * of a run with identical attributes.
   */
  int ATTRIBUTE_RUN = 5;

  /**
   * Determines the bounding box of some text held by this object.
   * @param start the starting index
   * @param end the ending index
   * @return the bounding box
   * @see AccessibleText#getCharacterBounds(int)
   */
  Rectangle getTextBounds(int start, int end);

  /**
   * Return a range of text from the underlying object.
   * @param start the starting index
   * @param end the ending index
   */
  String getTextRange(int start, int end);

  /**
   * Return a text sequence from the underlying object.  The part
   * parameter describes the type of sequence to return; it is one
   * of the constants from {@link AccessibleText} or from this
   * class.
   * @param part the type of the sequence to return
   * @param index start of the sequence
   */
  AccessibleTextSequence getTextSequenceAfter(int part, int index);

  /**
   * Return a text sequence from the underlying object.  The part
   * parameter describes the type of sequence to return; it is one
   * of the constants from {@link AccessibleText} or from this
   * class.
   * @param part the type of the sequence to return
   * @param index start of the sequence
   */
  AccessibleTextSequence getTextSequenceAt(int part, int index);

  /**
   * Return a text sequence from the underlying object.  The part
   * parameter describes the type of sequence to return; it is one
   * of the constants from {@link AccessibleText} or from this
   * class.
   * @param part the type of the sequence to return
   * @param index end of the sequence
   */
  AccessibleTextSequence getTextSequenceBefore(int part, int index);
}
