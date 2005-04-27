/* Document.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

package javax.swing.text;

import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;

/**
 * A Document is the model that backs up all text components in Swing.
 * This interface supports different kinds of implementations, from
 * simple plain text model up to complex styled HTML or RTF models.
 */
public interface Document
{
  /**
   * The key for the property that describes the source of a document.
   */
  String StreamDescriptionProperty = "stream";

  /**
   * The key for the property that is the title of a document.
   */
  String TitleProperty = "title";

  /**
   * Adds a {@link DocumentListener} to this document.
   *
   * @param listener the DocumentListener to add
   */
  void addDocumentListener(DocumentListener listener);

  /**
   * Adds an {@link UndoableEditListener} to this document.
   *
   * @param listener the UndoableEditListener to add
   */
  void addUndoableEditListener(UndoableEditListener listener);

  /**
   * Creates a mark in the character content at the specified offset.
   *
   * @param offs the offset where to place the mark
   *
   * @return the created Position object
   *
   * @throws BadLocationException of the specified offset is not a valid
   *         position in the documents content
   */
  Position createPosition(int offs)
    throws BadLocationException;

  /**
   * Returns the default root element. Views should be using this element
   * unless other mechanisms for assigning views to element structure is
   * provided.
   *
   * @return the default root element
   */
  Element getDefaultRootElement();

  /**
   * Returns the position that marks the end of the document.
   *
   * @return the position that marks the end of the document
   */
  Position getEndPosition();

  /**
   * Returns the length of the document content.
   *
   * @return the length of the document content
   */
  int getLength();

  /**
   * Returns a document property with the specified key.
   *
   * @param key the (non-null) key for the property to fetch
   *
   * @return the property for <code>key</code> or null if no such property
   *         is stored
   */
  Object getProperty(Object key);

  /**
   * Returns the root elements of the document content.
   *
   * @return the root elements of the document content
   */
  Element[] getRootElements();

  /**
   * Returns the position that marks the beginning of the document
   * content.
   *
   * @return the start position
   */
  Position getStartPosition();

  /**
   * Returns the textual content starting at <code>offset</code> with
   * a length of <code>length</code>.
   *
   * @param offset the beginning of the text fragment to fetch
   * @param length the length of the text fragment to fetch
   *
   * @return the text fragment starting at <code>offset</code> with
   *         a length of <code>length</code>
   *
   * @throws BadLocationException if <code>offset</code> or <code>length</code>
   *         are no valid locations in the document content
   */
  String getText(int offset, int length)
    throws BadLocationException;

  /**
   * Fetch the textual content starting at <code>offset</code> with
   * a length of <code>length</code> and store it in <code>txt</code>.
   *
   * @param offset the beginning of the text fragment to fetch
   * @param length the length of the text fragment to fetch
   * @param txt the Segment where to store the text fragment
   *
   * @throws BadLocationException if <code>offset</code> or <code>length</code>
   *         are no valid locations in the document content
   */
  void getText(int offset, int length, Segment txt)
    throws BadLocationException;

  /**
   * Inserts a piece of text with an AttributeSet at the specified
   * <code>offset</code>.
   *
   * @param offset the location where to insert the content
   * @param str the textual content to insert
   * @param a the Attributes associated with the piece of text
   *
   * @throws BadLocationException if <code>offset</code>
   *         is not a valid location in the document content
   */
  void insertString(int offset, String str, AttributeSet a)
    throws BadLocationException;

  /**
   * Sets a document property.
   *
   * @param key the key of the property
   * @param value the value of the property
   */
  void putProperty(Object key, Object value);

  /**
   * Removes a piece of content.
   *
   * @param offs the location of the fragment to remove
   * @param len the length of the fragment to remove
   *
   * @throws BadLocationException if <code>offs</code> or <code>len</code>
   *         are no valid locations in the document content
   */
  void remove(int offs, int len)
    throws BadLocationException;

  /**
   * Removes a DocumentListener from this Document.
   *
   * @param listener the DocumentListener to remove
   */
  void removeDocumentListener(DocumentListener listener);

  /**
   * Removes an UndoableEditListener from this Document.
   *
   * @param listener the UndoableEditListener to remove
   */
  void removeUndoableEditListener(UndoableEditListener listener);

  /**
   * This allows the Document to be rendered safely. It is made sure that
   * the Runnable can read the document without any changes while reading.
   * The Runnable is not allowed to change the Document itself.
   *
   * @param r the Runnable that renders the Document
   */
  void render(Runnable r);
}
