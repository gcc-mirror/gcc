/* GapContent.java --
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

import java.io.Serializable;

// too lazy to make a real gapcontent.
// lets just use a stringbuffer instead.
import javax.swing.undo.UndoableEdit;

/**
 * This implementation of {@link AbstractDocument.Content} uses a gapped
 * buffer. This takes advantage of the fact that text area content is
 * mostly inserted sequentially. The buffer is a char array that maintains
 * a gap at the current insertion point. If characters a inserted at
 * gap boundaries, the cost is minimal (simple array access). The array only
 * has to be shifted around when the insertion point moves (then the gap also
 * moves and one array copy is necessary) or when the gap is filled up and
 * the buffer has to be enlarged.
 */
public class GapContent
  implements AbstractDocument.Content, Serializable
{
  private static final long serialVersionUID = 8374645204155842629L;
    
  StringBuffer buf = new StringBuffer();

  /**
   * Creates a new GapContent object.
   */
  public GapContent()
  {
    this(10);
  }

  /**
   * Creates a new GapContent object with a specified initial size.
   *
   * @param size the initial size of the buffer
   */
  public GapContent(int size)
  {
    buf.append("\n");
  }

  /**
   * Creates and returns a mark at the specified position.
   *
   * @param offset the position at which to create the mark
   *
   * @return the create Position object for the mark
   *
   * @throws BadLocationException if the offset is not a valid position in
   *         the buffer
   */
  public Position createPosition(final int offset) throws BadLocationException
  {
    return new Position()
      {
	int off = offset;

	public int getOffset()
	{
	  return off;
	}
      };
  }

  /**
   * Returns the length of the content.
   *
   * @return the length of the content
   */
  public int length()
  {
    return buf.length();
  }

  /**
   * Inserts a string at the specified position.
   *
   * @param where the position where the string is inserted
   * @param str the string that is to be inserted
   *
   * @return an UndoableEdit object (currently not supported, so
   *         <code>null</code> is returned)
   *
   * @throws BadLocationException if <code>where</code> is not a valid location
   *         in the buffer
   */
  public UndoableEdit insertString(int where, String str)
    throws BadLocationException
  {
    buf.insert(where, str);
    return null;
  }

  /**
   * Removes a piece of content at th specified position.
   *
   * @param where the position where the content is to be removed
   * @param nitems number of characters to be removed
   *
   * @return an UndoableEdit object (currently not supported, so
   *         <code>null</code> is returned)
   *
   * @throws BadLocationException if <code>where</code> is not a valid location
   *         in the buffer
   */
  public UndoableEdit remove(int where, int nitems)
    throws BadLocationException
  {
    buf.delete(where, where + nitems);
    return null;
  }

  /**
   * Returns a piece of content as String.
   *
   * @param where the start location of the fragment
   * @param len the length of the fragment
   *
   * @throws BadLocationException if <code>where</code> or
   *         <code>where + len</code> are no valid locations in the buffer
   */
  public String getString(int where, int len) throws BadLocationException
  {
    return buf.substring(where, where+len);
  }

  /**
   * Fetches a piece of content and stores it in a {@link Segment} object.
   *
   * @param where the start location of the fragment
   * @param len the length of the fragment
   * @param txt the Segment object to store the fragment in
   *
   * @throws BadLocationException if <code>where</code> or
   *         <code>where + len</code> are no valid locations in the buffer
   */
  public void getChars(int where, int len, Segment txt)
    throws BadLocationException
  {
    txt.array = new char[len];

    System.arraycopy(buf.toString().toCharArray(), where, txt.array, 0, len);

    txt.count = len;
    txt.offset = 0;
  }
}
