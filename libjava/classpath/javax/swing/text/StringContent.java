/* StringContent.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package javax.swing.text;

import java.io.Serializable;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoableEdit;

/**
 * An implementation of the <code>AbstractDocument.Content</code>
 * interface useful for small documents or debugging. The character
 * content is a simple character array. It's not really efficient.
 * 
 * <p>Do not use this class for large size.</p>
 */
public final class StringContent 
  implements AbstractDocument.Content, Serializable
{
  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 4755994433709540381L;

  // This is package-private to avoid an accessor method.
  char[] content;

  private int count;

  private Vector positions = new Vector();

  private class InsertUndo extends AbstractUndoableEdit
  {
    private int start;
    
    private int length;

    private String redoContent;

    public InsertUndo(int start, int length)
    {
      super();
      this.start = start;
      this.length = length;
    }

    public void undo()
    {
      super.undo();
      try
        {
          StringContent.this.checkLocation(this.start, this.length);
          this.redoContent = new String(StringContent.this.content, this.start,
              this.length);
          StringContent.this.remove(this.start, this.length);
        }
      catch (BadLocationException b)
        {
          throw new CannotUndoException();
        }
    }
    
    public void redo()
    {
      super.redo();
      try
        {
          StringContent.this.insertString(this.start, this.redoContent);
        }
      catch (BadLocationException b)
        {
          throw new CannotRedoException();
        }
    }
  }

  private class RemoveUndo extends AbstractUndoableEdit
  {
    private int start;

    private String undoString;

    public RemoveUndo(int start, String str)
    {
      super();
      this.start = start;
      this.undoString = str;
    }

    public void undo()
    {
      super.undo();
      try
        {
          StringContent.this.insertString(this.start, this.undoString);
        }
      catch (BadLocationException bad)
        {
          throw new CannotUndoException();
        }
    }

    public void redo()
    {
      super.redo();
      try
        {
          int end = this.undoString.length();
          StringContent.this.remove(this.start, end);
        }
      catch (BadLocationException bad)
        {
          throw new CannotRedoException();
        }
    }
  }

  private class StickyPosition implements Position
  {
    private int offset = -1;

    public StickyPosition(int offset)
    {
      this.offset = offset;
    }

    // This is package-private to avoid an accessor method.
    void setOffset(int offset)
    {
      this.offset = this.offset >= 0 ? offset : -1;
    }

    /**
     * Should be >=0.
     */
    public int getOffset()
    {
      return offset < 0 ? 0 : offset;
    }
  }

  /**
   * Creates a new instance containing the string "\n".
   */
  public StringContent()
  {
    this(1);
  }

  /**
   * Creates a new instance containing the string "\n".
   * 
   * @param initialLength  the initial length of the underlying character 
   *                       array used to store the content.
   */
  public StringContent(int initialLength)
  {
    super();
    if (initialLength < 1)
      initialLength = 1;
    this.content = new char[initialLength];
    this.content[0] = '\n';
    this.count = 1;
  }

  protected Vector getPositionsInRange(Vector v,
                                       int offset,
                                       int length)
  {
    Vector refPos = new Vector();
    Iterator iter = this.positions.iterator();
    while(iter.hasNext())
      {
        Position p = (Position) iter.next();
        if ((offset <= p.getOffset())
            && (p.getOffset() <= (offset + length)))
          refPos.add(p);
      }
    return refPos;
  }

  /**
   * Creates a position reference for the character at the given offset.  The
   * position offset will be automatically updated when new characters are
   * inserted into or removed from the content.
   * 
   * @param offset  the character offset.
   * 
   * @throws BadLocationException if offset is outside the bounds of the 
   *         content.
   */
  public Position createPosition(int offset) throws BadLocationException
  {
    if (offset < this.count || offset > this.count)
      checkLocation(offset, 0);
    StickyPosition sp = new StickyPosition(offset);
    this.positions.add(sp);
    return sp;
  }
  
  /**
   * Returns the length of the string content, including the '\n' character at
   * the end.
   * 
   * @return The length of the string content.
   */
  public int length()
  {
    return this.count;
  }
  
  /**
   * Inserts <code>str</code> at the given position and returns an 
   * {@link UndoableEdit} that enables undo/redo support.
   * 
   * @param where  the insertion point (must be less than 
   *               <code>length()</code>).
   * @param str  the string to insert (<code>null</code> not permitted).
   * 
   * @return An object that can undo the insertion.
   */
  public UndoableEdit insertString(int where, String str)
    throws BadLocationException
  {
    checkLocation(where, 0);
    if (where == this.count)
      throw new BadLocationException("Invalid location", 1);
    if (str == null)
      throw new NullPointerException();
    char[] insert = str.toCharArray();
    char[] temp = new char[this.content.length + insert.length];
    this.count += insert.length;
    // Copy array and insert the string.
    if (where > 0)
      System.arraycopy(this.content, 0, temp, 0, where);
    System.arraycopy(insert, 0, temp, where, insert.length);
    System.arraycopy(this.content, where, temp, (where + insert.length), 
        (temp.length - where - insert.length));
    if (this.content.length < temp.length)
      this.content = new char[temp.length];
    // Copy the result in the original char array.
    System.arraycopy(temp, 0, this.content, 0, temp.length);
    // Move all the positions.
    Vector refPos = getPositionsInRange(this.positions, where, 
                                        temp.length - where);
    Iterator iter = refPos.iterator();
    while (iter.hasNext())
      {
        StickyPosition p = (StickyPosition)iter.next();
        p.setOffset(p.getOffset() + str.length());
      }
    InsertUndo iundo = new InsertUndo(where, insert.length);
    return iundo;
  }
  
  /**
   * Removes the specified range of characters and returns an 
   * {@link UndoableEdit} that enables undo/redo support.
   * 
   * @param where  the starting index.
   * @param nitems  the number of characters.
   * 
   * @return An object that can undo the removal.
   * 
   * @throws BadLocationException if the character range extends outside the
   *         bounds of the content OR includes the last character.
   */
  public UndoableEdit remove(int where, int nitems) throws BadLocationException
  {
    checkLocation(where, nitems + 1);
    char[] temp = new char[(this.content.length - nitems)];
    this.count = this.count - nitems;
    RemoveUndo rundo = new RemoveUndo(where, new String(this.content, where, 
        nitems));
    // Copy array.
    System.arraycopy(this.content, 0, temp, 0, where);
    System.arraycopy(this.content, where + nitems, temp, where, 
        this.content.length - where - nitems);
    this.content = new char[temp.length];
    // Then copy the result in the original char array.
    System.arraycopy(temp, 0, this.content, 0, this.content.length);
    // Move all the positions.
    Vector refPos = getPositionsInRange(this.positions, where, 
        this.content.length + nitems - where);
    Iterator iter = refPos.iterator();
    while (iter.hasNext())
      {
        StickyPosition p = (StickyPosition)iter.next();
        int result = p.getOffset() - nitems;
        p.setOffset(result);
        if (result < 0)
          this.positions.remove(p);
      }
    return rundo;
  }
  
  /**
   * Returns a new <code>String</code> containing the characters in the 
   * specified range.
   * 
   * @param where  the start index.
   * @param len  the number of characters.
   * 
   * @return A string.
   * 
   * @throws BadLocationException if the requested range of characters extends 
   *         outside the bounds of the content.
   */
  public String getString(int where, int len) throws BadLocationException
  {
    checkLocation(where, len);
    return new String(this.content, where, len);
  }
  
  /**
   * Updates <code>txt</code> to contain a direct reference to the underlying 
   * character array.
   * 
   * @param where  the index of the first character.
   * @param len  the number of characters.
   * @param txt  a carrier for the return result (<code>null</code> not 
   *             permitted).
   *             
   * @throws BadLocationException if the requested character range is not 
   *                              within the bounds of the content.
   * @throws NullPointerException if <code>txt</code> is <code>null</code>.
   */
  public void getChars(int where, int len, Segment txt) 
    throws BadLocationException
  {
    checkLocation(where, len);
    txt.array = this.content;
    txt.offset = where;
    txt.count = len;
  }


  /**
   * @specnote This method is not very well specified and the positions vector
   *           is implementation specific. The undo positions are managed
   *           differently in this implementation, this method is only here
   *           for binary compatibility.
   */
  protected void updateUndoPositions(Vector positions)
  {
    // We do nothing here.
  }

  /** 
   * A utility method that checks the validity of the specified character
   * range.
   * 
   * @param where  the first character in the range.
   * @param len  the number of characters in the range.
   * 
   * @throws BadLocationException if the specified range is not within the
   *         bounds of the content.
   */
  void checkLocation(int where, int len) throws BadLocationException
  {
    if (where < 0)
      throw new BadLocationException("Invalid location", 1);
    else if (where > this.count)
      throw new BadLocationException("Invalid location", this.count);
    else if ((where + len) > this.count)
      throw new BadLocationException("Invalid range", this.count);
  }
  
}

