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
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
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
  /**
   * Stores a reference to a mark that can be resetted to the original value
   * after a mark has been moved. This is used for undoing actions. 
   */
  private class UndoPosRef
  {
    /**
     * The mark that might need to be reset.
     */
    private Mark mark;

    /**
     * The original offset to reset the mark to.
     */
    private int undoOffset;

    /**
     * Creates a new UndoPosRef.
     *
     * @param m the mark
     */
    UndoPosRef(Mark m)
    {
      mark = m;
      undoOffset = mark.mark;
    }

    /**
     * Resets the position of the mark to the value that it had when
     * creating this UndoPosRef.
     */
    void reset()
    {
      mark.mark = undoOffset;
    }
  }

  /**
   * Holds a mark into the buffer that is used by StickyPosition to find
   * the actual offset of the position. This is pulled out of the
   * GapContentPosition object so that the mark and position can be handled
   * independently, and most important, so that the StickyPosition can
   * be garbage collected while we still hold a reference to the Mark object. 
   */
  private class Mark
  {
    /**
     * The actual mark into the buffer.
     */
    int mark;


    /**
     * The number of GapContentPosition object that reference this mark. If
     * it reaches zero, it get's deleted by
     * {@link StringContent#garbageCollect()}.
     */
    int refCount;

    /**
     * Creates a new Mark object for the specified offset.
     *
     * @param offset the offset
     */
    Mark(int offset)
    {
      mark = offset;
    }
  }

  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 4755994433709540381L;

  // This is package-private to avoid an accessor method.
  char[] content;

  private int count;

  /**
   * Holds the marks for the positions.
   *
   * This is package private to avoid accessor methods.
   */
  Vector marks;

  private class InsertUndo extends AbstractUndoableEdit
  {
    private int start;
    
    private int length;

    private String redoContent;

    private Vector positions;

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
          if (marks != null)
            positions = getPositionsInRange(null, start, length);
          redoContent = getString(start, length);
          remove(start, length);
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
          insertString(start, redoContent);
          redoContent = null;
          if (positions != null)
            {
              updateUndoPositions(positions);
              positions = null;
            }
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
    private int len;
    private String undoString;

    Vector positions;

    public RemoveUndo(int start, String str)
    {
      super();
      this.start = start;
      len = str.length();
      this.undoString = str;
      if (marks != null)
        positions = getPositionsInRange(null, start, str.length());
    }

    public void undo()
    {
      super.undo();
      try
        {
          StringContent.this.insertString(this.start, this.undoString);
          if (positions != null)
            {
              updateUndoPositions(positions);
              positions = null;
            }
          undoString = null;
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
          undoString = getString(start, len);
          if (marks != null)
            positions = getPositionsInRange(null, start, len);
          remove(this.start, len);
        }
      catch (BadLocationException bad)
        {
          throw new CannotRedoException();
        }
    }
  }

  private class StickyPosition implements Position
  {
    Mark mark;

    public StickyPosition(int offset)
    {
      // Try to make space.
      garbageCollect();

      mark = new Mark(offset);
      mark.refCount++;
      marks.add(mark);

      new WeakReference(this, queueOfDeath);
    }

    /**
     * Should be >=0.
     */
    public int getOffset()
    {
      return mark.mark;
    }
  }

  /**
   * Used in {@link #remove(int,int)}.
   */
  private static final char[] EMPTY = new char[0];

  /**
   * Queues all references to GapContentPositions that are about to be
   * GC'ed. This is used to remove the corresponding marks from the
   * positionMarks array if the number of references to that mark reaches zero.
   *
   * This is package private to avoid accessor synthetic methods.
   */
  ReferenceQueue queueOfDeath;

  /**
   * Creates a new instance containing the string "\n".  This is equivalent
   * to calling {@link #StringContent(int)} with an <code>initialLength</code>
   * of 10.
   */
  public StringContent()
  {
    this(10);
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
    queueOfDeath = new ReferenceQueue();
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
    Vector refPos = v == null ? new Vector() : v;
    Iterator iter = marks.iterator();
    while(iter.hasNext())
      {
        Mark m = (Mark) iter.next();
        if (offset <= m.mark && m.mark <= offset + length)
          refPos.add(new UndoPosRef(m));
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
    // Lazily create marks vector.
    if (marks == null)
      marks = new Vector();
    StickyPosition sp = new StickyPosition(offset);
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
    return count;
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
    replace(where, 0, insert);

    // Move all the positions.
    if (marks != null)
      {
        Iterator iter = marks.iterator();
        int start = where;
        if (start == 0)
          start = 1;
        while (iter.hasNext())
          {
            Mark m = (Mark) iter.next();
            if (m.mark >= start)
              m.mark += str.length();
          }
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
    RemoveUndo rundo = new RemoveUndo(where, new String(this.content, where, 
        nitems));

    replace(where, nitems, EMPTY);
    // Move all the positions.
    if (marks != null)
      {
        Iterator iter = marks.iterator();
        while (iter.hasNext())
          {
            Mark m = (Mark) iter.next();
            if (m.mark >= where + nitems)
              m.mark -= nitems;
            else if (m.mark >= where)
              m.mark = where;
          }
      }
    return rundo;
  }

  private void replace(int offs, int numRemove, char[] insert)
  {
    int insertLength = insert.length;
    int delta = insertLength - numRemove;
    int src = offs + numRemove;
    int numMove = count - src;
    int dest = src + delta;
    if (count + delta >= content.length)
      {
        // Grow data array.
        int newLength = Math.max(2 * content.length, count + delta);
        char[] newContent = new char[newLength];
        System.arraycopy(content, 0, newContent, 0, offs);
        System.arraycopy(insert, 0, newContent, offs, insertLength);
        System.arraycopy(content, src, newContent, dest, numMove);
        content = newContent;
      }
    else
      {
        System.arraycopy(content, src, content, dest, numMove);
        System.arraycopy(insert, 0, content, offs, insertLength);
      }
    count += delta;
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
    // The RI throws a StringIndexOutOfBoundsException here, which
    // smells like a bug. We throw a BadLocationException instead.
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
    if (where + len > count)
      throw new BadLocationException("Invalid location", where + len);
    txt.array = content;
    txt.offset = where;
    txt.count = len;
  }


  /**
   * Resets the positions in the specified vector to their original offset
   * after a undo operation is performed. For example, after removing some
   * content, the positions in the removed range will all be set to one
   * offset. This method restores the positions to their original offsets
   * after an undo.
   */
  protected void updateUndoPositions(Vector positions)
  {
    for (Iterator i = positions.iterator(); i.hasNext();)
      {
        UndoPosRef pos = (UndoPosRef) i.next();
        pos.reset();
      }
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

  /**
   * Polls the queue of death for GapContentPositions, updates the
   * corresponding reference count and removes the corresponding mark
   * if the refcount reaches zero.
   *
   * This is package private to avoid accessor synthetic methods.
   */
  void garbageCollect()
  {
    Reference ref = queueOfDeath.poll();
    while (ref != null)
      {
        if (ref != null)
          {
            StickyPosition pos = (StickyPosition) ref.get();
            Mark m = pos.mark;
            m.refCount--;
            if (m.refCount == 0)
              marks.remove(m);
          }
        ref = queueOfDeath.poll();
      }
  }
}

