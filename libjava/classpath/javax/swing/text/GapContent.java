/* GapContent.java --
   Copyright (C) 2002, 2004, 2005, 2006 Free Software Foundation, Inc.

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
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoableEdit;

/**
 * This implementation of {@link AbstractDocument.Content} uses a gapped buffer.
 * This takes advantage of the fact that text area content is mostly inserted
 * sequentially. The buffer is a char array that maintains a gap at the current
 * insertion point. If characters a inserted at gap boundaries, the cost is
 * minimal (simple array access). The array only has to be shifted around when
 * the insertion point moves (then the gap also moves and one array copy is
 * necessary) or when the gap is filled up and the buffer has to be enlarged.
 */
public class GapContent
    implements AbstractDocument.Content, Serializable
{
  
  /**
   * A {@link Position} implementation for <code>GapContent</code>.
   */
  class GapContentPosition
    implements Position
  {

    /**
     * The index to the positionMarks array entry, which in turn holds the
     * mark into the buffer array.
     */
    Mark mark;

    /**
     * Returns the current offset of this Position within the content.
     * 
     * @return the current offset of this Position within the content.
     */
    public int getOffset()
    {
      return mark.getOffset();
    }
  }

  /**
   * Holds a mark into the buffer that is used by GapContentPosition to find
   * the actual offset of the position. This is pulled out of the
   * GapContentPosition object so that the mark and position can be handled
   * independently, and most important, so that the GapContentPosition can
   * be garbage collected while we still hold a reference to the Mark object. 
   */
  private class Mark
    extends WeakReference
  {
    /**
     * The actual mark into the buffer.
     */
    int mark;

    /**
     * Creates a new Mark object for the specified offset.
     *
     * @param offset the offset
     */
    Mark(int offset)
    {
      super(null);
      mark = offset;
    }

    Mark(int offset, GapContentPosition pos, ReferenceQueue queue)
    {
      super(pos, queue);
      mark = offset;
    }

    /**
     * Returns the offset of the mark.
     *
     * @return the offset of the mark
     */
    int getOffset()
    {
      int res = mark;
      if (mark >= gapStart)
        res -= (gapEnd - gapStart);
      return Math.max(0, res);
    }

    /**
     * Returns the GapContentPosition that is associated ith this mark.
     * This fetches the weakly referenced position object.
     *
     * @return the GapContentPosition that is associated ith this mark
     */
    GapContentPosition getPosition()
    {
      return (GapContentPosition) get();
    }

  }

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
      undoOffset = mark.getOffset();
    }

    /**
     * Resets the position of the mark to the value that it had when
     * creating this UndoPosRef.
     */
    void reset()
    {
      if (undoOffset <= gapStart)
        mark.mark = undoOffset;
      else
        mark.mark = (gapEnd - gapStart) + undoOffset;
    }
  }

  private class InsertUndo extends AbstractUndoableEdit
  {
    public int where, length;
    String text;
    private Vector positions;

    public InsertUndo(int start, int len)
    {
      where = start;
      length = len;
    }

    public void undo () throws CannotUndoException
    {
      super.undo();
      try
        {
          positions = getPositionsInRange(null, where, length);
          text = getString(where, length);
          remove(where, length);
        }
      catch (BadLocationException ble)
        {
          throw new CannotUndoException();
        }
    }
    
    public void redo () throws CannotUndoException
    {
      super.redo();
      try
        {
          insertString(where, text);
          if (positions != null)
            {
              updateUndoPositions(positions, where, length);
              positions = null;
            }
        }
      catch (BadLocationException ble)
        {
          throw new CannotRedoException();
        }
    }
    
  }
  
  private class UndoRemove extends AbstractUndoableEdit
  {
    public int where;
    String text;

    /**
     * The positions in the removed range.
     */
    private Vector positions;

    public UndoRemove(int start, String removedText)
    {
      where = start;
      text = removedText;
      positions = getPositionsInRange(null, start, removedText.length());
    }

    public void undo () throws CannotUndoException
    {
      super.undo();
      try
      {
        insertString(where, text);
        if (positions != null)
          updateUndoPositions(positions, where, text.length());
      }
      catch (BadLocationException ble)
      {
        throw new CannotUndoException();
      }
    }
    
    public void redo () throws CannotUndoException
    {
      super.redo();
      try
        {
          text = getString(where, text.length());
          positions = getPositionsInRange(null, where, text.length());
          remove(where, text.length());
        }
      catch (BadLocationException ble)
        {
          throw new CannotRedoException();
        }
    }
    
  }

  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = -6226052713477823730L;

  /**
   * This is the default buffer size and the amount of bytes that a buffer is
   * extended if it is full.
   */
  static final int DEFAULT_BUFSIZE = 10;

  /**
   * The text buffer.
   */
  char[] buffer;

  /**
   * The index of the first character of the gap.
   */
  int gapStart;

  /**
   * The index of the character after the last character of the gap.
   */
  int gapEnd;

  // FIXME: We might want to track GC'ed GapContentPositions and remove their
  // corresponding marks, or alternativly, perform some regular cleanup of
  // the positionMarks array.

  /**
   * Holds the marks for positions. These marks are referenced by the
   * GapContentPosition instances by an index into this array.
   *
   * This is package private to avoid accessor synthetic methods.
   */
  ArrayList marks;

  /**
   * The number of unused marks.
   */
  private int garbageMarks;

  /**
   * A 'static' mark that is used for searching.
   */
  private Mark searchMark = new Mark(0);

  /**
   * Queues all references to GapContentPositions that are about to be
   * GC'ed. This is used to remove the corresponding marks from the
   * positionMarks array if the number of references to that mark reaches zero.
   *
   * This is package private to avoid accessor synthetic methods.
   */
  ReferenceQueue queueOfDeath;

  /**
   * Creates a new GapContent object.
   */
  public GapContent()
  {
    this(DEFAULT_BUFSIZE);
  }

  /**
   * Creates a new GapContent object with a specified initial size.
   * 
   * @param size the initial size of the buffer
   */
  public GapContent(int size)
  {
    size = Math.max(size, 2);
    buffer = (char[]) allocateArray(size);
    gapStart = 1;
    gapEnd = size;
    buffer[0] = '\n';
    marks = new ArrayList();
    queueOfDeath = new ReferenceQueue();
  }

  /**
   * Allocates an array of the specified length that can then be used as
   * buffer.
   * 
   * @param size the size of the array to be allocated
   * 
   * @return the allocated array
   */
  protected Object allocateArray(int size)
  {
    return new char[size];
  }

  /**
   * Returns the length of the allocated buffer array.
   * 
   * @return the length of the allocated buffer array
   */
  protected int getArrayLength()
  {
    return buffer.length;
  }

  /**
   * Returns the length of the content.
   * 
   * @return the length of the content
   */
  public int length()
  {
    return buffer.length - (gapEnd - gapStart);
  }

  /**
   * Inserts a string at the specified position.
   * 
   * @param where the position where the string is inserted
   * @param str the string that is to be inserted
   * 
   * @return an UndoableEdit object
   * 
   * @throws BadLocationException if <code>where</code> is not a valid
   *         location in the buffer
   */
  public UndoableEdit insertString(int where, String str)
      throws BadLocationException
  {
    // check arguments
    int length = length();
    int strLen = str.length();

    if (where < 0)
      throw new BadLocationException("The where argument cannot be smaller"
                                     + " than the zero", where);

    if (where > length)
      throw new BadLocationException("The where argument cannot be greater"
          + " than the content length", where);

    InsertUndo undo = new InsertUndo(where, strLen);
    replace(where, 0, str.toCharArray(), strLen);

    return undo;
  }

  /**
   * Removes a piece of content at th specified position.
   * 
   * @param where the position where the content is to be removed
   * @param nitems number of characters to be removed
   * 
   * @return an UndoableEdit object
   * 
   * @throws BadLocationException if <code>where</code> is not a valid
   *         location in the buffer
   */
  public UndoableEdit remove(int where, int nitems) throws BadLocationException
  {
    // check arguments
    int length = length();
    
    if ((where + nitems) >= length)
      throw new BadLocationException("where + nitems cannot be greater"
          + " than the content length", where + nitems);
    
    String removedText = getString(where, nitems);
    UndoRemove undoRemove = new UndoRemove(where, removedText);
    replace(where, nitems, null, 0);

    return undoRemove;
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
    Segment seg = new Segment();
    try
      {
        getChars(where, len, seg);
        return new String(seg.array, seg.offset, seg.count);
      }
    catch (StringIndexOutOfBoundsException ex)
      {
        int invalid = 0;
        if (seg.offset < 0 || seg.offset >= seg.array.length)
          invalid = seg.offset;
        else
          invalid = seg.offset + seg.count;
        throw new BadLocationException("Illegal location: array.length = "
                                       + seg.array.length + ", offset = "
                                       + seg.offset + ", count = "
                                       + seg.count, invalid);
      }
  }

  /**
   * Fetches a piece of content and stores it in a {@link Segment} object.
   * 
   * If the requested piece of text spans the gap, the content is copied into a
   * new array. If it doesn't then it is contiguous and the actual content
   * store is returned.
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
    // check arguments
    int length = length();
    if (where < 0)
      throw new BadLocationException("the where argument may not be below zero", where);
    if (where >= length)
      throw new BadLocationException("the where argument cannot be greater"
          + " than the content length", where);
    if ((where + len) > length)
      throw new BadLocationException("len plus where cannot be greater"
          + " than the content length", len + where);
    if (len < 0)
      throw new BadLocationException("negative length not allowed: ", len);

    // Optimized to copy only when really needed. 
    if (where + len <= gapStart)
      {
        // Simple case: completely before gap.
        txt.array = buffer;
        txt.offset = where;
        txt.count = len;
      }
    else if (where > gapStart)
      {
        // Completely after gap, adjust offset.
        txt.array = buffer;
        txt.offset = gapEnd + where - gapStart;
        txt.count = len;
      }
    else
      {
        // Spans the gap.
        int beforeGap = gapStart - where;
        if (txt.isPartialReturn())
          {
            // Return the part before the gap when partial return is allowed.
            txt.array = buffer;
            txt.offset = where;
            txt.count = beforeGap;
          }
        else
          {
            // Copy pieces together otherwise.
            txt.array = new char[len];
            txt.offset = 0;
            System.arraycopy(buffer, where, txt.array, 0, beforeGap);
            System.arraycopy(buffer, gapEnd, txt.array, beforeGap,
                             len - beforeGap);
            txt.count = len;
          }
      }
  }

  /**
   * Creates and returns a mark at the specified position.
   * 
   * @param offset the position at which to create the mark
   * 
   * @return the create Position object for the mark
   * 
   * @throws BadLocationException if the offset is not a valid position in the
   *         buffer
   */
  public Position createPosition(final int offset) throws BadLocationException
  {
    // Implementation note: We used to perform explicit check on the offset
    // here. However, this makes some Mauve and Intel/Harmony tests fail
    // and luckily enough the GapContent can very well deal with offsets
    // outside the buffer bounds. So I removed that check.

    // First do some garbage collections.
    while (queueOfDeath.poll() != null)
      garbageMarks++;
    if (garbageMarks > Math.max(5, marks.size() / 10))
      garbageCollect();

    // We try to find a GapContentPosition at the specified offset and return
    // that. Otherwise we must create a new one.
    Mark m;
    GapContentPosition pos;
    int index = offset;
    if (offset >= gapStart)
      index += (gapEnd - gapStart);
    searchMark.mark = index;
    int insertIndex = search(searchMark);
    if (!(insertIndex < marks.size()
          && (m = (Mark) marks.get(insertIndex)).mark == index
          && (pos = m.getPosition()) != null))
      {
        // Create new position if none was found.
        pos = new GapContentPosition();
        m = new Mark(index, pos, queueOfDeath);
        pos.mark = m;
        marks.add(insertIndex, m);
      }
    // Otherwise use the found position.
      
    return pos;
  }

  /**
   * Enlarges the gap. This allocates a new bigger buffer array, copy the
   * segment before the gap as it is and the segment after the gap at the end
   * of the new buffer array. This does change the gapEnd mark but not the
   * gapStart mark.
   * 
   * @param newSize the new size of the gap
   */
  protected void shiftEnd(int newSize)
  {
    assert newSize > (gapEnd - gapStart) : "The new gap size must be greater "
                                           + "than the old gap size";

    int oldEnd = getGapEnd();
    int oldSize = getArrayLength();
    int upper = oldSize - oldEnd;
    int size = (newSize + 1) * 2;
    int newEnd = size - upper;

    // Copy the data around.
    char[] newBuf = (char[]) allocateArray(size);
    System.arraycopy(buffer, 0, newBuf, 0, Math.min(size, oldSize));
    buffer = newBuf;
    gapEnd = newEnd;
    if (upper != 0)
      System.arraycopy(buffer, oldEnd, buffer, newEnd, upper);

    // Adjust marks.
    int delta = gapEnd - oldEnd;
    int adjIndex = searchFirst(oldEnd);
    int count = marks.size();
    for (int i = adjIndex; i < count; i++)
      {
        Mark m = (Mark) marks.get(i);
        m.mark += delta;
      }
  }

  /**
   * Shifts the gap to the specified position.
   * 
   * @param newGapStart the new start position of the gap
   */
  protected void shiftGap(int newGapStart)
  {
    int oldStart = gapStart;
    int delta = newGapStart - oldStart;
    int oldEnd = gapEnd;
    int newGapEnd = oldEnd + delta;
    int size = oldEnd - oldStart;

    // Shift gap in array.
    gapStart = newGapStart;
    gapEnd = newGapEnd;
    if (delta > 0)
      System.arraycopy(buffer, oldEnd, buffer, oldStart, delta);
    else
      System.arraycopy(buffer, newGapStart, buffer, newGapEnd, -delta);

    // Adjust marks.
    if (delta > 0)
      {
        int adjIndex = searchFirst(oldStart);
        int count = marks.size();
        for (int i = adjIndex; i < count; i++)
          {
            Mark m = (Mark) marks.get(i);
            if (m.mark >= newGapEnd)
              break;
            m.mark -= size;
          }
      }
    else if (delta < 0)
      {
        int adjIndex = searchFirst(newGapStart);
        int count = marks.size();
        for (int i = adjIndex; i < count; i++)
          {
            Mark m = (Mark) marks.get(i);
            if (m.mark >= oldEnd)
              break;
            m.mark += size;
          }
      }
    resetMarksAtZero();
  }

  /**
   * Shifts the gap start downwards. This does not affect the content of the
   * buffer. This only updates the gap start and all the marks that are between
   * the old gap start and the new gap start. They all are squeezed to the start
   * of the gap, because their location has been removed.
   *
   * @param newGapStart the new gap start
   */
  protected void shiftGapStartDown(int newGapStart)
  {
    if (newGapStart == gapStart)
      return;

    assert newGapStart < gapStart : "The new gap start must be less than the "
                                    + "old gap start.";

    // Adjust positions.
    int adjIndex = searchFirst(newGapStart);
    int count = marks.size();
    for (int i = adjIndex; i < count; i++)
      {
        Mark m = (Mark) marks.get(i);
        if (m.mark > gapStart)
          break;
        m.mark = gapEnd;
      }

    gapStart = newGapStart;
    resetMarksAtZero();
  }

  /**
   * Shifts the gap end upwards. This does not affect the content of the
   * buffer. This only updates the gap end and all the marks that are between
   * the old gap end and the new end start. They all are squeezed to the end
   * of the gap, because their location has been removed.
   *
   * @param newGapEnd the new gap start
   */
  protected void shiftGapEndUp(int newGapEnd)
  {
    if (newGapEnd == gapEnd)
      return;

    assert newGapEnd > gapEnd : "The new gap end must be greater than the "
                                + "old gap end.";

    // Adjust marks.
    int adjIndex = searchFirst(gapEnd);
    int count = marks.size();
    for (int i = adjIndex; i < count; i++)
      {
        Mark m = (Mark) marks.get(i);
        if (m.mark >= newGapEnd)
          break;
        m.mark = newGapEnd;
      }

    
    gapEnd = newGapEnd;
    resetMarksAtZero();
  }

  /**
   * Returns the allocated buffer array.
   * 
   * @return the allocated buffer array
   */
  protected final Object getArray()
  {
    return buffer;
  }

  /**
   * Replaces a portion of the storage with the specified items.
   * 
   * @param position the position at which to remove items
   * @param rmSize the number of items to remove
   * @param addItems the items to add at location
   * @param addSize the number of items to add
   */
  protected void replace(int position, int rmSize, Object addItems,
                         int addSize)
  {
    if (addSize == 0)
      {
        removeImpl(position, rmSize);
        return;
      }
    else if (rmSize > addSize)
      {
        removeImpl(position + addSize, rmSize - addSize);
      }
    else
      {
        int endSize = addSize - rmSize;
        int end = addImpl(position + rmSize, endSize);
        System.arraycopy(addItems, rmSize, buffer, end, endSize);
        addSize = rmSize;
      }
    System.arraycopy(addItems, 0, buffer, position, addSize);
  }

  /**
   * Adjusts the positions and gap in response to a remove operation.
   *
   * @param pos the position at which to remove
   * @param num the number of removed items
   */
  private void removeImpl(int pos, int num)
  {
    if (num > 0)
      {
        int end = pos + num;
        int newGapSize = (gapEnd - gapStart) + num;
        if (end <= gapStart)
          {
            if (gapStart != end)
              {
                shiftGap(end);
              }
            shiftGapStartDown(gapStart - num);
          }
        else if (pos >= gapStart)
          {
            if (gapStart != pos)
              {
                shiftGap(pos);
              }
            shiftGapEndUp(gapStart + newGapSize);
          }
        else
          {
            shiftGapStartDown(pos);
            shiftGapEndUp(gapStart + newGapSize);
          }
      }
  }

  /**
   * Adjusts the positions and gap in response to an add operation.
   *
   * @param pos the position at which to add
   * @param num the number of added items
   *
   * @return the adjusted position
   */
  private int addImpl(int pos, int num)
  {
    int size = gapEnd - gapStart;
    if (num == 0)
      {
        if (pos > gapStart)
          pos += size;
        return pos;
      }

    shiftGap(pos);
    if (num >= size)
      {
        shiftEnd(getArrayLength() - size + num);
        size = gapEnd - gapStart;
      }

    gapStart += num;
    return pos;
  }

  /**
   * Returns the start index of the gap within the buffer array.
   *
   * @return the start index of the gap within the buffer array
   */
  protected final int getGapStart()
  {
    return gapStart;
  }

  /**
   * Returns the end index of the gap within the buffer array.
   *
   * @return the end index of the gap within the buffer array
   */
  protected final int getGapEnd()
  {
    return gapEnd;
  }

  /**
   * Returns all <code>Position</code>s that are in the range specified by
   * <code>offset</code> and </code>length</code> within the buffer array.
   *
   * @param v the vector to use; if <code>null</code>, a new Vector is allocated
   * @param offset the start offset of the range to search
   * @param length the length of the range to search
   *
   * @return the positions within the specified range
   */
  protected Vector getPositionsInRange(Vector v, int offset, int length)
  {
    int end = offset + length;
    int startIndex;
    int endIndex;
    if (offset < gapStart)
      {
        if (offset == 0)
          startIndex = 0;
        else
          startIndex = searchFirst(offset);
        if (end >= gapStart)
          endIndex = searchFirst(end + (gapEnd - gapStart) + 1);
        else
          endIndex = searchFirst(end + 1);
      }
    else
      {
        startIndex = searchFirst(offset + (gapEnd - gapStart));
        endIndex = searchFirst(end + (gapEnd - gapStart) + 1);
      }
    if (v == null)
      v = new Vector();
    for (int i = startIndex; i < endIndex; i++)
      {
        v.add(new UndoPosRef((Mark) marks.get(i)));
      }
    return v;
  }
  
  /**
   * Resets all <code>Position</code> that have an offset of <code>0</code>,
   * to also have an array index of <code>0</code>. This might be necessary
   * after a call to <code>shiftGap(0)</code>, since then the marks at offset
   * <code>0</code> get shifted to <code>gapEnd</code>.
   */
  protected void resetMarksAtZero()
  {
    if (gapStart != 0)
      return;

    for (int i = 0; i < marks.size(); i++)
      {
        Mark m = (Mark) marks.get(i);
        if (m.mark <= gapEnd)
          m.mark = 0;
      }
  }

  /**
   * Resets the positions in the specified range to their original offset
   * after a undo operation is performed. For example, after removing some
   * content, the positions in the removed range will all be set to one
   * offset. This method restores the positions to their original offsets
   * after an undo.
   *
   * @param positions the positions to update
   * @param offset
   * @param length
   */
  protected void updateUndoPositions(Vector positions, int offset, int length)
  {
    for (Iterator i = positions.iterator(); i.hasNext();)
      {
        UndoPosRef undoPosRef = (UndoPosRef) i.next();
        undoPosRef.reset();
      }

    // Resort marks.
    Collections.sort(marks);
  }

  /**
   * Outputs debugging info to System.err. It prints out the buffer array,
   * the gapStart is marked by a &lt; sign, the gapEnd is marked by a &gt;
   * sign and each position is marked by a # sign.
   */
  private void dump()
  {
    System.err.println("GapContent debug information");
    System.err.println("buffer length: " + buffer.length);
    System.err.println("gap start: " + gapStart);
    System.err.println("gap end: " + gapEnd);
    for (int i = 0; i < buffer.length; i++)
      {
        if (i == gapStart)
          System.err.print('<');
        if (i == gapEnd)
          System.err.print('>');

        if (!Character.isISOControl(buffer[i]))
          System.err.print(buffer[i]);
        else
          System.err.print('.');
      }
    System.err.println();
  }

  /**
   * Prints out the position marks.
   */
  private void dumpMarks()
  {
    System.out.print("positionMarks: ");
    for (int i = 0; i < marks.size(); i++)
      System.out.print(((Mark) marks.get(i)).mark + ", ");
    System.out.println();
  }

  /**
   * Searches the first occurance of object <code>o</code> in list
   * <code>l</code>. This performs a binary search by calling
   * {@link Collections#binarySearch(List, Object)} and when an object has been
   * found, it searches backwards to the first occurance of that object in the
   * list. The meaning of the return value is the same as in
   * <code>Collections.binarySearch()</code>.
   *
   * @param o the object to be searched
   *
   * @return the index of the first occurance of o in l, or -i + 1 if not found
   */
  int search(Mark o)
  {
    int foundInd = 0;
    boolean found = false;
    int low = 0;
    int up = marks.size() - 1;
    int mid = 0;
    if (up > -1)
      {
        int cmp = 0;
        Mark last = (Mark) marks.get(up);
        cmp = compare(o, last);
        if (cmp > 0)
          {
            foundInd = up + 1;
            found = true;
          }
        else
          {
            while (low <= up && ! found)
              {
                mid = low + (up - low) / 2;
                Mark m = (Mark) marks.get(mid);
                cmp = compare(o, m);
                if (cmp == 0)
                  {
                    foundInd = mid;
                    found = true;
                  }
                else if (cmp < 0)
                  up = mid - 1;
                else
                  low = mid + 1;
              }

            if (! found)
              foundInd = cmp < 0 ? mid : mid + 1;
          }
      }
    return foundInd;
  }

  private int searchFirst(int index)
  {
    searchMark.mark = Math.max(index, 1);
    int i = search(searchMark);
    for (int j = i - 1; j >= 0; j--)
      {
        Mark m = (Mark) marks.get(j);
        if (m.mark != index)
          break;
        i--;
      }
    return i;
  }

  /**
   * Compares two marks.
   *
   * @param m1 the first mark
   * @param m2 the second mark
   *
   * @return negative when m1 < m2, positive when m1 > m2 and 0 when equal
   */
  private int compare(Mark m1, Mark m2)
  {
    return m1.mark - m2.mark;
  }

  /**
   * Collects and frees unused marks.
   */
  private void garbageCollect()
  {
    int count = marks.size();
    ArrayList clean = new ArrayList();
    for (int i = 0; i < count; i++)
      {
        Mark m = (Mark) marks.get(i);
        if (m.get() != null)
          clean.add(m);
      }
    marks = clean;
    garbageMarks = 0;
  }
}
