/* Segment.java --
   Copyright (C) 2002, 2004, 2006 Free Software Foundation, Inc.

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

import java.text.CharacterIterator;

/**
 * A text fragment represented by a sequence of characters stored in an array.
 */
public class Segment implements Cloneable, CharacterIterator
{
  private boolean partialReturn;
  
  /** The current index. */
  private int current;
  
  /** Storage for the characters (may contain additional characters). */
  public char[] array;
  
  /** The number of characters in the segment. */
  public int count;
  
  /** The offset of the first character in the segment. */
  public int offset;

  /**
   * Creates a new <code>Segment</code>.
   */
  public Segment()
  {
    // Nothing to do here.
  }

  /**
   * Creates a new <code>Segment</code>.
   * 
   * @param array  the underlying character data.
   * @param offset  the offset of the first character in the segment.
   * @param count  the number of characters in the segment.
   */
  public Segment(char[] array, int offset, int count)
  {
    this.array = array;
    this.offset = offset;
    this.count = count;
  }
  
  /**
   * Clones the segment (note that the underlying character array is not cloned,
   * just the reference to it).
   * 
   * @return A clone of the segment.
   */
  public Object clone()
  {
    try
      {
	return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
	return null;
      }
  }

  /**
   * Returns the character at the current index.  If the segment consists of
   * zero characters, or the current index has passed the end of the 
   * characters in the segment, this method returns {@link #DONE}.
   * 
   * @return The character at the current index.
   */
  public char current()
  {
    if (count == 0
	|| current >= getEndIndex())
      return DONE;
    
    return array[current];
  }

  /**
   * Sets the current index to the first character in the segment and returns
   * that character.  If the segment contains zero characters, this method
   * returns {@link #DONE}.
   * 
   * @return The first character in the segment, or {@link #DONE} if the 
   *         segment contains zero characters.
   */
  public char first()
  {
    if (count == 0)
      return DONE;

    current = getBeginIndex();
    return array[current];
  }

  /**
   * Returns the index of the first character in the segment.
   * 
   * @return The index of the first character.
   */
  public int getBeginIndex()
  {
    return offset;
  }

  /**
   * Returns the end index for the segment (one position beyond the last 
   * character in the segment - note that this can be outside the range of the 
   * underlying character array).
   * 
   * @return The end index for the segment.
   */
  public int getEndIndex()
  {
    return offset + count;
  }

  /**
   * Returns the index of the current character in the segment.
   * 
   * @return The index of the current character.
   */
  public int getIndex()
  {
    return current;
  }

  /**
   * Sets the current index to point to the last character in the segment and 
   * returns that character.  If the segment contains zero characters, the 
   * current index is set to {@link #getEndIndex()} and this method returns 
   * {@link #DONE}.
   * 
   * @return The last character in the segment, or {@link #DONE} if the 
   *         segment contains zero characters.
   */
  public char last()
  {
    if (count == 0)
      {
        current = getEndIndex();
        return DONE;
      }
    
    current = getEndIndex() - 1;
    return array[current];
  }

  /**
   * Sets the current index to point to the next character in the segment and 
   * returns that character.  If the next character position is past the end of
   * the segment, the index is set to {@link #getEndIndex()} and the method
   * returns {@link #DONE}.  If the segment contains zero characters, this 
   * method returns {@link #DONE}.
   * 
   * @return The next character in the segment or {@link #DONE} (if the next
   *         character position is past the end of the segment or if the 
   *         segment contains zero characters).
   */
  public char next()
  {
    if (count == 0)
      return DONE;

    if ((current + 1) >= getEndIndex())
      {
	current = getEndIndex();
	return DONE;
      }
    
    current++;
    return array[current];
  }

  /**
   * Sets the current index to point to the previous character in the segment 
   * and returns that character.  If the current index is equal to 
   * {@link #getBeginIndex()}, or if the segment contains zero characters, this 
   * method returns {@link #DONE}.
   * 
   * @return The previous character in the segment or {@link #DONE} (if the 
   *         current character position is at the beginning of the segment or 
   *         if the segment contains zero characters).
   */
  public char previous()
  {
    if (count == 0
	|| current == getBeginIndex())
      return DONE;
    
    current--;
    return array[current];
  }

  /**
   * Sets the current index and returns the character at that position (or
   * {@link #DONE} if the index is equal to {@link #getEndIndex()}.
   * 
   * @param position  the current position.
   * 
   * @return The character at the specified <code>position</code>, or
   *         {@link #DONE} if <code>position</code> is equal to 
   *         {@link #getEndIndex()}.
   *         
   * @throws IllegalArgumentException if <code>position</code> is not in the
   *         range {@link #getBeginIndex()} to {@link #getEndIndex()}.
   */
  public char setIndex(int position)
  {
    if (position < getBeginIndex()
	|| position > getEndIndex())
      throw new IllegalArgumentException("position: " + position
                                         + ", beginIndex: " + getBeginIndex()
                                         + ", endIndex: " + getEndIndex()
                                         + ", text: " + toString());

    current = position;

    if (position == getEndIndex())
      return DONE;
    
    return array[current];
  }

  /**
   * Returns a <code>String</code> containing the same characters as this 
   * <code>Segment</code>.
   * 
   * @return A <code>String</code> containing the same characters as this 
   *         <code>Segment</code>.
   */
  public String toString()
  {
    return (array != null) ? new String(array, offset, count) : "";
  }

  /**
   * Sets the partial return flag.
   * 
   * @param p  the new value of the flag.
   * 
   * @since 1.4
   */
  public void setPartialReturn(boolean p)
  {
    partialReturn = p;
  }
  
  /**
   * Returns the partial return flag.
   * 
   * @return The partial return flag.
   * @since 1.4
   */
  public boolean isPartialReturn()
  {
    return partialReturn;
  }
}
