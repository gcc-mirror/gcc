/* Buffer.java -- 
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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


package java.nio;

import gnu.classpath.RawData;

/**
 * @since 1.4
 */
public abstract class Buffer
{
  int cap = 0;
  int limit = 0;
  int pos = 0;
  int mark = -1;
  RawData address;

  /**
   * Creates a new Buffer.
   *
   * Should be package private.
   */
  Buffer (int capacity, int limit, int position, int mark)
  {
    if (capacity < 0)
      throw new IllegalArgumentException ();
    
    cap = capacity;
    limit (limit);
    position (position);
    
    if (mark >= 0)
    {
      if (mark > pos)
        throw new IllegalArgumentException ();
      
      this.mark = mark;
    }
  }
  
  /**
   * Retrieves the capacity of the buffer.
   *
   * @return the capacity of the buffer
   */
  public final int capacity ()
  {
    return cap;
  }

  /**
   * Clears the buffer.
   *
   * @return this buffer
   */
  public final Buffer clear ()
  {
    limit = cap;
    pos = 0;
    mark = -1;
    return this;
  }
    
  /**
   * Flips the buffer.
   *
   * @return this buffer
   */
  public final Buffer flip ()
  {
    limit = pos;
    pos = 0;
    mark = -1;
    return this;
  }
    
  /**
   * Tells whether the buffer has remaining data to read or not.
   *
   * @return true if the buffer contains remaining data to read,
   * false otherwise
   */
  public final boolean hasRemaining ()
  {
    return remaining() > 0;
  }

  /**
   * Tells whether this buffer is read only or not.
   *
   * @return true if the buffer is read only, false otherwise
   */
  public abstract boolean isReadOnly ();

  /**
   * Retrieves the current limit of the buffer.
   *
   * @return the limit of the buffer
   */
  public final int limit ()
  {
    return limit;
  }

  /**
   * Sets this buffer's limit.
   * 
   * @param newLimit The new limit value; must be non-negative and no larger
   * than this buffer's capacity.
   *
   * @return this buffer
   *
   * @exception IllegalArgumentException If the preconditions on newLimit
   * do not hold.
   */
  public final Buffer limit (int newLimit)
  {
    if ((newLimit < 0) || (newLimit > cap))
      throw new IllegalArgumentException ();

    if (newLimit < mark)
        mark = -1;

    if (pos > newLimit)
        pos = newLimit;

    limit = newLimit;
    return this;
  }

  /**
   * Sets this buffer's mark at its position.
   *
   * @return this buffer
   */
  public final Buffer mark ()
  {
    mark = pos;
    return this;
  }

  /**
   * Retrieves the current position of this buffer.
   *
   * @return the current position of this buffer
   */
  public final int position ()
  {
    return pos;
  }
    
  /**
   * Sets this buffer's position. If the mark is defined and larger than the
   * new position then it is discarded.
   * 
   * @param newPosition The new position value; must be non-negative and no
   * larger than the current limit.
   *
   * @return this buffer
   *
   * @exception IllegalArgumentException If the preconditions on newPosition
   * do not hold
   */
  public final Buffer position (int newPosition)
  {
    if ((newPosition < 0) || (newPosition > limit))
      throw new IllegalArgumentException ();

    if (newPosition <= mark)
        mark = -1;

    pos = newPosition;
    return this;
  }

  /**
   * Returns the number of elements between the current position and the limit.
   *
   * @return the number of remaining elements
   */
  public final int remaining()
  {
    return limit - pos;
  }

  /**
   * Resets this buffer's position to the previously-marked position.
   *
   * @return this buffer
   *
   * @exception InvalidMarkException If the mark has not been set.
   */
  public final Buffer reset()
  {
    if (mark == -1)
      throw new InvalidMarkException ();

    pos = mark;
    return this;
  }

  /**
   * Rewinds this buffer. The position is set to zero and the mark
   * is discarded.
   *
   * @return this buffer
   */
  public final Buffer rewind()
  {
    pos = 0;
    mark = -1;
    return this;
  }

  /**
   * Checks for underflow. This method is used internally to check
   * whether a buffer has enough elements left to satisfy a read 
   * request.
   *
   * @exception BufferUnderflowException If there are no remaining
   * elements in this buffer.
   */
  final void checkForUnderflow()
  {
    if (!hasRemaining())
      throw new BufferUnderflowException();
  }

  /**
   * Checks for underflow. This method is used internally to check
   * whether a buffer has enough elements left to satisfy a read 
   * request for a given number of elements.
   *
   * @param length The length of a sequence of elements.
   *
   * @exception BufferUnderflowException If there are not enough 
   * remaining elements in this buffer.
   */
  final void checkForUnderflow(int length)
  {
    if (remaining() < length)
      throw new BufferUnderflowException();
  }

  /**
   * Checks for overflow. This method is used internally to check
   * whether a buffer has enough space left to satisfy a write 
   * request.
   *
   * @exception BufferOverflowException If there is no remaining
   * space in this buffer.
   */
  final void checkForOverflow()
  {
    if (!hasRemaining())
      throw new BufferOverflowException();
  }

  /**
   * Checks for overflow. This method is used internally to check
   * whether a buffer has enough space left to satisfy a write 
   * request for a given number of elements.
   *
   * @param length The length of a sequence of elements.
   *
   * @exception BufferUnderflowException If there is not enough 
   * remaining space in this buffer.
   */
  final void checkForOverflow(int length)
  {
    if (remaining() < length)
      throw new BufferOverflowException();
  }

  /**
   * Checks if index is negative or not smaller than the buffer's 
   * limit. This method is used internally to check whether
   * an indexed request can be fulfilled.
   *
   * @param index The requested position in the buffer.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  final void checkIndex(int index)
  {
    if (index < 0
        || index >= limit ())
      throw new IndexOutOfBoundsException ();
  }

  /**
   * Checks if buffer is read-only. This method is used internally to
   * check if elements can be put into a buffer.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  final void checkIfReadOnly() 
  {
    if (isReadOnly())
      throw new ReadOnlyBufferException ();
  }

  /**
   * Checks whether an array is large enough to hold the given number of
   * elements at the given offset. This method is used internally to
   * check if an array is big enough.
   *
   * @param arraylength The length of the array.
   * @param offset The offset within the array of the first byte to be read;
   * must be non-negative and no larger than arraylength.
   * @param length The number of bytes to be read from the given array;
   * must be non-negative and no larger than arraylength - offset.
   *
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  static final void checkArraySize(int arraylength, int offset, int length)
  {
    if ((offset < 0) ||
        (length < 0) ||
        (arraylength < length + offset))
      throw new IndexOutOfBoundsException ();
  }
}
