/* Buffer.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio;

public abstract class Buffer
{
  private int cap = 0;
  private int limit = 0;
  private int pos = 0;
  private int mark = -1;

  // Creates a new Buffer.
  //
  // Should be package private.
  //
  Buffer (int capacity, int limit, int position, int mark)
  {
    if (capacity < 0)
      throw new IllegalArgumentException ();
    
    cap = capacity;
    limit (limit);
    position (position);
    
    if (mark > 0)
    {
      if (mark > pos)
        throw new IllegalArgumentException ();
      
      this.mark = mark;
    }
  }
  
  /**
   * Retrieves the capacity of the buffer.
   */
  public final int capacity ()
  {
    return cap;
  }

  /**
   * Clears the buffer.
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
   */
  public final boolean hasRemaining ()
  {
    return limit > pos;
  }

  /**
   * Tells whether this buffer is read only or not.
   */
  public abstract boolean isReadOnly ();

  /**
   * Retrieves the current limit of the buffer.
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
   * @exception IllegalArgumentException If the preconditions on newLimit
   * do not hold.
   */
  public final Buffer limit (int newLimit)
  {
    if ((newLimit < 0) || (newLimit > cap))
      throw new IllegalArgumentException ();

    if (newLimit <= mark)
        mark = -1;

    if (pos > newLimit)
        pos = newLimit - 1;

    limit = newLimit;
    return this;
  }

  /**
   * Sets this buffer's mark at its position.
   */
  public final Buffer mark ()
  {
    mark = pos;
    return this;
  }

  /**
   * Retrieves the current position of this buffer.
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
   */
  public final int remaining()
  {
    return limit - pos;
  }

  /**
   * Resets this buffer's position to the previously-marked position.
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
   */
  public final Buffer rewind()
  {
    pos = 0;
    mark = -1;
    return this;
  }
}
