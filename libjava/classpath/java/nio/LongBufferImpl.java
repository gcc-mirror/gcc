/* LongBufferImpl.java --
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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

/**
 * This is a Heap memory implementation
 */
final class LongBufferImpl extends LongBuffer
{
  private final boolean readOnly;

  LongBufferImpl (int capacity)
  {
    this (new long [capacity], 0, capacity, capacity, 0, -1, false);
  }

  LongBufferImpl (long[] buffer, int offset, int capacity, int limit,
                  int position, int mark, boolean readOnly)
  {
    super (capacity, limit, position, mark, null, buffer, offset);
    this.readOnly = readOnly;
  }

  public boolean isReadOnly ()
  {
    return readOnly;
  }

  public LongBuffer slice ()
  {
    return new LongBufferImpl (backing_buffer, array_offset + position (),
                               remaining (), remaining (), 0, -1, isReadOnly ());
  }

  public LongBuffer duplicate ()
  {
    return new LongBufferImpl (backing_buffer, array_offset, capacity (), limit (),
                               position (), mark, isReadOnly ());
  }

  public LongBuffer asReadOnlyBuffer ()
  {
    return new LongBufferImpl (backing_buffer, array_offset, capacity (), limit (),
                               position (), mark, true);
  }

  public LongBuffer compact ()
  {
    checkIfReadOnly();
    mark = -1;
    int p = position();
    int n = limit() - p;
    if (n > 0)
      {
        System.arraycopy(backing_buffer, array_offset + p,
                         backing_buffer, array_offset, n);
      }
    position(n);
    limit(capacity());
    return this;
  }

  public boolean isDirect ()
  {
    return false;
  }

  /**
   * Reads the <code>long</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferUnderflowException If there are no remaining
   * <code>longs</code> in this buffer.
   */
  public long get ()
  {
    checkForUnderflow();

    long result = backing_buffer [position ()];
    position (position () + 1);
    return result;
  }

  /**
   * Relative put method. Writes <code>value</code> to the next position
   * in the buffer.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public LongBuffer put (long value)
  {
    checkIfReadOnly();
    checkForOverflow();

    backing_buffer [position ()] = value;
    position (position () + 1);
    return this;
  }

  /**
   * Absolute get method. Reads the <code>long</code> at position
   * <code>index</code>.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  public long get (int index)
  {
    checkIndex(index);

    return backing_buffer [index];
  }

  /**
   * Absolute put method. Writes <code>value</code> to position
   * <code>index</code> in the buffer.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public LongBuffer put (int index, long value)
  {
    checkIfReadOnly();
    checkIndex(index);

    backing_buffer [index] = value;
    return this;
  }

  public ByteOrder order ()
  {
    return ByteOrder.nativeOrder ();
  }
}
