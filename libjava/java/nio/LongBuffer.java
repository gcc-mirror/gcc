/* LongBuffer.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

/**
 * @since 1.4
 */
public abstract class LongBuffer extends Buffer
  implements Comparable
{
  int array_offset;
  long[] backing_buffer;

  LongBuffer (int capacity, int limit, int position, int mark)
  {
    super (capacity, limit, position, mark);
    array_offset = 0;
  }

  LongBuffer (long[] buffer, int offset, int capacity, int limit, int position, int mark)
  {
    super (capacity, limit, position, mark);
    this.backing_buffer = buffer;
    this.array_offset = offset;
  }

  /**
   * Allocates a new <code>LongBuffer</code> object with a given capacity.
   */
  public static LongBuffer allocate (int capacity)
  {
    return new LongBufferImpl (capacity);
  }

  /**
   * Wraps a <code>long</code> array into a <code>LongBuffer</code>
   * object.
   *
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public static LongBuffer wrap (long[] array, int offset, int length)
  {
    return new LongBufferImpl (array, 0, array.length, offset + length, offset, -1, false);
  }

  /**
   * Wraps a <code>long</code> array into a <code>LongBuffer</code>
   * object.
   */
  final public static LongBuffer wrap (long[] array)
  {
    return wrap (array, 0, array.length);
  }
  
  /**
   * This method transfers <code>longs<code> from this buffer into the given
   * destination array.
   *
   * @param dst The destination array
   * @param offset The offset within the array of the first <code>long</code>
   * to be written; must be non-negative and no larger than dst.length.
   * @param length The maximum number of bytes to be written to the given array;
   * must be non-negative and no larger than dst.length - offset.
   *
   * @exception BufferUnderflowException If there are fewer than length
   * <code>longs</code> remaining in this buffer.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold.
   */
  public LongBuffer get (long[] dst, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      {
        dst [i] = get ();
      }

    return this;
  }

  /**
   * This method transfers <code>longs<code> from this buffer into the given
   * destination array.
   *
   * @param dst The byte array to write into.
   *
   * @exception BufferUnderflowException If there are fewer than dst.length
   * <code>longs</code> remaining in this buffer.
   */
  public LongBuffer get (long[] dst)
  {
    return get (dst, 0, dst.length);
  }

  /**
   * Writes the content of the the <code>LongBUFFER</code> src
   * into the buffer.
   *
   * @param src The source data.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>longs<code> in the source buffer.
   * @exception IllegalArgumentException If the source buffer is this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public LongBuffer put (LongBuffer src)
  {
    if (src == this)
      throw new IllegalArgumentException ();

    if (src.remaining () > remaining ())
      throw new BufferOverflowException ();

    if (src.remaining () > 0)
      {
        long[] toPut = new long [src.remaining ()];
        src.get (toPut);
        src.put (toPut);
      }

    return this;
  }

  /**
   * Writes the content of the the <code>long array</code> src
   * into the buffer.
   *
   * @param src The array to copy into the buffer.
   * @param offset The offset within the array of the first byte to be read;
   * must be non-negative and no larger than src.length.
   * @param length The number of bytes to be read from the given array;
   * must be non-negative and no larger than src.length - offset.
   * 
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>longs<code> in the source array.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public LongBuffer put (long[] src, int offset, int length)
  {
    for (int i = offset; i < offset + length; i++)
      put (src [i]);

    return this;
  }

  /**
   * Writes the content of the the <code>long array</code> src
   * into the buffer.
   *
   * @param src The array to copy into the buffer.
   * 
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>longs<code> in the source array.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public final LongBuffer put (long[] src)
  {
    return put (src, 0, src.length);
  }

  /**
   * Tells whether ot not this buffer is backed by an accessible
   * <code>long</code> array.
   */
  public final boolean hasArray ()
  {
    return (backing_buffer != null
            && !isReadOnly ());
  }

  /**
   * Returns the <code>long</code> array that backs this buffer.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final long[] array ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    if (isReadOnly ())
      throw new ReadOnlyBufferException ();
    
    return backing_buffer;
  }

  /**
   * Returns the offset within this buffer's backing array of the first element.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final int arrayOffset ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    if (isReadOnly ())
      throw new ReadOnlyBufferException ();
    
    return array_offset;
  }

  /**
   * Calculates a hash code for this buffer.
   */
  public int hashCode ()
  {
    // FIXME: Check what SUN calculates here.
    return super.hashCode ();
  }

  /**
   * Checks if this buffer is equal to obj.
   */
  public boolean equals (Object obj)
  {
    if (obj instanceof LongBuffer)
      {
        return compareTo (obj) == 0;
      }

    return false;
  }

  /**
   * Compares two <code>LongBuffer</code> objects.
   *
   * @exception ClassCastException If obj is not an object derived from
   * <code>LongBuffer</code>.
   */
  public int compareTo (Object obj)
  {
    LongBuffer a = (LongBuffer) obj;

    if (a.remaining () != remaining ())
      return 1;

    if (! hasArray () ||
        ! a.hasArray ())
      {
        return 1;
      }

    int r = remaining ();
    int i1 = position ();
    int i2 = a.position ();

    for (int i = 0; i < r; i++)
      {
        int t = (int) (get (i1) - a.get (i2));

        if (t != 0)
          {
            return (int) t;
          }
      }

    return 0;
  }

  /**
   * Returns the byte order of this buffer.
   */
  public abstract ByteOrder order ();

  /**
   * Reads the <code>long</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferUnderflowException If there are no remaining
   * <code>longs</code> in this buffer.
   */
  public abstract long get ();

  /**
   * Writes the <code>long</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferOverflowException If there no remaining 
   * <code>longs</code> in this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract LongBuffer put (long b);

  /**
   * Absolute get method.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  public abstract long get (int index);
  
  /**
   * Absolute put method.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract LongBuffer put (int index, long b);

  /**
   * Compacts this buffer.
   * 
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract LongBuffer compact ();

  /**
   * Tells wether or not this buffer is direct.
   */
  public abstract boolean isDirect ();

  /**
   * Creates a new <code>LongBuffer</code> whose content is a shared
   * subsequence of this buffer's content.
   */
  public abstract LongBuffer slice ();

  /**
   * Creates a new <code>LongBuffer</code> that shares this buffer's
   * content.
   */
  public abstract LongBuffer duplicate ();

  /**
   * Creates a new read-only <code>LongBuffer</code> that shares this
   * buffer's content.
   */
  public abstract LongBuffer asReadOnlyBuffer ();
}
