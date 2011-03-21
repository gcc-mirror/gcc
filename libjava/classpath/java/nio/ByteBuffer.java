/* ByteBuffer.java --
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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

// GCJ LOCAL: Change gnu.classpath.Pointer to RawData
import gnu.gcj.RawData;
import gnu.classpath.Pointer;

/**
 * @since 1.4
 */
public abstract class ByteBuffer extends Buffer
  implements Comparable<ByteBuffer>
{
  ByteOrder endian = ByteOrder.BIG_ENDIAN;
  final byte[] backing_buffer;
  final int array_offset;

  ByteBuffer (int capacity, int limit, int position, int mark,
              RawData address, byte[] backing_buffer, int array_offset)
  {
    super (capacity, limit, position, mark, address);
    this.backing_buffer = backing_buffer;
    this.array_offset = array_offset;
  }

  /**
   * Allocates a new direct byte buffer.
   */
  public static ByteBuffer allocateDirect (int capacity)
  {
    return DirectByteBufferImpl.allocate (capacity);
  }

  /**
   * Allocates a new <code>ByteBuffer</code> object with a given capacity.
   */
  public static ByteBuffer allocate (int capacity)
  {
    return wrap(new byte[capacity], 0, capacity);
  }

  /**
   * Wraps a <code>byte</code> array into a <code>ByteBuffer</code>
   * object.
   *
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  public static final ByteBuffer wrap (byte[] array, int offset, int length)
  {
    // FIXME: In GCJ and other implementations where arrays may not
    // move we might consider, at least when offset==0:
    // return new DirectByteBufferImpl(array,
    //                                 address_of_data(array) + offset,
    //                                 length, length, 0, false);
    // This may be more efficient, mainly because we can then use the
    // same logic for all ByteBuffers.

    return new ByteBufferImpl (array, 0, array.length, offset + length, offset, -1, false);
  }

  /**
   * Wraps a <code>byte</code> array into a <code>ByteBuffer</code>
   * object.
   */
  public static final ByteBuffer wrap (byte[] array)
  {
    return wrap (array, 0, array.length);
  }

  /**
   * This method transfers <code>byte</code>s from this buffer into the given
   * destination array. Before the transfer, it checks if there are fewer than
   * length <code>byte</code>s remaining in this buffer.
   *
   * @param dst The destination array
   * @param offset The offset within the array of the first <code>byte</code>
   * to be written; must be non-negative and no larger than dst.length.
   * @param length The maximum number of bytes to be written to the given array;
   * must be non-negative and no larger than dst.length - offset.
   *
   * @exception BufferUnderflowException If there are fewer than length
   * <code>byte</code>s remaining in this buffer.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold.
   */
  public ByteBuffer get (byte[] dst, int offset, int length)
  {
    checkArraySize(dst.length, offset, length);
    checkForUnderflow(length);

    for (int i = offset; i < offset + length; i++)
      {
        dst [i] = get ();
      }

    return this;
  }

  /**
   * This method transfers <code>byte</code>s from this buffer into the given
   * destination array.
   *
   * @param dst The byte array to write into.
   *
   * @exception BufferUnderflowException If there are fewer than dst.length
   * <code>byte</code>s remaining in this buffer.
   */
  public ByteBuffer get (byte[] dst)
  {
    return get (dst, 0, dst.length);
  }

  /**
   * Writes the content of the the <code>ByteBUFFER</code> src
   * into the buffer. Before the transfer, it checks if there is fewer than
   * <code>src.remaining()</code> space remaining in this buffer.
   *
   * @param src The source data.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>byte</code>s in the source buffer.
   * @exception IllegalArgumentException If the source buffer is this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public ByteBuffer put (ByteBuffer src)
  {
    if (src == this)
      throw new IllegalArgumentException ();

    checkForOverflow(src.remaining());

    if (src.remaining () > 0)
      {
        byte[] toPut = new byte [src.remaining ()];
        src.get (toPut);
        put (toPut);
      }

    return this;
  }

  /**
   * Writes the content of the the <code>byte array</code> src
   * into the buffer. Before the transfer, it checks if there is fewer than
   * length space remaining in this buffer.
   *
   * @param src The array to copy into the buffer.
   * @param offset The offset within the array of the first byte to be read;
   * must be non-negative and no larger than src.length.
   * @param length The number of bytes to be read from the given array;
   * must be non-negative and no larger than src.length - offset.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>byte</code>s in the source array.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public ByteBuffer put (byte[] src, int offset, int length)
  {
    checkArraySize(src.length, offset, length);
    checkForOverflow(length);

    for (int i = offset; i < offset + length; i++)
      put (src [i]);

    return this;
  }

  /**
   * Writes the content of the the <code>byte array</code> src
   * into the buffer.
   *
   * @param src The array to copy into the buffer.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining <code>byte</code>s in the source array.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public final ByteBuffer put (byte[] src)
  {
    return put (src, 0, src.length);
  }

  /**
   * Tells whether ot not this buffer is backed by an accessible
   * <code>byte</code> array.
   */
  public final boolean hasArray ()
  {
    return (backing_buffer != null
            && !isReadOnly ());
  }

  /**
   * Returns the <code>byte</code> array that backs this buffer.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final byte[] array ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    checkIfReadOnly();

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

    checkIfReadOnly();

    return array_offset;
  }

  /**
   * Calculates a hash code for this buffer.
   *
   * This is done with <code>int</code> arithmetic,
   * where ** represents exponentiation, by this formula:<br>
   * <code>s[position()] + 31 + (s[position()+1] + 30)*31**1 + ... +
   * (s[limit()-1]+30)*31**(limit()-1)</code>.
   * Where s is the buffer data. Note that the hashcode is dependent
   * on buffer content, and therefore is not useful if the buffer
   * content may change.
   *
   * @return the hash code
   */
  public int hashCode ()
  {
    int hashCode = get(position()) + 31;
    int multiplier = 1;
    for (int i = position() + 1; i < limit(); ++i)
      {
          multiplier *= 31;
          hashCode += (get(i) + 30)*multiplier;
      }
    return hashCode;
  }

  /**
   * Checks if this buffer is equal to obj.
   */
  public boolean equals (Object obj)
  {
    if (obj instanceof ByteBuffer)
      {
        return compareTo ((ByteBuffer) obj) == 0;
      }

    return false;
  }

  /**
   * Compares two <code>ByteBuffer</code> objects.
   *
   * @exception ClassCastException If obj is not an object derived from
   * <code>ByteBuffer</code>.
   */
  public int compareTo (ByteBuffer other)
  {
    int num = Math.min(remaining(), other.remaining());
    int pos_this = position();
    int pos_other = other.position();

    for (int count = 0; count < num; count++)
      {
        byte a = get(pos_this++);
        byte b = other.get(pos_other++);

        if (a == b)
          continue;

        if (a < b)
          return -1;

        return 1;
      }

    return remaining() - other.remaining();
  }

  /**
   * Returns the byte order of this buffer.
   */
  public final ByteOrder order ()
  {
    return endian;
  }

  /**
   * Modifies this buffer's byte order.
   */
  public final ByteBuffer order (ByteOrder endian)
  {
    this.endian = endian;
    return this;
  }

  /**
   * Reads the <code>byte</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferUnderflowException If there are no remaining
   * <code>byte</code>s in this buffer.
   */
  public abstract byte get ();

  /**
   * Writes the <code>byte</code> at this buffer's current position,
   * and then increments the position.
   *
   * @exception BufferOverflowException If there no remaining
   * <code>byte</code>s in this buffer.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract ByteBuffer put (byte b);

  /**
   * Absolute get method.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   */
  public abstract byte get (int index);

  /**
   * Absolute put method.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract ByteBuffer put (int index, byte b);

  /**
   * Compacts this buffer.
   *
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract ByteBuffer compact ();

  void shiftDown (int dst_offset, int src_offset, int count)
  {
    for (int i = 0; i < count; i++)
      put(dst_offset + i, get(src_offset + i));
  }

  /**
   * Tells whether or not this buffer is direct.
   */
  public abstract boolean isDirect ();

  /**
   * Creates a new <code>ByteBuffer</code> whose content is a shared
   * subsequence of this buffer's content.
   */
  public abstract ByteBuffer slice ();

  /**
   * Creates a new <code>ByteBuffer</code> that shares this buffer's
   * content.
   */
  public abstract ByteBuffer duplicate ();

  /**
   * Creates a new read-only <code>ByteBuffer</code> that shares this
   * buffer's content.
   */
  public abstract ByteBuffer asReadOnlyBuffer ();

  /**
   * Creates a view of this byte buffer as a short buffer.
   */
  public abstract ShortBuffer asShortBuffer ();

  /**
   * Creates a view of this byte buffer as a char buffer.
   */
  public abstract CharBuffer asCharBuffer ();

  /**
   * Creates a view of this byte buffer as an integer buffer.
   */
  public abstract IntBuffer asIntBuffer ();

  /**
   * Creates a view of this byte buffer as a long buffer.
   */
  public abstract LongBuffer asLongBuffer ();

  /**
   * Creates a view of this byte buffer as a float buffer.
   */
  public abstract FloatBuffer asFloatBuffer ();

  /**
   * Creates a view of this byte buffer as a double buffer.
   */
  public abstract DoubleBuffer asDoubleBuffer ();

  /**
   * Relative get method for reading a character value.
   *
   * @exception BufferUnderflowException  If there are fewer than two bytes
   * remaining in this buffer.
   */
  public abstract char getChar ();

  /**
   * Relative put method for writing a character value.
   *
   * @exception BufferOverflowException If this buffer's current position is
   * not smaller than its limit.
   */
  public abstract ByteBuffer putChar (char value);

  /**
   * Absolute get method for reading a character value.
   *
   * @exception IndexOutOfBoundsException If there are fewer than two bytes
   * remaining in this buffer
   */
  public abstract char getChar (int index);

  /**
   * Absolute put method for writing a character value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus one.
   */
  public abstract ByteBuffer putChar (int index, char value);

  /**
   * Relative get method for reading a short value.
   *
   * @exception BufferUnderflowException If index is negative or not smaller
   * than the buffer's limit, minus one.
   */
  public abstract short getShort ();

  /**
   * Relative put method for writing a short value.
   *
   * @exception BufferOverflowException If this buffer's current position is
   * not smaller than its limit.
   */
  public abstract ByteBuffer putShort (short value);

  /**
   * Absolute get method for reading a short value.
   *
   * @exception IndexOutOfBoundsException If there are fewer than two bytes
   * remaining in this buffer
   */
  public abstract short getShort (int index);

  /**
   * Absolute put method for writing a short value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus one.
   */
  public abstract ByteBuffer putShort (int index, short value);

  /**
   * Relative get method for reading an integer value.
   *
   * @exception BufferUnderflowException If there are fewer than four bytes
   * remaining in this buffer.
   */
  public abstract int getInt ();

  /**
   * Relative put method for writing an integer value.
   *
   * @exception BufferOverflowException If this buffer's current position is
   * not smaller than its limit.
   */
  public abstract ByteBuffer putInt (int value);

  /**
   * Absolute get method for reading an integer value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus three.
   */
  public abstract int getInt (int index);

  /**
   * Absolute put method for writing an integer value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus three.
   */
  public abstract ByteBuffer putInt (int index, int value);

  /**
   * Relative get method for reading a long value.
   *
   * @exception BufferUnderflowException If there are fewer than eight bytes
   * remaining in this buffer.
   */
  public abstract long getLong ();

  /**
   * Relative put method for writing a long value.
   *
   * @exception BufferOverflowException If this buffer's current position is
   * not smaller than its limit.
   */
  public abstract ByteBuffer putLong (long value);

  /**
   * Absolute get method for reading a long value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus seven.
   */
  public abstract long getLong (int index);

  /**
   * Absolute put method for writing a float value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus seven.
   */
  public abstract ByteBuffer putLong (int index, long value);

  /**
   * Relative get method for reading a float value.
   *
   * @exception BufferUnderflowException If there are fewer than four bytes
   * remaining in this buffer.
   */
  public abstract float getFloat ();

  /**
   * Relative put method for writing a float value.
   *
   * @exception BufferOverflowException If there are fewer than four bytes
   * remaining in this buffer.
   */
  public abstract ByteBuffer putFloat (float value);

  /**
   * Absolute get method for reading a float value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus three.
   */
  public abstract float getFloat (int index);

  /**
   * Relative put method for writing a float value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus three.
   */
  public abstract ByteBuffer putFloat (int index, float value);

  /**
   * Relative get method for reading a double value.
   *
   * @exception BufferUnderflowException If there are fewer than eight bytes
   * remaining in this buffer.
   */
  public abstract double getDouble ();

  /**
   * Relative put method for writing a double value.
   *
   * @exception BufferOverflowException If this buffer's current position is
   * not smaller than its limit.
   */
  public abstract ByteBuffer putDouble (double value);

  /**
   * Absolute get method for reading a double value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus seven.
   */
  public abstract double getDouble (int index);

  /**
   * Absolute put method for writing a double value.
   *
   * @exception IndexOutOfBoundsException If index is negative or not smaller
   * than the buffer's limit, minus seven.
   */
  public abstract ByteBuffer putDouble (int index, double value);

  /**
   * Returns a string summarizing the state of this buffer.
   */
  public String toString ()
  {
    return getClass ().getName () +
            "[pos=" + position () +
            " lim=" + limit () +
            " cap=" + capacity () + "]";
  }
}
