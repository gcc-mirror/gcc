/* ByteBuffer.java -- 
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

import gnu.java.nio.ByteBufferImpl;

/**
 * @since 1.4
 */
public abstract class ByteBuffer extends Buffer implements Comparable
{
  int offset;
  byte[] backing_buffer;
  
  /**
   * Allocates a new direct byte buffer.
   */ 
  public static ByteBuffer allocateDirect (int capacity)
  {
    throw new Error ("direct buffers are not implemented");
  }

  /**
   * Allocates a new byte buffer.
   */
  public static ByteBuffer allocate (int capacity)
  {
    return new ByteBufferImpl (capacity, 0, capacity);
  }
 
  /**
   * Wraps a byte array into a buffer.
   * 
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold
   */
  final public static ByteBuffer wrap (byte[] array, int offset, int length)
  {
    return new ByteBufferImpl (array, offset, length);
  }

  /**
   * Wraps a byte array into a buffer.
   */
  final public static ByteBuffer wrap (byte[] array)
  {
    return wrap (array, 0, array.length);
  }

  ByteBuffer (int capacity, int limit, int position, int mark)
  {
    super (capacity, limit, position, mark);
  }

  /**
   * Writes the content of src into the buffer.
   *
   * @param src The source data.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining bytes in the source buffer.
   * @exception IllegalArgumentException If the source buffer is this buffer.
   * @exception ReadOnlyBufferException If this buffer is read only.
   */
  public ByteBuffer put (ByteBuffer src)
  {
    if (src == this)
      throw new IllegalArgumentException ();

    while (src.hasRemaining ())
      put (src.get ());
    
    return this;
  }

  /**
   * Writes the content of the the array src into the buffer.
   *
   * @param src The array to copy into the buffer.
   * @param offset The offset within the array of the first byte to be read;
   * must be non-negative and no larger than src.length.
   * @param length The number of bytes to be read from the given array;
   * must be non-negative and no larger than src.length - offset.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining bytes in the source buffer.
   * @exception IndexOutOfBoundsException If the preconditions on the offset
   * and length parameters do not hold.
   * @exception ReadOnlyBufferException If this buffer is read only.
   */
  public ByteBuffer put (byte[] src, int offset, int length)
  {
    if ((offset < 0) ||
        (offset > src.length) ||
        (length < 0) ||
        (length > src.length - offset))
      throw new IndexOutOfBoundsException ();

    for (int i = offset; i < offset + length; i++)
      put (src [i]);
    
    return this;
  }

  /**
   * Writes the content of the the array src into the buffer.
   *
   * @param src The array to copy into the buffer.
   *
   * @exception BufferOverflowException If there is insufficient space in this
   * buffer for the remaining bytes in the source buffer.
   * @exception ReadOnlyBufferException If this buffer is read only.
   */
  public final ByteBuffer put (byte[] src)
  {
    return put (src, 0, src.length);
  }

  /**
   * Tells whether or not this buffer is backed by an accessible byte array.
   */
  public final boolean hasArray ()
  {
    return (backing_buffer != null
             && !isReadOnly ());
  }

  /**
   * Returns the byte array that backs this buffer.
   *
   * @exception ReadOnlyBufferException If this buffer is backed by an array
   * but is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final byte[] array ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    if (isReadOnly ())
      throw new ReadOnlyBufferException ();

    return backing_buffer;
  }

  /**
   * Returns the offset within this buffer's backing array of the first element
   * of the buffer  
   *
   * @exception ReadOnlyBufferException If this buffer is backed by an array
   * but is read-only.
   * @exception UnsupportedOperationException If this buffer is not backed
   * by an accessible array.
   */
  public final int arrayOffset ()
  {
    if (backing_buffer == null)
      throw new UnsupportedOperationException ();

    if (isReadOnly ())
      throw new ReadOnlyBufferException ();

    return offset;
  }
  
  /**
   * Relative get method.
   */
  public abstract byte get ();
  
  /**
   * Relative put method.
   *
   * @exception BufferOverflowException If this buffer's current position is
   * not smaller than its limit.
   * @exception ReadOnlyBufferException If this buffer is read-only.
   */
  public abstract ByteBuffer put (byte b);
}
